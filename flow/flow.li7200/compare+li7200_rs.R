# Compare a fluxes for the same day

compare.stream = function(stream, interval = "01", test.week = "1", Percentages = TRUE){
  # Libraries...
  require(rhdf5)
  require(ggplot2)
  require(dplyr)
  library(tidyr)
  require(reshape2)
  
  # Base Directory of this experiment
  base.dir = "~/eddy/neon-sensor-test/data/2021_Li7200_RS_Comparision/"
  
  # List files in both folder
  day_range_dt = data.table::data.table(
    "file" = base::list.files(base.dir, recursive = TRUE)
  ) %>% 
    tidyr::separate(col = file, sep = "/", into = c("Sensor", "file_name")) %>% 
    dplyr::mutate(date = base::substr(file_name, 34, 43)) %>% 
    dplyr::select(Sensor, date) %>% 
    reshape2::dcast(date ~ Sensor, value.var = "date") %>% 
    dplyr::select(date)
  
  day_range = day_range_dt$date
  
  week_1 = day_range_dt %>% dplyr::filter(date <= "2021-05-01")
  week_2 = day_range_dt %>% dplyr::filter(date >= "2021-05-07")
  
  # Empty table to join to in for loop
  Li7200.join = data.table::data.table()
  
  #############################################  Pressure Comparison ##############################################
  
  if(stream %in% c("pres")){
    message("Grabbing Pressure Comparison")
    
    for(day in day_range){
      day = as.Date(day, origin = "1970-01-01")
      # Set Dyanmic File Names for A and B sensors... 
      Li7200A.file = paste0(base.dir, "Li7200/NEON.D10.HQTW.IP4.00200.001.ecte.", day,".expanded.h5")
      Li7200B.file = paste0(base.dir, "Li7200RS/NEON.D10.HQTW.IP4.00200.001.ecte.", day,".expanded.h5")
      
      # Read in A and B sensors data
      # PresSum
      Li7200A.presSum.data = rhdf5::h5read(file = Li7200A.file, name = paste0("/HQTW/dp01/data/co2Turb/000_040_",interval,"m/", stream, "Sum")) %>%
        dplyr::mutate(streamID = "presSum") %>%
        dplyr::mutate(sensorID = "Li7200 (regular)") 
      Li7200B.presSum.data = rhdf5::h5read(file = Li7200B.file, name = paste0("/HQTW/dp01/data/co2Turb/000_040_",interval,"m/", stream, "Sum")) %>%
        dplyr::mutate(streamID = "presSum") %>%
        dplyr::mutate(sensorID = "Li7200 (RS)")
      # PresAtm
      Li7200A.presAtm.data = rhdf5::h5read(file = Li7200A.file, name = paste0("/HQTW/dp01/data/co2Turb/000_040_",interval,"m/", stream, "Atm")) %>%
        dplyr::mutate(streamID = "presAtm") %>%
        dplyr::mutate(sensorID = "Li7200 (regular)") 
      Li7200B.presAtm.data = rhdf5::h5read(file = Li7200B.file, name = paste0("/HQTW/dp01/data/co2Turb/000_040_",interval,"m/", stream, "Atm")) %>%
        dplyr::mutate(streamID = "presAtm") %>%
        dplyr::mutate(sensorID = "Li7200 (RS)")
      
      # Read in units
      Li7200.units.Atm = rhdf5::h5readAttributes(file = Li7200A.file, name = paste0("/HQTW/dp01/data/co2Turb/000_040_",interval,"m/", stream, "Atm"))$unit[1]
      
      # Join all the pres data together
      Li7200.pres.join = data.table::rbindlist(l = list(Li7200A.presSum.data, Li7200B.presSum.data, Li7200A.presAtm.data, Li7200B.presAtm.data))%>%
        dplyr::mutate(timeBgn = as.POSIXct(lubridate::ymd_hms(timeBgn))) %>%
        dplyr::mutate(timeEnd = as.POSIXct(lubridate::ymd_hms(timeEnd))) %>%
        dplyr::mutate(units = Li7200.units.Atm)
      
      # Join the days data to the repeating join data.table
      Li7200.join = data.table::rbindlist(l = list(Li7200.join, Li7200.pres.join))
    }
    
    Li7200.join = Li7200.join %>%
      dplyr::mutate(day = as.Date(timeEnd))
    
    Li7200.dcast = Li7200.join %>%
      reshape2::dcast(timeBgn + timeEnd + units + sensorID ~ streamID, value.var = "mean")  %>%
      dplyr::mutate(difference = presSum - presAtm)  
    
    Li7200.stat.dcast = Li7200.join %>%
      reshape2::dcast(timeBgn + timeEnd + streamID ~ sensorID, value.var = "mean") 
    
    Li7200.stat.dcast.presAtm = Li7200.stat.dcast %>%
      dplyr::filter(streamID == "presAtm")
    Li7200.stat.dcast.presSum = Li7200.stat.dcast %>%
      dplyr::filter(streamID == "presSum")
    
    Li7200.mad.stats.presAtm = data.table::data.table(eddy4R.base::def.med.mad(test = Li7200.stat.dcast.presAtm$`Li7200 (regular)`, refe = Li7200.stat.dcast.presAtm$`Li7200 (RS)`, Perc = Percentages)) %>%
      dplyr::mutate(streamID = "presAtm")
    Li7200.mad.stats.presSum = data.table::data.table(eddy4R.base::def.med.mad(test = Li7200.stat.dcast.presSum$`Li7200 (regular)`, refe = Li7200.stat.dcast.presSum$`Li7200 (RS)`, Perc = Percentages))%>%
      dplyr::mutate(streamID = "presSum")
    
    Li7200.mad.stats = data.table::rbindlist(l = list(Li7200.mad.stats.presAtm, Li7200.mad.stats.presSum)) %>%
      dplyr::select(streamID, med, mad, NumSamp)
    
    # Add units depending on what Percentage Variable is chosen
    if(Percentages == TRUE){
      Li7200.mad.stats = Li7200.mad.stats %>% 
        dplyr::mutate(units = "%")
    } 
    else {
      Li7200.mad.stats = Li7200.mad.stats %>%
        dplyr::mutate(units = Li7200.dcast$units[1])
    }
    
    Li7200.rmsd.stats.presAtm = data.table::data.table(eddy4R.base::def.rmsd.diff.prcs.rsq(test = Li7200.stat.dcast.presAtm$`Li7200 (regular)`, refe = Li7200.stat.dcast.presAtm$`Li7200 (RS)`, Perc = Percentages)) %>%
      dplyr::mutate(streamID = "presAtm")
    Li7200.rmsd.stats.presSum = data.table::data.table(eddy4R.base::def.rmsd.diff.prcs.rsq(test = Li7200.stat.dcast.presSum$`Li7200 (regular)`, refe = Li7200.stat.dcast.presSum$`Li7200 (RS)`, Perc = Percentages)) %>%
      dplyr::mutate(streamID = "presSum")
    
    Li7200.rmsd.stats = data.table::rbindlist(l = list(Li7200.rmsd.stats.presAtm, Li7200.rmsd.stats.presSum)) 
    
    # Add units depending on what Percentage Variable is chosen
    if(Percentages == TRUE){
      names(Li7200.rmsd.stats) = c("rmsd","diffMean", "prcs", "rsq", "samp", "streamID")
      Li7200.rmsd.stats = Li7200.rmsd.stats %>%
        dplyr::mutate(units = "%") %>%
        dplyr::select(streamID, units, `rmsd`, `diffMean`, `prcs`, rsq, samp)
      
    } 
    else {
      names(Li7200.rmsd.stats) = c("rmsd","diffMean", "prcs", "rsq", "samp", "streamID")
      Li7200.rmsd.stats = Li7200.rmsd.stats %>%
        dplyr::mutate(units = Li7200.dcast$units[1]) %>%
        dplyr::select(streamID, units, `rmsd`, `diffMean`, `prcs`, rsq, samp)
    }
    
    plot1 = ggplot(Li7200.join, aes(x = timeEnd, y = mean, linetype = streamID, color = sensorID)) +
      geom_line(alpha = 1, size = 1) +
      scale_x_datetime(date_breaks = "1 day", date_labels = paste0("%Y\n%m-%d"))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), sec.axis = dup_axis(name = "")) +
      scale_color_manual(values = c("#1f78b4", "#1b9e77","firebrick", "#004000")) +
      labs(title = "Time-series of Ambient Air Pressure (presAtm) vs Instantaneous Cell Air Pressure\n(presSum)",
           x = "", y = "Pressure (kPa)", color = "Stream")+
      theme(legend.position = "top", text = element_text(size = 16))+ 
      guides(colour = guide_legend(override.aes = list(size=4, alpha = 1))) +
      facet_wrap(~sensorID, scales = "free_x")
    
    plot2 = ggplot(Li7200.dcast, aes(x = timeEnd, y = difference, color = sensorID)) +
      geom_line(alpha = .7) +
      scale_x_datetime(date_breaks = "1 day", date_labels = paste0("%Y\n%m-%d"))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), sec.axis = dup_axis(name = "")) +
      scale_color_manual(values = c("#1f78b4", "#1b9e77","firebrick", "#004000")) +
      labs(title = "Time-series of Raw Differences Between Ambient Air Pressure (presAtm) and Instantaneous Cell Air Pressure (presSum)",
           y = "Pressure Difference (kPa)", x = "", color = "Sensor Configuration")+
      guides(colour = guide_legend(override.aes = list(size=4, alpha = 1))) +
      theme(legend.position = "top", text = element_text(size = 16))
    
    
    # Return the data and the plot :D
    return(list(Li7200.dcast, plot1, plot2,Li7200.mad.stats,Li7200.rmsd.stats))
    
  } else if(stream %in% c("frt00Samp", "rtioMoleDryCo2Raw", "rtioMoleDryH2oRaw")){
    
    #############################################  frt00Samp Comparison         ######################################
    #############################################  rtioMoleDryCo2Raw Comparison ######################################
    
    message(paste0("Grabbing ", stream ," Comparison"))
    
    for(day in day_range){
      day = as.Date(day, origin = "1970-01-01")
      # Set Dyanmic File Names for A and B sensors... 
      Li7200A.file = paste0(base.dir, "Li7200/NEON.D10.HQTW.IP4.00200.001.ecte.", day,".expanded.h5")
      Li7200B.file = paste0(base.dir, "Li7200RS/NEON.D10.HQTW.IP4.00200.001.ecte.", day,".expanded.h5")
      
      # Create variable that determines what week the test file occured durring
      if(day < "2021-05-03"){
        phase_insert = "Phase 1"
      } else {
        phase_insert = "Phase 2"
      }
      
      ls_of_file = rhdf5::h5ls(file = Li7200A.file, datasetinfo = FALSE)
      
      file_of_interest = ls_of_file %>% 
        dplyr::filter(name == stream) %>% 
        dplyr::filter(stringr::str_detect(string = group, pattern = "data")) %>% 
        dplyr::filter(stringr::str_detect(string = group, pattern = paste0(interval,"m"))) %>% 
        tidyr::unite(col = file_path, c("group", "name"), sep = "/") %>% dplyr::select(file_path)
      
      # Read in A and B sensors data
      Li7200A.flow.data = rhdf5::h5read(file = Li7200A.file, name = file_of_interest$file_path[1]) %>%
        dplyr::mutate(sensorID = "Li7200 (regular)") %>% 
        dplyr::mutate(phase = phase_insert)
      Li7200B.flow.data = rhdf5::h5read(file = Li7200B.file, name = file_of_interest$file_path[1]) %>%
        dplyr::mutate(sensorID = "Li7200 (RS)")%>% 
        dplyr::mutate(phase = phase_insert)
      
      # Read in units
      Li7200.units = rhdf5::h5readAttributes(file = Li7200A.file, name = file_of_interest$file_path[1])$unit[1]
      
      # Join all the pres data together
      Li7200.flow.join = data.table::rbindlist(l = list(Li7200A.flow.data, Li7200B.flow.data))%>%
        dplyr::mutate(timeBgn = as.POSIXct(lubridate::ymd_hms(timeBgn))) %>%
        dplyr::mutate(timeEnd = as.POSIXct(lubridate::ymd_hms(timeEnd))) %>%
        dplyr::mutate(units = Li7200.units)
      
      # Join the days data to the repeating join data.table
      Li7200.join = data.table::rbindlist(l = list(Li7200.join, Li7200.flow.join))
    }
    
    # Seperating the flow data and giving it the correct ecte-a/ecte-b designation...
    if(stream %in% c("frt00Samp") & test.week %in% c("1", "2")){
      Li7200.join = Li7200.join %>%
        dplyr::mutate(day = as.Date(timeEnd))
    } 
    else {
      Li7200.join = Li7200.join %>%
        dplyr::mutate(day = as.Date(timeEnd))
    }
    
    Li7200.flow.vari = Li7200.join %>%
      dplyr::group_by(day, sensorID) %>%
      dplyr::summarise(
        mean.vari   = mean(vari, na.rm = TRUE),
        median.vari = median(vari, na.rm = TRUE),
        max.vari    = max(vari, na.rm = TRUE),
        mean.mean   = mean(mean, na.rm = TRUE),
        mean.min    = mean(min, na.rm = TRUE),
        mean.max    = mean(max, na.rm = TRUE),
      )
    
    Li7200.dcast = Li7200.join %>%
      reshape2::dcast(phase + timeBgn + timeEnd + units ~ sensorID, value.var = "mean") %>%
      dplyr::mutate(difference = `Li7200 (regular)` - `Li7200 (RS)`)  
    
    # Calculate med and mad stats
    Li7200.mad.stats = data.table::data.table(eddy4R.base::def.med.mad(test = Li7200.dcast$`Li7200 (regular)`, refe = Li7200.dcast$`Li7200 (RS)`, Perc = Percentages)) %>%
      dplyr::mutate(streamID = stream)%>%
      dplyr::select(streamID, med, mad, NumSamp)
    
    # Add units depending on what Percentage Variable is chosen
    if(Percentages == TRUE){
      Li7200.mad.stats = Li7200.mad.stats %>% 
        dplyr::mutate(units = "%")
    } 
    else {
      Li7200.mad.stats = Li7200.mad.stats %>%
        dplyr::mutate(units = Li7200.dcast$units[1])
    }
    
    # Calculate rmsd.diff.prcs.rsq stats
    Li7200.rmsd.stats = data.table::data.table(eddy4R.base::def.rmsd.diff.prcs.rsq(test = Li7200.dcast$`Li7200 (regular)`, refe = Li7200.dcast$`Li7200 (RS)`, Perc = Percentages)) %>%
      dplyr::mutate(streamID = stream) 
    
    # Add units depending on what Percentage Variable is chosen
    if(Percentages == TRUE){
      names(Li7200.rmsd.stats) = c("rmsd","diffMean", "prcs", "rsq", "samp", "streamID")
      Li7200.rmsd.stats = Li7200.rmsd.stats %>%
        dplyr::mutate(units = "%") %>%
        dplyr::select(streamID, units, `rmsd`, `diffMean`, `prcs`, rsq, samp)
    } 
    else {
      names(Li7200.rmsd.stats) = c("rmsd","diffMean", "prcs", "rsq", "samp", "streamID")
      Li7200.rmsd.stats = Li7200.rmsd.stats %>%
        dplyr::mutate(units = Li7200.dcast$units[1]) %>%
        dplyr::select(streamID, units, `rmsd`, `diffMean`, `prcs`, rsq, samp)
    }
    # Create flow comparison plot
    plot1 = ggplot(Li7200.join, aes(x = timeEnd, y = mean, color = sensorID)) +
      geom_point(alpha = .5) +
      # geom_line(alpha = .3) +
      scale_x_datetime(date_breaks = "2 day", date_labels = paste0("%Y\n%m-%d"))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(title = paste0("Time-series of ", stream), 
           x ="", y = Li7200.join$units[1])+
      theme(legend.position = "none", text = element_text(size = 16))+
      facet_wrap(~sensorID, scales = "free_x")
    # Create raw difference in flow plot
    plot2 = ggplot(Li7200.dcast, aes(x = timeEnd, y = difference)) +
      geom_point(alpha = .5) +
      # geom_line(alpha = .3) +
      scale_x_datetime(date_breaks = "2 day", date_labels = paste0("%Y\n%m-%d"))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(title =  paste0("Time-series of ", stream, " differences between Li7200 and Li7200 RS"),
           x = "", y = Li7200.join$units[1]) +
      theme(
        text = element_text(size = 16)
      )
    # Create comparison between both sensors for a single stream
    plot3 = ggplot(Li7200.dcast %>%  dplyr::mutate(day = as.Date(timeBgn, origin = "1970-01-01")), aes(x = `Li7200 (regular)`, y = `Li7200 (RS)`)) +
      geom_point() + 
      scale_y_continuous(sec.axis = dup_axis(name = "")) +
      facet_wrap(~day) +
      labs(title = "Comparing Li7200 and Li7200 RS Directly During both phases", 
           subtitle = paste0("Stream: ", stream), 
           caption = "Phase 1: 2021-04-22 to 2021-05-01\nPhase 2: 2021-05-07 to 2021-05-16"
      )+
      theme(
        text = element_text(size = 16)
      )
    
    ecte_boxplot_data = Li7200.join %>% 
      dplyr::mutate(day = factor(as.Date(timeBgn, origin = "1970-01-01")) )
    
    plot_max = max(ecte_boxplot_data$mean, na.rm = TRUE)
    plot_min = min(ecte_boxplot_data$mean, na.rm = TRUE)
    
    plot4 = ggplot(ecte_boxplot_data, aes(x = day, y = mean, fill = sensorID)) +
      geom_boxplot() +
      annotate("rect", xmin = "2021-04-21", xmax = "2021-05-02", ymin = plot_max, ymax = plot_min, alpha = 0.1, fill = "#00cc00") +
      annotate("rect", xmin = "2021-05-02", xmax = "2021-05-17", ymin = plot_max, ymax = plot_min, alpha = 0.1, fill = "blue") +  
      labs(x = "", y = ecte_boxplot_data$units[1], fill = "Sensor",title = paste0("Li7200 Obsolecence Test: Time-series boxplots of raw ", stream)) +
      annotate("text", x = "2021-04-22", y = plot_max, label = "Phase 1")+
      annotate("text", x = "2021-05-07", y = plot_max, label = "Phase 2")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 270), legend.position = "top",text = element_text(size = 16))
    
    
    # Return the data and the plot :D
    return(list(Li7200.dcast, plot1, plot2,Li7200.mad.stats,Li7200.rmsd.stats,plot3, plot4))
    
  } else { 
    message(paste0("Grabbing ", stream ," Comparison"))
    
    ############################################  Flux Comparison #############################################
    
    # For loop to grab each day's files and join them together
    for(day in day_range){
      day = as.Date(day, origin = "1970-01-01")
      # Set Dyanmic File Names for A and B sensors... 
      Li7200A.file = paste0(base.dir, "Li7200/NEON.D10.HQTW.IP4.00200.001.ecte.", day,".expanded.h5")
      Li7200B.file = paste0(base.dir, "Li7200RS/NEON.D10.HQTW.IP4.00200.001.ecte.", day,".expanded.h5")
      
      # Create variable that determines what week the test file occured durring
      if(day < "2021-05-03"){
        phase_insert = "Phase 1"
      } else {
        phase_insert = "Phase 2"
      }
      
      # Read in A and B sensors data
      Li7200A.data = rhdf5::h5read(file = Li7200A.file, name = paste0("/HQTW/dp04/data/", stream)) %>%
        dplyr::mutate(sensorID = "Li7200 (regular)") %>% 
        dplyr::mutate(phase = phase_insert)
      
      Li7200B.data = rhdf5::h5read(file = Li7200B.file, name = paste0("/HQTW/dp04/data/", stream)) %>%
        dplyr::mutate(sensorID = "Li7200 (RS)") %>% 
        dplyr::mutate(phase = phase_insert)
      
      # Read in units
      Li7200.units = rhdf5::h5readAttributes(file = Li7200A.file, name = paste0("/HQTW/dp04/data/", stream))$unit[5]
      
      # Join the "days"data together 
      Li7200.compare = data.table::rbindlist(l = list(Li7200A.data, Li7200B.data)) %>%
        dplyr::mutate(timeBgn = as.POSIXct(lubridate::ymd_hms(timeBgn))) %>%
        dplyr::mutate(timeEnd = as.POSIXct(lubridate::ymd_hms(timeEnd))) %>% 
        dplyr::mutate(units = Li7200.units)
      
      # Join the days data to the repeating join data.table
      Li7200.join = data.table::rbindlist(l = list(Li7200.join, Li7200.compare))
    }
    
    # Message NaN's by date
    nan.data = Li7200.join %>%
      dplyr::mutate(day = cut(timeBgn, breaks = "1 day")) %>%
      dplyr::group_by(day, sensorID) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(nan.count = is.na(fluxRaw)) %>%
      dplyr::group_by(day, sensorID) %>%
      dplyr::summarise(
        nan.sum = sum(nan.count)
      ) %>%
      reshape2::dcast(day ~ sensorID, value.var = "nan.sum") %>%
      dplyr::mutate(eddy_term = stream) %>%
      dplyr::select(day, eddy_term, `Li7200 (regular)`, `Li7200 (RS)`)
    
    # Reshape the data and perform a simple absolute difference calculation
    Li7200.dcast = Li7200.join %>%
      reshape2::dcast(phase + timeBgn + timeEnd + units ~ sensorID, value.var = "fluxRaw") %>%
      dplyr::mutate(difference = `Li7200 (regular)` - `Li7200 (RS)`) %>%
      dplyr::mutate(day = as.Date(timeEnd))
    
    # Calculate med and mad
    Li7200.mad.stats = data.table::data.table(eddy4R.base::def.med.mad(test = Li7200.dcast$`Li7200 (regular)`, refe = Li7200.dcast$`Li7200 (RS)`, Perc = Percentages)) %>%
      dplyr::mutate(streamID = stream) %>%
      dplyr::select(streamID, med, mad, NumSamp)
    
    # Add units depending on what Percentage Variable is chosen
    if(Percentages == TRUE){
      Li7200.mad.stats = Li7200.mad.stats %>% 
        dplyr::mutate(units = "%")
    } 
    else {
      Li7200.mad.stats = Li7200.mad.stats %>%
        dplyr::mutate(units = Li7200.dcast$units[1])
    }
    
    Li7200.rmsd.stats = data.table::data.table(eddy4R.base::def.rmsd.diff.prcs.rsq(test = Li7200.dcast$`Li7200 (regular)`, refe = Li7200.dcast$`Li7200 (RS)`, Perc = Percentages)) %>%
      dplyr::mutate(streamID = stream) 
    
    # Add units depending on what Percentage Variable is chosen
    if(Percentages == TRUE){
      names(Li7200.rmsd.stats) = c("rmsd","diffMean", "prcs", "rsq", "samp", "streamID")
      Li7200.rmsd.stats = Li7200.rmsd.stats %>%
        dplyr::mutate(units = "%") %>%
        dplyr::select(streamID, units, `rmsd`, `diffMean`, `prcs`, rsq, samp)
    } 
    else {
      names(Li7200.rmsd.stats) = c("rmsd","diffMean", "prcs", "rsq", "samp", "streamID")
      Li7200.rmsd.stats = Li7200.rmsd.stats %>%
        dplyr::mutate(units = Li7200.dcast$units[1]) %>%
        dplyr::select(streamID, units, `rmsd`, `diffMean`, `prcs`, rsq, samp)
    }
    
    # Create the actual Flux overlayed plots CO2
    if(stream == "fluxCo2/turb"){
      
      Li7200.join = Li7200.join %>%
        dplyr::mutate(units = Li7200.units) %>% 
        dplyr::mutate(fluxRaw = fluxRaw * 10^6) %>% dplyr::mutate(units = paste0("(", units, ") x (10^6)"))
      
      # Raw Flux over time
      ecte_plot_1 = ggplot(Li7200.join, aes(x =  timeEnd, y = fluxRaw, color = sensorID)) +
        geom_point(size = 3, alpha = .3) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_x_datetime(date_breaks = "2 day", date_labels = paste0("%Y\n%m-%d"))+
        labs(y = paste0("Raw Flux ",Li7200.join$units[1]), x = "", color = "Sensor ID",
             title = paste0("Time-series of raw ", stream))+
        theme_bw()+
        theme(legend.position = "top", text = element_text(size = 16))
      
      # Raw Flux boxplots over time
      ecte_boxplot_data = Li7200.join %>% 
        dplyr::mutate(day = factor(as.Date(timeBgn, origin = "1970-01-01")) )
      
      plot_max = max(ecte_boxplot_data$fluxRaw, na.rm = TRUE)
      plot_min = min(ecte_boxplot_data$fluxRaw, na.rm = TRUE)
      
      ecte_plot_2 = ggplot(ecte_boxplot_data, aes(x = day, y = fluxRaw, fill = sensorID)) +
        geom_boxplot() +
        annotate("rect", xmin = "2021-04-21", xmax = "2021-05-02", ymin = plot_max, ymax = plot_min, alpha = 0.1, fill = "#00cc00") +
        annotate("rect", xmin = "2021-05-02", xmax = "2021-05-17", ymin = plot_max, ymax = plot_min, alpha = 0.1, fill = "blue") +  
        labs(x = "", y = ecte_boxplot_data$units[1], fill = "Sensor",title = paste0("Li7200 Obsolecence Test: Time-series boxplots of raw ", stream)) +
        annotate("text", x = "2021-04-22", y = plot_max, label = "Phase 1")+
        annotate("text", x = "2021-05-07", y = plot_max, label = "Phase 2")+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 270), legend.position = "top", text = element_text(size = 16))
      
      # Raw difference over time
      ecte_boxplot_data = Li7200.dcast  %>% 
        dplyr::mutate(difference = difference * 10^6) %>% dplyr::mutate(units = paste0("(", units, ") x (10^6)")) %>% 
        dplyr::mutate(day = factor(as.Date(timeBgn, origin = "1970-01-01")) )
      
      plot_max = max(ecte_boxplot_data$difference, na.rm = TRUE)
      plot_min = min(ecte_boxplot_data$difference, na.rm = TRUE)
      
      ecte_plot_3 = ggplot(ecte_boxplot_data, aes(x =  timeEnd, y = difference)) +
        geom_point(size = 3, alpha = .60, color = "#4daf4a") +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_x_datetime(date_breaks = "2 day", date_labels = paste0("%Y\n%m-%d"))+
        labs(y = paste0("Raw Flux ",ecte_boxplot_data$units[1]), x = "", color = "Sensor ID",
             title = paste0("Time-series for ", stream, " differences between the Li7200 and Li7200 RS")
        ) +
        theme_bw()+
        theme(text = element_text(size = 16))
      
      # Raw difference BOXPLOT over time

      
      ecte_plot_4 = ggplot(ecte_boxplot_data, aes(x = day, y = difference)) +
        geom_boxplot() +
        annotate("rect", xmin = "2021-04-21", xmax = "2021-05-02", ymin = plot_max, ymax = plot_min, alpha = 0.1, fill = "#00cc00") +
        annotate("rect", xmin = "2021-05-02", xmax = "2021-05-17", ymin = plot_max, ymax = plot_min, alpha = 0.1, fill = "blue") +  
        labs(x = "", y = paste0(ecte_boxplot_data$units[1]), fill = "Sensor", subtitle = "48 30-minute average per day",
             title =  paste0("Li7200 Obsolecence Test: \n\tTime-series boxplots of raw ", stream, " differences between the Li7200 and Li7200 RS")) +
        annotate("text", x = "2021-04-22", y = plot_max+1, label = "Phase 1")+
        annotate("text", x = "2021-05-07", y = plot_max+1, label = "Phase 2")+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 270), legend.position = "top", text = element_text(size = 16))
      
      # Histogram of all differences
      ecte_plot_5 = ggplot(data = ecte_boxplot_data, aes(x = difference)) + 
        geom_histogram(bins = 45, alpha = .85, fill = "#4daf4a") +
        geom_vline(xintercept = 0, linetype = "dashed") +
        theme_bw()+
        theme(text = element_text(size = 16), legend.position = "none") +
        facet_wrap(~phase)+
        labs(x = paste0(ecte_boxplot_data$units[1]), y = "Bin Count",
             title = paste0(stream)) 
      
      # Box plot of all data
      ecte_plot_6 = ggplot(data = Li7200.join, aes(x = sensorID, y = fluxRaw, fill = sensorID)) +
        geom_boxplot() +
        facet_wrap(~phase)+
        labs(y = paste0("Raw Flux ", Li7200.join$units[1]), x = "", title = paste0(stream)) +
        theme_bw()+
        theme(text = element_text(size = 16), legend.position = "none")
    }
    # Create the actual Flux overlayed plots H2O
    if(stream == "fluxH2o/turb"){

      # Raw Flux over time
      ecte_plot_1 = ggplot(Li7200.join, aes(x =  timeEnd, y = fluxRaw, color = sensorID)) +
        geom_point(size = 3, alpha = .3) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_x_datetime(date_breaks = "2 day", date_labels = paste0("%Y\n%m-%d"))+
        labs(y = paste0("Raw Flux (",Li7200.join$units[1], ")"), x = "", color = "Sensor ID",
             title = paste0("Time-series of raw ", stream))+
        theme_bw()+
        theme(legend.position = "top", text = element_text(size = 16) )
      
      # Raw Flux boxplots over time
      ecte_boxplot_data = Li7200.join %>% 
        dplyr::mutate(day = factor(as.Date(timeBgn, origin = "1970-01-01")) )
      
      plot_max = max(ecte_boxplot_data$fluxRaw, na.rm = TRUE)
      plot_min = min(ecte_boxplot_data$fluxRaw, na.rm = TRUE)
      
      ecte_plot_2 = ggplot(ecte_boxplot_data, aes(x = day, y = fluxRaw, fill = sensorID)) +
        geom_boxplot() +
        annotate("rect", xmin = "2021-04-21", xmax = "2021-05-02", ymin = plot_max, ymax = plot_min, alpha = 0.1, fill = "#00cc00") +
        annotate("rect", xmin = "2021-05-02", xmax = "2021-05-17", ymin = plot_max, ymax = plot_min, alpha = 0.1, fill = "blue") +  
        labs(x = "", y = ecte_boxplot_data$units[1], fill = "Sensor",title = paste0("Li7200 Obsolecence Test: Time-series boxplots of raw ", stream)) +
        annotate("text", x = "2021-04-22", y = plot_max, label = "Phase 1")+
        annotate("text", x = "2021-05-07", y = plot_max, label = "Phase 2")+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 270), legend.position = "top", text = element_text(size = 16))
      
      
      
      # Raw difference over time
      ecte_plot_3 = ggplot(Li7200.dcast, aes(x =  timeEnd, y = difference)) +
        geom_point(size = 3, alpha = .60, color = "#7570b3") +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_x_datetime(date_breaks = "2 day", date_labels = paste0("%Y\n%m-%d"))+
        # scale_y_continuous(limits = c(-12, 12)) +
        labs(y = paste0("Raw Flux (",Li7200.dcast$units[1], ")"), x = "", color = "Sensor ID",
             title = paste0("Time-series for ", stream, " differences between the Li7200 and Li7200 RS")
        ) +
        theme_bw()+
        theme(text = element_text(size = 16))
      
      # Raw difference BOXPLOT over time
      ecte_boxplot_data = Li7200.dcast %>% 
        dplyr::mutate(day = factor(as.Date(timeBgn, origin = "1970-01-01")) )
      
      plot_max = max(ecte_boxplot_data$difference, na.rm = TRUE)
      plot_min = min(ecte_boxplot_data$difference, na.rm = TRUE)
      
      ecte_plot_4 = ggplot(ecte_boxplot_data, aes(x = day, y = difference)) +
        geom_boxplot() +
        annotate("rect", xmin = "2021-04-21", xmax = "2021-05-02", ymin = plot_max, ymax = plot_min, alpha = 0.1, fill = "#00cc00") +
        annotate("rect", xmin = "2021-05-02", xmax = "2021-05-17", ymin = plot_max, ymax = plot_min, alpha = 0.1, fill = "blue") +  
        labs(x = "", y = paste0(ecte_boxplot_data$units[1]), fill = "Sensor", subtitle = "48 30-minute average per day",
             title =  paste0("Li7200 Obsolecence Test: \n\tTime-series boxplots of raw ", stream, " differences between the Li7200 and Li7200 RS")) +
        annotate("text", x = "2021-04-22", y = plot_max+1, label = "Phase 1")+
        annotate("text", x = "2021-05-07", y = plot_max+1, label = "Phase 2")+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 270), legend.position = "top")

      ecte_plot_5 = ggplot(data = Li7200.dcast, aes(x = difference)) + 
        geom_histogram(bins = 45, alpha = .85, fill = "#7570b3") +
        geom_vline(xintercept = 0, linetype = "dashed") +
        theme_bw()+
        theme(text = element_text(size = 16), legend.position = "none") +
        facet_wrap(~phase)+
        labs(x = paste0("Difference (",Li7200.dcast$units[1],")"), y = "Bin Count", 
             title = paste0(stream)) 

      ecte_plot_6 = ggplot(data = Li7200.join, aes(x = sensorID, y = fluxRaw, fill = sensorID)) +
        geom_boxplot() +
        facet_wrap(~phase)+
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(-70,70)) +
        labs(y = paste0("Raw Flux (",Li7200.join$units[1], ")"), x = "", title = paste0(stream)) +
        theme_bw()+
        theme(text = element_text(size = 16),legend.position = "none")
    }
    # Return the data and the plot :D
    return( list(Li7200.dcast, Li7200.mad.stats, Li7200.rmsd.stats, ecte_plot_1, ecte_plot_2, ecte_plot_3, ecte_plot_4, ecte_plot_5, ecte_plot_6) )
  }
}

plot.stream = function(week = "1"){

  data.1 = compare.stream(stream = "fluxCo2/turb",      Percentages = FALSE, test.week = week)
  data.1[[4]]
  data.2 = compare.stream(stream = "fluxH2o/turb",      Percentages = FALSE, test.week = week)
  data.2[[4]]
  data.3 = compare.stream(stream = "pres",              Percentages = FALSE, test.week = week)
  data.3[[4]]
  data.4 = compare.stream(stream = "frt00Samp",         Percentages = FALSE, test.week = week)
  data.4[[4]]
  
  data.5 = compare.stream(stream = "rtioMoleDryCo2Raw", Percentages = FALSE, test.week = week)

  data.6 = compare.stream(stream = "rtioMoleDryH2oRaw", Percentages = FALSE, test.week = week)

  require(grid)
  # Plots for presentations 
  gridExtra::grid.arrange(data.1[[4]], data.2[[4]], ncol=2,
                          top=grid::textGrob(paste0("WEEK ", week, " - Direct Comparison of Li7200 (regular) and Li7200 (RS) Raw Fluxes"),
                                             gp=grid::gpar(fontsize=20,font=3)),
                          bottom = grid::textGrob("*",
                            gp = gpar(fontface = 3, fontsize = 12),
                            hjust = 1,
                            x = 1
                          )
  )
  
  
  gridExtra::grid.arrange(data.1[[5]], data.2[[5]], ncol=2, 
                          bottom = grid::textGrob("*",
                            gp = gpar(fontface = 3, fontsize = 12),
                            hjust = 1,
                            x = 1
                          )
  ) 
  
  gridExtra::grid.arrange(data.1[[6]], data.2[[6]], ncol=2, 
                          top=grid::textGrob(paste0("Difference Comparison Between Li7200 (regular) and Li7200 (RS)"),
                                             gp=grid::gpar(fontsize=20,font=3)),
                          bottom = grid::textGrob("*",
                                                  gp = gpar(fontface = 3, fontsize = 12),
                                                  hjust = 1,
                                                  x = 1
                          )
  ) 
  
  gridExtra::grid.arrange(data.1[[7]], data.2[[7]], ncol=2, 
                          bottom = grid::textGrob("*",
                                                  gp = gpar(fontface = 3, fontsize = 12),
                                                  hjust = 1,
                                                  x = 1
                          )
  )
  
  gridExtra::grid.arrange(data.1[[8]], data.2[[8]], ncol=2, 
                          top=grid::textGrob(paste0("Histogram of the Differences Between Li7200 (regular) and Li7200 (RS)"),
                                             gp=grid::gpar(fontsize=20,font=3)),
                          bottom = grid::textGrob("*",
                            gp = gpar(fontface = 3, fontsize = 12),
                            hjust = 1,
                            x = 1
                          )
  )
  
  gridExtra::grid.arrange(data.1[[9]], data.2[[9]], ncol=2, 
                          top=grid::textGrob(paste0("Boxplot of the Raw Fluxes Between Li7200 (regular) and Li7200 (RS)"),
                                             gp=grid::gpar(fontsize=20,font=3)),
                          bottom = grid::textGrob("*",
                            gp = gpar(fontface = 3, fontsize = 12),
                            hjust = 1,
                            x = 1
                          )
  )
  
  
  # Pressure Fluctuations
  print(data.3[[3]])

  # Sample Flow
  print(data.4[[2]])

  # Raw Co2
  print(data.5[[2]])
  print(data.5[[3]])
  print(data.5[[6]])
  print(data.5[[7]])
  # Raw H2o
  print(data.6[[2]])
  print(data.6[[3]])
  print(data.6[[6]])
  print(data.6[[7]])

  
}

plot.stream(week = "1")


# grab.stats = function(week = "1"){
# 
#   data.1.false = compare.stream(stream = "fluxCo2/turb",      Percentages = FALSE, test.week = week)
#   data.2.false = compare.stream(stream = "fluxH2o/turb",      Percentages = FALSE, test.week = week)
#   data.3.false = compare.stream(stream = "pres",              Percentages = FALSE, test.week = week)
#   data.4.false = compare.stream(stream = "frt00Samp",         Percentages = FALSE, test.week = week)
#   data.5.false = compare.stream(stream = "rtioMoleDryCo2Raw", Percentages = FALSE, test.week = week)
#   
#   data.1.true  = compare.stream(stream = "fluxCo2/turb",      Percentages =  TRUE, test.week = week)
#   data.2.true  = compare.stream(stream = "fluxH2o/turb",      Percentages =  TRUE, test.week = week)
#   data.3.true  = compare.stream(stream = "pres",              Percentages =  TRUE, test.week = week)
#   data.4.true  = compare.stream(stream = "frt00Samp",         Percentages =  TRUE, test.week = week)
#   data.5.true  = compare.stream(stream = "rtioMoleDryCo2Raw", Percentages =  TRUE, test.week = week)
#   
#   
#   data.mad.stats.true =   data.table::rbindlist(l = list(data.1.true[[2]],  data.2.true[[2]],  data.3.true[[4]],  data.4.true[[4]],  data.5.true[[4]]))
#   data.mad.stats.false =  data.table::rbindlist(l = list(data.1.false[[2]], data.2.false[[2]], data.3.false[[4]], data.4.false[[4]], data.5.false[[4]]))
#   data.rsmd.stats.true =  data.table::rbindlist(l = list(data.1.true[[3]],  data.2.true[[3]],  data.3.true[[5]],  data.4.true[[5]],  data.5.true[[5]]))
#   data.rsmd.stats.false = data.table::rbindlist(l = list(data.1.false[[3]], data.2.false[[3]], data.3.false[[5]], data.4.false[[5]], data.5.false[[5]]))
#   
#   
#   data.stats.med.mad = data.table::rbindlist(l = list(data.mad.stats.true, data.mad.stats.false), fill = TRUE)
#   data.stats.rmsd.rsq = data.table::rbindlist(l = list(data.rsmd.stats.true, data.rsmd.stats.false), fill = TRUE)
#   data.stats = dplyr::left_join(data.stats.med.mad, data.stats.rmsd.rsq, by = c("streamID", "units")) %>%
#     dplyr::mutate(`Test Period` = week) %>%
#     dplyr::select(streamID, `Test Period`, units, NumSamp, med, mad, rmsd, diffMean, prcs, rsq) %>%
#     dplyr::filter(streamID %in% c("fluxCo2/turb", "fluxH2o/turb","rtioMoleDryCo2Raw"))
#   
#   return(data.stats)
#   
# }
# 
# options(scipen = 6)
# 
# stats.return = grab.stats(week = "1") 
# 
# knitr::kable(stats.return %>%
#                dplyr::filter(units == "%")
# )
# knitr::kable(stats.return %>%
#                dplyr::filter(units != "%")
# )
# 
# stats.return = grab.stats(week = "2") 
# 
# knitr::kable(stats.return %>%
#                dplyr::filter(units == "%")
# )
# knitr::kable(stats.return %>%
#                dplyr::filter(units != "%")
# )
