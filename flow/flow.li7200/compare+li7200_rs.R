# Compare a fluxes for the same day

compare.stream = function(stream, interval = "01", test.week = "1", Percentages = TRUE){
  # Libraries...
  require(rhdf5)
  require(ggplot2)
  require(dplyr)
  require(reshape2)
  
  # Base Directory of this experiment
  base.dir = "~/eddy/neon-sensor-test/data/2021_Li7200_RS_Comparision/"
  
  # Range of Dates to analyze
  if(test.week == "1"){
    day_range = base::seq.Date(from = as.Date("2021-04-22", origin = "1970-01-01"), to = as.Date("2021-05-01", origin = "1970-01-01"), by = "1 day")
    
  } else if(test.week == "2"){
    day_range = base::seq.Date(from = as.Date("2021-04-22", origin = "1970-01-01"), to = as.Date("2021-05-01", origin = "1970-01-01"), by = "1 day")
  } else {
    stop("The test.week variable must be set! 1 or 2 are acceptable values")
  }
  
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
      dplyr::mutate(day = as.Date(timeEnd)) %>%
      dplyr::mutate(pump = ifelse(test = day >= "2021-01-01" & day <= "2021-01-18" & sensorID == "Li7200 (regular)", yes = "Diaphram",   
                                  no = ifelse(test = day >= "2021-01-01" & day <= "2021-01-18" & sensorID == "Li7200 (RS)", yes = "Rotary", 
                                              no = ifelse(test = day >= "2021-01-18" & day <= "2021-02-21" & sensorID == "Li7200 (regular)", yes = "Rotary", 
                                                          no = ifelse(test = day >= "2021-01-18" & day <= "2021-02-21" & sensorID == "Li7200 (RS)", yes = "Diaphram",   no = "Undetermined"))))) %>%
      dplyr::mutate(Sensor_Config = paste0(sensorID, "-", pump))
    
    Li7200.dcast = Li7200.join %>%
      reshape2::dcast(timeBgn + timeEnd + units + Sensor_Config ~ streamID, value.var = "mean")  %>%
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
    
    plot1 = ggplot(Li7200.join, aes(x = timeEnd, y = mean, linetype = streamID, color = Sensor_Config)) +
      geom_line(alpha = 1, size = 1) +
      scale_x_datetime(date_breaks = "1 day", date_labels = paste0("%Y\n%m-%d"))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), sec.axis = dup_axis(name = "")) +
      scale_color_manual(values = c("#1f78b4", "#1b9e77","firebrick", "#004000")) +
      labs(title = "Time-series of Ambient Air Pressure (presAtm) vs Instantaneous Cell Air Pressure\n(presSum)",
           x = "", y = "Pressure (kPa)", color = "Stream")+
      theme(legend.position = "top", text = element_text(size = 16))+ 
      guides(colour = guide_legend(override.aes = list(size=4, alpha = 1))) +
      facet_wrap(~Sensor_Config, scales = "free_x")
    
    plot2 = ggplot(Li7200.dcast, aes(x = timeEnd, y = difference, color = Sensor_Config)) +
      geom_point(alpha = .7) +
      scale_x_datetime(date_breaks = "1 day", date_labels = paste0("%Y\n%m-%d"))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(-4.5,0), sec.axis = dup_axis(name = "")) +
      scale_color_manual(values = c("#1f78b4", "#1b9e77","firebrick", "#004000")) +
      labs(title = "Time-series of Raw Differences Between Ambient Air Pressure (presAtm) and Instantaneous Cell Air Pressure (presSum)",
           y = "Pressure Difference (kPa)", x = "", color = "Sensor Configuration")+
      guides(colour = guide_legend(override.aes = list(size=4, alpha = 1))) +
      theme(legend.position = "top", text = element_text(size = 16))
    
    
    # Return the data and the plot :D
    return(list(Li7200.dcast, plot1, plot2,Li7200.mad.stats,Li7200.rmsd.stats))
    
  } else if(stream %in% c("frt00Samp", "rtioMoleDryCo2Raw")){
    
    #############################################  frt00Samp Comparison         ######################################
    #############################################  rtioMoleDryCo2Raw Comparison ######################################
    
    message(paste0("Grabbing ", stream ," Comparison"))
    
    for(day in day_range){
      day = as.Date(day, origin = "1970-01-01")
      # Set Dyanmic File Names for A and B sensors... 
      Li7200A.file = paste0(base.dir, "Li7200/NEON.D10.HQTW.IP4.00200.001.ecte.", day,".expanded.h5")
      Li7200B.file = paste0(base.dir, "Li7200RS/NEON.D10.HQTW.IP4.00200.001.ecte.", day,".expanded.h5")
      
      # Read in A and B sensors data
      Li7200A.flow.data = rhdf5::h5read(file = Li7200A.file, name = paste0("/HQTW/dp01/data/co2Turb/000_040_",interval,"m/", stream)) %>%
        dplyr::mutate(sensorID = "Li7200 (regular)") 
      Li7200B.flow.data = rhdf5::h5read(file = Li7200B.file, name = paste0("/HQTW/dp01/data/co2Turb/000_040_",interval,"m/", stream)) %>%
        dplyr::mutate(sensorID = "Li7200 (RS)")
      
      # Read in units
      Li7200.units = rhdf5::h5readAttributes(file = Li7200A.file, name = paste0("/HQTW/dp01/data/co2Turb/000_040_",interval,"m/", stream))$unit[1]
      
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
        dplyr::mutate(day = as.Date(timeEnd)) %>%
        dplyr::mutate(pump = ifelse(test = day >= "2021-01-01" & day <= "2021-01-18" & sensorID == "Li7200 (regular)", yes = "Diaphram",   
                        no = ifelse(test = day >= "2021-01-01" & day <= "2021-01-18" & sensorID == "Li7200 (RS)", yes = "Rotary", 
                        no = ifelse(test = day >= "2021-01-18" & day <= "2021-02-21" & sensorID == "Li7200 (regular)", yes = "Rotary", 
                        no = ifelse(test = day >= "2021-01-18" & day <= "2021-02-21" & sensorID == "Li7200 (RS)", yes = "Diaphram",   no = "Undetermined"))))) %>%
        dplyr::mutate(Sensor_Config = paste0(sensorID, "-", pump)) %>%
        dplyr::mutate(Sensor_Config = ifelse(Sensor_Config == "Li7200 (RS)-Rotary", yes = "Li7200 C-Rotary", no = Sensor_Config)) %>%
        dplyr::mutate(Sensor_Config = ifelse(Sensor_Config == "Li7200 (regular)-Diaphram", yes = "Li7200 (RS)-Rotary", no = Sensor_Config)) %>%
        dplyr::mutate(Sensor_Config = ifelse(Sensor_Config == "Li7200 C-Rotary", yes = "Li7200 (regular)-Diaphram", no = Sensor_Config)) 
    } 
    else {
      Li7200.join = Li7200.join %>%
        dplyr::mutate(day = as.Date(timeEnd)) %>%
        dplyr::mutate(pump = ifelse(test = day >= "2021-01-01" & day <= "2021-01-18" & sensorID == "Li7200 (regular)", yes = "Diaphram",   
                        no = ifelse(test = day >= "2021-01-01" & day <= "2021-01-18" & sensorID == "Li7200 (RS)", yes = "Rotary", 
                        no = ifelse(test = day >= "2021-01-18" & day <= "2021-02-21" & sensorID == "Li7200 (regular)", yes = "Rotary", 
                        no = ifelse(test = day >= "2021-01-18" & day <= "2021-02-21" & sensorID == "Li7200 (RS)", yes = "Diaphram",   no = "Undetermined"))))) %>%
        dplyr::mutate(Sensor_Config = paste0(sensorID, "-", pump))
    }
    
    Li7200.flow.vari = Li7200.join %>%
      dplyr::group_by(day, Sensor_Config) %>%
      dplyr::summarise(
        mean.vari   = mean(vari, na.rm = TRUE),
        median.vari = median(vari, na.rm = TRUE),
        max.vari    = max(vari, na.rm = TRUE),
        mean.mean   = mean(mean, na.rm = TRUE),
        mean.min    = mean(min, na.rm = TRUE),
        mean.max    = mean(max, na.rm = TRUE),
      )
    
    Li7200.dcast = Li7200.join %>%
      reshape2::dcast(timeBgn + timeEnd + units ~ sensorID, value.var = "mean") %>%
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
    plot1 = ggplot(Li7200.join, aes(x = timeEnd, y = mean, color = Sensor_Config)) +
      geom_point(alpha = .5) +
      # geom_line(alpha = .3) +
      scale_x_datetime(date_breaks = "1 day", date_labels = paste0("%Y\n%m-%d"))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(title = "Time-series of ECTE Sample Flows", 
           x ="", y = "Flow (SLPM)")+
      theme(legend.position = "none", text = element_text(size = 16))+
      facet_wrap(~Sensor_Config, scales = "free_x")
    # Create raw difference in flow plot
    plot2 = ggplot(Li7200.dcast, aes(x = timeEnd, y = difference)) +
      geom_point(alpha = .5) +
      # geom_line(alpha = .3) +
      scale_x_datetime(date_breaks = "1 day", date_labels = paste0("%Y\n%m-%d"))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(title = "Time-series of Flow Differences Between Li7200 (regular) and Li7200 (RS)",
           x = "", y = "Difference (SLPM)") +
      theme(
        text = element_text(size = 16)
      )
    
    # Return the data and the plot :D
    return(list(Li7200.dcast, plot1, plot2,Li7200.mad.stats,Li7200.rmsd.stats))
    
  } else { 
    message(paste0("Grabbing ", stream ," Comparison"))
    
    ############################################  Flux Comparison #############################################
    
    # For loop to grab each day's files and join them together
    for(day in day_range){
      day = as.Date(day, origin = "1970-01-01")
      # Set Dyanmic File Names for A and B sensors... 
      Li7200A.file = paste0(base.dir, "Li7200/NEON.D10.HQTW.IP4.00200.001.ecte.", day,".expanded.h5")
      Li7200B.file = paste0(base.dir, "Li7200RS/NEON.D10.HQTW.IP4.00200.001.ecte.", day,".expanded.h5")
      
      
      # Read in A and B sensors data
      Li7200A.data = rhdf5::h5read(file = Li7200A.file, name = paste0("/HQTW/dp04/data/", stream)) %>%
        dplyr::mutate(sensorID = "Li7200 (regular)")
      
      Li7200B.data = rhdf5::h5read(file = Li7200B.file, name = paste0("/HQTW/dp04/data/", stream)) %>%
        dplyr::mutate(sensorID = "Li7200 (RS)")
      
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
    ## Message how many nan's there are in the data
    # message(print(knitr::kable(nan.data)))
    
    # Reshape the data and perform a simple absolute difference calculation
    Li7200.dcast = Li7200.join %>%
      reshape2::dcast(timeBgn + timeEnd + units ~ sensorID, value.var = "fluxRaw") %>%
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
      Li7200.plot.1 = ggplot(Li7200.join, aes(x =  timeEnd, y = fluxRaw, color = sensorID)) +
        geom_point(size = 3, alpha = .3) +
        # geom_line(size = 1, alpha = .5) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_x_datetime(date_breaks = "1 day", date_labels = paste0("%Y\n%m-%d"))+
        scale_y_continuous(limits = c(-.0001, .0001)) +
        labs(y = paste0("Raw Flux (",Li7200.join$units[1], ")"), x = "", color = "Sensor ID",
             title = paste0("Time-series of raw ", stream))+
        theme(legend.position = "none", text = element_text(size = 16))
    }
    # Create the actual Flux overlayed plots H2O
    if(stream == "fluxH2o/turb"){
      Li7200.plot.1 = ggplot(Li7200.join, aes(x =  timeEnd, y = fluxRaw, color = sensorID)) +
        geom_point(size = 3, alpha = .3) +
        # geom_line(size = 1, alpha = .5) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_y_continuous(limits = c(-75, 75)) +
        scale_x_datetime(date_breaks = "1 day", date_labels = paste0("%Y\n%m-%d"))+
        labs(y = paste0("Raw Flux (",Li7200.join$units[1], ")"), x = "", color = "Sensor ID",
             title = paste0("Time-series of raw ", stream))+
        theme(
          legend.position = "none", text = element_text(size = 16)
        )
    }
    # Create the raw Differences plot CO2
    if(stream == "fluxCo2/turb"){
      Li7200.plot.2 = ggplot(Li7200.dcast, aes(x =  timeEnd, y = difference)) +
        geom_point(size = 3, alpha = .60, color = "#4daf4a") +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_x_datetime(date_breaks = "1 day", date_labels = paste0("%Y\n%m-%d"))+
        scale_y_continuous(limits = c(-.000010, .000010)) +
        labs(y = paste0("Raw Flux (",Li7200.dcast$units[1], ")"), x = "", color = "Sensor ID",
             title = paste0("Time-series for ", stream)
        ) +
        theme(
          text = element_text(size = 16)
        )
    }
    # Create the raw Differences plot H2O
    if(stream == "fluxH2o/turb"){
      Li7200.plot.2 = ggplot(Li7200.dcast, aes(x =  timeEnd, y = difference)) +
        geom_point(size = 3, alpha = .60, color = "#7570b3") +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_x_datetime(date_breaks = "1 day", date_labels = paste0("%Y\n%m-%d"))+
        scale_y_continuous(limits = c(-12, 12)) +
        labs(y = paste0("Raw Flux (",Li7200.dcast$units[1], ")"), x = "", color = "Sensor ID",
             title = paste0("Time-series for ", stream)
        ) +
        theme(
          text = element_text(size = 16)
        )
    }
    # Create the HISTORGRAM plot CO2
    if(stream == "fluxCo2/turb"){
      Li7200.plot.3 = ggplot(data = Li7200.dcast, aes(x = difference)) + 
        geom_histogram(bins = 45, alpha = .85, fill = "#4daf4a") +
        geom_vline(xintercept = 0, linetype = "dashed") +
        scale_x_continuous(limits = c(-.000010, .000010)) +
        scale_y_continuous(limits = c(0, 105)) +
        theme(
          text = element_text(size = 16),
          legend.position = "none"
        ) +
        labs(x = paste0("Difference (",Li7200.dcast$units[1],")"), y = "Bin Count",
             title = paste0(stream)) 
    }
    # Create the HISTORGRAM plot H2O
    if(stream == "fluxH2o/turb"){
      Li7200.plot.3 = ggplot(data = Li7200.dcast, aes(x = difference)) + 
        geom_histogram(bins = 45, alpha = .85, fill = "#7570b3") +
        geom_vline(xintercept = 0, linetype = "dashed") +
        scale_x_continuous(limits = c(-15, 15))+
        scale_y_continuous(limits = c(0, 200)) +
        theme(
          text = element_text(size = 16), 
          legend.position = "none"
        ) +
        labs(x = paste0("Difference (",Li7200.dcast$units[1],")"), y = "Bin Count", 
             title = paste0(stream)) 
    }
    # Create the BOXPLOT plot CO2
    if(stream == "fluxCo2/turb"){
      Li7200.plot.4 = ggplot(data = Li7200.join, aes(x = sensorID, y = fluxRaw, fill = sensorID)) +
        geom_boxplot() +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(-0.0001, 0.0001)) +
        labs(y = paste0("Raw Flux (",Li7200.join$units[1], ")"), x = "", title = paste0(stream)) +
        theme(text = element_text(size = 16),legend.position = "none")
    }
    # Create the BOXPLOT plot H2O
    if(stream == "fluxH2o/turb"){
      Li7200.plot.4 = ggplot(data = Li7200.join, aes(x = sensorID, y = fluxRaw, fill = sensorID)) +
        geom_boxplot() +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(-70,70)) +
        labs(y = paste0("Raw Flux (",Li7200.join$units[1], ")"), x = "", title = paste0(stream)) +
        theme(text = element_text(size = 16),legend.position = "none")
    }
    # Return the data and the plot :D
    return(list(Li7200.dcast, Li7200.plot.1, Li7200.plot.2, Li7200.plot.3,Li7200.mad.stats,Li7200.rmsd.stats, Li7200.plot.4))
  }
}

plot.stream = function(week = "1"){
  data.1 = compare.stream(stream = "fluxCo2/turb",      Percentages = FALSE, test.week = week)
  data.1[[6]]
  data.2 = compare.stream(stream = "fluxH2o/turb",      Percentages = FALSE, test.week = week)
  data.2[[6]]
  data.3 = compare.stream(stream = "pres",              Percentages = FALSE, test.week = week)
  data.3[[4]]
  data.4 = compare.stream(stream = "frt00Samp",         Percentages = FALSE, test.week = week)
  data.4[[4]]
  data.5 = compare.stream(stream = "rtioMoleDryCo2Raw", Percentages = FALSE, test.week = week)
  data.5[[4]]
  
  require(grid)
  
  # Plots for presentations 
  gridExtra::grid.arrange(data.1[[2]], data.2[[2]], ncol=2,
                          top=grid::textGrob(paste0("WEEK ", week, " - Direct Comparison of Li7200 (regular) and Li7200 (RS) Raw Fluxes"),
                                             gp=grid::gpar(fontsize=20,font=3)),
                          bottom = grid::textGrob(
                            # "Li7200 (regular) is configured with a Rotary Pump while Li7200 (RS) is configured with a Diaphram Pump   \t",
                            "Li7200 (regular) is configured with a Diaphram Pump while Li7200 (RS) is configured with a Rotary Pump   \t",
                            gp = gpar(fontface = 3, fontsize = 12),
                            hjust = 1,
                            x = 1
                          )
  )
  
  
  gridExtra::grid.arrange(data.1[[3]], data.2[[3]], ncol=2, 
                          top=grid::textGrob(paste0("WEEK ", week, " - Difference Comparison Between Li7200 (regular) and Li7200 (RS)"),
                                             gp=grid::gpar(fontsize=20,font=3)),
                          bottom = grid::textGrob(
                            "Li7200 (regular) is configured with a Diaphram Pump while Li7200 (RS) is configured with a Rotary Pump   \t",
                            # "Li7200 (regular) is configured with a Rotary Pump while Li7200 (RS) is configured with a Diaphram Pump   \t",
                            gp = gpar(fontface = 3, fontsize = 12),
                            hjust = 1,
                            x = 1
                          )
  ) 
  
  gridExtra::grid.arrange(data.1[[4]], data.2[[4]], ncol=2, 
                          top=grid::textGrob(paste0("WEEK ", week, " - Histogram of the Differences Between Li7200 (regular) and B's\n"),
                                             gp=grid::gpar(fontsize=20,font=3)),
                          bottom = grid::textGrob(
                            "Li7200 (regular) is configured with a Diaphram Pump while Li7200 (RS) is configured with a Rotary Pump   \t",
                            # "Li7200 (regular) is configured with a Rotary Pump while Li7200 (RS) is configured with a Diaphram Pump   \t",
                            gp = gpar(fontface = 3, fontsize = 12),
                            hjust = 1,
                            x = 1
                          )
  )
  
  gridExtra::grid.arrange(data.1[[7]], data.2[[7]], ncol=2, 
                          top=grid::textGrob(paste0("WEEK ", week, " - Boxplot of the Raw Fluxes Between Li7200 (regular) and B's\n"),
                                             gp=grid::gpar(fontsize=20,font=3)),
                          bottom = grid::textGrob(
                            "Li7200 (regular) is configured with a Diaphram Pump while Li7200 (RS) is configured with a Rotary Pump   \t",
                            # "Li7200 (regular) is configured with a Rotary Pump while Li7200 (RS) is configured with a Diaphram Pump   \t",
                            gp = gpar(fontface = 3, fontsize = 12),
                            hjust = 1,
                            x = 1
                          )
  )
  
  
  # Pressure Fluctuations 
  print(data.3[[3]])
  
  # Sample Flow  
  print(data.4[[2]])
  
  
}

plot.stream(week = "1")
plot.stream(week = "2")

grab.stats = function(week = "1"){
  
  data.1.false = compare.stream(stream = "fluxCo2/turb",      Percentages = FALSE, test.week = week)
  data.2.false = compare.stream(stream = "fluxH2o/turb",      Percentages = FALSE, test.week = week)
  data.3.false = compare.stream(stream = "pres",              Percentages = FALSE, test.week = week)
  data.4.false = compare.stream(stream = "frt00Samp",         Percentages = FALSE, test.week = week)
  data.5.false = compare.stream(stream = "rtioMoleDryCo2Raw", Percentages = FALSE, test.week = week)
  
  data.1.true  = compare.stream(stream = "fluxCo2/turb",      Percentages =  TRUE, test.week = week)
  data.2.true  = compare.stream(stream = "fluxH2o/turb",      Percentages =  TRUE, test.week = week)
  data.3.true  = compare.stream(stream = "pres",              Percentages =  TRUE, test.week = week)
  data.4.true  = compare.stream(stream = "frt00Samp",         Percentages =  TRUE, test.week = week)
  data.5.true  = compare.stream(stream = "rtioMoleDryCo2Raw", Percentages =  TRUE, test.week = week)
  
  
  data.mad.stats.true =   data.table::rbindlist(l = list(data.1.true[[5]],  data.2.true[[5]],  data.3.true[[4]],  data.4.true[[4]],  data.5.true[[4]]))
  data.mad.stats.false =  data.table::rbindlist(l = list(data.1.false[[5]], data.2.false[[5]], data.3.false[[4]], data.4.false[[4]], data.5.false[[4]]))
  data.rsmd.stats.true =  data.table::rbindlist(l = list(data.1.true[[6]],  data.2.true[[6]],  data.3.true[[5]],  data.4.true[[5]],  data.5.true[[5]]))
  data.rsmd.stats.false = data.table::rbindlist(l = list(data.1.false[[6]], data.2.false[[6]], data.3.false[[5]], data.4.false[[5]], data.5.false[[5]]))
  
  
  data.stats.med.mad = data.table::rbindlist(l = list(data.mad.stats.true, data.mad.stats.false), fill = TRUE)
  data.stats.rmsd.rsq = data.table::rbindlist(l = list(data.rsmd.stats.true, data.rsmd.stats.false), fill = TRUE)
  data.stats = dplyr::left_join(data.stats.med.mad, data.stats.rmsd.rsq, by = c("streamID", "units")) %>%
    dplyr::mutate(`Test Period` = week) %>%
    dplyr::select(streamID, `Test Period`, units, NumSamp, med, mad, rmsd, diffMean, prcs, rsq) %>%
    dplyr::filter(streamID %in% c("fluxCo2/turb", "fluxH2o/turb","rtioMoleDryCo2Raw"))
  
  return(data.stats)
  
}

options(scipen = 6)

stats.return = grab.stats(week = "1") 

knitr::kable(stats.return %>%
               dplyr::filter(units == "%")
)
knitr::kable(stats.return %>%
               dplyr::filter(units != "%")
)

stats.return = grab.stats(week = "2") 

knitr::kable(stats.return %>%
               dplyr::filter(units == "%")
)
knitr::kable(stats.return %>%
               dplyr::filter(units != "%")
)
