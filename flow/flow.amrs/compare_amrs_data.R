compare_amrs_data = function(){
  # library()
  library(aws.s3)
  library(aws.signature)
  library(dplyr)
  library(data.table)
  library(stringr)
  library(fst)
  library(here)
  
  
  # S3 Connection
  Sys.setenv(
    "AWS_S3_ENDPOINT"       = "neonscience.org",
    "AWS_DEFAULT_REGION"    = "s3.data"
  )
  
  sensor_test_bucket = "neon-sensor-test"
  
  test_data_lookup = aws.s3::get_bucket_df(bucket = sensor_test_bucket, prefix = "sensor/amrs/round_3/") %>% 
    dplyr::filter(Size > 0) %>%  dplyr::filter(stringr::str_detect(string = Key, pattern = ".fst")) %>% 
    dplyr::mutate(date = stringr::str_extract(Key, "[0-9]{4}-[0-9]{2}-[0-9]{2}"))
  
  test_dates = unique(test_data_lookup$date)
  
  # Run comparison for each date and export findings
  for(i in base::seq_along(test_dates)){
    
    date_i = test_dates[i]
    
    # Lookup files for ith date
    date_file_lookup = aws.s3::get_bucket_df(bucket = sensor_test_bucket, prefix = paste0("sensor/amrs/round_3/", date_i)) %>% 
      dplyr::filter(stringr::str_detect(string = Key, pattern = ".fst")) %>% 
      dplyr::arrange(Key) # Arrange such that AMRS_01 is on top for naming later 
    
    # Function to read in data through the lappy() call
    read_in_amrs_data = function(x){
      output = aws.s3::s3read_using(FUN = fst::read_fst, object = x, bucket = sensor_test_bucket) # Read in data x
      
      if(nrow(output) > 0){
        output_final = output %>% 
          dplyr::mutate(DATE = date_i)
        
        names(output_final)[names(output_final)=="time"] <- "TIME"
      } else {
        stop("Data was missing rows...")
      }
      
      return(output_final)
    }
    
    # Read in data into List
    dataList <- lapply(date_file_lookup$Key, read_in_amrs_data)
    
    # Check that dataList is the correct length; 3.
    if(length(dataList) != 3){
      stop(paste0("Check ", date_i, " for missing files, 3 files were not read in as expected"))
    }
    
    # Names list items 
    names(dataList) = c("soniAmrs01", "soniAmrs02", "soniAmrs03") # This is

    # Print POSIX date/timestamps with fractional seconds
    options(digits.secs=3) 
    
    for(idx in 1:length(names(dataList))){
      message(paste0(Sys.time(), ": ", names(dataList)[idx]," started with ", nrow(dataList[[idx]]), " rows to clean..."))
  
      ## as.POSIXlt is not tidyverse compatible D: after much troubleshooting I discoverd that first making it into ct, you can easily transform it to lt
      dataList[[idx]]$time <- as.POSIXlt(x = dataList[[idx]]$TIME, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
      
      sensor.format.check.str <- dataList[[idx]] %>% summary.default %>% as.data.frame %>% 
        dplyr::group_by(Var1) %>%  
        tidyr::spread(key = Var2, value = Freq) %>%
        dplyr::filter(Var1 == "time")
      
      if(sensor.format.check.str$Class == "POSIXlt"){ 
        message(paste0(Sys.time(),": POSIXlt achieved!"))
        
        
        if(names(dataList)[idx] == "soni"){
          message(paste0(Sys.time(),": CSAT3 Field Naming"))
          # Now that we have achieved the correctly formatted timestamp, we have to order the columns such that eddy4R can digest these dataframes
          names(dataList[[idx]]) <- c("sample", "date", "time.hms", "veloXaxs", "veloYaxs", "veloZaxs",
                                      "veloSoni", "diag", "idx", "time")
          dataList[[idx]] <- dataList[[idx]] %>%
            dplyr::select(time, 
                          veloXaxs, veloYaxs, veloZaxs, veloSoni,
                          diag, idx)
          
        } else if(stringr::str_detect(string = names(dataList)[idx], pattern = "soniAmrs")){
          message(paste0(Sys.time(), ": AMRS Field Naming"))
          
          # Now that we have achieved the correctly formatted timestamp, we have to order the columns such that eddy4R can digest these dataframes
          dataList[[idx]] <- dataList[[idx]] %>%
            dplyr::select(time, idx, 
                          angXaxs, angYaxs,angZaxs, 
                          accXaxs, accYaxs, accZaxs, 
                          accXaxsDiff, accYaxsDiff,accZaxsDiff, 
                          avelXaxs, avelYaxs, avelZaxs)
        }
      } else {
        message(paste0(Sys.time(), ": Timestamp POSIXlt failed."))
      }
    }
    
    #sensor frequency
    Freq20 <- 20
    
    timeRglr20 <- seq.POSIXt(
      from = base::as.POSIXlt(paste(date_i, " ", "00:00:00.0001", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
      to = base::as.POSIXlt(paste(date_i, " ", "23:59:59.9502", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
      by = 1/Freq20
    )
    
    Freq40 <- 40
    
    timeRglr40 <- seq.POSIXt(
      from = base::as.POSIXlt(paste(date_i, " ", "00:00:00.0001", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
      to = base::as.POSIXlt(paste(date_i, " ", "23:59:59.9752", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
      by = 1/Freq40
    )
    
    
    for (idx in names(dataList)) {
      startTime.time_regularization <- Sys.time()
      if(idx %in% "soni"){
        message(paste0(Sys.time(), ": Wrangling CSAT3 data"))
        
        #Creating the index to organize the variables in alphabetical order
        idxSoni <- order(names(dataList[[idx]]))
        #Changing the order of the variables to alphabetical order using the index
        dataList[[idx]] <- dataList[[idx]][,idxSoni]
        
        message(paste0(Sys.time(), ": CSAT3:\t Adding units as an attribute"))
        attr(dataList[[idx]], which = "unit") <- c("NA", "NA", "NA", "m s-1", "m s-1", 
                                                   "m s-1", "m s-1")
        attr(dataList[[idx]]$veloSoni, which = "unit") <- c("m s-1")
        
        message(paste0(Sys.time(), ": CSAT3:\t Calculating sonic temperature"))
        dataList[[idx]]$tempSoni <- eddy4R.base::def.temp.soni(veloSoni =  dataList[[idx]]$veloSoni)
        
        message(paste0(Sys.time(), ": CSAT3:\t Adding units as an attribute"))
        attr(dataList[[idx]], which = "unit") <- c("NA", "NA", "NA", "m s-1", "m s-1", 
                                                   "m s-1", "m s-1", "K")
        message(paste0(Sys.time(), ": CSAT3:\t Performing Unit Conversion"))
        dataList[[idx]] <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = dataList[[idx]],
                                                                             unitFrom = attributes(dataList[[idx]])$unit,
                                                                             unitTo = "intl"))
        message(paste0(Sys.time(), ": CSAT3:\t Performing Time Regularization"))
        dataList[[idx]] <- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(dataList[[idx]][,"time"], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                                                 dataMeas = dataList[[idx]],
                                                 BgnRglr = as.POSIXlt(min(timeRglr20)),
                                                 EndRglr = as.POSIXlt(max(timeRglr20)+(0.5*(1/Freq20))),
                                                 FreqRglr = Freq20,
                                                 MethRglr = "CybiEc"
        )$dataRglr
        
        message(paste0(Sys.time(), ": CSAT3:\t Adding Timestamp to regularized data"))
        #add timestamp
        dataList[[idx]]$time <- timeRglr20
        #Creating the index to organize the variables in alphabetical order
        idxSoni <- order(names(dataList[[idx]]))
        #Changing the order of the variables to alphabetical order using the index
        dataList[[idx]] <- dataList[[idx]][,idxSoni]
        #Adding units back to output
        attributes(dataList[[idx]])$unit <- c("NA", "NA", "K","NA", "m s-1", "m s-1", 
                                              "m s-1", "m s-1")
        #Adding names to units attributes
        names(attributes(dataList[[idx]])$unit) <- names(dataList[[idx]])
        message(paste0(Sys.time(), ": CSAT3:\t Complete!"))
      } else {
        #####################################################################################################
        #AMRS data wrangling
        #####################################################################################################
        message(paste0(Sys.time(), ": AMRS Data Wrangling"))
        # TODO change this so we are not reordering our variables... This is taking too long to perform
        #Creating the index to organize the variables in alphabetical order
        idxAmrs <- order(names(dataList[[idx]]))
        #Changing the order of the variables to alphabetical order using the index
        dataList[[idx]] <- dataList[[idx]][,idxAmrs]
        
        #Adding units as an attribute
        attr(dataList[[idx]], which = "unit") <- c("m s-2", "m s-2", "m s-2", "m s-2", "m s-2", "m s-2",  "deg", "deg", "deg","rad s-1", "rad s-1", "rad s-1", "NA", "NA")
        #perform unit conversion
        message(paste0(Sys.time(), ": AMRS:\t Performing Unit Conversion"))
        dataList[[idx]] <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = dataList[[idx]],
                                                                             unitFrom = attributes(dataList[[idx]])$unit,
                                                                             unitTo = "intl"))
        # perform time regularization
        message(paste0(Sys.time(), ": AMRS:\t Performing Time Regularization"))
        dataList[[idx]] <- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(dataList[[idx]][,"time"], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                                                 dataMeas = dataList[[idx]],
                                                 BgnRglr = as.POSIXlt(min(timeRglr40)),
                                                 EndRglr = as.POSIXlt(max(timeRglr40)+(0.5*(1/Freq40))),
                                                 FreqRglr = Freq40,
                                                 MethRglr = "CybiEc"
        )$dataRglr
        
        message(paste0(Sys.time(), ": AMRS:\t Adding regularized timestamp"))
        dataList[[idx]]$time <- timeRglr40
        
        #Adding units back to output
        attributes(dataList[[idx]])$unit <- c("m s-2", "m s-2", "m s-2", "m s-2", "m s-2", "m s-2",  
                                              "deg", "deg", "deg","rad s-1", "rad s-1", "rad s-1", "NA", "NA")
        #Adding names to units attributes
        names(attributes(dataList[[idx]])$unit) <- names(dataList[[idx]])
        message(paste0(Sys.time(), ": AMRS:\t Completed!"))
        
      }#end of else statement
      message(paste0(Sys.time(), ": ", idx, " sensor time regularization completed!\n", round(difftime(Sys.time(), startTime.time_regularization, units = "secs"),2)," seconds"))
    } #end for loop
    
    
    #####################################################################################
    #perform de-spiking and calculate derived quantities
    #####################################################################################
    # Assign list for de-spiking algorythm
    dskData <- list()
    for(idx in names(dataList)) {
      
      message(paste0(Sys.time(), ": Starting ", idx))
      
      dskData[[idx]] <- dataList[[idx]][,which(!names(dataList[[idx]]) %in% c("idx", "diag", "diag32","time"))] # Pick the variables to despike
      for(idxVar in colnames(dskData[[idx]])) {
        
        base::source("~/eddy/neon-sensor-test/flow/flow.amrs/def.amrs.cmpr.def.dspk.br86.R")
        
        message(paste0(Sys.time(), ": \tDespiking ", idx, "'s ", idxVar, " field."))
        dskData[[idx]][,idxVar] <- def.dspk.br86(
          # input data, univariate vector of integers or numerics
          dataInp = as.vector(dskData[[idx]][,idxVar]),
          # filter width
          WndwFilt = 9,
          # initial number/step size of histogram bins
          NumBin = 2,
          # resolution threshold
          ThshReso = 10
        )$dataOut
      }
    }
    
    ### NEED SONI DATA FOR THIS....
    # #calculate derived quantities for soni
    # #correction for attitude and motion via AMRS
    # #Convert the angle of installation to radians
    # AngZaxsSoniInst <- eddy4R.base::def.unit.conv(data=AngZaxsSoniInst,unitFrom="deg",unitTo="rad")
    # 
    # #rotate wind vector into meteorological coordinate system (positive from west, south and below)
    # dskData$soni <- def.met.body(AngZaxsSoniInst = AngZaxsSoniInst, veloBody = dskData$soni)
    # 
    # #magnitude of horizontal wind speed
    # dskData$soni$veloXaxsYaxsErth <- sqrt(dskData$soni$veloXaxs^2 + dskData$soni$veloYaxs^2)
    # 
    # # wind direction
    # # need to redo for vector averaging, see REYNflux_P5.R line 139
    # dskData$soni$angZaxsErth <- ff::as.ff((2*pi + atan2(-dskData$soni$veloYaxs[], -dskData$soni$veloXaxs[]))%%(2*pi))
    # #dskData$soni$angZaxsErth <- (2*pi + atan2(-dskData$soni$veloYaxs[], -dskData$soni$veloXaxs[]))%%(2*pi)
    # invisible(gc())
    
    ######################################################################################
    #lag time correction 
    ######################################################################################
    # select variables for which to perform
    sens <- c("soniAmrs02", "soniAmrs03")
    for (idxSens in sens){
      for(idxVar in names(dskData$soniAmrs01)) {
        # actual cross-correlation
        
        lag <- eddy4R.base::def.lag(
          refe = dskData$soniAmrs01[[idxVar]],
          meas = dskData[[idxSens]][[idxVar]],
          # dataRefe = wrk$data$soni,
          # max. 2 s lag time
          # atm. transport time = 1 s = separation distance 0.15 m / minimum mean horizontal wind speed 0.15 m s-1
          # tube transport time = 0.15 s = volume of tube and cell 0.03 L / flow rate 12 L min-1 * 60 s min-1
          lagMax = 2 * Freq40,
          lagCnst = TRUE,
          # only negative lags permitted (reference leads)
          lagNgtvPstv = c("n", "p", "np")[3],
          # consider positive and negative extrema
          lagAll = TRUE,
          freq = Freq40,
          hpf = TRUE
        )
        
        # shift and reassign data
        if(!is.na(lag$lag)) dskData[[idxSens]][[idxVar]] <- DataCombine:::shift(VarVect = dskData[[idxSens]][[idxVar]], shiftBy = - lag$lag, reminder = FALSE)
        
      }
    }
    
    
    
    
    #####################################################################################
    #calculate the inertial velocities for each of AMRS
    ######################################################################################
    #define parameter
    #filter period in second
    filtTime <- 30
    #filter width
    filtWidt <- filtTime * Freq40
    #width of the trailing integration window; value is greater than filtWidt
    intgWndw <- 2*filtWidt 
    
    message(paste0(Sys.time(), ": Calculating internal velocities"))
    
    for (idx in names(dskData)) {
      if(!(idx %in% "soni")){
        #idx <- "soniAmrs01"
        #calculate 40Hz of accDiff/Freq
        dskData[[idx]]$accXaxsDiffIntg <- dskData[[idx]]$accXaxsDiff/Freq40
        dskData[[idx]]$accYaxsDiffIntg <- dskData[[idx]]$accYaxsDiff/Freq40
        dskData[[idx]]$accZaxsDiffIntg <- dskData[[idx]]$accZaxsDiff/Freq40
        
        # #calculate the inertial velocities
        # dskData[[idx]]$accXaxsDiffSum <- rollapply(data = dskData[[idx]]$accXaxsDiffIntg, width = intgWndw,
        #                                             FUN = sum, by = 1, fill = NA, na.rm = TRUE, align = "right")
        # dskData[[idx]]$accYaxsDiffSum <- rollapply(data = dskData[[idx]]$accYaxsDiffIntg, width = intgWndw,
        #                                             FUN = sum, by = 1, fill = NA, na.rm = TRUE, align = "right")
        # dskData[[idx]]$accZaxsDiffSum <- rollapply(data = dskData[[idx]]$accZaxsDiffIntg, width = intgWndw,
        #                                             FUN = sum, by = 1, fill = NA, na.rm = TRUE, align = "right")
        
        #apply low filter and substract the low filter out
        dskData[[idx]]$accXaxsDiffFilt <- dskData[[idx]]$accXaxsDiffIntg - (stats::filter(dskData[[idx]]$accXaxsDiffIntg, rep(1 / filtWidt, filtWidt), sides=2))
        dskData[[idx]]$accYaxsDiffFilt <- dskData[[idx]]$accYaxsDiffIntg - (stats::filter(dskData[[idx]]$accYaxsDiffIntg, rep(1 / filtWidt, filtWidt), sides=2))
        dskData[[idx]]$accZaxsDiffFilt <- dskData[[idx]]$accZaxsDiffIntg - (stats::filter(dskData[[idx]]$accZaxsDiffIntg, rep(1 / filtWidt, filtWidt), sides=2))
        
        
      }
    }
    
    ##########################################################################################
    #calculate the bias (accuracy) and precision of the test units against the reference AMRS
    ##########################################################################################
    message(paste0(Sys.time(), ": Calculating the bias/precision of the test AMRS against the reference AMRS"))
    out <- list()
    
    dataBgn <-1
    dataEnd <-length(dskData$soniAmrs01$accXaxs)
    testVar <- c("accXaxsDiffIntg", "accYaxsDiffIntg", "accZaxsDiffIntg",
                 "accXaxsDiffFilt", "accYaxsDiffFilt", "accZaxsDiffFilt",
                 "angXaxsDeg", "angYaxsDeg", "angZaxsDeg",
                 "avelXaxsDeg", "avelYaxsDeg", "avelZaxsDeg" )
    
    sens <- c("soniAmrs02", "soniAmrs03")
    
    #convert unit of ang from rad to deg for the reference sensor
    dskData$soniAmrs01$angXaxsDeg <- eddy4R.base::def.unit.conv(data = dskData$soniAmrs01$angXaxs, unitFrom = "rad", unitTo = "deg", MethGc = FALSE)
    dskData$soniAmrs01$angYaxsDeg <- eddy4R.base::def.unit.conv(data = dskData$soniAmrs01$angYaxs, unitFrom = "rad", unitTo = "deg", MethGc = FALSE)
    dskData$soniAmrs01$angZaxsDeg <- eddy4R.base::def.unit.conv(data = dskData$soniAmrs01$angZaxs, unitFrom = "rad", unitTo = "deg", MethGc = FALSE)
    #convert unit of angular rate from rad s-1 to deg s-1 for the reference sensor
    dskData$soniAmrs01$avelXaxsDeg <- eddy4R.base::def.unit.conv(data = dskData$soniAmrs01$avelXaxs, unitFrom = "rad s-1", unitTo = "deg s-1", MethGc = FALSE)
    dskData$soniAmrs01$avelYaxsDeg <- eddy4R.base::def.unit.conv(data = dskData$soniAmrs01$avelYaxs, unitFrom = "rad s-1", unitTo = "deg s-1", MethGc = FALSE)
    dskData$soniAmrs01$avelZaxsDeg <- eddy4R.base::def.unit.conv(data = dskData$soniAmrs01$avelZaxs, unitFrom = "rad s-1", unitTo = "deg s-1", MethGc = FALSE)
    
    for (idxSens in sens){
      #convert unit of ang from rad to deg
      dskData[[idxSens]]$angXaxsDeg <- eddy4R.base::def.unit.conv(data = dskData[[idxSens]]$angXaxs, unitFrom = "rad", unitTo = "deg", MethGc = FALSE)
      dskData[[idxSens]]$angYaxsDeg <- eddy4R.base::def.unit.conv(data = dskData[[idxSens]]$angYaxs, unitFrom = "rad", unitTo = "deg", MethGc = FALSE)
      dskData[[idxSens]]$angZaxsDeg <- eddy4R.base::def.unit.conv(data = dskData[[idxSens]]$angZaxs, unitFrom = "rad", unitTo = "deg", MethGc = FALSE)
      #convert unit of angular rate from rad s-1 to deg s-1
      dskData[[idxSens]]$avelXaxsDeg <- eddy4R.base::def.unit.conv(data = dskData[[idxSens]]$avelXaxs, unitFrom = "rad s-1", unitTo = "deg s-1", MethGc = FALSE)
      dskData[[idxSens]]$avelYaxsDeg <- eddy4R.base::def.unit.conv(data = dskData[[idxSens]]$avelYaxs, unitFrom = "rad s-1", unitTo = "deg s-1", MethGc = FALSE)
      dskData[[idxSens]]$avelZaxsDeg <- eddy4R.base::def.unit.conv(data = dskData[[idxSens]]$avelZaxs, unitFrom = "rad s-1", unitTo = "deg s-1", MethGc = FALSE)
      
      for (idxVar in testVar){
        out[[idxSens]][[idxVar]] <- eddy4R.base::def.rmsd.diff.prcs.rsq(refe=dskData$soniAmrs01[[idxVar]][dataBgn:dataEnd],
                                                                        test=dskData[[idxSens]][[idxVar]][dataBgn:dataEnd],
                                                                        Perc = FALSE,Deba=NULL, DebaRltv=FALSE)  
      }
      # TODO Pull out accuracy
    }
    
    stats.soniAmrs02 <- data.table()
    
    # acc___axsDiffIntg
    stats.soniAmrs02$accXaxsDiffIntg.rmsd      <- out$soniAmrs02$accXaxsDiffIntg[1]
    stats.soniAmrs02$accXaxsDiffIntg.diffMean  <- out$soniAmrs02$accXaxsDiffIntg[2]
    stats.soniAmrs02$accXaxsDiffIntg.prcs      <- out$soniAmrs02$accXaxsDiffIntg[3]
    stats.soniAmrs02$accXaxsDiffIntg.rsq       <- out$soniAmrs02$accXaxsDiffIntg[4]
    stats.soniAmrs02$accXaxsDiffIntg.sample    <- out$soniAmrs02$accXaxsDiffIntg[5]
    
    stats.soniAmrs02$accZaxsDiffIntg.rmsd      <- out$soniAmrs02$accZaxsDiffIntg[1]
    stats.soniAmrs02$accZaxsDiffIntg.diffMean  <- out$soniAmrs02$accZaxsDiffIntg[2]
    stats.soniAmrs02$accZaxsDiffIntg.prcs      <- out$soniAmrs02$accZaxsDiffIntg[3]
    stats.soniAmrs02$accZaxsDiffIntg.rsq       <- out$soniAmrs02$accZaxsDiffIntg[4]
    stats.soniAmrs02$accZaxsDiffIntg.sample    <- out$soniAmrs02$accZaxsDiffIntg[5]
    
    stats.soniAmrs02$accYaxsDiffIntg.rmsd      <- out$soniAmrs02$accYaxsDiffIntg[1]
    stats.soniAmrs02$accYaxsDiffIntg.diffMean  <- out$soniAmrs02$accYaxsDiffIntg[2]
    stats.soniAmrs02$accYaxsDiffIntg.prcs      <- out$soniAmrs02$accYaxsDiffIntg[3]
    stats.soniAmrs02$accYaxsDiffIntg.rsq       <- out$soniAmrs02$accYaxsDiffIntg[4]
    stats.soniAmrs02$accYaxsDiffIntg.sample    <- out$soniAmrs02$accYaxsDiffIntg[5]
    
    # acc___axsDiffFilt
    stats.soniAmrs02$accXaxsDiffFilt.rmsd      <- out$soniAmrs02$accXaxsDiffFilt[1]
    stats.soniAmrs02$accXaxsDiffFilt.diffMean  <- out$soniAmrs02$accXaxsDiffFilt[2]
    stats.soniAmrs02$accXaxsDiffFilt.prcs      <- out$soniAmrs02$accXaxsDiffFilt[3]
    stats.soniAmrs02$accXaxsDiffFilt.rsq       <- out$soniAmrs02$accXaxsDiffFilt[4]
    stats.soniAmrs02$accXaxsDiffFilt.sample    <- out$soniAmrs02$accXaxsDiffFilt[5]
    
    stats.soniAmrs02$accYaxsDiffFilt.rmsd      <- out$soniAmrs02$accYaxsDiffFilt[1]
    stats.soniAmrs02$accYaxsDiffFilt.diffMean  <- out$soniAmrs02$accYaxsDiffFilt[2]
    stats.soniAmrs02$accYaxsDiffFilt.prcs      <- out$soniAmrs02$accYaxsDiffFilt[3]
    stats.soniAmrs02$accYaxsDiffFilt.rsq       <- out$soniAmrs02$accYaxsDiffFilt[4]
    stats.soniAmrs02$accYaxsDiffFilt.sample    <- out$soniAmrs02$accYaxsDiffFilt[5]
    
    stats.soniAmrs02$accZaxsDiffFilt.rmsd      <- out$soniAmrs02$accZaxsDiffFilt[1]
    stats.soniAmrs02$accZaxsDiffFilt.diffMean  <- out$soniAmrs02$accZaxsDiffFilt[2]
    stats.soniAmrs02$accZaxsDiffFilt.prcs      <- out$soniAmrs02$accZaxsDiffFilt[3]
    stats.soniAmrs02$accZaxsDiffFilt.rsq       <- out$soniAmrs02$accZaxsDiffFilt[4]
    stats.soniAmrs02$accZaxsDiffFilt.sample    <- out$soniAmrs02$accZaxsDiffFilt[5]
    
    # ang___axsDeg
    stats.soniAmrs02$angYaxsDeg.rmsd      <- out$soniAmrs02$angYaxsDeg[1]
    stats.soniAmrs02$angYaxsDeg.diffMean  <- out$soniAmrs02$angYaxsDeg[2]
    stats.soniAmrs02$angYaxsDeg.prcs      <- out$soniAmrs02$angYaxsDeg[3]
    stats.soniAmrs02$angYaxsDeg.rsq       <- out$soniAmrs02$angYaxsDeg[4]
    stats.soniAmrs02$angYaxsDeg.sample    <- out$soniAmrs02$angYaxsDeg[5]
    
    stats.soniAmrs02$angXaxsDeg.rmsd      <- out$soniAmrs02$angXaxsDeg[1]
    stats.soniAmrs02$angXaxsDeg.diffMean  <- out$soniAmrs02$angXaxsDeg[2]
    stats.soniAmrs02$angXaxsDeg.prcs      <- out$soniAmrs02$angXaxsDeg[3]
    stats.soniAmrs02$angXaxsDeg.rsq       <- out$soniAmrs02$angXaxsDeg[4]
    stats.soniAmrs02$angXaxsDeg.sample    <- out$soniAmrs02$angXaxsDeg[5]
    
    stats.soniAmrs02$angZaxsDeg.rmsd      <- out$soniAmrs02$angZaxsDeg[1]
    stats.soniAmrs02$angZaxsDeg.diffMean  <- out$soniAmrs02$angZaxsDeg[2]
    stats.soniAmrs02$angZaxsDeg.prcs      <- out$soniAmrs02$angZaxsDeg[3]
    stats.soniAmrs02$angZaxsDeg.rsq       <- out$soniAmrs02$angZaxsDeg[4]
    stats.soniAmrs02$angZaxsDeg.sample    <- out$soniAmrs02$angZaxsDeg[5]
    
    # avel___axsDeg
    stats.soniAmrs02$avelXaxsDeg.rmsd<- out$soniAmrs02$avelXaxsDeg[1]
    stats.soniAmrs02$avelXaxsDeg.diffMean<- out$soniAmrs02$avelXaxsDeg[2]
    stats.soniAmrs02$avelXaxsDeg.prcs<- out$soniAmrs02$avelXaxsDeg[3]
    stats.soniAmrs02$avelXaxsDeg.rsq<- out$soniAmrs02$avelXaxsDeg[4]
    stats.soniAmrs02$avelXaxsDeg.sample<- out$soniAmrs02$avelXaxsDeg[5]
    
    stats.soniAmrs02$avelYaxsDeg.rmsd      <- out$soniAmrs02$avelYaxsDeg[1]
    stats.soniAmrs02$avelYaxsDeg.diffMean  <- out$soniAmrs02$avelYaxsDeg[2]
    stats.soniAmrs02$avelYaxsDeg.prcs      <- out$soniAmrs02$avelYaxsDeg[3]
    stats.soniAmrs02$avelYaxsDeg.rsq       <- out$soniAmrs02$avelYaxsDeg[4]
    stats.soniAmrs02$avelYaxsDeg.sample    <- out$soniAmrs02$avelYaxsDeg[5]
    
    stats.soniAmrs02$avelZaxsDeg.rmsd      <- out$soniAmrs02$avelZaxsDeg[1]
    stats.soniAmrs02$avelZaxsDeg.diffMean  <- out$soniAmrs02$avelZaxsDeg[2]
    stats.soniAmrs02$avelZaxsDeg.prcs      <- out$soniAmrs02$avelZaxsDeg[3]
    stats.soniAmrs02$avelZaxsDeg.rsq       <- out$soniAmrs02$avelZaxsDeg[4]
    stats.soniAmrs02$avelZaxsDeg.sample    <- out$soniAmrs02$avelZaxsDeg[5]
    
    ############################################
    
    stats.reshape.1 <- stats.soniAmrs02 %>%
      reshape2::melt() %>%
      tidyr::separate(variable, into = c("measurement", "stat")) %>%
      dplyr::mutate(test.date = date_i) %>% 
      dplyr::mutate(sensor = "soniAmrs02") %>%
      dplyr::select(sensor, test.date, measurement, stat, value)
    
    stats.soniAmrs03 <- data.table()
    
    # acc___axsDiffIntg
    stats.soniAmrs03$accXaxsDiffIntg.rmsd      <- out$soniAmrs03$accXaxsDiffIntg[1]
    stats.soniAmrs03$accXaxsDiffIntg.diffMean  <- out$soniAmrs03$accXaxsDiffIntg[2]
    stats.soniAmrs03$accXaxsDiffIntg.prcs      <- out$soniAmrs03$accXaxsDiffIntg[3]
    stats.soniAmrs03$accXaxsDiffIntg.rsq       <- out$soniAmrs03$accXaxsDiffIntg[4]
    stats.soniAmrs03$accXaxsDiffIntg.sample    <- out$soniAmrs03$accXaxsDiffIntg[5]
    
    stats.soniAmrs03$accZaxsDiffIntg.rmsd      <- out$soniAmrs03$accZaxsDiffIntg[1]
    stats.soniAmrs03$accZaxsDiffIntg.diffMean  <- out$soniAmrs03$accZaxsDiffIntg[2]
    stats.soniAmrs03$accZaxsDiffIntg.prcs      <- out$soniAmrs03$accZaxsDiffIntg[3]
    stats.soniAmrs03$accZaxsDiffIntg.rsq       <- out$soniAmrs03$accZaxsDiffIntg[4]
    stats.soniAmrs03$accZaxsDiffIntg.sample    <- out$soniAmrs03$accZaxsDiffIntg[5]
    
    stats.soniAmrs03$accYaxsDiffIntg.rmsd      <- out$soniAmrs03$accYaxsDiffIntg[1]
    stats.soniAmrs03$accYaxsDiffIntg.diffMean  <- out$soniAmrs03$accYaxsDiffIntg[2]
    stats.soniAmrs03$accYaxsDiffIntg.prcs      <- out$soniAmrs03$accYaxsDiffIntg[3]
    stats.soniAmrs03$accYaxsDiffIntg.rsq       <- out$soniAmrs03$accYaxsDiffIntg[4]
    stats.soniAmrs03$accYaxsDiffIntg.sample    <- out$soniAmrs03$accYaxsDiffIntg[5]
    
    # acc___axsDiffFilt
    stats.soniAmrs03$accXaxsDiffFilt.rmsd      <- out$soniAmrs03$accXaxsDiffFilt[1]
    stats.soniAmrs03$accXaxsDiffFilt.diffMean  <- out$soniAmrs03$accXaxsDiffFilt[2]
    stats.soniAmrs03$accXaxsDiffFilt.prcs      <- out$soniAmrs03$accXaxsDiffFilt[3]
    stats.soniAmrs03$accXaxsDiffFilt.rsq       <- out$soniAmrs03$accXaxsDiffFilt[4]
    stats.soniAmrs03$accXaxsDiffFilt.sample    <- out$soniAmrs03$accXaxsDiffFilt[5]
    
    stats.soniAmrs03$accYaxsDiffFilt.rmsd      <- out$soniAmrs03$accYaxsDiffFilt[1]
    stats.soniAmrs03$accYaxsDiffFilt.diffMean  <- out$soniAmrs03$accYaxsDiffFilt[2]
    stats.soniAmrs03$accYaxsDiffFilt.prcs      <- out$soniAmrs03$accYaxsDiffFilt[3]
    stats.soniAmrs03$accYaxsDiffFilt.rsq       <- out$soniAmrs03$accYaxsDiffFilt[4]
    stats.soniAmrs03$accYaxsDiffFilt.sample    <- out$soniAmrs03$accYaxsDiffFilt[5]
    
    stats.soniAmrs03$accZaxsDiffFilt.rmsd      <- out$soniAmrs03$accZaxsDiffFilt[1]
    stats.soniAmrs03$accZaxsDiffFilt.diffMean  <- out$soniAmrs03$accZaxsDiffFilt[2]
    stats.soniAmrs03$accZaxsDiffFilt.prcs      <- out$soniAmrs03$accZaxsDiffFilt[3]
    stats.soniAmrs03$accZaxsDiffFilt.rsq       <- out$soniAmrs03$accZaxsDiffFilt[4]
    stats.soniAmrs03$accZaxsDiffFilt.sample    <- out$soniAmrs03$accZaxsDiffFilt[5]
    
    # ang___axsDeg
    stats.soniAmrs03$angYaxsDeg.rmsd      <- out$soniAmrs03$angYaxsDeg[1]
    stats.soniAmrs03$angYaxsDeg.diffMean  <- out$soniAmrs03$angYaxsDeg[2]
    stats.soniAmrs03$angYaxsDeg.prcs      <- out$soniAmrs03$angYaxsDeg[3]
    stats.soniAmrs03$angYaxsDeg.rsq       <- out$soniAmrs03$angYaxsDeg[4]
    stats.soniAmrs03$angYaxsDeg.sample    <- out$soniAmrs03$angYaxsDeg[5]
    
    stats.soniAmrs03$angXaxsDeg.rmsd      <- out$soniAmrs03$angXaxsDeg[1]
    stats.soniAmrs03$angXaxsDeg.diffMean  <- out$soniAmrs03$angXaxsDeg[2]
    stats.soniAmrs03$angXaxsDeg.prcs      <- out$soniAmrs03$angXaxsDeg[3]
    stats.soniAmrs03$angXaxsDeg.rsq       <- out$soniAmrs03$angXaxsDeg[4]
    stats.soniAmrs03$angXaxsDeg.sample    <- out$soniAmrs03$angXaxsDeg[5]
    
    stats.soniAmrs03$angZaxsDeg.rmsd      <- out$soniAmrs03$angZaxsDeg[1]
    stats.soniAmrs03$angZaxsDeg.diffMean  <- out$soniAmrs03$angZaxsDeg[2]
    stats.soniAmrs03$angZaxsDeg.prcs      <- out$soniAmrs03$angZaxsDeg[3]
    stats.soniAmrs03$angZaxsDeg.rsq       <- out$soniAmrs03$angZaxsDeg[4]
    stats.soniAmrs03$angZaxsDeg.sample    <- out$soniAmrs03$angZaxsDeg[5]
    
    # avel___axsDeg
    stats.soniAmrs03$avelXaxsDeg.rmsd      <- out$soniAmrs03$avelXaxsDeg[1]
    stats.soniAmrs03$avelXaxsDeg.diffMean  <- out$soniAmrs03$avelXaxsDeg[2]
    stats.soniAmrs03$avelXaxsDeg.prcs      <- out$soniAmrs03$avelXaxsDeg[3]
    stats.soniAmrs03$avelXaxsDeg.rsq       <- out$soniAmrs03$avelXaxsDeg[4]
    stats.soniAmrs03$avelXaxsDeg.sample    <- out$soniAmrs03$avelXaxsDeg[5]
    
    stats.soniAmrs03$avelYaxsDeg.rmsd      <- out$soniAmrs03$avelYaxsDeg[1]
    stats.soniAmrs03$avelYaxsDeg.diffMean  <- out$soniAmrs03$avelYaxsDeg[2]
    stats.soniAmrs03$avelYaxsDeg.prcs      <- out$soniAmrs03$avelYaxsDeg[3]
    stats.soniAmrs03$avelYaxsDeg.rsq       <- out$soniAmrs03$avelYaxsDeg[4]
    stats.soniAmrs03$avelYaxsDeg.sample    <- out$soniAmrs03$avelYaxsDeg[5]
    
    stats.soniAmrs03$avelZaxsDeg.rmsd      <- out$soniAmrs03$avelZaxsDeg[1]
    stats.soniAmrs03$avelZaxsDeg.diffMean  <- out$soniAmrs03$avelZaxsDeg[2]
    stats.soniAmrs03$avelZaxsDeg.prcs      <- out$soniAmrs03$avelZaxsDeg[3]
    stats.soniAmrs03$avelZaxsDeg.rsq       <- out$soniAmrs03$avelZaxsDeg[4]
    stats.soniAmrs03$avelZaxsDeg.sample    <- out$soniAmrs03$avelZaxsDeg[5]

    ##########################################################################################################
    #calculate resolution
    ##########################################################################################################\
    message(paste0(Sys.time(), ": Calculating resolution"))
    reso <- list()
    sens <- c("soniAmrs01", "soniAmrs02", "soniAmrs03")
    testVar <- c("angXaxsDeg", "angYaxsDeg", "angZaxsDeg",
                 "avelXaxsDeg", "avelYaxsDeg", "avelZaxsDeg" )
    for (idxSens in sens){
      for (idxVar in testVar){
        reso[[idxSens]][[idxVar]] <- as.data.frame(diff(dskData[[idxSens]][[idxVar]][dataBgn:dataEnd], lag =1, differences =1, na.rm = T))
        colnames(reso[[idxSens]][[idxVar]]) <- idxVar
        reso[[idxSens]][[idxVar]][[idxVar]] <- abs(reso[[idxSens]][[idxVar]][[idxVar]])
        #plot histogram
        if (idxVar %in% c("angXaxsDeg", "angYaxsDeg", "angZaxsDeg")) {
          xlimMax <- 0.1
        }else {
          xlimMax <- 0.5
        }
        png(filename = paste0(here(), "/data/2021_AMRS_Round_3/plots/histReso_", idxSens,idxVar,".png"))
        hist(reso[[idxSens]][[idxVar]][[idxVar]], breaks = 500, xlim = c(0,xlimMax), xlab = idxVar, main = idxSens)
        dev.off()
      }
    }
    
    ### soniAmrs01 Tidying
    message(paste0(Sys.time(), ": Tidying soniAmrs01 reso data..."))
    
    stats.soniAmrs01 <- data.table()
    
    # Grab the median resolution
    stats.soniAmrs01$angXaxsDeg.reso <- summary(reso$soniAmrs01$angXaxsDeg)[3] 
    stats.soniAmrs01$angYaxsDeg.reso <- summary(reso$soniAmrs01$angYaxsDeg)[3] 
    stats.soniAmrs01$angZaxsDeg.reso <- summary(reso$soniAmrs01$angZaxsDeg)[3] 
    stats.soniAmrs01$avelXaxsDeg.reso <- summary(reso$soniAmrs01$avelXaxsDeg)[3] 
    stats.soniAmrs01$avelYaxsDeg.reso <- summary(reso$soniAmrs01$avelYaxsDeg)[3] 
    stats.soniAmrs01$avelZaxsDeg.reso <- summary(reso$soniAmrs01$avelZaxsDeg)[3] 
    
    # Tidy the data a little
    stats.soniAmrs01 <- stats.soniAmrs01 %>%
      tidyr::separate(angXaxsDeg.reso, into = c("stat","angXaxsDeg.reso"), sep = ":") %>%
      tidyr::separate(angYaxsDeg.reso, into = c("stat","angYaxsDeg.reso"), sep = ":") %>%
      tidyr::separate(angZaxsDeg.reso, into = c("stat","angZaxsDeg.reso"), sep = ":") %>%
      tidyr::separate(avelXaxsDeg.reso, into = c("stat","avelXaxsDeg.reso"), sep = ":") %>%
      tidyr::separate(avelYaxsDeg.reso, into = c("stat","avelYaxsDeg.reso"), sep = ":") %>%
      tidyr::separate(avelZaxsDeg.reso, into = c("stat","avelZaxsDeg.reso"), sep = ":") %>%
      dplyr::select(-stat)
    
    # Transpose the data
    stats.soniAmrs01.reshape <- data.table::as.data.table(stats.soniAmrs01) %>%
      data.table::transpose()
    
    # Name the values - tidy style
    stats.soniAmrs01.reshape$measurement <- colnames(stats.soniAmrs01)
    names(stats.soniAmrs01.reshape) <- c("value", "measurement")
    
    # Finalize the data
    stats.soniAmrs01.reshape <- stats.soniAmrs01.reshape %>%
      dplyr::mutate(test.date = date_i) %>% 
      dplyr::mutate(sensor = "soniAmrs01") %>%
      tidyr::separate(measurement, into = c("measurement", "stat")) %>%
      dplyr::select(sensor, test.date, measurement, stat, value)
    
    ### soniAmrs02 Tidying
    message(paste0(Sys.time(), ": Tidying soniAmrs02 reso data..."))
    
    stats.soniAmrs02.reso <- data.table()
    
    # Grab the median resolution
    stats.soniAmrs02.reso$angXaxsDeg.reso <- summary(reso$soniAmrs02$angXaxsDeg)[3] 
    stats.soniAmrs02.reso$angYaxsDeg.reso <- summary(reso$soniAmrs02$angYaxsDeg)[3] 
    stats.soniAmrs02.reso$angZaxsDeg.reso <- summary(reso$soniAmrs02$angZaxsDeg)[3] 
    stats.soniAmrs02.reso$avelXaxsDeg.reso <- summary(reso$soniAmrs02$avelXaxsDeg)[3] 
    stats.soniAmrs02.reso$avelYaxsDeg.reso <- summary(reso$soniAmrs02$avelYaxsDeg)[3] 
    stats.soniAmrs02.reso$avelZaxsDeg.reso <- summary(reso$soniAmrs02$avelZaxsDeg)[3] 
    
    # Tidy the data a little
    stats.soniAmrs02.reso <- stats.soniAmrs02.reso %>%
      tidyr::separate(angXaxsDeg.reso, into = c("stat","angXaxsDeg.reso"), sep = ":") %>%
      tidyr::separate(angYaxsDeg.reso, into = c("stat","angYaxsDeg.reso"), sep = ":") %>%
      tidyr::separate(angZaxsDeg.reso, into = c("stat","angZaxsDeg.reso"), sep = ":") %>%
      tidyr::separate(avelXaxsDeg.reso, into = c("stat","avelXaxsDeg.reso"), sep = ":") %>%
      tidyr::separate(avelYaxsDeg.reso, into = c("stat","avelYaxsDeg.reso"), sep = ":") %>%
      tidyr::separate(avelZaxsDeg.reso, into = c("stat","avelZaxsDeg.reso"), sep = ":") %>%
      dplyr::select(-stat)
    
    # Transpose the data
    stats.soniAmrs02.reso.reshape <- data.table::as.data.table(stats.soniAmrs02.reso) %>%
      data.table::transpose()
    
    # Name the values - tidy style
    stats.soniAmrs02.reso.reshape$measurement <- colnames(stats.soniAmrs02.reso)
    names(stats.soniAmrs02.reso.reshape) <- c("value", "measurement")
    
    # Finalize the data
    stats.soniAmrs02.reso.reshape <- stats.soniAmrs02.reso.reshape %>%
      dplyr::mutate(test.date = date_i) %>% 
      dplyr::mutate(sensor = "soniAmrs02") %>%
      tidyr::separate(measurement, into = c("measurement", "stat")) %>%
      dplyr::select(sensor, test.date, measurement, stat, value)
    
    ### soniAmrs3 Tidying
    message(paste0(Sys.time(), ": Tidying soniAmrs03 reso data...") )
    
    stats.soniAmrs03.reso <- data.table()
    
    # Grab the median resolution
    stats.soniAmrs03.reso$angXaxsDeg.reso <- summary(reso$soniAmrs03$angXaxsDeg)[3] 
    stats.soniAmrs03.reso$angYaxsDeg.reso <- summary(reso$soniAmrs03$angYaxsDeg)[3] 
    stats.soniAmrs03.reso$angZaxsDeg.reso <- summary(reso$soniAmrs03$angZaxsDeg)[3] 
    stats.soniAmrs03.reso$avelXaxsDeg.reso <- summary(reso$soniAmrs03$avelXaxsDeg)[3] 
    stats.soniAmrs03.reso$avelYaxsDeg.reso <- summary(reso$soniAmrs03$avelYaxsDeg)[3] 
    stats.soniAmrs03.reso$avelZaxsDeg.reso <- summary(reso$soniAmrs03$avelZaxsDeg)[3] 
    
    # Tidy the data a little
    stats.soniAmrs03.reso <- stats.soniAmrs03.reso %>%
      tidyr::separate(angXaxsDeg.reso, into = c("stat","angXaxsDeg.reso"), sep = ":") %>%
      tidyr::separate(angYaxsDeg.reso, into = c("stat","angYaxsDeg.reso"), sep = ":") %>%
      tidyr::separate(angZaxsDeg.reso, into = c("stat","angZaxsDeg.reso"), sep = ":") %>%
      tidyr::separate(avelXaxsDeg.reso, into = c("stat","avelXaxsDeg.reso"), sep = ":") %>%
      tidyr::separate(avelYaxsDeg.reso, into = c("stat","avelYaxsDeg.reso"), sep = ":") %>%
      tidyr::separate(avelZaxsDeg.reso, into = c("stat","avelZaxsDeg.reso"), sep = ":") %>%
      dplyr::select(-stat)
    
    # Transpose the data
    stats.soniAmrs03.reso.reshape <- data.table::as.data.table(stats.soniAmrs03.reso) %>%
      data.table::transpose()
    
    # Name the values - tidy style
    stats.soniAmrs03.reso.reshape$measurement <- colnames(stats.soniAmrs03.reso)
    names(stats.soniAmrs03.reso.reshape) <- c("value", "measurement")
    
    # Finalize the data
    stats.soniAmrs03.reso.reshape <- stats.soniAmrs03.reso.reshape %>%
      dplyr::mutate(test.date = date_i) %>% 
      dplyr::mutate(sensor = "soniAmrs03") %>%
      tidyr::separate(measurement, into = c("measurement", "stat")) %>%
      dplyr::select(sensor, test.date, measurement, stat, value)
    
    ############################
    
    stats.reshape.2 <- stats.soniAmrs03 %>%
      reshape2::melt() %>%
      tidyr::separate(variable, into = c("measurement", "stat")) %>%
      dplyr::mutate(test.date = date_i) %>% 
      dplyr::mutate(sensor = "soniAmrs03") %>%
      dplyr::select(sensor, test.date, measurement, stat, value)
    
    stats.reshape.final <- data.table::rbindlist(l = list(stats.reshape.1, stats.reshape.2, stats.soniAmrs01.reshape,stats.soniAmrs02.reso.reshape,stats.soniAmrs03.reso.reshape))
    
    message(paste0(Sys.time(), ": Saving rmsd.diff.prcs.rsq.reso file!. . ."))
    saveRDS(stats.reshape.final, paste0(here::here(),"/data/2021_AMRS_Round_3/stats/", date_i,".RDS"))

    message("Data Aggregation")
    # assign list
    # assign list for working parameters and variables
    wrk <- list()
    rpt <- list()
    #Assign list for outputs from qfqm
    qfqm <- list()
    
    # begin and end time for each averaging intervals
    avgTime <- c(60, 90, 1800)
    #avgTime <- c(1800)
    for (idxAvgTime in avgTime) {
      nameTime <- paste0("avg", idxAvgTime)
      invisible(lapply(names(dataList), function(x) {
        if(x == "soni") {
          wrk$idx[[nameTime]][[x]] <<-eddy4R.base::def.idx.agr(time = dataList$soni$time, PrdAgr = idxAvgTime, FreqLoca = 20)
        }  else {  
          wrk$idx[[nameTime]][[x]] <<- eddy4R.base::def.idx.agr(time = dataList$soniAmrs01$time, PrdAgr = idxAvgTime, FreqLoca = 40)
        }}))
      #number of iteration
      iter <- max(sapply(names(wrk$idx[[nameTime]]), function(x) length(wrk$idx[[nameTime]][[x]]$idxBgn)))
      numAgr <- 0
      #begin: loop around aggregation interval
      for(idxAgr in 1:iter){
        # idxAgr <- 1
        
        numAgr <- numAgr + 1
        # create a list identifier for the Aggregation loops
        levlAgr <- paste0("numAgr", ifelse(numAgr < 10, paste0("0",numAgr) ,numAgr))
        #assign list
        wrk$data <- list()
        
        # loop around sensors
        for(idxSens in names(dataList)){
          #idxSens <- "soni"
          wrk$data[[nameTime]][[idxSens]] <- dskData[[idxSens]][wrk$idx[[nameTime]][[idxSens]]$idxBgn[idxAgr]:wrk$idx[[nameTime]][[idxSens]]$idxEnd[idxAgr],]  
        }; rm()
        
        #assign lists
        #for data
        wrk$tmp$data <- list()
        # assemble data
        # for soni
        # wrk$tmp$data$soni <- data.frame(stringsAsFactors = FALSE,
        #                                 "veloXaxsErth"     = wrk$data[[nameTime]]$soni$veloXaxs,
        #                                 "veloYaxsErth"     = wrk$data[[nameTime]]$soni$veloYaxs, 
        #                                 "veloZaxsErth"     = wrk$data[[nameTime]]$soni$veloZaxs,
        #                                 "veloXaxsYaxsErth" = wrk$data[[nameTime]]$soni$veloXaxsYaxsErth,
        #                                 "angZaxsErth"      = wrk$data[[nameTime]]$soni$angZaxsErth,
        #                                 "tempSoni"         = wrk$data[[nameTime]]$soni$tempSoni
        # )
        wrk$tmp$data$soniAmrs01 <- wrk$data[[nameTime]]$soniAmrs01
        wrk$tmp$data$soniAmrs02 <- wrk$data[[nameTime]]$soniAmrs02
        wrk$tmp$data$soniAmrs03 <- wrk$data[[nameTime]]$soniAmrs03
        
        # 30-minute data products
        # call dp01 processing, assign each result as list element numAgr in wrk$dp01
        wrk$dp01[[nameTime]][[levlAgr]] <- eddy4R.base::wrap.dp01(
          # assign data: data.frame or list of type numeric or integer
          data = wrk$tmp$data,
          # if data is a list, which list entries should be processed into Level 1 data products?
          # defaults to NULL which expects data to be a data.frame
          idx = c("soniAmrs01", "soniAmrs02", "soniAmrs03")
        )
      }
      
      # clean up
      wrk$data <- NULL
      wrk$tmp <- NULL
      invisible(gc())
      
      #concatenate results
      
      #loop around data products
      for(idxDp01 in names(wrk$dp01[[nameTime]][[1]])) {
        #idxDp01 <-  names(wrk$dp01[[1]])[1]
        #nameTime <- paste0("avg", idxAvgTime)
        rpt$time[[nameTime]][[idxDp01]] <- data.frame(
          timeBgn = wrk$idx[[nameTime]][[idxDp01]]$timeBgn,
          timeEnd = wrk$idx[[nameTime]][[idxDp01]]$timeEnd
        )[1:length(wrk$dp01[[nameTime]]),]
        
        rpt$data[[nameTime]][[idxDp01]] <- 
          
          #first call to lapply, targeting the result data.frames to be created (data sub-products: mean, min, max, vari", numSamp)
          lapply(names(wrk$dp01[[nameTime]][[1]][[idxDp01]]), function(y)
            
            # second call to lapply, targeting the observations to be combined into the result data.frames
            do.call(rbind, lapply(1:length(wrk$dp01[[nameTime]]), function(x) wrk$dp01[[nameTime]][[x]][[idxDp01]][[y]] ))
            
          )
        #assign names to data.frames      
        names(rpt$data[[nameTime]][[idxDp01]]) <- names(wrk$dp01[[nameTime]][[1]][[idxDp01]])
        #calculate standard deviation
        rpt$data[[nameTime]][[idxDp01]]$sd <- sqrt(rpt$data[[nameTime]][[idxDp01]]$vari)
        
      }
    }
    
    
    #convert the unit mean and sd of angle to degree
    soniAmrs <- c("soniAmrs01", "soniAmrs02", "soniAmrs03")
    for (idxAvg in names(rpt$data)){
      for (idxSoniAmrs in soniAmrs){
        #mean
        rpt$data[[idxAvg]][[idxSoniAmrs]]$mean$angXaxsDeg <- 
          eddy4R.base::def.unit.conv(data = rpt$data[[idxAvg]][[idxSoniAmrs]]$mean$angXaxs, unitFrom = "rad", unitTo = "deg", MethGc = FALSE)
        rpt$data[[idxAvg]][[idxSoniAmrs]]$mean$angYaxsDeg <- 
          eddy4R.base::def.unit.conv(data = rpt$data[[idxAvg]][[idxSoniAmrs]]$mean$angYaxs, unitFrom = "rad", unitTo = "deg", MethGc = FALSE)
        #apply rotation before unit conversion
        rpt$data[[idxAvg]][[idxSoniAmrs]]$mean$angZaxsDeg <- 
          eddy4R.base::def.unit.conv(eddy4R.base::def.rot.enu.ned(angEnu = rpt$data[[idxAvg]][[idxSoniAmrs]]$mean$angZaxs), unitFrom = "rad", unitTo = "deg")$data
        #sd 
        rpt$data[[idxAvg]][[idxSoniAmrs]]$sd$angXaxsDeg <- 
          eddy4R.base::def.unit.conv(data = rpt$data[[idxAvg]][[idxSoniAmrs]]$sd$angXaxs, unitFrom = "rad", unitTo = "deg", MethGc = FALSE)
        rpt$data[[idxAvg]][[idxSoniAmrs]]$sd$angYaxsDeg <- 
          eddy4R.base::def.unit.conv(data = rpt$data[[idxAvg]][[idxSoniAmrs]]$sd$angYaxs, unitFrom = "rad", unitTo = "deg", MethGc = FALSE)
        rpt$data[[idxAvg]][[idxSoniAmrs]]$sd$angZaxsDeg <- 
          eddy4R.base::def.unit.conv(data = rpt$data[[idxAvg]][[idxSoniAmrs]]$sd$angZaxs, unitFrom = "rad", unitTo = "deg", MethGc = FALSE)
      }
    }
    
    message("Saving rpt master aggregation file")
    saveRDS(object = rpt, file = paste0(here::here(), "/data/2021_AMRS_Round_3/final_outputs/", date_i, ".RDS"))
    
    
  
     
  }
}

compare_amrs_data()
