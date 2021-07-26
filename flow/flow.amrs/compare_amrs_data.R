compare_amrs_data = function(){
  # library()
  library(aws.s3)
  library(aws.signature)
  library(dplyr)
  library(data.table)
  library(stringr)
  library(fst)
  
  
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
      startTime.time_regularization <- Sys.time()()
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
        message(paste0(Sys.time()(), ": AMRS:\t Performing Unit Conversion"))
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
        
        message(paste0(Sys.time()(), ": AMRS:\t Adding regularized timestamp"))
        dataList[[idx]]$time <- timeRglr40
        
        #Adding units back to output
        attributes(dataList[[idx]])$unit <- c("m s-2", "m s-2", "m s-2", "m s-2", "m s-2", "m s-2",  
                                              "deg", "deg", "deg","rad s-1", "rad s-1", "rad s-1", "NA", "NA")
        #Adding names to units attributes
        names(attributes(dataList[[idx]])$unit) <- names(dataList[[idx]])
        message(paste0(Sys.time(), ": AMRS:\t Completed!"))
        
      }#end of else statement
      message(paste0(idx, " sensor time regularization completed!\n", round(difftime(Sys.time()(), startTime.time_regularization, units = "secs"),2)," seconds"))
    }#end for loop
    



    
    
    
    
  }
  
  
}