# This script downloads data for the AMRS tests, extracts necessary testing info from the files, then uploads the condensed data to the neon-sensor-test S3 Bucket
download_amrs_comparison_data = function(sensorID = "AMRS_01", round = "3"){
  message(paste0(Sys.time(), ": calling libraries"))
  # Library
  library(aws.s3)
  library(aws.signature)
  library(dplyr)
  library(stringr)
  library(data.table)
  
  message(paste0(Sys.time(), ": connecting to S3"))
  amrs_bucket = "dev-sae-artifacts"
  
  # S3 Connection
  Sys.setenv("AWS_ACCESS_KEY_ID"     = amrs_bucket,
             "AWS_S3_ENDPOINT"       = "neonscience.org",
             "AWS_DEFAULT_REGION"    = "test-s3.data")
  
  message(paste0(Sys.time(), ": reading in available test data"))
  # Logical control
  if(sensorID == "AMRS_01"){
    test_data_lookup = aws.s3::get_bucket_df(bucket = amrs_bucket, prefix = "AMRS_tests/int040/") %>% 
      dplyr::filter(stringr::str_detect(string = Key, pattern = ".l0p.h5.gz") == TRUE)
  } else if(sensorID == "AMRS_02"){
    test_data_lookup = aws.s3::get_bucket_df(bucket = amrs_bucket, prefix = "AMRS_tests/int041/") %>% 
      dplyr::filter(stringr::str_detect(string = Key, pattern = ".l0p.h5.gz") == TRUE)
  } else if(sensorID == "AMRS_03"){
    test_data_lookup = aws.s3::get_bucket_df(bucket = amrs_bucket, prefix = "AMRS_tests/dev042/") %>% 
      dplyr::filter(stringr::str_detect(string = Key, pattern = ".l0p.h5.gz") == TRUE)
  } else {
    stop("Sensor ID not allowed, must be AMRS_01, AMRS_02, or AMRS_03")
  }
  
  test_folder = paste0(sensorID, "")
  
  message(paste0(Sys.time(), ": establishing temp directory"))
  temp_dir_path = "/tmp/amrs_tests/"
  if(base::dir.exists(temp_dir_path)){} else {
    base::dir.create(path = temp_dir_path)
  }
  message(paste0(Sys.time(), ": establishing temp sensorID directory"))
  if(base::dir.exists(paste0(temp_dir_path, "/", sensorID))){} else {
    base::dir.create(path = paste0(temp_dir_path, "/", sensorID))
  }
  
  message(paste0(Sys.time(), ": downloading data"))
  for(i in base::seq_along(test_data_lookup$Key)){
    
    # Form Download URL Path
    download_s3_path = test_data_lookup$Key[i]
    file_date = str_extract(download_s3_path, "[0-9]{4}-[0-9]{2}-[0-9]{2}")
    download_url = paste0("https://", amrs_bucket, ".test-s3.data.neonscience.org/", download_s3_path)
    
    # Form Local Save Path
    local_file_name = base::substr(test_data_lookup$Key[i], start = 19, stop = 999)
    local_save_path = paste0(temp_dir_path, sensorID, "/", local_file_name)
    
    utils::download.file(url = download_url, destfile = local_save_path, quiet = TRUE)
    
    # Check files downloaded properly
    if(file.exists(local_save_path)){
      # Unzip the files
      message(paste0(Sys.time(), ": unzipping file"))
      R.utils::gunzip(local_save_path)
      
      # Unzipped file name
      localfile = gsub(local_save_path, pattern = ".gz", replacement = "")
      
      localfile.ls = rhdf5::h5ls(file = localfile,
                                 datasetinfo = FALSE)
      
      if(nrow(localfile.ls) > 0){
        
        localfile.check = localfile.ls %>%
          dplyr::filter(stringr::str_detect(string = group, pattern = "dp0p/data") == TRUE & name == "amrs")
        
        if(nrow(localfile.check) == 1){
          
          h5.path = paste0(localfile.check$group[1], "/", localfile.check$name[1])
          
          data.in = rhdf5::h5read(file = localfile, name = h5.path)
          
          ml = names(data.in)
          
          data.out = data.in[[ml]]
          
          data.out.len = length(data.out)
          
          if(data.out.len == 14){
            message(paste0(Sys.time(), ": extracting data"))
            
            # Set up S3 Write Creds
            write_key = readRDS("~/eddy/neon-sensor-test/write.RDS")
            Sys.setenv("AWS_ACCESS_KEY_ID"     = "sensor-test-writer",
                       "AWS_SECRET_ACCESS_KEY" =  write_key,
                       "AWS_S3_ENDPOINT"       = "neonscience.org",
                       "AWS_DEFAULT_REGION"    = "s3.data")
            
            s3filename = paste0("sensor/amrs/round_", round, "/", file_date, "/", sensorID, ".fst")
            
            # Make data into a data frame
            data.save = data.table::data.table() 
            data.save$accXaxs = data.out$accXaxs
            data.save$accXaxsDiff = data.out$accXaxsDiff
            data.save$accYaxs = data.out$accYaxs
            data.save$accYaxsDiff = data.out$accYaxsDiff
            data.save$accZaxs = data.out$accZaxs
            data.save$accZaxsDiff = data.out$accZaxsDiff
            data.save$angXaxs = data.out$angXaxs
            data.save$angYaxs = data.out$angYaxs
            data.save$angZaxs = data.out$angZaxs
            data.save$avelXaxs = data.out$avelXaxs
            data.save$avelYaxs = data.out$avelYaxs
            data.save$avelZaxs = data.out$avelZaxs
            data.save$idx = data.out$idx
            data.save$time = data.out$time
            
            # Format time
            data.save = data.save %>% 
              dplyr::mutate(time = as.POSIXct(time,format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
            
            # fst files have better compression read about 1.5 times faster and save at the same size with the 100 compression
            # reading in a single column is twice as fast as reading in just 1 column, so if memory is limited this may be a good approach, but otherwise its smarter to just read in all the data
            message(paste0(Sys.time(), ": saving data"))
            aws.s3::s3write_using(x = data.save, FUN = fst::write.fst, compress = 100, object = s3filename, bucket = "neon-sensor-test")
            
            # Delete all data in the tmp folder
            lapply(X = list.files(paste0(temp_dir_path, sensorID, "/"), full.names = TRUE), file.remove)
            
          } else {
            stop("Unexpected number of columns, check data.out variable...")
          }
        } else {
          stop("AMRS L0p data not found")
        }
      } else {
        stop("File did not have any data...?")
      }
    } else {
      stop("Files did not download properly...")
    }
    
    
    
    
  }
  
  
    
    
   
}


download_amrs_comparison_data(sensorID = "AMRS_01")
.rs.restartR()
download_amrs_comparison_data(sensorID = "AMRS_02")
.rs.restartR()
download_amrs_comparison_data(sensorID = "AMRS_03")
