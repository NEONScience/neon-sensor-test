# Amrs round 3
# Pull wind data from published data
# We need wind direction and wind speed
clean_wind_data = function(){
  
  # .rs.restartR()
  
  # Libraries
  library(aws.s3)
  library(aws.signature)
  library(data.table)
  library(dplyr)
  library(rhdf5)
  
  amrs_bucket = "dev-sae-artifacts"
  Sys.unsetenv("AWS_ACCESS_KEY_ID"     )
  Sys.unsetenv("AWS_S3_ENDPOINT"       )
  Sys.unsetenv("AWS_DEFAULT_REGION"    )
  Sys.unsetenv("AWS_SECRET_ACCESS_KEY" )
  
  Sys.setenv(
    "AWS_ACCESS_KEY_ID"     = amrs_bucket,
    "AWS_S3_ENDPOINT"       = "neonscience.org",
    "AWS_DEFAULT_REGION"    = "test-s3.data"
  )
  
  pub_data_files_run1 = aws.s3::get_bucket_df(bucket = amrs_bucket, prefix = "AMRS_tests/int040/") %>% 
    dplyr::filter(stringr::str_detect(string = Key, pattern = ".l0p.h5.gz") == TRUE) %>% 
    dplyr::mutate(run = 1)

  pub_data_files_run2 = aws.s3::get_bucket_df(bucket = amrs_bucket, prefix = "AMRS_tests/run_2/int040/") %>% 
    dplyr::filter(stringr::str_detect(string = Key, pattern = ".l0p.h5.gz") == TRUE) %>% 
    dplyr::mutate(run = 2)

  pub_data_files_run3 = aws.s3::get_bucket_df(bucket = amrs_bucket, prefix = "AMRS_tests/run_3/int040/") %>% 
    dplyr::filter(stringr::str_detect(string = Key, pattern = ".l0p.h5.gz") == TRUE) %>% 
    dplyr::mutate(run = 3)
  
  
  pub_data_files = data.table::rbindlist(l = list(pub_data_files_run1, pub_data_files_run2, pub_data_files_run3))

  temp_dir = tempdir()
  
  for(i in seq_along(pub_data_files$Key)){
    
    # Create url variable based upon key in for loop
    url = paste0("https://dev-sae-artifacts.test-s3.data.neonscience.org/", pub_data_files$Key[i])
    
    # Create destination file
    if(pub_data_files$run[i] == 1){
      zip_location = paste0(temp_dir, "/", base::substr(pub_data_files$Key[i], 19, 999))
      s3_date = base::substr(pub_data_files$Key[i], 52, 61)
    } else if(pub_data_files$run[i] == 2 | pub_data_files$run[i] == 3){
      zip_location = paste0(temp_dir, "/", base::substr(pub_data_files$Key[i], 25, 999))
      s3_date = base::substr(pub_data_files$Key[i], 58, 67)
    } else {
      stop("Run invalid: ", pub_data_files$run[i])
    }
    
    # Download the zip file
    utils::download.file(
      url = url,
      destfile = zip_location
    )
    
    ### Unzip the file
    # Create unzip location
    h5_location = base::substr(zip_location, 1, 66)
    # Unzip
    R.utils::gunzip(
      filename = zip_location, 
      destname = h5_location
    )
    
    # Check for soni data
    ls_file = rhdf5::h5ls(file = h5_location, datasetinfo = FALSE)
    
    if(nrow(ls_file) > 0){
      
      ls_soni = ls_file %>% 
        dplyr::filter(name == "soni") %>% 
        dplyr::filter(stringr::str_detect(string = group, pattern = "dp0p") == TRUE)
      
      if(nrow(ls_soni) == 2){
        
        ls_soni_read = ls_soni %>% 
          tidyr::unite(col = h5_path, group, name, sep = "/")
        
        # Read in file
        soni_read = rhdf5::h5read(file = h5_location, name = ls_soni_read$h5_path[1])
        # browser()
        soni_qfqm_read = rhdf5::h5read(file = h5_location, name = ls_soni_read$h5_path[2])
        # Delete files
        file.remove(h5_location)
        
        # browser()
        # Clean/Compress QFQM data
        # soni_qfqm_raw = data.table::as.data.table(soni_qfqm_read$`000_040`)
        # message(paste0("total soni rows: ", nrow(soni_qfqm_raw)))
        # # Aggregate data into 1 second bins
        # soni_qfqm_data = soni_qfqm_raw %>% 
        #   dplyr::mutate(time = substr(time, 0, 19)) %>% # remove fractional time
        #   dplyr::group_by(time) %>% 
        #   dplyr::summarise(
        #     idx = mean(idx, na.rm = TRUE),
        #     tempSoni = mean(tempSoni, na.rm = TRUE),
        #     veloSoni = mean(veloSoni, na.rm = TRUE),
        #     veloXaxs = mean(veloXaxs, na.rm = TRUE),
        #     veloYaxs = mean(veloYaxs, na.rm = TRUE),
        #     veloZaxs = mean(veloZaxs, na.rm = TRUE)
        #   )
        # 
        
        
        # Clean/Compress Wind data
        soni_data_raw = data.table::as.data.table(soni_read$`000_040`)
        message(paste0("total soni rows: ", nrow(soni_data_raw)))
        # Aggregate data into 1 second bins
        soni_data = soni_data_raw %>% 
          dplyr::mutate(time = substr(time, 0, 19)) %>% # remove fractional time
          dplyr::group_by(time) %>% 
          dplyr::summarise(
            idx = mean(idx, na.rm = TRUE),
            tempSoni = mean(tempSoni, na.rm = TRUE),
            veloSoni = mean(veloSoni, na.rm = TRUE),
            veloXaxs = mean(veloXaxs, na.rm = TRUE),
            veloYaxs = mean(veloYaxs, na.rm = TRUE),
            veloZaxs = mean(veloZaxs, na.rm = TRUE)
          )
        
        
        
        # Set up S3 Write Creds
        Sys.setenv(
          "AWS_ACCESS_KEY_ID"     = "sensor-test-writer",
          "AWS_SECRET_ACCESS_KEY" =  readRDS("~/eddy/neon-sensor-test/write.RDS"),
          "AWS_S3_ENDPOINT"       = "neonscience.org",
          "AWS_DEFAULT_REGION"    = "s3.data"
        )
        # Set up place to save the data 
        # Going to just make it it's own folder
        s3_path = paste0("sensor/amrs/soni/", s3_date, ".RDS")
        
        aws.s3::s3saveRDS(x = soni_data, object = s3_path, bucket = amrs_bucket)
        
      } else{
        message("Not 2 dp0p soni files")
        browser()
      }
    } else {
      stop("ls showed no data in the file")
    }
  }
  
  Sys.unsetenv("AWS_ACCESS_KEY_ID"     )
  Sys.unsetenv("AWS_S3_ENDPOINT"       )
  Sys.unsetenv("AWS_DEFAULT_REGION"    )
  Sys.unsetenv("AWS_SECRET_ACCESS_KEY" )
  
  
  
}
clean_wind_data()
