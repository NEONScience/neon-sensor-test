
download_amrs_comparison_data = function(sensorID = "AMRS_01"){
  
  # Library
  library(aws.s3)
  library(aws.signature)
  library(dplyr)
  library(stringr)
  library(data.table)
  
  # Test Bucket 
  amrs_bucket = "dev-sae-artifacts"
  
  # S3 Connection
  Sys.setenv("AWS_ACCESS_KEY_ID"     = amrs_bucket,
             "AWS_S3_ENDPOINT"       = "neonscience.org",
             "AWS_DEFAULT_REGION"    = "test-s3.data")
  
  
  
  
  
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
  
  
  
  
    
    
   
}
