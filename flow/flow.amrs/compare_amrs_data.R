compare_amrs_data = function(){
  # library()
  library(aws.s3)
  library(aws.signature)
  library(dplyr)
  library(data.table)
  library(stringr)
  
  
  
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
      dplyr::filter(stringr::str_detect(string = Key, pattern = ".fst"))
    # Read in data
    dataList = lapply(
      X = date_file_lookup$Key, 
      FUN = aws.s3::s3read_using(
        FUN = fst::read.fst, 
        object = X,
        bucket = sensor_test_bucket
      )
    )
    
    
    
    
  }
  
  
  
}