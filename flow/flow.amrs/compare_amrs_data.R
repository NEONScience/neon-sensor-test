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
      dplyr::filter(stringr::str_detect(string = Key, pattern = ".fst"))
    
    # Function to read in data through the lappy() call
    read_in_amrs_data = function(x){
      output = aws.s3::s3read_using(FUN = fst::read_fst, object = x, bucket = sensor_test_bucket) # Read in data x
      return(output)
    }
    
    # Read in data into List
    dataList <- lapply(date_file_lookup$Key, read_in_amrs_data)
    
    



    
    
    
    
  }
  
  
}