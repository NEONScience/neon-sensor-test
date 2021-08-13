

quick_check = function(){
  # Libraries
  library(data.table)
  library(dplyr)
  library(tidyr)
  # library()
  # library()
  
  # List available dates during second test period
  test_dates = data.table::data.table(test_files = base::list.files("/home/kstyers/eddy/neon-sensor-test/data/2021_AMRS_Round_3/final_outputs/")) 
  
  # Join table
  avg60   = data.table::data.table()
  avg90   = data.table::data.table()
  avg1800 = data.table::data.table()
  for(j in test_dates$test_files){
    message(j)
    # Read in the data
    data_test = base::readRDS(paste0("/home/kstyers/eddy/neon-sensor-test/data/2021_AMRS_Round_3/final_outputs/", j))
    # List out AMRS to analyze
    amrs_tests = names(data_test$data$avg60)
    
    # Pull out the data for each time frame
    
    # 60
    avg60_i = data.table::data.table()
    for(i in seq_along(amrs_tests)){

      avg60_in = data.table::data.table(timeBgn = data_test$time$avg60[[i]][[1]], data_test$data$avg60[[i]]$mean) %>% 
        dplyr::mutate(sensor = amrs_tests[i]) %>% 
        dplyr::mutate(run = ifelse(timeBgn < "2021-07-24", yes = 1, no = 2)) %>% 
        dplyr::select(timeBgn, sensor, run, tidyr::everything()) 
      avg60_i = data.table::rbindlist(l = list(avg60_i, avg60_in)) %>% 
        dplyr::arrange(timeBgn)
      rm(avg60_in)
    }
    
    # 90
    avg90_i = data.table::data.table()
    for(i in seq_along(amrs_tests)){
      
      avg90_in = data.table::data.table(timeBgn = data_test$time$avg90[[i]][[1]], data_test$data$avg90[[i]]$mean) %>% 
        dplyr::mutate(sensor = amrs_tests[i]) %>% 
        dplyr::mutate(run = ifelse(timeBgn < "2021-07-24", yes = 1, no = 2)) %>% 
        dplyr::select(timeBgn, sensor, run, tidyr::everything()) 
      avg90_i = data.table::rbindlist(l = list(avg90_i, avg90_in)) %>% 
        dplyr::arrange(timeBgn)
      rm(avg90_in)
    }
    
    # 1800
    avg1800_i = data.table::data.table()
    for(i in seq_along(amrs_tests)){
      
      avg1800_in = data.table::data.table(timeBgn = data_test$time$avg1800[[i]][[1]], data_test$data$avg1800[[i]]$mean) %>% 
        dplyr::mutate(sensor = amrs_tests[i]) %>% 
        dplyr::mutate(run = ifelse(timeBgn < "2021-07-24", yes = 1, no = 2)) %>% 
        dplyr::select(timeBgn, sensor, run, tidyr::everything()) 
      avg1800_i = data.table::rbindlist(l = list(avg1800_i, avg1800_in)) %>% 
        dplyr::arrange(timeBgn)
      rm(avg1800_in)
    }
    
    avg60 = data.table::rbindlist(l = list(avg60 , avg60_i))
    avg90 = data.table::rbindlist(l = list(avg90 , avg90_i))
    avg1800 = data.table::rbindlist(l = list(avg1800 , avg1800_i))
    
  }
  
  data_out = list("avg60" = avg60, "avg90" = avg90, "avg1800" = avg1800)

  return(data_out)
  
}
data = quick_check()



library(ggplot2)
ggplot(data[[1]] %>%  dplyr::filter(timeBgn > "2021-07-28"), aes(x = timeBgn)) +
  geom_point(aes(y = accXaxs, color = "blue")) +
  geom_point(aes(y = accYaxs, color = "red")) +
  geom_point(aes(y = accZaxs, color = "green")) +
  scale_y_continuous(limits = c(-0.25, 0.25)) +
  facet_grid(run~sensor, scales = "free_x")

