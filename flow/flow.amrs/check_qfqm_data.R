# Analyze QFQM data for determining if test data is flagged within test parameters 
check_qfqm_results = function(the_run = 1){
  
  message(paste0(Sys.time(), ": calling libraries"))
  # Library
  library(aws.s3)
  library(aws.signature)
  library(dplyr)
  library(stringr)
  library(data.table)
  library(ggplot2)
  
  message(paste0(Sys.time(), ": connecting to S3"))
  amrs_bucket = "dev-sae-artifacts"
  
  # S3 Connection
  Sys.setenv("AWS_ACCESS_KEY_ID"     = amrs_bucket,
             "AWS_S3_ENDPOINT"       = "neonscience.org",
             "AWS_DEFAULT_REGION"    = "test-s3.data")
  
  qfqm_report_keys = aws.s3::get_bucket_df(bucket = amrs_bucket, prefix = "AMRS_tests", max = Inf) %>% 
    dplyr::select(Key) %>% 
    dplyr::filter(stringr::str_detect(string = Key, pattern = "qfqm") == TRUE) %>% # Filter to just QFQM
    dplyr::filter(stringr::str_detect(string = Key, pattern = ".rds") == TRUE) %>% # Filter to just .rds
    dplyr::mutate(sensor = ifelse(test = stringr::str_detect(string = Key, pattern = "int040") == TRUE, yes = "soniAmrs01", no = NA)) %>% 
    dplyr::mutate(sensor = ifelse(test = stringr::str_detect(string = Key, pattern = "int041") == TRUE, yes = "soniAmrs02", no = sensor)) %>% 
    dplyr::mutate(sensor = ifelse(test = stringr::str_detect(string = Key, pattern = "dev042") == TRUE, yes = "soniAmrs03", no = sensor)) %>% 
    dplyr::mutate(run = ifelse(test = stringr::str_detect(string = Key, pattern = "run_2") == TRUE, yes = 2, no = 1)) %>% 
    dplyr::mutate(run = ifelse(test = stringr::str_detect(string = Key, pattern = "run_3") == TRUE, yes = 3, no = run))
  
  qfqm_out = data.table::data.table()
  for(i in seq_along(qfqm_report_keys$Key)){
    
    qfqm_in = aws.s3::s3readRDS(object = qfqm_report_keys$Key[i], bucket = amrs_bucket) %>% 
      dplyr::mutate(date = as.Date(date, origin = "1970-01-01")) 
    
    qfqm_amrs = qfqm_in %>% 
      dplyr::filter(dp == "amrs")
    
    if(nrow(qfqm_amrs) > 0 ){
      qfqm_amrs_results = qfqm_amrs %>% 
        dplyr::mutate(daily_flags_percentage = 100 * round(qfFinlTotl/48,2)) %>% 
        dplyr::mutate(sensor = qfqm_report_keys$sensor[i]) %>% 
        dplyr::mutate(run = qfqm_report_keys$run[i]) %>% 
        dplyr::select(site, date, sensor, run, qfFinlTotl, daily_flags_percentage, -metric) %>% 
        dplyr::distinct()
    } else {
      qfqm_amrs_results = data.table::data.table(
        "site"       = qfqm_in$site[1],
        "date"       = qfqm_in$date[1],
        "sensor"     = qfqm_report_keys$sensor[i],
        "run"        = qfqm_report_keys$run[i],
        "qfFinlTotl" = 0,
        daily_flags_percentage = 0
      )
    }
    
    qfqm_soni = qfqm_in %>% 
      dplyr::filter(dp == "soni")
    
    if(nrow(qfqm_soni) > 0 ){
      qfqm_soni_results = qfqm_soni %>% 
        dplyr::mutate(daily_flags_percentage = 100 * round(qfFinlTotl/48,2)) %>% 
        dplyr::mutate(sensor = "soni") %>% 
        dplyr::mutate(run = qfqm_report_keys$run[i]) %>% 
        dplyr::select(site, date, sensor, run, qfFinlTotl, daily_flags_percentage, -metric) %>% 
        dplyr::distinct()
    } else {
      qfqm_soni_results = data.table::data.table(
        "site"       = qfqm_in$site[1],
        "date"       = qfqm_in$date[1],
        "sensor"     = "soni",
        "run"        = qfqm_report_keys$run[i],
        "qfFinlTotl" = 0,
        daily_flags_percentage = 0
      )
    }
    
    qfqm_i_out = data.table::rbindlist(l = list(qfqm_amrs_results, qfqm_soni_results))
    rm(qfqm_amrs_results, qfqm_soni_results)
    
    qfqm_out = data.table::rbindlist(l = list(qfqm_i_out, qfqm_out)) %>% 
      dplyr::distinct() %>%
      dplyr::arrange(date, sensor)
    rm(qfqm_i_out)
    
  }
  
  qfqm_plot = qfqm_out %>% 
    dplyr::filter(run == the_run)
  
  base::print(
    ggplot2::ggplot(qfqm_plot, ggplot2::aes(x = date, y = daily_flags_percentage, fill = sensor)) +
      ggplot2::geom_bar(stat="identity") +
      ggplot2::geom_hline(yintercept = 10) + 
      ggplot2::scale_y_continuous(limits = c(0,100), sec.axis = ggplot2::dup_axis(name = "", breaks = 10) ) +
      ggplot2::scale_x_date(date_breaks = "1 day") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 270), text = ggplot2::element_text(size = 15), legend.position = "top") +
      ggplot2::labs(y = "Percent of Total Quality Flags", x = "", title = base::paste0("AMRS Round 3 Tests: QFQM Report Results for Run ", the_run), fill = "Sensor",caption = "Note, if there were 0 flags for the day, no bar will be present") +
      ggplot2::facet_wrap(~sensor, ncol = 1)
  )
}
check_qfqm_results(the_run = 3)
