base::source("~/neon-sensor-test/flow/flow.amrs/flow.amrs.test.R")

amrs_01.s3filepath = "https://neon-sae-files.s3.data.neonscience.org/ods/dataproducts/IP0/2021-02-09/WREF/NEON.D16.WREF.IP0.00200.001.ecte.2021-02-09.l0p.h5.gz"
amrs_02.s3filepath = "https://neon-sae-files.s3.data.neonscience.org/ods/dataproducts/IP0/2021-02-09/JORN/NEON.D14.JORN.IP0.00200.001.ecte.2021-02-09.l0p.h5.gz"
amrs_03.s3filepath = "https://neon-sae-files.s3.data.neonscience.org/ods/dataproducts/IP0/2021-02-09/WOOD/NEON.D09.WOOD.IP0.00200.001.ecte.2021-02-09.l0p.h5.gz"

download.amrs.test.data(s3filepath = amrs_01.s3filepath)
download.amrs.test.data(s3filepath = amrs_02.s3filepath)
download.amrs.test.data(s3filepath = amrs_03.s3filepath)

# Set up read creds
Sys.setenv("AWS_S3_ENDPOINT"       = "neonscience.org",
           "AWS_DEFAULT_REGION"    = "s3.data")

require(tidyr)
require(dplyr)
require(stringr)
require(rhdf5)
require(aws.s3)
amrs01.in = aws.s3::s3read_using(FUN = fst::read.fst, object = "sensor/amrs/test/2021-02-09/amrs01.fst", bucket = "neon-sensor-test")
amrs02.in = aws.s3::s3read_using(FUN = fst::read.fst, object = "sensor/amrs/test/2021-02-09/amrs02.fst", bucket = "neon-sensor-test")
amrs03.in = aws.s3::s3read_using(FUN = fst::read.fst, object = "sensor/amrs/test/2021-02-09/amrs03.fst", bucket = "neon-sensor-test")

sum(amrs01.in$time == amrs02.in$time, na.rm = TRUE) == nrow(amrs01.in)
sum(amrs01.in$accXaxs == amrs02.in$accXaxs, na.rm = TRUE)
sum(amrs01.in$accXaxs == amrs03.in$accXaxs, na.rm = TRUE)
eddy4R.base::def.med.mad(test = amrs01.in$accXaxs, refe = amrs02.in$accXaxs,Perc = FALSE)
eddy4R.base::def.med.mad(test = amrs01.in$accXaxs, refe = amrs03.in$accXaxs,Perc = FALSE)


library(ggplot2)
library(lubridate)

amrs01.reduce = amrs01.in %>% 
  dplyr::mutate(time.group = cut(lubridate::ymd_hms(time), breaks = "1 min")) %>%  
  dplyr::group_by(time.group) %>% 
  dplyr::summarise(
    mean = mean(accXaxs, na.rm = TRUE)
  ) %>% 
  dplyr::mutate(time.group = lubridate::ymd_hms(time.group))

amrs02.reduce = amrs02.in %>% 
  dplyr::mutate(time.group = cut(lubridate::ymd_hms(time), breaks = "1 min")) %>%  
  dplyr::group_by(time.group) %>% 
  dplyr::summarise(
    mean = mean(accXaxs, na.rm = TRUE)
  ) %>% 
  dplyr::mutate(time.group = lubridate::ymd_hms(time.group))

amrs03.reduce = amrs03.in %>% 
  dplyr::mutate(time.group = cut(lubridate::ymd_hms(time), breaks = "1 min")) %>%  
  dplyr::group_by(time.group) %>% 
  dplyr::summarise(
    mean = mean(accXaxs, na.rm = TRUE)
  ) %>% 
  dplyr::mutate(time.group = lubridate::ymd_hms(time.group))

ggplot() +
  geom_point(aes(x = amrs01.reduce$time.group, y = amrs01.reduce$mean, color = "AMRS01")) +
  geom_point(aes(x = amrs01.reduce$time.group, y = amrs02.reduce$mean, color = "AMRS02")) +
  geom_point(aes(x = amrs01.reduce$time.group, y = amrs03.reduce$mean, color = "AMRS03"))