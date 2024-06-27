#### SETUP ####
library(tidyverse)
library(magrittr)

#### rcaaqs issue to file - throws an error when all NA's ####


data<-rcaaqs::pm25_sample_data %>% 
  dplyr::filter(lubridate::year(date_time)==2012)

data %>%
  dplyr::summarise(p50 = rcaaqs:::quantile2(value,
    probs = 0.5,
    na.rm = TRUE,
    type = "caaqs"))

# # # Kelowna non-unique station_Name # # # 

data %>%
  dplyr::filter(STATION_NAME=="Kelowna") %>%
  dplyr::distinct(year=lubridate::year(DATE_PST),
                  STATION_NAME,
                  STATION_NAME_FULL)

# # # haley park # # # 
# so2: 365 valid days, 8390 valid hours

data<-readr::read_rds("./preppedData/Warfield Haley Park.rds")

so2 <- data %>% 
  dplyr::filter(PARAMETER=="SO2")

so2 %>%
  dplyr::filter(lubridate::hour(DATE_PST)==5 & !is.na(RAW_VALUE))

so2 %>% dplyr::filter(is.na(RAW_VALUE)) %>% dplyr::count(.)

8759-369 # = 8390 (1 hour each day for span, plus 4 more)

utils::View(so2)
