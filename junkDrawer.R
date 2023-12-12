test<-readr::read_rds("./preppedData/Warfield Haley Park.rds")

data<-test

# rcaaqs issue to file

library(tidyverse)

data<-rcaaqs::pm25_sample_data %>% 
  dplyr::filter(lubridate::year(date_time)==2012)

data %>%
  dplyr::summarise(p50 = rcaaqs:::quantile2(value,
    probs = 0.5,
    na.rm = TRUE,
    type = "caaqs"))

