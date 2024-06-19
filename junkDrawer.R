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



