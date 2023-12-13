test<-readr::read_rds("./preppedData/Willow Creek Mine.rds")

data<-test

willowCreekMine<-envair::importBC_data(parameter_or_station = "Willow Creek Mine",
                                       use_openairformat = FALSE,
                                       years = 2022)

willowCreekMine %>%
  dplyr::group_by(INSTRUMENT) %>%
  dplyr::summarise(min=min(DATE_PST),
                   max=max(DATE_PST))

# rcaaqs issue to file

library(tidyverse)

data<-rcaaqs::pm25_sample_data %>% 
  dplyr::filter(lubridate::year(date_time)==2012)

data %>%
  dplyr::summarise(p50 = rcaaqs:::quantile2(value,
    probs = 0.5,
    na.rm = TRUE,
    type = "caaqs"))

