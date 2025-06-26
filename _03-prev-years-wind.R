# _03-prev-years-wind.R

source("_00-setup.R")

#### import-data ####
# import previous 5 years wind data for comparison
prev_yrs_wind <- envair::importBC_data(
  parameter_or_station = c("wdir_vect",
                           "wspd_sclr"),
  years = (year_to_validate - 6):(year_to_validate - 1),
  use_openairformat = FALSE
) |>
  dplyr::filter(lubridate::year(DATE_PST)!=year_to_validate)

#save prevYrWind
readr::write_rds(prev_yrs_wind,
                 "prev-yrs-wind.rds")
