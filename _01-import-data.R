# _01-import-data.R

source("_00-setup.R")

#### declare parameters ####
parameters <- tolower(
  c(
    "CO",
    "H2S",
    "HF",
    "HUMIDITY",
    "NO",
    "NO2",
    "NOX",
    "O3",
    "PM10",
    "PM25",
    "SO2",
    "TEMP_MEAN",
    "TRS",
    "WDIR_VECT",
    "WSPD_SCLR"
  )
)

#### import unverified data ####
system.time({
  data<-envair::importBC_data(
    parameter_or_station = parameters,
    years=yearToValidate,
    use_openairformat = FALSE
  ) %>%
    dplyr::distinct(.) %>%
    dplyr::filter(lubridate::year(DATE_PST) == year_to_validate)
})

#### save unverified data ####
readr::write_rds(data,"unverified-data.rds")
