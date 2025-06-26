# _02-data-checks.R

source("_00-setup.R")

#### read in data ####
data<-readr::read_rds("unverified-data.rds")

#### data check - valid status ####
# all data should have validation status 0.
data |>
  dplyr::distinct(year=lubridate::year(DATE_PST),
                  STATION_NAME_FULL,
                  PARAMETER,
                  VALIDATION_STATUS) |> 
  dplyr::filter(VALIDATION_STATUS!="Level 0")

#### data check - NA stations ####
#check if there are any stations with NA for STATION_NAME
data |> 
  dplyr::filter(is.na(STATION_NAME))

#### data check - duplicates ####
# look for duplicates
duplicates<-data |>
  
  dplyr::group_by(DATE_PST, 
                  STATION_NAME_FULL,
                  STATION_NAME,
                  PARAMETER, 
                  VALIDATION_STATUS,
                  INSTRUMENT) |>
  
  dplyr::summarise(n = dplyr::n(), 
                   .groups = "drop") |>
  
  dplyr::filter(n > 1L) |>
  
  dplyr::left_join(x=_,
                   y=data,
                   by=c("DATE_PST",
                        "STATION_NAME_FULL",
                        "STATION_NAME",
                        "PARAMETER",
                        "VALIDATION_STATUS",
                        "INSTRUMENT"))|>
  
  dplyr::arrange(DATE_PST,
                 STATION_NAME,
                 STATION_NAME_FULL,
                 PARAMETER) |>
  
  dplyr::select(DATE_PST,
                STATION_NAME,
                STATION_NAME_FULL,
                PARAMETER,
                INSTRUMENT,
                RAW_VALUE,
                everything())

#### data check - campbell loggers ####
# issue: when datalogger is switched to campbell ""_60|_MET_60| MET_60" is appended to STATION_NAME_FULL. we want same id's for same station
#fix: strip "_60|_MET_60| MET_60" from station names
data %<>%
  dplyr::mutate(STATION_NAME_FULL=stringr::str_remove(STATION_NAME_FULL,
                                                    "_60|_MET_60| MET_60"))


# save checked data
readr::write_rds(data,
                 "unverified-data-checked.rds")
