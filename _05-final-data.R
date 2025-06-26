# _05-final-data.R

source("_00-setup.R")

#### read in and bind data ####
data<-dplyr::bind_rows(readr::read_rds("unverified-data-checked.rds") |>
                         dplyr::select(DATE_PST,
                                       STATION_NAME,
                                       STATION_NAME_FULL,
                                       PARAMETER,
                                       INSTRUMENT,
                                       RAW_VALUE),
                       readr::read_rds("prev-yrs-wind-checked.rds")
) %>% 
  dplyr::distinct(.)

#### all stations ####
# save list of stations in year_to_validate with data:
all_stations<-data |>
  dplyr::filter(lubridate::year(DATE_PST) == year_to_validate) |>
  dplyr::group_by(STATION_NAME) |>
  dplyr::filter(!is.na(RAW_VALUE)) |>
  dplyr::distinct(STATION_NAME) |>
  dplyr::arrange(STATION_NAME)

# save all_stations as rds
readr::write_rds(all_stations,
                 "all-stations.rds")

# save all_stations as csv
readr::write_csv(all_stations,
                 "all-stations.csv")

#### write station data ####
# create prepped-data folder if it doesn't already exist
ifelse(dir.exists(file.path("./prepped-data")),
       "prepped-data folder already exists",
       dir.create(file.path("./prepped-data"
       )))

# Save each station's data to prepped-data folder 
purrr::walk(readr::read_rds("all-stations.rds")$STATION_NAME,
            
            function(x) {
              # test
              # x<-"Abbotsford A Columbia Street"
              # end test
              
              stn_data <- data %>%
                dplyr::filter(STATION_NAME == x)
              
              readr::write_rds(stn_data,
                               file.path("./prepped-data",
                                         stringr::str_c(x, ".rds", collapse = "")))
              
            })

