
# source from github:
#https://github.com/opetchey/RREEBES/wiki/Reading-data-and-code-from-an-online-github-repository

# source importFeatherFcn from github:
devtools::source_url("https://raw.githubusercontent.com/DonnaHaga/dataImport/master/functions/importFeatherFcn.R")

# Import all unverified data

# Path to feather data
airFTP<-"C:/airFTPdata"

# path to unverified data
(dataPath<-file.path(airFTP,
                    "Hourly_Raw_Air_Data/Year_to_Date"))

# FILTER FOR YEAR TO VALIDATE:
yearToValidate<-2019

# Import feather data for all parameters and combine into one tibble,
# filter for yearToValidate

# list all files in dataPath:
dir(dataPath)

parameters<-c("CO",
              "H2S",
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
              "WSPD_SCLR")

# generate a meta data set from the ftp that lists all station names and city's:
(allStations<-feather::read_feather(file.path(dataPath,
                                             "bc_air_monitoring_stations.feather")) %>%
  pull(STATION_NAME)) %>% unique

data<-purrr::map_dfr(parameters,
                     function(x){
                       
                       importFeatherFcn(feather=dataPath,
                                        parameter = stringr::str_replace(x,
                                                                         ".feather",
                                                                         ""),
                                        station=allStations,
                                        year=yearToValidate)$stationData
                     })  %>%# map_dfr
  filter(format(DATE_PST,"%Y") %in% yearToValidate)

# fill gaps with NA:
devtools::source_url("https://raw.githubusercontent.com/DonnaHaga/dataImport/master/functions/NAfillFcn.R")
data<-NAfillFcn(data)


# save imported and formatted data to 

feather::write_feather(data,
                       "unverifiedData.feather")

# save list of stations:
saveRDS(data %>%
          dplyr::pull(STATION_NAME) %>%
          unique %>%
          sort,
        "stations.rds")
