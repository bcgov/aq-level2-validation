
library(tidyverse)

######
# # # 1. SEARCH FOR DUPLICATES # # # 

# DH
data<-dplyr::bind_rows(
  feather::read_feather("C:\\HagaR\\AnnualDataValidation/unverifieddata_unpadded.feather")
)


# for others
data <- tibble::as_tibble(
  
  data.table::fread(
    "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/2020/WSPD_SCLR.csv",
    header = TRUE,
    verbose = TRUE,
    showProgress = TRUE,
    fill = TRUE,
    colClasses = "character"
  ) %>%
  
  dplyr::bind_rows(
    
    data.table::fread(
      "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Year_to_Date/WSPD_SCLR.csv",
      header = TRUE,
      verbose = TRUE,
      showProgress = TRUE,
      fill = TRUE,
      colClasses = "character"
    )  
  )
    
  )



# find duplicates
duplicates<-data %>%
  dplyr::group_by(DATE_PST, STATION_NAME, PARAMETER,INSTRUMENT) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) %>%
  dplyr::arrange(STATION_NAME, 
                 DATE_PST,PARAMETER) %>% 
  dplyr::distinct(DATE_PST,
                  STATION_NAME,
                  PARAMETER,INSTRUMENT) %>%
  dplyr::left_join(.,
                   data,
                   by=c("DATE_PST",
                        "STATION_NAME",
                        "PARAMETER",
                        "INSTRUMENT"))



readr::write_csv(duplicates %>%
                   dplyr::mutate(DATE_PST=format(DATE_PST, "%Y-%m-%d %H:%M")),
                 "duplicates.csv")
 
write.csv(duplicates,
          "duplicates.csv")

# look at first 10 rows for each station and parameter
utils::View(duplicates %>%
              dplyr::group_by(STATION_NAME,PARAMETER) %>%
              dplyr::slice(1:10))

# summarize when duplicates start and end for each station
duplicates %>% 
  dplyr::group_by(STATION_NAME,PARAMETER,INSTRUMENT) %>%
  dplyr::summarise(Start=min(DATE_PST),
                   End=max(DATE_PST))


# duplicates identified at:
# colwood city hall (wspd_sclr, wdir_vect)
# prince george exploration place (humidity, wdir_vect, wspd_sclr)
# vanderhoof courthouse (wdir_vect, wspd_sclr)

# remaining stations that need to be troubleshooted:

# crofton elementary: report compiles (but didnt before?!) - no northerly winds
# crofton substatoin: no wind speed data

##########
# DUNCAN COLLEGE STREET - similar message to the one for hte stations with 
            # duplicates in teh wind data but it wasn't identified above, 
            # need to dig into the data

data %<>%
  dplyr::filter(STATION_NAME == "Duncan College Street" & 
                  PARAMETER %in% c("WSPD_SCLR","WDIR_VECT"))

# find duplicates
duplicates<-data %>%
  dplyr::group_by(DATE_PST, STATION_NAME, PARAMETER) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) %>%
  dplyr::arrange(STATION_NAME, 
                 DATE_PST,PARAMETER) %>% 
  dplyr::distinct(DATE_PST,
                  STATION_NAME,
                  PARAMETER) %>%
  dplyr::left_join(.,
                   data,
                   by=c("DATE_PST",
                        "STATION_NAME",
                        "PARAMETER"))

# THERE ARE DUPLICATES WHEN INSTRUMENT IS REMOVED from grouping - IS THIS AN ISSUE WITH MY SCRIPT OR ON THE FTP?
# for others
data <- tibble::as_tibble(
  
  data.table::fread(
    "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/2020/WSPD_SCLR.csv",
    header = TRUE,
    verbose = TRUE,
    showProgress = TRUE,
    fill = TRUE,
    colClasses = "character"
  ) %>%
    
    dplyr::bind_rows(
      
      data.table::fread(
        "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Year_to_Date/WSPD_SCLR.csv",
        header = TRUE,
        verbose = TRUE,
        showProgress = TRUE,
        fill = TRUE,
        colClasses = "character"
      )  
    ) %>%
    
    dplyr::filter(STATION_NAME == "Duncan College Street")
  
)

# find duplicates
duplicates<-data %>%
  dplyr::group_by(DATE_PST, STATION_NAME, PARAMETER) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) %>%
  dplyr::arrange(STATION_NAME, 
                 DATE_PST,PARAMETER) %>% 
  dplyr::distinct(DATE_PST,
                  STATION_NAME,
                  PARAMETER) %>%
  dplyr::left_join(.,
                   data,
                   by=c("DATE_PST",
                        "STATION_NAME",
                        "PARAMETER"))

duplicates %>%
          dplyr::filter(INSTRUMENT=="UNSPECIFIED" & RAW_VALUE!="")

write.csv(duplicates,
          "duplicates.csv")

# sent a note to env aqhi station - vancouver island and removed 
# instrument =="unspecified" from data set (06-windRoses.Rmd)

###########
# PRINCE GEORGE PULP - issue with humidity
data <- tibble::as_tibble(
  
  data.table::fread(
    "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/2020/HUMIDITY.csv",
    header = TRUE,
    verbose = TRUE,
    showProgress = TRUE,
    fill = TRUE,
    colClasses = "character"
  ) %>%
    
    dplyr::bind_rows(
      
      data.table::fread(
        "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Year_to_Date/HUMIDITY.csv",
        header = TRUE,
        verbose = TRUE,
        showProgress = TRUE,
        fill = TRUE,
        colClasses = "character"
      )  
    )
  
)

data %<>% dplyr::filter(STATION_NAME=="Prince George Pulp") 


data %>%
  dplyr::filter(PARAMETER=="HUMIDITY" & RAW_VALUE!="") %>%
  utils::View(.)



# prince rupert roosevelt park school (wdir_vect)

# valemount - wind


# warfield elementary

