# Script: so2sasFcn.R
# Description: so2 sas script for ftp data

# FOR TESTING
# # subset for a single station and param for testing the function
# data<-feather::read_feather("unverifiedData.feather") %>%
#   dplyr::filter(STATION_NAME=="Trail Butler Park" &
#            PARAMETER=="SO2") %>% distinct()
# 
# so2SASFcn(data)

so2SASFcn<-function(data,so2column,dateColumn){ 
  
  
  library(openair) 
  library(Hmisc)
  library(tidyverse)
  library(rlang)
  
  #for testing
  # so2column<-"RAW_VALUE"
  # dateColumn<-"DATE_PST"
  # data<-data %>%
  #   dplyr::filter(PARAMETER %in% toupper("so2") &
  #                   STATION_NAME=="BIRCHBANK GOLF COURSE")
  # END TESTING
  
  #default arguments
  if(missing(so2column)){so2column<-"RAW_VALUE"}
  if(missing(dateColumn)){dateColumn<-"DATE_PST"}
  if(missing(data)){data<-so2}
  
  so2sub <- data %>%
    dplyr::select_(date=dateColumn,
                   SO2=so2column)
  
  
  #calculate daily averages time series with data completeness of 75%
  dt<-timeAverage(so2sub,avg.time="day",data.thresh=75)
  
  #Count the number of days with valid data:
  nd<-nrow(dt[!is.na(dt[,2]),])
  
  #Count the number of hours with valid data:
  nh<-so2sub %>% filter(!is.na(SO2)) %>% nrow(.)
  
  #AQO: 99th percentile of the d1hm:
  #calculate daily 1-hr max:
  d1hm<-timeAverage(mydata=so2sub,
                     pollutant="SO2",
                     avg.time="day",         #averaging period
                     statistic="max")
  
  #calculate the annual 99th percentile of d1hrm
  d1hm_p99<-timeAverage(d1hm,
                   pollutant="SO2",
                   avg.time="year",
                   statistic="percentile",
                   percentile=99) %>%
    select(SO2)
  
  #Calculate hourly percentiles over the year: 
  hp <- calcPercentile(
    so2sub,
    pollutant = "SO2",
    avg.time = "year",
    percentile = c(0, 10, 25, 50, 75, 90, 95, 98, 99, 99.5, 99.9, 100)
  ) %>%
    
    dplyr::rename(
      `0%(hr)` = percentile.0,
      `10%(hr)` = percentile.10,
      `25%(hr)` = percentile.25,
      `50%(hr)` = percentile.50,
      `75%(hr)` = percentile.75,
      `90%(hr)` = percentile.90,
      `95%(hr)` = percentile.95,
      `98%(hr)` = percentile.98,
      `99%(hr)` = percentile.99,
      `99.5%(hr)` = percentile.99.5,
      `99.9%(hr)` = percentile.99.9,
      `100%(hr)` = percentile.100
    )
  
  #count hourly exceedances of 70 and 75 ppb.
  hoursAbove70<-nrow(subset(so2sub,so2sub[,2]>=70))
  hoursAbove75<-nrow(subset(so2sub,so2sub[,2]>=75))

  #calculate d1hm percentiles over the year:
  d1hmp <- calcPercentile(
    d1hm,
    pollutant = "SO2",
    avg.time = "year",
    percentile = c(0, 10, 25, 50, 75, 90, 95, 98, 99, 99.5, 99.9, 100)
  ) %>%
    
    dplyr::rename(
      `0%(d1hm)` = percentile.0,
      `10%(d1hm)` = percentile.10,
      `25%(d1hm)` = percentile.25,
      `50%(d1hm)` = percentile.50,
      `75%(d1hm)` = percentile.75,
      `90%(d1hm)` = percentile.90,
      `95%(d1hm)` = percentile.95,
      `98%(d1hm)` = percentile.98,
      `99%(d1hm)` = percentile.99,
      `99.5%(d1hm)` = percentile.99.5,
      `99.9%(d1hm)` = percentile.99.9,
      `100%(d1hm)` = percentile.100
    )
  
  
  #count d1hm exceedances of 70 and 75 ppb.
  d1hmAbove70<-nrow(subset(d1hm,d1hm[,2]>=70))
  d1hmAbove75<-nrow(subset(d1hm,d1hm[,2]>=75))

  #calculate number of monitoring days each month
  dm<-timeAverage(dt,avg.time="month",statistic="frequency")
  
  #calculate number of monitoring days each quarter
  dq<-timeAverage(dt,avg.time="3 month",statistic="frequency")
  
  #calculate total no. days each quarter
  alld<-timeAverage(so2sub,avg.time="day")
  allq<-c(sum(monthDays(dm$date)[1:3]),sum(monthDays(dm$date)[4:6]),
          sum(monthDays(dm$date)[7:9]),sum(monthDays(dm$date)[10:12]))
  
  #calculate quarterly data capture (%)
  q<-as_tibble(round((dq[,2]/allq)*100,0))
  
  
  #Change dm from 12 observations of 2 variables to 1 observation of 12
  # variables for making summary (sas) below
  dm %<>% 
    dplyr::mutate(date=format(date,"%m")) %>%
    tidyr::spread(key=date,value=SO2)
  
  #
  names(dm) <- stringr::str_c(
    format(as.POSIXct(stringr::str_c("2000", names(dm), "01", sep = "-"),
                      "%Y-%m-%d", tz = "UTC"),
           "%b"),
    "(days)",sep = " ")
  
  #do something similar for q
  q %<>%
    dplyr::mutate(QUARTER=stringr::str_c("Q",
                                         1:4,
                                         " (%days)")) %>%
    tidyr::spread(QUARTER,SO2) 
  
  #Create and print summary table
  (
    sas <- tibble::tibble(
      
      `STATION NAME` = data %>%
        dplyr::pull(STATION_NAME) %>%
        unique,
      
      YEAR = as.numeric(format(hp$date, "%Y")),
      
      `VALID DAYS`= nd,
      
      `VALID HOURS`=nh,
      
      `ANNUAL 1-HR AVG`=round(mean(so2sub$SO2,na.rm=T),2),
      
    ) %>%
      
      dplyr::bind_cols(
        
        # Hourly Percentiles
        round(hp %>% dplyr::select(-date),
              2),
        
        # Hourly Exceedances of 70 ppb
        tibble::tibble(`HOURLY EXCEEDANCES > 70 ppb`=
                         hoursAbove70),
        
        # Hourly Exceedances of 75 ppb
        tibble::tibble(`HOURLY EXCEEDANCES > 75 ppb`=
                         hoursAbove75),
        
        # D1hm Percentiles
        round(d1hmp %>% dplyr::select(-date),
              2),
        
        # Exceedances of D1HM >70 PPB
        tibble::tibble(`EXCEEDANCES OF D1HM > 70 ppb`=d1hmAbove70),
        
        # Exceedances of D1HM >75 PPB
        tibble::tibble(`EXCEEDANCES OF D1HM > 75 ppb`=d1hmAbove75),
        
        # Annual 99P of D1HM
        d1hm_p99 %>% 
          dplyr::mutate(SO2=round(SO2,2)) %>%
          dplyr::rename(`ANNUAL 99P D1HM`=SO2),
        
        # Annual 99P of D1HM 3-yr ave
        tibble::tibble(`99P_DAILY,3-YR AVG`=NA_real_),
        
        # days of monitoring/month
        dm,
        
        # percent of monitoring/quarter
        q
      ) 
    
  ) # end sas
  
}
