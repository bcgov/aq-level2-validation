#Script: no2sasFcn.R
#Description: calculates no2 sas stats on ftp data
# # # 

# # subset for a single station and param for testing the function
# data<-feather::read_feather("unverifiedData.feather") %>%
#   dplyr::filter(STATION_NAME=="Castlegar Zinio Park" &
#            PARAMETER=="NO2") %>%
#   distinct()
# 
# no2SASFcn(data)

no2SASFcn<-function(data,no2column,dateColumn){
  
  library(openair) 
  library(Hmisc)
  library(tidyverse)
  library(rlang)
  
  #for testing
  # no2column<-"RAW_VALUE"
  # dateColumn<-"DATE_PST"
  # data<-data %>%
  #   dplyr::filter(PARAMETER %in% toupper("no2") &
  #                   STATION_NAME=="CRANBROOK MURIEL BAXTER" &
  #                   INSTRUMENT=="NOX_API200E")
  # END TESTING
  
  #default arguments
  if(missing(no2column)){no2column<-"RAW_VALUE"}
  if(missing(dateColumn)){dateColumn<-"DATE_PST"}
  if(missing(data)){data<-no2}
  
  no2sub <- data %>%
    dplyr::select(date=!!dateColumn,
                  NO2=!!no2column)
  
  #calculate daily averages time series with data completeness of 75%
  dt<-timeAverage(no2sub,avg.time="day",data.thresh=75)
  
  #Count the number of days with valid data:
  nd<-nrow(dt[!is.na(dt[,2]),])
  
  #Count the number of hours with valid data:
  nh<-no2sub %>% filter(!is.na(NO2)) %>% nrow(.)
  
  
  #AQO: 98th percentile of the maximum hourly concentration during the day:
  #calculate daily 1-hr max:
  d1hm<-timeAverage(mydata=no2sub,
                     pollutant="NO2",
                     avg.time="day",         #averaging period
                     statistic="max")
  
  #count d1hm exceedances of 100ppb:
  d1hmAbove100<-nrow(subset(d1hm,d1hm[,2]>=100))

  #calculate the annual 98th percentile
  d1hm_p98<-timeAverage(d1hm,
                   pollutant="NO2",
                   avg.time="year",
                   statistic="percentile",
                   percentile=98) %>%
    select(NO2)
  
  #Calculate hourly percentiles over the year: 
  hp <- calcPercentile(
    no2sub,
    pollutant = "NO2",
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
  
  #count hourly exceedances of 100 ppb:
  hourlyAbove100<-nrow(subset(no2sub,no2sub[,2]>=100))

  #calculate d1hm percentiles over the year:
  d1hmp <- calcPercentile(
    d1hm,
    pollutant = "NO2",
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
  
  #calculate number of monitoring days each month
  dm<-timeAverage(dt,avg.time="month",statistic="frequency")
  
  #calculate number of monitoring days each quarter
  dq<-timeAverage(dt,avg.time="3 month",statistic="frequency")
  
  #calculate total no. days each quarter
  alld<-timeAverage(no2sub,avg.time="day")
  allq<-c(sum(monthDays(dm$date)[1:3]),sum(monthDays(dm$date)[4:6]),
          sum(monthDays(dm$date)[7:9]),sum(monthDays(dm$date)[10:12]))
  
  #calculate quarterly data capture (%)
  q<-as_tibble(round((dq[,2]/allq)*100,0))
  
  
  #Change dm from 12 observations of 2 variables to 1 observation of 12
  # variables for making summary (sas) below
  dm %<>% 
    dplyr::mutate(date=format(date,"%m")) %>%
    tidyr::spread(key=date,value=NO2)
  
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
    tidyr::spread(QUARTER,NO2) 
  
  #Create and print summary table
  #Create and print summary table
  (
    sas <- tibble::tibble(
      
      `STATION NAME` = data %>%
        dplyr::pull(STATION_NAME) %>%
        unique,
      
      YEAR = as.numeric(format(hp$date, "%Y")),
      
      `VALID DAYS`= nd,
      
      `VALID HOURS`=nh,
      
      `ANNUAL 1-HR AVG`=round(mean(no2sub$NO2,na.rm=T),2),
      
      # `ANNUAL DAILY AVG`=round(mean(dt$NO2, na.rm = T), 2)
    ) %>%
      
      dplyr::bind_cols(
        
        # Hourly Percentiles
        round(hp %>% dplyr::select(-date),
                             2),
        
        # Hourly Exceedances of 100 ppb
        tibble::tibble(`HOURLY EXCEEDANCES > 100 ppb`=
                         hourlyAbove100),
        
        # D1hm Percentiles
        round(d1hmp %>% dplyr::select(-date),
              2),
        
        # Exceedances of D1HM >100 PPB
        tibble::tibble(`EXCEEDANCES OF D1HM > 100 ppb`=d1hmAbove100),
        
        # Annual 98P of D1HM
        d1hm_p98 %>% 
          dplyr::mutate(NO2=round(NO2,2)) %>%
          dplyr::rename(`ANNUAL 98P D1HM`=NO2),
        
        # Annual 98P of D1HM 3-yr ave
        tibble::tibble(`98P_DAILY,3-YR AVG`=NA_real_),
        
        # days of monitoring/month
        dm,
        
        # percent of monitoring/quarter
        q
        ) 

  ) # end sas
  
}