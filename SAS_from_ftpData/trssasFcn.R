# Script: trssasFcn.R
# Description: trs sas script for ftp data
# NOTE: EXACT SAME STATS AS TRS

# FOR TESTING
# subset for a single station and param for testing the function
# data<-feather::read_feather("unverifiedData.feather") %>%
#   dplyr::filter(STATION_NAME=="Pine River Gas Plant" &
#            PARAMETER=="TRS") %>% distinct()
# 
# trsSASFcn(data)

trsSASFcn<-function(data,trscolumn,dateColumn){
  
  library(openair) 
  library(Hmisc)
  library(tidyverse)
  library(rlang)
  
  #for testing
  # trscolumn<-"RAW_VALUE"
  # dateColumn<-"DATE_PST"
  # data<-data %>%
  #   dplyr::filter(PARAMETER %in% toupper("trs") &
  # STATION_NAME=="BESSBOROUGH 237 ROAD")
  # END TESTING
  
  #default arguments
  if(missing(trscolumn)){trscolumn<-"RAW_VALUE"}
  if(missing(dateColumn)){dateColumn<-"DATE_PST"}
  if(missing(data)){data<-trs}
  
  trssub <- data %>%
    dplyr::select(date=!!dateColumn,
                  TRS=!!trscolumn)
  
  # # # VALID DATA (DAYS AND HR.) # # # 
  
  #calculate daily averages time series, data completeness = 75%
  dt<-timeAverage(trssub,avg.time="day",data.thresh=75)
  
  #Count the number of days with valid data:
  nd<-nrow(dt[complete.cases(dt[,2]),])
  
  #Count the number of hours with valid data:
  nh<-trssub %>% filter(!is.na(TRS)) %>% nrow(.)
  
  ############### HR. PERC. & EXCEEDANCES ###############  
  
  #Calculate hourly percentiles over the year: 
  hp <- calcPercentile(
    trssub,
    pollutant = "TRS",
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
  
  #count hourly exceedances of 5 ppb.
  hoursAbove5<-nrow(subset(trssub,trssub[,2]>=5))
  
  ############### DAILY PERC. & EXCEEDANCES ############### 
  
  #calculate daily percentiles over the year:
  dp <- calcPercentile(
    dt,
    pollutant = "TRS",
    avg.time = "year",
    percentile = c(0, 10, 25, 50, 75, 90, 95, 98, 99, 99.5, 99.9, 100)
  ) %>%
    
    dplyr::rename(
      `0%(day)` = percentile.0,
      `10%(day)` = percentile.10,
      `25%(day)` = percentile.25,
      `50%(day)` = percentile.50,
      `75%(day)` = percentile.75,
      `90%(day)` = percentile.90,
      `95%(day)` = percentile.95,
      `98%(day)` = percentile.98,
      `99%(day)` = percentile.99,
      `99.5%(day)` = percentile.99.5,
      `99.9%(day)` = percentile.99.9,
      `100%(day)` = percentile.100
    )
  
  #count daily exceedances of 2 ppb.
  daysAbove2<-nrow(subset(dt,dt[,2]>=2))
  
  ############### DATA CAPTURE (d/MO, QUARTERLY) ############### 
  
  # # # MONITORING DAYS/MO & /Q # # #
  
  #calculate number of monitoring days each month
  dm<-timeAverage(dt,
                  avg.time="month",
                  statistic="frequency")
  
  #calculate number of monitoring days each quarter
  dq<-timeAverage(dt,
                  avg.time="3 month",
                  statistic="frequency")
  
  #calculate total no. days each quarter
  alld<-timeAverage(trssub,avg.time="day")
  allq<-c(sum(monthDays(dm$date)[1:3]),sum(monthDays(dm$date)[4:6]),
          sum(monthDays(dm$date)[7:9]),sum(monthDays(dm$date)[10:12]))
  
  #calculate quarterly data capture (%)
  q<-as_tibble(round((dq[,2]/allq)*100,0))
  
  
  #Change dm from 12 observations of 2 variables to 1 observation of 12
  # variables for making summary (sas) below
  dm %<>% 
    dplyr::mutate(date=format(date,"%m")) %>%
    tidyr::spread(key=date,value=TRS)
  
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
    tidyr::spread(QUARTER,TRS) 
  
  
  # # # CREATE SUMMARY TABLE (SAME AS SAS)  # # #
  
  (
    sas <- tibble::tibble(
      
      `STATION NAME` = data %>%
        dplyr::pull(STATION_NAME) %>%
        unique,
      
      YEAR = as.numeric(format(hp$date, "%Y")),
      
      `VALID DAYS`= nd,
      
      `VALID HOURS`=nh,
      
      `ANNUAL 1-HR AVG`=round(mean(trssub$TRS,na.rm=T),2),
      
    ) %>%
      
      dplyr::bind_cols(
        
        # Hourly Percentiles
        round(hp %>% dplyr::select(-date),
              2),
        
        # Hourly Exceedances of 5 ppb
        tibble::tibble(`HOURLY EXCEEDANCES > 5 ppb`=
                         hoursAbove5),
        
        # Daily Percentiles
        round(dp %>% dplyr::select(-date),
              2),
        
        # Exceedances of daily > 2 ppb
        tibble::tibble(`EXCEEDANCES OF DAILY AVG > 2 ppb`=daysAbove2),
        
        # days of monitoring/month
        dm,
        
        # percent of monitoring/quarter
        q
      ) 
    
  ) # end sas
}