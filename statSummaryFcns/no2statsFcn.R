#Script: no2StatsFcn.R
#Description: calculates no2 stats on ftp data
# # # 

# # subset for a single station and param for testing the function
# data<-readr::read_rds("unverified_data.rds") %>%
#   dplyr::filter(STATION_NAME=="Cranbrook Muriel Baxter" &
#            PARAMETER=="NO2") %>%
#   distinct()
# 
# no2StatsFcn(data)

no2StatsFcn<-function(data,no2column,dateColumn){
  
  library(openair) 
  library(Hmisc)
  library(tidyverse)
  library(rlang)
  library(rcaaqs)
  
  #for testing
  # no2column<-"RAW_VALUE"
  # dateColumn<-"DATE_PST"
  # data<-data %>%
  #   dplyr::filter(PARAMETER %in% toupper("no2"))
  # END TESTING
  
  #default arguments
  if(missing(no2column)){no2column<-"RAW_VALUE"}
  if(missing(dateColumn)){dateColumn<-"DATE_PST"}
  if(missing(data)){data<-no2}
  
  sub <- data %>%
    dplyr::select(date=!!dateColumn,
                  value=!!no2column)
  
  #calculate daily averages time series with data completeness of 75%
  dt<-openair::timeAverage(sub,
                           avg.time="day",
                           data.thresh=75)
  
  #Count the number of days with valid data:
  nd<-dt %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::pull(n)
  
  #Count the number of hours with valid data:
  nh<-sub %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::pull(n)
  
  
  #AQO: 98th percentile of the maximum hourly concentration during the day:
  
  #calculate daily 1-hr max:
  d1hm <- openair::timeAverage(
    mydata = sub,
    pollutant = "value",
    avg.time = "day",
    #averaging period
    statistic = "max"
  )
  
   #calculate the annual 98th percentile
  d1hm_p98 <- d1hm %>%
    dplyr::summarise(p98=rcaaqs:::quantile2(value,
                                           probs=0.98,
                                           na.rm=TRUE,
                                           type="caaqs")) %>%
    dplyr::select(p98)
    
  #Calculate hourly percentiles over the year: 
  hp<-sub %>%
    dplyr::group_by(date=lubridate::year(date)) %>%
    dplyr::summarise(`0%(hr)`=rcaaqs:::quantile2(value,
                                                 probs=0,
                                                 na.rm=TRUE,
                                                 type="caaqs"
    ),
    `10%(hr)`=rcaaqs:::quantile2(value,
                                 probs=0.1,
                                 na.rm=TRUE,
                                 type="caaqs"
    ),
    `25%(hr)`=rcaaqs:::quantile2(value,
                                 probs=0.25,
                                 na.rm=TRUE,
                                 type="caaqs"
    ),
    `50%(hr)`=rcaaqs:::quantile2(value,
                                 probs=0.5,
                                 na.rm=TRUE,
                                 type="caaqs"
    ),
    `75%(hr)`=rcaaqs:::quantile2(value,
                                 probs=0.75,
                                 na.rm=TRUE,
                                 type="caaqs"
    ),
    `90%(hr)`=rcaaqs:::quantile2(value,
                                 probs=0.9,
                                 na.rm=TRUE,
                                 type="caaqs"
    ),
    `95%(hr)`=rcaaqs:::quantile2(value,
                                 probs=0.95,
                                 na.rm=TRUE,
                                 type="caaqs"
    ),
    `98%(hr)`=rcaaqs:::quantile2(value,
                                 probs=.98,
                                 na.rm=TRUE,
                                 type="caaqs"
    ),
    `99%(hr)`=rcaaqs:::quantile2(value,
                                 probs=0.99,
                                 na.rm=TRUE,
                                 type="caaqs"
    ),
    `99.5%(hr)`=rcaaqs:::quantile2(value,
                                   probs=0.995,
                                   na.rm=TRUE,
                                   type="caaqs"
    ),
    `99.9%(hr)`=rcaaqs:::quantile2(value,
                                   probs=0.999,
                                   na.rm=TRUE,
                                   type="caaqs"
    ),
    # rcaaqs(probs=1,type="caaqs") isn't working, filed an issue on github
    # `100%(hr)`=rcaaqs:::quantile2(value,
    #                             probs=1,
    #                             na.rm=TRUE,
    #                             type="caaqs"
    # ),
    `100%(hr)`=max(value,
                   na.rm = TRUE))
  
  #count hourly exceedances of 100 ppb:
  hoursAbove100 <- sub %>%
    dplyr::filter(value>100) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::pull(n)
  
  #calculate d1hm percentiles over the year:
  d1hmp <- d1hm %>%
    dplyr::group_by(date=lubridate::year(date)) %>%
    dplyr::summarise(`0%(d1hm)`=rcaaqs:::quantile2(value,
                                                 probs=0,
                                                 na.rm=TRUE,
                                                 type="caaqs"
    ),
    `10%(d1hm)`=rcaaqs:::quantile2(value,
                                 probs=0.1,
                                 na.rm=TRUE,
                                 type="caaqs"
    ),
    `25%(d1hm)`=rcaaqs:::quantile2(value,
                                 probs=0.25,
                                 na.rm=TRUE,
                                 type="caaqs"
    ),
    `50%(d1hm)`=rcaaqs:::quantile2(value,
                                 probs=0.5,
                                 na.rm=TRUE,
                                 type="caaqs"
    ),
    `75%(d1hm)`=rcaaqs:::quantile2(value,
                                 probs=0.75,
                                 na.rm=TRUE,
                                 type="caaqs"
    ),
    `90%(d1hm)`=rcaaqs:::quantile2(value,
                                 probs=0.9,
                                 na.rm=TRUE,
                                 type="caaqs"
    ),
    `95%(d1hm)`=rcaaqs:::quantile2(value,
                                 probs=0.95,
                                 na.rm=TRUE,
                                 type="caaqs"
    ),
    `98%(d1hm)`=rcaaqs:::quantile2(value,
                                 probs=.98,
                                 na.rm=TRUE,
                                 type="caaqs"
    ),
    `99%(d1hm)`=rcaaqs:::quantile2(value,
                                 probs=0.99,
                                 na.rm=TRUE,
                                 type="caaqs"
    ),
    `99.5%(d1hm)`=rcaaqs:::quantile2(value,
                                   probs=0.995,
                                   na.rm=TRUE,
                                   type="caaqs"
    ),
    `99.9%(d1hm)`=rcaaqs:::quantile2(value,
                                   probs=0.999,
                                   na.rm=TRUE,
                                   type="caaqs"
    ),
    # rcaaqs(probs=1,type="caaqs") isn't working, filed an issue on github
    # `100%(hr)`=rcaaqs:::quantile2(value,
    #                             probs=1,
    #                             na.rm=TRUE,
    #                             type="caaqs"
    # ),
    `100%(d1hm)`=max(value,
                   na.rm = TRUE))
  
  #count d1hm exceedances of 100ppb:
  d1hmAbove100 <- d1hm %>%
    dplyr::filter(value>100) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::pull(n)
  
  #calculate number of monitoring days each month
  dm<-openair::timeAverage(dt,
                           avg.time="month",
                           statistic="frequency")
  
  #calculate number of monitoring days each quarter
  dq<-openair::timeAverage(dt,
                           avg.time="3 month",
                           statistic="frequency")
  
  #calculate total no. days each quarter
  alld<-openair::timeAverage(sub,
                             avg.time="day")
  allq<-c(sum(Hmisc::monthDays(dm$date)[1:3]),
          sum(Hmisc::monthDays(dm$date)[4:6]),
          sum(Hmisc::monthDays(dm$date)[7:9]),
          sum(Hmisc::monthDays(dm$date)[10:12]))

  #calculate quarterly data capture (%)
  q<-as_tibble(round((dq[,2]/allq)*100,0))
  
  
  #Change dm from 12 observations of 2 variables to 1 observation of 12
  # variables for making summary (Stats) below
  dm %<>% 
    dplyr::mutate(date=format(date,"%m")) %>%
    tidyr::spread(key=date,value=value)
  
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
    tidyr::spread(QUARTER,value) 
  
  #Create and print summary table
  (
    stats <- tibble::tibble(
      
      `STATION NAME` = data %>%
        dplyr::pull(STATION_NAME) %>%
        unique,
      
      YEAR = hp$date,
      
      `VALID DAYS`= nd,
      
      `VALID HOURS`=nh,
      
      `ANNUAL 1-HR AVG`=round(mean(sub$value,na.rm=T),2),
      
      # `ANNUAL DAILY AVG`=round(mean(dt$value, na.rm = T), 2)
    ) %>%
      
      dplyr::bind_cols(
        
        # Hourly Percentiles
        round(hp %>% dplyr::select(-date),
                             2),
        
        # Hourly Exceedances of 100 ppb
        tibble::tibble(`HOURLY EXCEEDANCES > 100 ppb`=
                         hoursAbove100),
        
        # D1hm Percentiles
        round(d1hmp %>% dplyr::select(-date),
              2),
        
        # Exceedances of D1HM >100 PPB
        tibble::tibble(`EXCEEDANCES OF D1HM > 100 ppb`=d1hmAbove100),
        
        # Annual 98P of D1HM
        d1hm_p98 %>% 
          dplyr::mutate(p98=round(p98,2)) %>%
          dplyr::rename(`ANNUAL 98P D1HM`=p98),
        
        # Annual 98P of D1HM 3-yr ave
        tibble::tibble(`98P_DAILY,3-YR AVG`=NA_real_),
        
        # days of monitoring/month
        dm,
        
        # percent of monitoring/quarter
        q
        ) 

  ) # end stats
  
}