# Script: so2_stats_fcn.R
# Description: so2 Stats script for ftp data

# FOR TESTING
# subset for a single station and param for testing the function
# data<-readr::read_rds("unverified_data.rds") %>%
#   dplyr::filter(STATION_NAME=="Trail Columbia Gardens Airport" &
#            PARAMETER=="SO2") %>% distinct()
# 

# so2_stats_fcn(data) %>% utils::View(.)

so2_stats_fcn<-function(data,so2column,dateColumn,year){ 
  
  
  library(openair) 
  library(Hmisc)
  library(tidyverse)
  library(rlang)
  library(rcaaqs)
  
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
  if(missing(year)){year<-lubridate::year(Sys.Date())-1}
  
  sub <- data %>%
    dplyr::select(date=tidyr::all_of(dateColumn),
                   value=tidyr::all_of(so2column))
  
  
  #calculate daily averages time series with data completeness of 75%
  dt<-openair::timeAverage(sub,
                           avg.time="day",
                           data.thresh=75,
                           start.date = stringr::str_c(year,"-01-01",sep=""),
                           end.date = stringr::str_c(year,"-12-31",sep="")
                           )
  
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
  
  #AQO: 99th percentile of the d1hm:
  #calculate daily 1-hr max:
  d1hm<-openair::timeAverage(mydata=sub,
                     avg.time="day",         #averaging period
                     statistic="max")
  
  #calculate the annual 99th percentile of d1hm (caaqs method)
  d1hm_p99<-d1hm %>%
    dplyr::summarise(value=rcaaqs:::quantile2(value,
                                              probs=0.99,
                                              na.rm=TRUE,
                                              type="caaqs")) %>%
    dplyr::select(value)
  
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
  
  #Annal 1-hr average > 5ppb.
  
  AAG5 <- if(mean(sub$value,na.rm=T)>5){
    "Yes"
  } else {
    "No"
  }
  
  #count hourly exceedances of 70 and 75 ppb - removed in the 2022 Stat summaries
  # hoursAbove70<-sub %>%
  #   dplyr::filter(value>=70) %>%
  #   dplyr::summarise(n=dplyr::n()) %>%
  #   dplyr::pull(n)
  # hoursAbove75<-sub %>%
  #   dplyr::filter(value>=75) %>%
  #   dplyr::summarise(n=dplyr::n()) %>%
  #   dplyr::pull(n)

  #calculate d1hm percentiles over the year:
  d1hmp <- d1hm %>%
    dplyr::group_by(date=lubridate::year(date)) %>%
    dplyr::summarise(`0%(D1HM)`=rcaaqs:::quantile2(value,
                                                  probs=0,
                                                  na.rm=TRUE,
                                                  type="caaqs"
    ),
    `10%(D1HM)`=rcaaqs:::quantile2(value,
                                  probs=0.1,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `25%(D1HM)`=rcaaqs:::quantile2(value,
                                  probs=0.25,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `50%(D1HM)`=rcaaqs:::quantile2(value,
                                  probs=0.5,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `75%(D1HM)`=rcaaqs:::quantile2(value,
                                  probs=0.75,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `90%(D1HM)`=rcaaqs:::quantile2(value,
                                  probs=0.9,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `95%(D1HM)`=rcaaqs:::quantile2(value,
                                  probs=0.95,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `98%(D1HM)`=rcaaqs:::quantile2(value,
                                  probs=.98,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `99%(D1HM)`=rcaaqs:::quantile2(value,
                                  probs=0.99,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `99.5%(D1HM)`=rcaaqs:::quantile2(value,
                                    probs=0.995,
                                    na.rm=TRUE,
                                    type="caaqs"
    ),
    `99.9%(D1HM)`=rcaaqs:::quantile2(value,
                                    probs=0.999,
                                    na.rm=TRUE,
                                    type="caaqs"
    ),
    # rcaaqs(probs=1,type="caaqs") isn't working, filed an issue on github
    # `100%(D1HM)`=rcaaqs:::quantile2(value,
    #                             probs=1,
    #                             na.rm=TRUE,
    #                             type="caaqs"
    # ),
    `100%(D1HM)`=max(value,
                    na.rm = TRUE))
  
  
  #count d1hm exceedances of 70 and 75 ppb.
  d1hmAbove70<-d1hm %>%
    dplyr::filter(value>=70) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::pull(n)
  d1hmAbove75<-d1hm %>%
    dplyr::filter(value>=75) %>%
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
  alld<-openair::timeAverage(sub,avg.time="day")
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
    Stats <- tibble::tibble(
      
      `STATION NAME` = data %>%
        dplyr::pull(STATION_NAME) %>%
        unique,
      
      YEAR = hp$date,
      
      `VALID DAYS`= nd,
      
      `VALID HOURS`=nh,
      
      `ANNUAL 1-HR AVG`=round(mean(sub$value,na.rm=T),2),
      
    ) %>%
      
      dplyr::bind_cols(
        
        # Hourly Percentiles
        round(hp %>% dplyr::select(-date),
              2),
        
        #Annual 1-hr average>5
        `ANNUAL 1-HR AVG > 5?` = AAG5,
        
        # Hourly Exceedances of 70 ppb - removed in 2022 stat summaries
        #tibble::tibble(`HOURLY EXCEEDANCES > 70 ppb`=
        #                 hoursAbove70),
        
        # Hourly Exceedances of 75 ppb - removed in 2022 stat summaries
        #tibble::tibble(`HOURLY EXCEEDANCES > 75 ppb`=
        #                 hoursAbove75),
        
        # D1hm Percentiles
        round(d1hmp %>% dplyr::select(-date),
              2),
        
        # Exceedances of D1HM >70 PPB- removed in 2022 stat summaries
        #tibble::tibble(`EXCEEDANCES OF D1HM > 70 ppb`=d1hmAbove70),
        
        # Exceedances of D1HM >75 PPB- removed in 2022 stat summaries
        #tibble::tibble(`EXCEEDANCES OF D1HM > 75 ppb`=d1hmAbove75),
        
        # Annual 99P of D1HM
        d1hm_p99 %>% 
          dplyr::mutate(value=round(value,2)) %>%
          dplyr::rename(`ANNUAL 99P D1HM`=value),
        
        # Annual 99P of D1HM 3-yr ave
        tibble::tibble(`99P_DAILY,3-YR AVG`=NA_real_),
        
        # Annual 99P of D1HM 3-yr ave > 70 ppb
        tibble::tibble(`99P_DAILY,3-YR AVG > 70 PPB`=NA_real_),
        
        # days of monitoring/month
        dm,
        
        # percent of monitoring/quarter
        q
      ) 
    
  ) # end Stats
  
}
