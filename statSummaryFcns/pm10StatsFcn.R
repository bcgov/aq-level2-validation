#Script: pm10StatsFcn.R
#Description: calculates pm10 Stats stats on ftp data
# NOTE: SAME AS PM25 EXCEPT FOR OBJECTIVE
# # # 

# # subset for a single station and instrument for testing the function
 data<-readr::read_rds("unverified_data.rds") %>%
   dplyr::filter(STATION_NAME=="Golden Helipad" &
            PARAMETER=="PM10") %>% distinct()
# 
# pm10Stats<-pm10StatsFcn(data)


pm10StatsFcn<-function(data,pm10column,dateColumn){ 
  
  library(openair)
  library(Hmisc)
  library(tidyverse)
  library(rlang)
  library(rcaaqs)
  
  #FOR TESTING
  # pm10column<-"RAW_VALUE"
  # dateColumn<-"DATE_PST"
  # data<-data %>%
  #   dplyr::filter(PARAMETER %in% "PM10" &
  #                   STATION_NAME=="REVELSTOKE_TRAILER" &
  #                   INSTRUMENT=="PM10_T640")
  # END TESTING
  
  #default arguments
  if(missing(pm10column)){pm10column<-"RAW_VALUE"}
  if(missing(dateColumn)){dateColumn<-"DATE_PST"}
  if(missing(data)){data<-pm10}
  
  sub <- data %>%
    dplyr::select(date=!!dateColumn,
                  value=!!pm10column)
  
  dt<-openair::timeAverage(sub,
                  pollutant="PM10",
                  avg.time="day",
                  data.thresh=75)
  
  nd<-dt %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::pull(n)
  
  #Count the number of hours with valid data:
  nh<-sub %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::pull(n)
  
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
  
  #calculate daily percentiles over the year:
  dp<-dt %>%
    dplyr::group_by(date=lubridate::year(date)) %>%
    dplyr::summarise(`0%(day)`=rcaaqs:::quantile2(value,
                                                  probs=0,
                                                  na.rm=TRUE,
                                                  type="caaqs"
    ),
    `10%(day)`=rcaaqs:::quantile2(value,
                                  probs=0.1,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `25%(day)`=rcaaqs:::quantile2(value,
                                  probs=0.25,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `50%(day)`=rcaaqs:::quantile2(value,
                                  probs=0.5,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `75%(day)`=rcaaqs:::quantile2(value,
                                  probs=0.75,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `90%(day)`=rcaaqs:::quantile2(value,
                                  probs=0.9,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `95%(day)`=rcaaqs:::quantile2(value,
                                  probs=0.95,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `98%(day)`=rcaaqs:::quantile2(value,
                                  probs=.98,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `99%(day)`=rcaaqs:::quantile2(value,
                                  probs=0.99,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `99.5%(day)`=rcaaqs:::quantile2(value,
                                    probs=0.995,
                                    na.rm=TRUE,
                                    type="caaqs"
    ),
    `99.9%(day)`=rcaaqs:::quantile2(value,
                                    probs=0.999,
                                    na.rm=TRUE,
                                    type="caaqs"
    ),
    # rcaaqs(probs=1,type="caaqs") isn't working, filed an issue on github
    # `100%(day)`=rcaaqs:::quantile2(value,
    #                             probs=1,
    #                             na.rm=TRUE,
    #                             type="caaqs"
    # ),
    `100%(day)`=max(value,
                    na.rm = TRUE))
  
  #count daily exceedances of 50 ug/m3:
  
  daysAbove50 <- dt %>%
    dplyr::filter(value>=50+0.5) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::pull(n)
  
  #calculate daily % of exceedances
  pdaysAbove50<-as_tibble(round((daysAbove50/nd)*100,2))
  
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
      INSTRUMENT = data %>%
        dplyr::pull(INSTRUMENT) %>%
        unique,
      YEAR = hp$date,
      `VALID HOURS`=nh,
      `ANNUAL 1-HR AVG`=round(mean(sub$value,na.rm=T),2),
      `VALID DAYS`= nd,
      `ANNUAL DAILY AVG`=round(mean(dt$value,na.rm=T),2),
      `DAILY AVG, 3-YR AVG` = NA_real_ # added to Stats summary in 2021, i don't calculate it
    ) %>%
      dplyr::bind_cols(round(hp %>% dplyr::select(-date),
                             2)) %>%
      dplyr::bind_cols(round(dp %>% dplyr::select(-date),
                             2)) %>%
      dplyr::bind_cols(
        tibble::tibble(
          `ANNUAL 98P_DAILY`=round(dp %>% dplyr::pull(`98%(day)`),
                                   2),
          `98P_DAILY,3-YR AVG`=NA_real_,
          `EXCEEDANCES OF DAILY AVG > 50ug/m3`=daysAbove50
        )) %>%
      dplyr::bind_cols(dm,q)
  )
  
  
}

