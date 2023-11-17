# Script: noStatsFcn.R
# Description: no Stats script for ftp data

# FOR TESTING
# subset for a single station and param for testing the function
# data<-readr::read_rds("unverified_data.rds") %>%
#   dplyr::filter(STATION_NAME=="Castlegar Zinio Park" &
#            PARAMETER=="NO") %>% distinct()
# 
# noStatsFcn(data) 

noStatsFcn<-function(data,nocolumn,dateColumn){
  
  library(openair) 
  library(Hmisc)
  library(tidyverse)
  library(rlang)
  library(rcaaqs)
  
  #for testing
  # nocolumn<-"RAW_VALUE"
  # dateColumn<-"DATE_PST"
  # data<-data %>%
  #   dplyr::filter(PARAMETER %in% toupper("no") &
  #                   STATION_NAME=="CRANBROOK MURIEL BAXTER")
  # END TESTING
  
  #default arguments
  if(missing(nocolumn)){nocolumn<-"RAW_VALUE"}
  if(missing(dateColumn)){dateColumn<-"DATE_PST"}
  if(missing(data)){data<-no}
  
  nosub <- data %>%
    dplyr::select(date=!!dateColumn,
                   value=!!nocolumn)
  
  #calculate daily averages time series with data completeness of 75%
  dt<-openair::timeAverage(nosub,
                           avg.time="day",
                           data.thresh=75)
  
  #Count the number of days with valid data:
  nd<-dt %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::pull(n)
  
  #Count the number of hours with valid data:
  nh<-nosub %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::pull(n)
  
  #Calculate hourly percentiles over the year: 
  hp<-nosub %>%
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
                                                  probs=0.98,
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
  
  #calculate number of monitoring days each month
  dm<-openair::timeAverage(dt,
                           avg.time="month",
                           statistic="frequency")
  
  #calculate number of monitoring days each quarter
  dq<-openair::timeAverage(dt,
                           avg.time="3 month",
                           statistic="frequency")
  
  #calculate total no. days each quarter (no data completeness)
  alld<-openair::timeAverage(nosub,avg.time="day")
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
      
      `ANNUAL 1-HR AVG`=round(mean(nosub$value,na.rm=T),2),
      
    ) %>%
      
      dplyr::bind_cols(
        
        # Hourly Percentiles
        round(hp %>% dplyr::select(-date),
              2),
        
        # days of monitoring/month
        dm,
        
        # percent of monitoring/quarter
        q
      ) 
    
  ) # end Stats
  
  # utils::View(sas)
}