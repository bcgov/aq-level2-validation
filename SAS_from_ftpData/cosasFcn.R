#Script: cosasFcn.R
#Description: calculates co sas stats on ftp data
# # # 

# subset for a single station and param for testing the function
# data<-readr::read_rds("unverified_data.rds") %>%
#   dplyr::filter(STATION_NAME=="Victoria Topaz" &
#            PARAMETER=="CO") %>%
#   distinct()
# 
# coSAS<-coSASFcn(data)


coSASFcn<-function(data,cocolumn,dateColumn){
  
  library(openair) 
  library(Hmisc)
  library(tidyverse)
  library(rlang)
  library(rcaaqs)
  
  #for testing
  # cocolumn<-"RAW_VALUE"
  # dateColumn<-"DATE_PST"
  # data<-data %>%
  #   dplyr::filter(PARAMETER %in% toupper("co") &
  #                   STATION_NAME=="KELOWNA COLLEGE")
  # END TESTING
  
  #default arguments
  if(missing(cocolumn)){cocolumn<-"RAW_VALUE"}
  if(missing(dateColumn)){dateColumn<-"DATE_PST"}
  if(missing(data)){data<-co}
  
  cosub <- data %>%
    dplyr::select(date=!!dateColumn,
                   value=!!cocolumn)

  #calculate daily averages time series with data completeness of 75%
  dt<-openair::timeAverage(cosub,
                           avg.time="day",
                           data.thresh=75)
  
  #Count the number of days with valid data:
  nd<-dt %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::pull(n)
  
  
  #Count the number of hours with valid data:
  nh<-cosub %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::pull(n)
  
  # # # HR. PERCENTILE & EXCEEDANCES  # # #
  
  #Calculate hourly percentiles over the year: 
  hp<-cosub %>%
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
  
 
  #count hourly exceedances of 13 ppm.
  hoursAbove13 <- cosub %>%
    dplyr::filter(value>=13) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::pull(n)
  
  # # # DAILY PERCENTILE & EXCEEDANCES  # # #
  
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
  
  # # # 8hr AVE. PERC. & EXCEEDANCES # # #
  
  #calculate 8 hr. rolling average. 
  roll8hr<-openair::rollingMean(cosub,
                    pollutant="value",
                    width=8,             #8 hour rolling mean
                    new.name="co.8hr", #column heading
                    data.thresh=75,       #>=18 hr. for rolling mean
                    align="right")   %>% #back running
    tibble::as_tibble(.)
 
 
  #count rolling ave. exceedances of 5 ppm:
  roll8hrAbove5 <- roll8hr %>%
    dplyr::filter(co.8hr >= 5) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::pull(n)
  
  # # # MONITORING DAYS/MO & /Q # # #
  
  #calculate number of monitoring days each month
  dm<-openair::timeAverage(dt,
                  avg.time="month",
                  statistic="frequency")
  
  #calculate number of monitoring days each quarter
  dq<-openair::timeAverage(dt,
                  avg.time="3 month",
                  statistic="frequency")
  
  #calculate total no. days each quarter (no data completeness)
  alld<-openair::timeAverage(cosub,avg.time="day")
  allq<-c(sum(Hmisc::monthDays(dm$date)[1:3]),
          sum(Hmisc::monthDays(dm$date)[4:6]),
          sum(Hmisc::monthDays(dm$date)[7:9]),
          sum(Hmisc::monthDays(dm$date)[10:12]))
  
  #calculate quarterly data capture (%)
  q<-as_tibble(round((dq[,2]/allq)*100,0))
  
  
  #Change dm from 12 observations of 2 variables to 1 observation of 12
  # variables for making summary (sas) below
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
  
  
  # # # CREATE SUMMARY TABLE (SAME AS SAS)  # # #
  
  (
    sas <- tibble::tibble(
      
      `STATION NAME` = data %>%
        dplyr::pull(STATION_NAME) %>%
        unique,
      
      YEAR = hp$date,
      
      `VALID DAYS`= nd,
      
      `VALID HOURS`=nh,
      
      `ANNUAL 1-HR AVG`=round(mean(cosub$value,na.rm=T),2),
      
    ) %>%
      
      dplyr::bind_cols(
        
        # Hourly Percentiles
        round(hp %>% dplyr::select(-date),
              2),
        
        # Hourly Exceedances of 13 ppm
        tibble::tibble(`HOURLY EXCEEDANCES > 13 ppm`=
                         hoursAbove13),
        
        # Daily Percentiles
        round(dp %>% dplyr::select(-date),
              2),
        
        # Exceedances of Rolling 8hr > 5 PPm
        tibble::tibble(`EXCEEDANCES OF ROLLING 8HR > 5 ppm`=roll8hrAbove5),
        
        # days of monitoring/month
        dm,
        
        # percent of monitoring/quarter
        q
      ) 
    
  ) # end sas
  
  # utils::View(sas)
 
}

