#Script: pm25StatsFcn.R
#Description: calculates pm25 Stats stats on ftp data
# # # 

#FOR TESTING
# pm25column<-"RAW_VALUE"
# dateColumn<-"DATE_PST"
# data<-readr::read_rds("./preppedData/Willow Creek Mine.rds") %>%
#   dplyr::filter(PARAMETER %in% "PM25")
# 
# data %>% distinct(INSTRUMENT)
# 
# data %>%
#   dplyr::filter(is.na(INSTRUMENT) & is.na(RAW_VALUE)) %>%
#   readr::write_csv(.,
#                    "merrittMAMLNAs.csv")
# 
# data %>%
#   dplyr::filter(!is.na(INSTRUMENT)) %>%
#   dplyr::summarise(min(DATE_PST))
# 
# pm25StatsFcn(data) %>% utils::View()

# END TESTING

pm25StatsFcn<-function(data,pm25column,dateColumn){ 
  
  library(openair)
  library(Hmisc)
  library(tidyverse)
  library(rlang)
  library(rcaaqs)

  
  #default arguments
  if(missing(pm25column)){pm25column<-"RAW_VALUE"}
  if(missing(dateColumn)){dateColumn<-"DATE_PST"}
  if(missing(data)){data<-pm25}
  
  sub <- data %>%
    dplyr::select(date=!!dateColumn,
            value=!!pm25column)

  dt<-openair::timeAverage(sub,
                  pollutant="PM25",
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
  
  #Is annual average greater than 8 ug/m3?
  AAG8 <- dplyr::case_when(
    
    mean(sub$value,na.rm=T)>8 ~ "Yes",
    mean(sub$value,na.rm=T)<=8 ~ "No",
    sum(is.na(sub$value))==length(sub$value) ~ NA_character_
    
  )
  
  #Calculate hourly percentiles over the year:
  
  if(sum(is.na(sub$value))==length(sub$value)){
  
  hp<-sub %>%
    dplyr::group_by(date=lubridate::year(date)) %>%
    dplyr::summarise(`0%(hr)`=NA_real_,
    `10%(hr)`=NA_real_,
    `25%(hr)`=NA_real_,
    `50%(hr)`=NA_real_,
    `75%(hr)`=NA_real_,
    `90%(hr)`=NA_real_,
    `95%(hr)`=NA_real_,
    `98%(hr)`=NA_real_,
    `99%(hr)`=NA_real_,
    `99.5%(hr)`=NA_real_,
    `99.9%(hr)`=NA_real_,
    `100%(hr)`=NA_real_)
  } else{
    
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
    
  }
  
    
  #calculate daily percentiles over the year:

  if(sum(is.na(dt$value))==nrow(dt)){
    
    dp<-dt %>%
      dplyr::group_by(date=lubridate::year(date)) %>%
      dplyr::summarise(`0%(day)`=NA_real_,
      `10%(day)`=NA_real_,
      `25%(day)`=NA_real_,
      `50%(day)`=NA_real_,
      `75%(day)`=NA_real_,
      `90%(day)`=NA_real_,
      `95%(day)`=NA_real_,
      `98%(day)`=NA_real_,
      `99%(day)`=NA_real_,
      `99.5%(day)`=NA_real_,
      `99.9%(day)`=NA_real_,
      `100%(day)`=NA_real_,)
    
  } else{
  
  
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
  }
  
  #count daily exceedances of 25 ug/m3:
  daysAbove25<-dt %>%
    dplyr::filter(value>25) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::pull(n)
  
  ##Annual 98P daily average > 25
  Annual98P <- dp %>% dplyr::pull(`98%(day)`)
  
  AnnualAQO <- dplyr::case_when(
    
    Annual98P>25 ~ "Yes",
    Annual98P<=25 ~ "No",
    is.na(Annual98P) ~ NA_character_
    )
    
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
  dm<-dm %>% 
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
      `VALID DAYS`= nd,
      `ANNUAL 1-HR AVG`=round(mean(sub$value,na.rm=T),2),
     # removed in 2022 stat summary
     # `ANNUAL DAILY AVG`=round(mean(dt$value,na.rm=T),2),
      `DAILY AVG, 3-YR AVG` = NA_real_, # added to Stats summary in 2021, i don't calculate it
      `ANNUAL AVG > 8?` = AAG8, #added in Stats summary in 2022
      `ANNUAL AVG, 3-YR AVG > 8.8?` = NA_real_, #added in Stats summary in 2022
    ) %>%
      dplyr::bind_cols(round(hp %>% dplyr::select(-date),
                             2)) %>%
      dplyr::bind_cols(round(dp %>% dplyr::select(-date),
                             2)) %>%
      dplyr::bind_cols(
        tibble::tibble(
          `ANNUAL 98P_DAILY`=round(dp %>% dplyr::pull(`98%(day)`),2),
          `98P_DAILY,3-YR AVG`=NA_real_,
          `ANNUAL 98P_DAILY > 25?`=AnnualAQO,
          `98P_DAILY,3-YR AVG > 27?`=NA_real_
          #`EXCEEDANCES OF DAILY AVG > 25ug/m3`=daysAbove25 #Removed in 2022 stat summary
        )) %>%
      dplyr::bind_cols(dm,q)
    
    
  )
}




