#Script: o3_stats_fcn.R
#Description: calculates o3 Stats stats on ftp data
# # # 

# subset for a single station and param for testing the function
 # data<-readr::read_rds("unverified_data.rds") %>%
 #   dplyr::filter(STATION_NAME=="Victoria Topaz" &
 #            PARAMETER=="O3") %>%
 #   distinct()
# 
# o3SAS<-o3_stats_fcn(data)

o3_stats_fcn<-function(data,o3column,dateColumn,year){ 
  
  library(openair)
  library(Hmisc)
  library(tidyverse)
  library(rlang)
  library(rcaaqs)

  # #for testing
  # o3column<-"RAW_VALUE"
  # dateColumn<-"DATE_PST"
  # data<-data %>%
  #   dplyr::filter(PARAMETER %in% toupper("o3") &
  #                   STATION_NAME=="CRANBROOK MURIEL BAXTER")
  # END TESTING
  
  #default arguments
  if(missing(o3column)){o3column<-"RAW_VALUE"}
  if(missing(dateColumn)){dateColumn<-"DATE_PST"}
  if(missing(data)){data<-o3}
  if(missing(year)){year<-lubridate::year(Sys.Date())-1}
  
  sub<-data %>%
    dplyr::select(date=!!dateColumn,
                  value=!!o3column)
  
  
  #Count the number of days with valid data:
  #calculate daily averages time series with data completeness of 75%
  dt<-openair::timeAverage(sub,
                  pollutant="O3",
                  avg.time="day",
                  data.thresh=75,
                  start.date = stringr::str_c(year,"-01-01",sep=""),
                  end.date = stringr::str_c(year,"-12-31",sep="")
                  )
  
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
  
  #count hourly 82 ppb.
  hoursAbove82<-sub %>%
    dplyr::filter(value>=82) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::pull(n)
  
  #calculate 8 hr. rolling average. 
  #To calc. for Jan 1, need last 7 hr. from Dec 
  roll<-rollingMean(sub,
                    pollutant="value",
                    width=8,              #8 hour rolling mean
                    new.name="roll.o3", #column heading
                    data.thresh=75,       #>=18 hr. for rolling mean
                    align="right")        #back running
  
  #remove o3 column
  roll<- roll %>%
    select(-value)
  
  #calculate max daily 8 hr. roll ave
  d8hm<-openair::timeAverage(roll,
                    avg.time="day",
                    statistic="max")
  
  # daily 8 hour maximum average percentiles
  #calculate d8hm percentiles over the year:
  d8hmp <-d8hm %>%
    dplyr::group_by(date=lubridate::year(date)) %>%
    dplyr::summarise(`0%(day)`=rcaaqs:::quantile2(roll.o3,
                                                  probs=0,
                                                  na.rm=TRUE,
                                                  type="caaqs"
    ),
    `10%(day)`=rcaaqs:::quantile2(roll.o3,
                                  probs=0.1,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `25%(day)`=rcaaqs:::quantile2(roll.o3,
                                  probs=0.25,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `50%(day)`=rcaaqs:::quantile2(roll.o3,
                                  probs=0.5,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `75%(day)`=rcaaqs:::quantile2(roll.o3,
                                  probs=0.75,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `90%(day)`=rcaaqs:::quantile2(roll.o3,
                                  probs=0.9,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `95%(day)`=rcaaqs:::quantile2(roll.o3,
                                  probs=0.95,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `98%(day)`=rcaaqs:::quantile2(roll.o3,
                                  probs=.98,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `99%(day)`=rcaaqs:::quantile2(roll.o3,
                                  probs=0.99,
                                  na.rm=TRUE,
                                  type="caaqs"
    ),
    `99.5%(day)`=rcaaqs:::quantile2(roll.o3,
                                    probs=0.995,
                                    na.rm=TRUE,
                                    type="caaqs"
    ),
    `99.9%(day)`=rcaaqs:::quantile2(roll.o3,
                                    probs=0.999,
                                    na.rm=TRUE,
                                    type="caaqs"
    ),
    # rcaaqs(probs=1,type="caaqs") isn't working, filed an issue on github
    # `100%(day)`=rcaaqs:::quantile2(roll.o3,
    #                             probs=1,
    #                             na.rm=TRUE,
    #                             type="caaqs"
    # ),
    `100%(day)`=max(roll.o3,
                    na.rm = TRUE))
  
  
  
  #count exceedances of max daily 8 hr. roll ave >63 ppb; commented out since this column doesn't exist in AQS file for 2022.
  # d8hmAbove63<-nrow(subset(d8hm,d8hm$roll.o3>=63))
  
  # d8hmAbove63<-d8hm %>%
  # dplyr::filter(roll.o3 >=63) %>%
  # dplyr::summarise(n = dplyr::n()) %>%
  # dplyr::pull(n)
  
  #calculate 4th highest max daily 8 hr. roll ave
  d8hmRank4<-as_tibble(round(d8hm$roll.o3[order(-d8hm$roll.o3)][4],2))
  
  #calculate number of monitoring days each month
  dm<-openair::timeAverage(dt,
                           avg.time="month",
                           statistic="frequency")
  
  #calculate number of monitoring days each quarter
  dq<-openair::timeAverage(dt,
                           avg.time="3 month",
                           statistic="frequency")
  
  #calculate total no. days in Q2+Q3
  alld<-openair::timeAverage(sub,avg.time="day")
  allq<-c(sum(Hmisc::monthDays(dm$date)[4:9]))
  
  #calculate Q2+Q3 capture (%)
  dq_total <- dq$value[2]+dq$value[3]
  q<-as_tibble(round((dq_total/allq)*100,0))
  
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
        
        # Hourly Exceedances of 82 ppb
        tibble::tibble(`HOURLY EXCEEDANCES > 82 ppb`=
                         hoursAbove82),
        
        # D8hm Percentiles
        round(d8hmp %>% dplyr::select(-date),
              2),
        
        # Exceedances of D8HM >63 PPB
        # tibble::tibble(`EXCEEDANCES OF D8HM > 63 ppb`=d8hmAbove63),
        
        # Annual 4TH rANK D8HM
        tibble::tibble(`ANNUAL 4TH RANK D8HM`=round(d8hmRank4$value,2)),
        
        # Annual 4TH rANK D8HM, 3-yr avg
        tibble::tibble(`ANNUAL 4TH RANK D8HM, 3-YR AVG`=NA_real_),
        
        # Annual 4TH rANK D8HM, 3-yr avg > 63 PPB
        tibble::tibble(`ANNUAL 4TH RANK D8HM, 3-YR AVG > 63 PPB?`=NA_real_),
        
        # Annual 4TH rANK D8HM, 3-yr avg > 62 PPB
        tibble::tibble(`ANNUAL 4TH RANK D8HM, 3-YR AVG > 62 PPB?`=NA_real_),
        
        # days of monitoring/month
        dm,
        
        # percent of monitoring/quarter
        tibble::tibble(`Q2+Q3(%days)`=q$value)
      ) 
    
  ) # end Stats
  
  # utils::View(Stats)
}

