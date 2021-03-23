#Script: o3sasFcn.R
#Description: calculates o3 sas stats on ftp data
# # # 



o3SASFcn<-function(data,o3column,dateColumn){ 
  
  library(openair)
  library(Hmisc)
  library(tidyverse)
  library(rlang)

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

  o3sub<-data %>%
    dplyr::select(date=!!dateColumn,
                  O3=!!o3column)
  
  
  #Count the number of days with valid data:
  #calculate daily averages time series with data completeness of 75%
  dt<-timeAverage(o3sub,
                  pollutant="O3",
                  avg.time="day",
                  data.thresh=75)
  
  nd<-nrow(dt[!is.na(dt[,2]),])
  
  #Count the number of hours with valid data:
  nh<-o3sub %>% filter(!is.na(O3)) %>% nrow(.)
  
  #Calculate hourly percentiles over the year: 
  hp<-calcPercentile(o3sub,
                     pollutant="O3",
                     avg.time="year",
                     percentile=c(0,10,25,50,75,90,95,98,99,99.5,99.9,100)
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
  
  #count hourly 82 ppb.
  hoursAbove82<-nrow(subset(o3sub,o3sub[,2]>=82))
  
  #calculate 8 hr. rolling average. 
  #To calc. for Jan 1, need last 7 hr. from Dec 
  roll<-rollingMean(o3sub,
                    pollutant="O3",
                    width=8,              #8 hour rolling mean
                    new.name="roll.o3", #column heading
                    data.thresh=75,       #>=18 hr. for rolling mean
                    align="right")        #back running
  
  #remove o3 column
  roll<- roll %>%
    select(-O3)
  
  #calculate max daily 8 hr. roll ave
  d8hm<-timeAverage(roll,
                    avg.time="day",
                    statistic="max")
  
  # daily 8 hour maximum average percentiles
  #calculate d8hm percentiles over the year:
  d8hmp <- calcPercentile(
    d8hm,
    pollutant = "roll.o3",
    avg.time = "year",
    percentile = c(0, 10, 25, 50, 75, 90, 95, 98, 99, 99.5, 99.9, 100)
  ) %>%
    
    dplyr::rename(
      `0%(d8hm)` = percentile.0,
      `10%(d8hm)` = percentile.10,
      `25%(d8hm)` = percentile.25,
      `50%(d8hm)` = percentile.50,
      `75%(d8hm)` = percentile.75,
      `90%(d8hm)` = percentile.90,
      `95%(d8hm)` = percentile.95,
      `98%(d8hm)` = percentile.98,
      `99%(d8hm)` = percentile.99,
      `99.5%(d8hm)` = percentile.99.5,
      `99.9%(d8hm)` = percentile.99.9,
      `100%(d8hm)` = percentile.100
    )
  
  #count exceedances of max daily 8 hr. roll ave >63 ppb:
  d8hmAbove63<-nrow(subset(d8hm,d8hm$roll.o3>=65))
  
  #calculate 4th highest max daily 8 hr. roll ave
  d8hmRank4<-round(d8hm$roll.o3[order(-d8hm$roll.o3)][4],2)
  
  #calculate number of monitoring days each month
  dm<-timeAverage(dt,avg.time="month",statistic="frequency")
  
  #calculate number of monitoring days each quarter
  dq<-timeAverage(dt,avg.time="3 month",statistic="frequency")
  
  #calculate total no. days in Q2+Q3
  d23<-sum(monthDays(dm$date)[4:9])
  
  #calculate Q2+Q3 capture (%)
  p23<-round((sum(dq[2:3,2])/d23)*100,0)
  
  #Change dm from 12 observations of 2 variables to 1 observation of 12
  # variables for making summary (sas) below
  dm %<>% 
    dplyr::mutate(date=format(date,"%m")) %>%
    tidyr::spread(key=date,value=O3)
  
  #
  names(dm) <- stringr::str_c(
    format(as.POSIXct(stringr::str_c("2000", names(dm), "01", sep = "-"),
                      "%Y-%m-%d", tz = "UTC"),
           "%b"),
    "(days)",sep = " ")
  
  
  #Create and print summary table
  (
    sas <- tibble::tibble(
      
      `STATION NAME` = data %>%
        dplyr::pull(STATION_NAME) %>%
        unique,
      
      YEAR = as.numeric(format(hp$date, "%Y")),
      
      `VALID DAYS`= nd,
      
      `VALID HOURS`=nh,
      
      `ANNUAL 1-HR AVG`=round(mean(o3sub$O3,na.rm=T),2),
      
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
        tibble::tibble(`EXCEEDANCES OF D8HM > 63 ppb`=d8hmAbove63),
        
        # Annual 4TH rANK D8HM
        tibble::tibble(`ANNUAL 4TH RANK D8HM`=round(d8hmRank4,2)),
        
        # Annual 4TH rANK D8HM, 3-yr avg
        tibble::tibble(`ANNUAL 4TH RANK D8HM, 3-YR AVG`=NA_real_),
        
        # days of monitoring/month
        dm,
        
        # percent of monitoring/quarter
        tibble::tibble(`Q2+Q3(%days)`=p23)
      ) 
    
  ) # end sas
  
  
}

