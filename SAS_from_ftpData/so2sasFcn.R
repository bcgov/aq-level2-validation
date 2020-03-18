# Script: so2sasFcn.R
# Description: so2 sas script for ftp data

# FOR TESTING
# # subset for a single station and param for testing the function
# data<-feather::read_feather("unverifiedData.feather") %>%
#   dplyr::filter(STATION_NAME=="Trail Butler Park" &
#            PARAMETER=="SO2") %>% distinct()
# 
# so2SASFcn(data)

so2SASFcn<-function(data,so2column,dateColumn){ 
  
  
  library(openair) 
  library(Hmisc)
  library(tidyverse)
  
  #for testing
  # so2column<-"RAW_VALUE"
  # dateColumn<-"DATE_PST"
  # data<-so2
  
  #default arguments
  if(missing(so2column)){so2column<-"RAW_VALUE"}
  if(missing(dateColumn)){dateColumn<-"DATE_PST"}
  if(missing(data)){data<-so2}
  
  # unique id for each combination of STATION_NAME, INSTRUMENT
  # should be length one as function is mapped over station_name and instrument
  id<-data %>% 
    mutate(id=stringr::str_c(STATION_NAME,INSTRUMENT,sep = "_")) %>%
    distinct(id)
  
  so2sub <- data %>%
    dplyr::select_(date=dateColumn,
                   SO2=so2column)
  
  #Count the number of days with valid data:
  #calculate daily averages time series with data completeness of 75%
  dt<-timeAverage(so2sub,avg.time="day",data.thresh=75)
  nd<-nrow(dt[!is.na(dt[,2]),])
  
  #Count the number of hours with valid data:
  nh<-so2sub %>% filter(!is.na(SO2)) %>% nrow(.)
  
  #AQO: 97th percentile of the d1hm:
  #calculate daily 1-hr max:
  d1hm<-timeAverage(mydata=so2sub,
                     pollutant="SO2",
                     avg.time="day",         #averaging period
                     statistic="max")
  
  #calculate the annual 97th percentile
  d1hm_p97<-timeAverage(d1hm,
                   pollutant="SO2",
                   avg.time="year",
                   statistic="percentile",
                   percentile=97) %>%
    select(SO2)
  
  #calculate the annual 99th percentile of d1hrm
  d1hm_p99<-timeAverage(d1hm,
                   pollutant="SO2",
                   avg.time="year",
                   statistic="percentile",
                   percentile=99) %>%
    select(SO2)
  
  #Calculate hourly percentiles over the year: 
  hp<-calcPercentile(so2sub,
                     pollutant="SO2",
                     avg.time="year",
                     percentile=c(0,10,25,50,75,90,95,98,99,99.5,99.9,100))
  
  #count hourly exceedances of level A (170), level B (340), and level C (NA) objectives.
  ha<-nrow(subset(so2sub,so2sub[,2]>=170))
  hb<-nrow(subset(so2sub,so2sub[,2]>=340))
  hc<-NA
  
  #calculate hourly % of exceedances
  pha<-round((ha/nh)*100,2)
  phb<-round((hb/nh)*100,2)
  phc<-NA
  
  
  #calculate daily percentiles over the year:
  dp<-calcPercentile(dt,
                     pollutant="SO2",
                     avg.time="year",
                     percentile=c(0,10,25,50,75,90,95,98,99,99.5,99.9,100))
  
  #count daily exceedances of level A (60), level B (100), and level C (NA) objectives:
  da<-nrow(subset(dt,dt[,2]>=60))
  db<-nrow(subset(dt,dt[,2]>=100))
  dc<-NA
  
  #calculate daily % of exceedances
  pda<-round((da/nd)*100,2)
  pdb<-round((db/nd)*100,2)
  pdc<-NA
  
  #calculate number of monitoring days each month
  dm<-timeAverage(dt,avg.time="month",statistic="frequency")
  
  #calculate number of monitoring days each quarter
  dq<-timeAverage(dt,avg.time="3 month",statistic="frequency")
  
  #calculate total no. days each quarter
  alld<-timeAverage(so2sub,avg.time="day")
  allq<-c(sum(monthDays(dm$date)[1:3]),sum(monthDays(dm$date)[4:6]),
          sum(monthDays(dm$date)[7:9]),sum(monthDays(dm$date)[10:12]))
  
  #calculate quarterly data capture (%)
  q<-round((dq[,2]/allq)*100,0)
  
  #Change dm from 12 observations of 2 variables to 1 observation of 12 variables for making summary (sas) below
  dm<-dm %>% 
    mutate(date=format(date,"%m")) %>%
    spread(key=date,value=SO2)
  
  #
  names(dm)<-format(as.POSIXct(stringr::str_c("2000",names(dm),"01",sep = "-"),
                               "%Y-%m-%d",tz="UTC"),
                    "%b")
  
  #do something similar for q
  q<-t(q)
  colnames(q)<-c("PercQ1","PercQ2","PercQ3","PercQ4")
  
  #Create and print summary table
  sas<-data.frame(Site=id,
                  Year=as.numeric(format(hp$date,"%Y")),
                  Valid_Days= nd,
                  Valid_Hrs= nh,
                  Hr_Mean=round(mean(so2sub$SO2,na.rm=T),2),
                  Hr_StDev=round(sd(so2sub$SO2,na.rm=T),2),
                  D1HM_P97=d1hm_p97$SO2,
                  D1HM_P99=d1hm_p99$SO2,
                  round(hp[2:length(hp)],2),
                  No_Hr_Ex_A=ha,
                  Perc_Hr_Ex_A=pha,
                  No_Hr_Ex_B=hb,
                  Perc_Hr_Ex_B=phb,
                  No_Hr_Ex_C=hc,
                  Perc_Hr_Ex_C=phc,
                  round(dp[2:length(dp)],2),
                  No_Day_Ex_A=da,
                  Perc_Day_Ex_A=pda,
                  No_Day_Ex_B=db,
                  Perc_Day_Ex_B=pdb,
                  No_Day_Ex_C=dc,
                  Perc_Day_Ex_C=pdc,
                  dm,
                  q
  )
  
  names(sas)[8:20]<-stringr::str_replace(names(sas[8:20]),
                                         "percentile",
                                         "Hr_P")
  
  names(sas)[26:38]<-stringr::str_replace(
    stringr::str_replace(names(sas)[26:38],".1$",""),
    "percentile","Day_P") 
  
  sas
  
}
