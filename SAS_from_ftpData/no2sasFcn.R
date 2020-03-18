#Script: no2sasFcn.R
#Description: calculates no2 sas stats on ftp data
# # # 

# # subset for a single station and param for testing the function
# data<-feather::read_feather("unverifiedData.feather") %>%
#   dplyr::filter(STATION_NAME=="Castlegar Zinio Park" &
#            PARAMETER=="NO2") %>%
#   distinct()
# 
# no2SASFcn(data)

no2SASFcn<-function(data,no2column,dateColumn){
  
  library(openair) 
  library(Hmisc)
  library(tidyverse)
  
  #for testing
  # no2column<-"RAW_VALUE"
  # dateColumn<-"DATE_PST"
  # data<-no2
  
  #default arguments
  if(missing(no2column)){no2column<-"RAW_VALUE"}
  if(missing(dateColumn)){dateColumn<-"DATE_PST"}
  if(missing(data)){data<-no2}
  
  # unique id for each combination of STATION_NAME, INSTRUMENT
  # should be length one as function is mapped over station_name and instrument
  id<-data %>% 
    mutate(id=stringr::str_c(STATION_NAME,INSTRUMENT,sep = "_")) %>%
    distinct(id)
  
  no2sub <- data %>%
    dplyr::select_(date=dateColumn,
                   NO2=no2column)
  
  #Count the number of days with valid data:
  #calculate daily averages time series with data completeness of 75%
  dt<-timeAverage(no2sub,avg.time="day",data.thresh=75)
  nd<-nrow(dt[!is.na(dt[,2]),])
  
  #Count the number of hours with valid data:
  nh<-no2sub %>% filter(!is.na(NO2)) %>% nrow(.)
  
  
  #AQO: 98th percentile of the maximum hourly concentration during the day:
  #calculate daily 1-hr max:
  d1hm<-timeAverage(mydata=no2sub,
                     pollutant="NO2",
                     avg.time="day",         #averaging period
                     statistic="max")
  
  #calculate the annual 98th percentile
  d1hm_p98<-timeAverage(d1hm,
                   pollutant="NO2",
                   avg.time="year",
                   statistic="percentile",
                   percentile=98) %>%
    select(NO2)
  
  #Calculate hourly percentiles over the year: 
  hp<-calcPercentile(no2sub,
                     pollutant="NO2",
                     avg.time="year",
                     percentile=c(0,10,25,50,75,90,95,98,99,99.5,99.9,100))
  
  #count hourly exceedances of level A (NA), level B (210), and level C (530) objectives.
  ha<-NA
  hb<-nrow(subset(no2sub,no2sub[,2]>=210))
  hc<-nrow(subset(no2sub,no2sub[,2]>=530))
  
  #calculate hourly % of exceedances
  pha<-NA
  phb<-round((hb/nh)*100,2)
  phc<-round((hc/nh)*100,2)
  
  #calculate daily percentiles over the year:
  dp<-calcPercentile(dt,
                     pollutant="NO2",
                     avg.time="year",
                     percentile=c(0,10,25,50,75,90,95,98,99,99.5,99.9,100))
  
  #count daily exceedances of level A (NA), level B (110), and level C (160) objectives:
  da<-NA
  db<-nrow(subset(dt,dt[,2]>=110))
  dc<-nrow(subset(dt,dt[,2]>=160))
  
  #calculate daily % of exceedances
  pda<-NA
  pdb<-round((db/nd)*100,2)
  pdc<-round((dc/nd)*100,2)
  
  #calculate number of monitoring days each month
  dm<-timeAverage(dt,avg.time="month",statistic="frequency")
  
  #calculate number of monitoring days each quarter
  dq<-timeAverage(dt,avg.time="3 month",statistic="frequency")
  
  #calculate total no. days each quarter
  alld<-timeAverage(no2sub,avg.time="day")
  allq<-c(sum(monthDays(dm$date)[1:3]),sum(monthDays(dm$date)[4:6]),
          sum(monthDays(dm$date)[7:9]),sum(monthDays(dm$date)[10:12]))
  
  #calculate quarterly data capture (%)
  q<-round((dq[,2]/allq)*100,0)
  
  #Change dm from 12 observations of 2 variables to 1 observation of 12 variables for making summary (sas) below
  dm<-dm %>% 
    mutate(date=format(date,"%m")) %>%
    spread(key=date,value=NO2)
  
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
                  Hr_Mean=round(mean(no2sub$NO2,na.rm=T),2),
                  Hr_StDev=round(sd(no2sub$NO2,na.rm=T),2),
                  D1HM_p98=d1hm_p98$NO2,
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
  names(sas)[8:19]<-sapply(c(0,10,25,50,75,90,95,98,99,99.5,99.9,100),
                           function(i){paste0("Hr_P",i)})
  names(sas)[26:37]<-sapply(c(0,10,25,50,75,90,95,98,99,99.5,99.9,100),
                            function(i){paste0("Day_P",i)})
  
  
  sas
  
}