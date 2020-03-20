# Script: nosasFcn.R
# Description: no sas script for ftp data

# FOR TESTING
# subset for a single station and param for testing the function
# data<-feather::read_feather("unverifiedData.feather") %>%
#   dplyr::filter(STATION_NAME_FULL=="Castlegar Zinio Park" &
#            PARAMETER=="NO") %>% distinct()
# 
# noSASFcn(data)

noSASFcn<-function(data,nocolumn,dateColumn){
  
  library(openair) 
  library(Hmisc)
  library(tidyverse)
  
  #for testing
  # nocolumn<-"RAW_VALUE"
  # dateColumn<-"DATE_PST"
  # data<-no
  
  #default arguments
  if(missing(nocolumn)){nocolumn<-"RAW_VALUE"}
  if(missing(dateColumn)){dateColumn<-"DATE_PST"}
  if(missing(data)){data<-no}
  
  # unique id for each combination of STATION_NAME_FULL, INSTRUMENT
  # should be length one as function is mapped over STATION_NAME_FULL and instrument
  id<-data %>% 
    mutate(id=stringr::str_c(STATION_NAME_FULL,INSTRUMENT,sep = "_")) %>%
    distinct(id)
  
  nosub <- data %>%
    dplyr::select_(date=dateColumn,
                   NO=nocolumn)
  
  ############# VALID DATA (DAYS AND HR.) ################ 
  
  #Count the number of days with valid data:
  #calculate daily averages time series, data completeness = 75%
  dt<-timeAverage(nosub,avg.time="day",data.thresh=75)
  nd<-nrow(dt[complete.cases(dt[,2]),])
  
  #Count the number of hours with valid data:
  nh<-nosub %>% filter(!is.na(NO)) %>% nrow(.)
  
  ############### HR. PERC. & EXCEEDANCES ###############  
  
  #Calculate hourly percentiles over the year: 
  hp<-calcPercentile(nosub,
                     pollutant="NO",
                     avg.time="year",
                     percentile=c(0,10,25,50,75,90,95,98,99,99.5,99.9,100))
    
  #count hourly exceedances of level A (210), level B (210), and level C (530) objectives.
  ha<-nrow(subset(nosub,nosub[,2]>=210))
  hb<-nrow(subset(nosub,nosub[,2]>=210))
  hc<-nrow(subset(nosub,nosub[,2]>=530))
  
  #calculate hourly % of exceedances
  pha<-round((ha/nh)*100,2)
  phb<-round((hb/nh)*100,2)
  phc<-round((hc/nh)*100,2)
  
  ############### DAILY PERC. & EXCEEDANCES ############### 
  
  #calculate daily percentiles over the year:
  dp<-calcPercentile(dt,
                     pollutant="NO",
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
  
  ############### DATA CAPTURE (d/MO, QUARTERLY) ############### 
  
  #calculate number of monitoring days each month
  dm<-timeAverage(dt,avg.time="month",statistic="frequency")
  
  #calculate number of monitoring days each quarter
  dq<-timeAverage(dt,avg.time="3 month",statistic="frequency")
  
  #calculate total no. days each quarter
  alld<-timeAverage(nosub,avg.time="day")
  allq<-c(sum(monthDays(dm$date)[1:3]),sum(monthDays(dm$date)[4:6]),
          sum(monthDays(dm$date)[7:9]),sum(monthDays(dm$date)[10:12]))
  
  #calculate quarterly data capture (%)
  q<-round((dq[,2]/allq)*100,0)
  
  #Change dm from 12 observations of 2 variables to 1 observation of 12 variables for making summary (sas) below
  dm<-dm %>% 
    mutate(date=format(date,"%m")) %>%
    spread(key=date,value=NO)
  
  #
  names(dm)<-format(as.POSIXct(stringr::str_c("2000",names(dm),"01",sep = "-"),
                               "%Y-%m-%d",tz="UTC"),
                    "%b")
  
  #do something similar for q
  q<-t(q)
  colnames(q)<-c("PercQ1","PercQ2","PercQ3","PercQ4")
  
  ############### COMPILE IN SUMMARY TABLE ############### 
  
  #Create and print summary table
  sas<-data.frame(Site=id,
                  Year=as.numeric(format(hp$date,"%Y")),
                  Valid_Days= nd,
                  Valid_Hrs= nh,
                  Hr_Mean=round(mean(nosub$NO,na.rm=T),2),
                  Hr_StDev=round(sd(nosub$NO,na.rm=T),2),
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
  
  names(sas)[7:18]<-stringr::str_replace(names(sas[7:18]),
                                         "percentile",
                                         "Hr_P")
  
  names(sas)[25:36]<-stringr::str_replace(
    stringr::str_replace(names(sas)[25:36],".1$",""),
    "percentile","Day_P") 
  
  
  sas
}