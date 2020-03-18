# Script: h2ssasFcn.R
# Description: h2s sas script for ftp data

# FOR TESTING
# subset for a single station and param for testing the function
# data<-feather::read_feather("unverifiedData.feather") %>%
#   dplyr::filter(STATION_NAME=="Pine River Gas Plant" &
#            PARAMETER=="H2S") %>% distinct()
# 
# h2sSASFcn(data)

h2sSASFcn<-function(data,h2scolumn,dateColumn){
  
  library(openair) 
  library(Hmisc)
  library(tidyverse)
  
  #for testing
  # h2scolumn<-"RAW_VALUE"
  # dateColumn<-"DATE_PST"
  # data<-h2s
  
  #default arguments
  if(missing(h2scolumn)){h2scolumn<-"RAW_VALUE"}
  if(missing(dateColumn)){dateColumn<-"DATE_PST"}
  if(missing(data)){data<-h2s}
  
  # unique id for each combination of STATION_NAME, INSTRUMENT
  # should be length one as function is mapped over station_name and instrument
  id<-data %>% 
    mutate(id=stringr::str_c(STATION_NAME,INSTRUMENT,sep = "_")) %>%
    distinct(id)
  
  h2ssub <- data %>%
    dplyr::select_(date=dateColumn,
                   H2S=h2scolumn)
  
  ############# VALID DATA (DAYS AND HR.) ################ 
  
  #Count the number of days with valid data:
  #calculate daily averages time series, data completeness = 75%
  dt<-timeAverage(h2ssub,avg.time="day",data.thresh=75)
  nd<-nrow(dt[complete.cases(dt[,2]),])
  
  #Count the number of hours with valid data:
  nh<-h2ssub %>% filter(!is.na(H2S)) %>% nrow(.)
  
  ############### HR. PERC. & EXCEEDANCES ###############  
  
  #Calculate hourly percentiles over the year: 
  hp<-calcPercentile(h2ssub,
                     pollutant="H2S",
                     avg.time="year",
                     percentile=c(0,10,25,50,75,90,95,98,99,99.5,99.9,100))
    
  #count hourly exceedances of level A (5), level B (20), and level C (NA) objectives.
  ha<-nrow(subset(h2ssub,h2ssub[,2]>=5))
  hb<-nrow(subset(h2ssub,h2ssub[,2]>=20))
  hc<-NA
  
  #calculate hourly % of exceedances
  pha<-round((ha/nh)*100,2)
  phb<-round((hb/nh)*100,2)
  phc<-NA
  
  ############### DAILY PERC. & EXCEEDANCES ############### 
  
  #calculate daily percentiles over the year:
  dp<-calcPercentile(dt,
                     pollutant="H2S",
                     avg.time="year",
                     percentile=c(0,10,25,50,75,90,95,98,99,99.5,99.9,100))
  
  #count daily exceedances of level A (2), level B (4), and level C (NA) objectives:
  da<-nrow(subset(dt,dt[,2]>=2))
  db<-nrow(subset(dt,dt[,2]>=4))
  dc<-NA
  
  #calculate daily % of exceedances
  pda<-round((da/nd)*100,2)
  pdb<-round((db/nd)*100,2)
  pdc<-NA
  
  ############### DATA CAPTURE (d/MO, QUARTERLY) ############### 
  
  #calculate number of monitoring days each month
  dm<-timeAverage(dt,avg.time="month",statistic="frequency")
  
  #calculate number of monitoring days each quarter
  dq<-timeAverage(dt,avg.time="3 month",statistic="frequency")
  
  #calculate total no. days each quarter
  alld<-timeAverage(h2ssub,avg.time="day")
  allq<-c(sum(monthDays(dm$date)[1:3]),sum(monthDays(dm$date)[4:6]),
          sum(monthDays(dm$date)[7:9]),sum(monthDays(dm$date)[10:12]))
  
  #calculate quarterly data capture (%)
  q<-round((dq[,2]/allq)*100,0)
  
  #Change dm from 12 observations of 2 variables to 1 observation of 12 variables for making summary (sas) below
  dm<-dm %>% 
    mutate(date=format(date,"%m")) %>%
    spread(key=date,value=H2S)
  
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
                  Hr_Mean=round(mean(h2ssub$H2S,na.rm=T),2),
                  Hr_StDev=round(sd(h2ssub$H2S,na.rm=T),2),
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