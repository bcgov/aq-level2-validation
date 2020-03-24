#Script: pm10sasFcn.R
#Description: calculates pm10 sas stats on ftp data
# # # 

# # subset for a single station and instrument for testing the function
# data<-feather::read_feather("unverifiedData.feather") %>%
#   dplyr::filter(STATION_NAME_FULL=="Golden Helipad" &
#            PARAMETER=="PM10") %>% distinct()
# 
# pm10SASFcn(data)


pm10SASFcn<-function(data,pm10column,dateColumn){
  
  library(openair) 
  library(Hmisc)
  library(tidyverse)
  
  #for testing
  # pm10column<-"RAW_VALUE"
  # dateColumn<-"DATE_PST"
  # data<-pm10
  
  #default arguments
  if(missing(pm10column)){pm10column<-"RAW_VALUE"}
  if(missing(dateColumn)){dateColumn<-"DATE_PST"}
  if(missing(data)){data<-pm10}
  
  # unique id for each combination of STATION_NAME_FULL, INSTRUMENT
  # should be length one as function is mapped over STATION_NAME_FULL and instrument
  id<-data %>% 
    mutate(id=stringr::str_c(STATION_NAME_FULL,INSTRUMENT,sep = "_")) %>%
    distinct(id)
  
  pm10sub <- data %>%
    dplyr::select_(date=dateColumn,
                   PM10=pm10column)

  #Count the number of days with valid data:
  #calculate daily averages time series with data completeness of 75%
  dt<-timeAverage(pm10sub,avg.time="day",data.thresh=75)
  nd<-nrow(dt[complete.cases(dt[,2]),])
  
  #Count the number of hours with valid data:
  nh<-nrow(pm10sub[!is.na(pm10sub[,2]),])
  
  ########### HR. PERCENTILE & EXCEEDANCES  #################
  
  #Calculate hourly percentiles over the year: 
  hp<-calcPercentile(pm10sub,
                     pollutant="PM10",
                     avg.time="year",
                     percentile=c(0,10,25,50,75,90,95,98,99,99.5,99.9,100))
    
  #count hourly exceedances of level A (25), level B (50), 
  #and level C (100) objectives.
  oa<-as.numeric(25)
  ha<-nrow(subset(pm10sub,pm10sub[,2]>=oa))
  ob<-as.numeric(50)
  hb<-nrow(subset(pm10sub,pm10sub[,2]>=ob))
  oc<-as.numeric(100)
  hc<-nrow(subset(pm10sub,pm10sub[,2]>=oc))
  
  #calculate hourly % of exceedances
  pha<-round((ha/nh)*100,2)
  phb<-round((hb/nh)*100,2)
  phc<-round((hc/nh)*100,2)
  
  ##############  DAILY PERCENTILE & EXCEEDANCES  ##############
  
  #calculate daily percentiles over the year:
  dp<-calcPercentile(dt,
                     pollutant="PM10",
                     avg.time="year",
                     percentile=c(0,10,25,50,75,90,95,98,99,99.5,99.9,100))
  
  #count daily exceedances of objectives:
  da<-nrow(subset(dt,dt[,2]>=oa))
  db<-nrow(subset(dt,dt[,2]>=ob))
  dc<-nrow(subset(dt,dt[,2]>=oc))
  
  #calculate daily % of exceedances
  pda<-round((da/nd)*100,2)
  pdb<-round((db/nd)*100,2)
  pdc<-round((dc/nd)*100,2)
  
  ######### ROLL. AVE. PERC. & EXCEEDANCES ################
  
  #calculate 24 hr. rolling average. 
  
  roll<-rollingMean(pm10sub,
                    pollutant="PM10",
                    width=24,             #24 hour rolling mean
                    new.name="roll.pm10", #column heading
                    data.thresh=75,       #>=18 hr. for rolling mean
                    align="right")        #back running
 
 
  #calculate percentiles for hourly rolling mean:
  rp<-calcPercentile(roll[,-2],
                     pollutant="roll.pm10",
                     avg.time="year",
                     percentile=c(0,10,25,50,75,90,95,98,99,99.5,99.9,100))
  
  #count rolling ave. exceedances of objectives:
  ra<-nrow(subset(roll,roll[,3]>=oa))
  rb<-nrow(subset(roll,roll[,3]>=ob))
  rc<-nrow(subset(roll,roll[,3]>=oc))
  
  #calculate rolling ave. % of exceedances  
  rh<-nrow(roll[complete.cases(roll[,3]),])
  
  pra<-round((ra/rh)*100,2)
  prb<-round((rb/rh)*100,2)
  prc<-round((rc/rh)*100,2)
  
  ######  MONITORING DAYS/MO & /Q  ##################
  
  
  #calculate number of monitoring days each month
  dm<-timeAverage(dt,avg.time="month",statistic="frequency")
  
  #calculate number of monitoring days each quarter
  dq<-timeAverage(dt,avg.time="3 month",statistic="frequency")
  
  #calculate total no. days each quarter
  alld<-timeAverage(pm10sub,avg.time="day")
  allq<-c(sum(monthDays(dm$date)[1:3]),sum(monthDays(dm$date)[4:6]),
          sum(monthDays(dm$date)[7:9]),sum(monthDays(dm$date)[10:12]))
  
  #calculate quarterly data capture (%)
  q<-round((dq[,2]/allq)*100,0)
  
  #Change dm from 12 observations of 2 variables to 1 observation of 12 variables for making summary (sas) below
  dm<-dm %>% 
    mutate(date=format(date,"%m")) %>%
    spread(key=date,value=PM10)
  
  #
  names(dm)<-format(as.POSIXct(stringr::str_c("2000",names(dm),"01",sep = "-"),
                               "%Y-%m-%d",tz="UTC"),
                    "%b")
  
  #do something similar for q
  q<-t(q)
  colnames(q)<-c("PercQ1","PercQ2","PercQ3","PercQ4")
  
  
  ########### CREATE SUMMARY TABLE (SAME AS SAS)  #############
  #Create and print summary table
  sas<-data.frame(Site=id,
                  Year=as.numeric(format(hp$date,"%Y")),
                  Valid_Days= nd,
                  Valid_Hrs= nh,
                  Hr_Mean=round(mean(pm10sub$PM10,na.rm=T),2),
                  Hr_StDev=round(sd(pm10sub$PM10,na.rm=T),2),
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
                  round(rp[2:length(rp)],2),
                  No_rollMean_Ex_A=ra,
                  Perc_rollMean_Ex_A=pra,
                  No_rollMean_Ex_B=rb,
                  Perc_rollMean_Ex_B=prb,
                  No_rollMean_Ex_C=rc,
                  Perc_rollMean_Ex_C=prc,
                  dm,
                  q
                  
                  ) #sas
 
  names(sas)[7:18]<-stringr::str_replace(names(sas[7:18]),
                                         "percentile",
                                         "Hr_P")
  
  names(sas)[25:36]<-stringr::str_replace(
    stringr::str_replace(names(sas)[25:36],".1$",""),
    "percentile","Day_P") 
  
  names(sas)[43:54]<-stringr::str_replace(
    stringr::str_replace(names(sas)[43:54],".2$",""),
    "percentile","Roll24hr_P") 
 
  
 sas
 
}

