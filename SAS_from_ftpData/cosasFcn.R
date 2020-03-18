#Script: cosasFcn.R
#Description: calculates co sas stats on ftp data
# # # 

# subset for a single station and param for testing the function
# data<-feather::read_feather("unverifiedData.feather") %>%
#   dplyr::filter(STATION_NAME=="Victoria Topaz" &
#            PARAMETER=="CO") %>%
#   distinct()
# 
# coSASFcn(data)


coSASFcn<-function(data,cocolumn,dateColumn){
  
  library(openair) 
  library(Hmisc)
  library(tidyverse)
  
  #for testing
  # cocolumn<-"RAW_VALUE"
  # dateColumn<-"DATE_PST"
  # data<-co
  
  #default arguments
  if(missing(cocolumn)){cocolumn<-"RAW_VALUE"}
  if(missing(dateColumn)){dateColumn<-"DATE_PST"}
  if(missing(data)){data<-co}
  
  # unique id for each combination of STATION_NAME, INSTRUMENT
  # should be length one as function is mapped over station_name and instrument
  id<-data %>% 
    mutate(id=stringr::str_c(STATION_NAME,INSTRUMENT,sep = "_")) %>%
    distinct(id)
  
  cosub <- data %>%
    dplyr::select_(date=dateColumn,
                   CO=cocolumn)

  #Count the number of days with valid data:
  #calculate daily averages time series with data completeness of 75%
  dt<-timeAverage(cosub,avg.time="day",data.thresh=75)
  nd<-nrow(dt[complete.cases(dt[,2]),])
  
  #Count the number of hours with valid data:
  nh<-nrow(cosub[!is.na(cosub[,2]),])
  
  ########### HR. PERCENTILE & EXCEEDANCES  #################
  
  #Calculate hourly percentiles over the year: 
  hp<-calcPercentile(cosub,
                     pollutant="CO",
                     avg.time="year",
                     percentile=c(0,10,25,50,75,90,95,98,99,99.5,99.9,100))
    
  #count hourly exceedances of level A (13), level B (25), 
  #and level C (30) objectives.
  oa<-as.numeric(13)
  ha<-nrow(subset(cosub,cosub[,2]>=oa))
  ob<-as.numeric(25)
  hb<-nrow(subset(cosub,cosub[,2]>=ob))
  oc<-as.numeric(30)
  hc<-nrow(subset(cosub,cosub[,2]>=oc))
  
  #calculate hourly % of exceedances
  pha<-round((ha/nh)*100,2)
  phb<-round((hb/nh)*100,2)
  phc<-round((hc/nh)*100,2)
  
  ##############  DAILY PERCENTILE & EXCEEDANCES  ##############
  
  #calculate daily percentiles over the year:
  dp<-calcPercentile(dt,
                     pollutant="CO",
                     avg.time="year",
                     percentile=c(0,10,25,50,75,90,95,98,99,99.5,99.9,100))
  
  #count daily exceedances of 24-hr objectives: level A(5), level B(10), level C(13).
  da<-nrow(subset(dt,dt[,2]>=5))
  db<-nrow(subset(dt,dt[,2]>=10))
  dc<-nrow(subset(dt,dt[,2]>=13))
  
  #calculate daily % of exceedances
  pda<-round((da/nd)*100,2)
  pdb<-round((db/nd)*100,2)
  pdc<-round((dc/nd)*100,2)
  
  ######### 8hr AVE. PERC. & EXCEEDANCES ################
  
  #calculate 8 hr. rolling average. 
  
  roll8hr<-rollingMean(cosub,
                    pollutant="CO",
                    width=8,             #8 hour rolling mean
                    new.name="co.8hr", #column heading
                    data.thresh=75,       #>=18 hr. for rolling mean
                    align="right")        #back running
 
 
  #calculate percentiles for hourly rolling mean:
  rp<-calcPercentile(roll8hr[,-2],
                     pollutant="co.8hr",
                     avg.time="year",
                     percentile=c(0,10,25,50,75,90,95,98,99,99.5,99.9,100))
  
  #count rolling ave. exceedances of objectives (same as for 24-hr):
  ra<-nrow(subset(roll8hr,roll8hr[,3]>=5))
  rb<-nrow(subset(roll8hr,roll8hr[,3]>=10))
  rc<-nrow(subset(roll8hr,roll8hr[,3]>=13))
  
  #calculate rolling ave. % of exceedances  
  rh<-nrow(roll8hr[complete.cases(roll8hr[,3]),])
  
  pra<-round((ra/rh)*100,2)
  prb<-round((rb/rh)*100,2)
  prc<-round((rc/rh)*100,2)
  
  ######  MONITORING DAYS/MO & /Q  ##################
  
  
  #calculate number of monitoring days each month
  dm<-timeAverage(dt,avg.time="month",statistic="frequency")
  
  #calculate number of monitoring days each quarter
  dq<-timeAverage(dt,avg.time="3 month",statistic="frequency")
  
  #calculate total no. days each quarter
  alld<-timeAverage(cosub,avg.time="day")
  allq<-c(sum(monthDays(dm$date)[1:3]),sum(monthDays(dm$date)[4:6]),
          sum(monthDays(dm$date)[7:9]),sum(monthDays(dm$date)[10:12]))
  
  #calculate quarterly data capture (%)
  q<-round((dq[,2]/allq)*100,0)
  
  #Change dm from 12 observations of 2 variables to 1 observation of 12 variables for making summary (sas) below
  dm<-dm %>% 
    mutate(date=format(date,"%m")) %>%
    spread(key=date,value=CO)
  
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
                  Hr_Mean=round(mean(cosub$CO,na.rm=T),2),
                  Hr_StDev=round(sd(cosub$CO,na.rm=T),2),
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
                  No_8hr_Ex_A=ra,
                  Perc_8hr_Ex_A=pra,
                  No_8hr_Ex_B=rb,
                  Perc_8hr_Ex_B=prb,
                  No_8hr_Ex_C=rc,
                  Perc_8hr_Ex_C=prc,
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
                "percentile","Roll8hr_P") 
 
  
 sas
 
}

