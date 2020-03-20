#Script: o3sasFcn.R
#Description: calculates o3 sas stats on ftp data
# # # 



o3SASFcn<-function(data,o3column,dateColumn){ 
  
  library(openair)
  library(Hmisc)
  library(tidyverse)

  # #for testing
  # o3column<-"RAW_VALUE"
  # dateColumn<-"DATE_PST"
  
  #default arguments
  if(missing(o3column)){o3column<-"RAW_VALUE"}
  if(missing(dateColumn)){dateColumn<-"DATE_PST"}
  if(missing(data)){data<-o3}

  # unique id for each combination of STATION_NAME_FULL, INSTRUMENT
  # should be length one as function is mapped over STATION_NAME_FULL and instrument
  id<-data %>% 
    mutate(id=stringr::str_c(STATION_NAME_FULL,INSTRUMENT,sep = "_")) %>%
    distinct(id)
  
  
  o3sub<-data %>%
    select_(date=dateColumn,
            O3=o3column)
  
  
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
  )
  
  #count hourly exceedances of level A (51), level B (82), 
  #and level C (153) objectives.
  ha<-nrow(subset(o3sub,o3sub[,2]>=51))
  hb<-nrow(subset(o3sub,o3sub[,2]>=82))
  hc<-nrow(subset(o3sub,o3sub[,2]>=153))
  
  #calculate hourly % of exceedances
  pha<-round((ha/nh)*100,2)
  phb<-round((hb/nh)*100,2)
  phc<-round((hc/nh)*100,2)
  
  #calculate daily percentiles over the year:
  dp<-calcPercentile(dt,
                     pollutant="O3",
                     avg.time="year",
                     percentile=c(0,10,25,50,75,90,95,98,99,99.5,99.9,100))
  
  #count exceedances of daily objectives level A (15) and B (25):
  da<-nrow(subset(dt,dt[,2]>=15))
  db<-nrow(subset(dt,dt[,2]>=25))
  
  #calculate daily % of exceedances
  pda<-round((da/nd)*100,2)
  pdb<-round((db/nd)*100,2)
  
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
  rmax<-timeAverage(roll,
                    avg.time="day",
                    statistic="max")
  
  #count exceedances of max daily 8 hr. roll ave obj. (65):
  e<-nrow(subset(rmax,rmax$roll.o3>=65))
  
  #calculate % exceedance  
  rh<-nrow(rmax[complete.cases(rmax$roll.o3),])
  pe<-round((e/rh)*100,2)
  
  #calculate 4th highest max daily 8 hr. roll ave
  f<-round(rmax$roll.o3[order(-rmax$roll.o3)][4],2)
  
  #calculate number of monitoring days each month
  dm<-timeAverage(dt,
                  avg.time="month",
                  statistic="frequency")
  
  #calculate number of monitoring days each quarter
  dq<-timeAverage(dt,
                  avg.time="3 month",
                  statistic="frequency")
  #calculate total no. days in Q2+Q3
  d23<-sum(monthDays(dm$date)[4:9])
  
  #calculate Q2+Q3 capture (%)
  p23<-round((sum(dq[2:3,2])/d23)*100,0)
  
  #Change dm from 12 observations of 2 variables to 1 observation of 12 variables for making summary (sas) below
  dm<-dm %>% 
    mutate(date=format(date,"%m")) %>%
    spread(key=date,value=O3) 
  
  #
  names(dm)<-format(as.POSIXct(stringr::str_c("2000",names(dm),"01",sep = "-"),
                               "%Y-%m-%d",tz="UTC"),
                    "%b")  
  
  
  #Create and print summary table
  sas<-data.frame(Site=id,
                  Year=as.numeric(format(hp$date,"%Y")),
                  Valid_Days= nd,
                  Valid_Hrs= nh,
                  Hr_Mean=round(mean(o3sub$O3,na.rm=T),2),
                  Hr_StDev=round(sd(o3sub$O3,na.rm=T),2),
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
                  Days_8hrMax_Ex_65ppb=e,
                  Perc_8hrMax_Ex_65ppb=pe,
                  Fourth_Highest_8hrMax=f,
                  rank=NA,
                  Roll3Yr=NA,
                  dm,
                  PercQ2Q3=p23
  )
  names(sas)[7:18]<-stringr::str_replace(names(sas[7:18]),
                                         "percentile",
                                         "Hr_P")
  
  names(sas)[25:36]<-stringr::str_replace(
    stringr::str_replace(names(sas)[25:36],".1$",""),
    "percentile","Day_P") 
  
  
  sas
  
  # names(sas)
  
}

# # # TESTING # # # 

# data<-feather::read_feather("./data/unverifiedData.feather")
# 
# data<-data %>%
#   filter(PARAMETER %in% "O3" &
#            stringr::str_detect(STATION_NAME_FULL,"Castlegar"))
# 
# o3sas<-purrr::map_dfr((data %>%
#               filter(PARAMETER %in% "O3") %>%
#               distinct(STATION_NAME_FULL) %>%
#               arrange(STATION_NAME_FULL))$STATION_NAME_FULL,
# 
#            function(station){
# 
#              #for testing
#              # allStations<-(data %>%
#              #                 filter(PARAMETER %in% "O3") %>%
#              #                 distinct(STATION_NAME_FULL) %>%
#              #                 arrange(STATION_NAME_FULL))$STATION_NAME_FULL
#              #
#              # station<-(data %>%
#              #   filter(PARAMETER %in% "O3") %>%
#              #   filter(STATION_NAME_FULL==allStations[17]) %>%
#              #     distinct(STATION_NAME_FULL))$STATION_NAME_FULL
#              #
#              # end testing
# 
# 
#              station<-data %>%
#                filter(PARAMETER %in% "O3" &
#                         STATION_NAME_FULL %in% station)
# 
#              purrr::map_dfr(unique(station$INSTRUMENT),
# 
#                function(instrument){
# 
#                  #for testing
#                  # instrument<-(station %>%
#                  #                distinct(INSTRUMENT))$INSTRUMENT
#                  #
#                  # instrument<-station %>%
#                  #   filter(INSTRUMENT %in% instrument[1])
# 
#                  #END TESTING
# 
#                  instrument<-station %>%
#                    filter(INSTRUMENT %in% instrument)
# 
#                  o3SASFcn(data=instrument)
# 
#                }
# 
# 
# 
#                ) #INSTRUMENT LOOP
# 
#            }) #STATION LOOP
# 
# 
# 
# 
# 
# 
# dataBackup<-data
# data<-dataBackup