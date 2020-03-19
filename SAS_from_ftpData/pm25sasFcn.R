#Script: pm25sasFcn.R
#Description: calculates pm25 sas stats on ftp data
# # # 

pm25SASFcn<-function(data,pm25column,dateColumn){ 
  
  library(openair)
  library(Hmisc)
  library(tidyverse)
  
  # #for testing
  # pm25column<-"RAW_VALUE"
  # dateColumn<-"DATE_PST"
  # data<-inst
  
  #default arguments
  if(missing(pm25column)){pm25column<-"RAW_VALUE"}
  if(missing(dateColumn)){dateColumn<-"DATE_PST"}
  if(missing(data)){data<-pm25}
  
  # unique id for each combination of STATION_NAME, INSTRUMENT
  # should be length one as function is mapped over station_name and instrument
  id<-data %>% 
    mutate(id=stringr::str_c(STATION_NAME,INSTRUMENT,sep = "_")) %>%
    distinct(id)
  
  pm25sub <- data %>%
    dplyr::select_(date=dateColumn,
            PM25=pm25column)

  dt<-timeAverage(pm25sub,
                  pollutant="PM25",
                  avg.time="day",
                  data.thresh=75)

  nd<-nrow(dt[!is.na(dt[,2]),])

  #Count the number of hours with valid data:
  nh<-nrow(pm25sub[!is.na(pm25sub[,2]),])

  #Calculate hourly percentiles over the year:
  hp<-calcPercentile(pm25sub,
                     pollutant="PM25",
                     avg.time="year",
                     percentile=c(0,10,25,50,75,90,95,98,99,99.5,99.9,100))

  #count hourly exceedances of level A (15), level B (25), and level C (30) objectives.
  #Hourly data must be rounded to whole number first:
  hrnd<-pm25sub
  hrnd[,2]<-round(pm25sub[,2],0)

  oa<-15
  ha<-nrow(subset(hrnd,hrnd[,2]>oa))

  ob<-25
  hb<-nrow(subset(hrnd,hrnd[,2]>ob))

  oc<-30
  hc<-nrow(subset(hrnd,hrnd[,2]>oc))

  #calculate hourly % of exceedances
  pha<-round((ha/nh)*100,2)
  phb<-round((hb/nh)*100,2)
  phc<-round((hc/nh)*100,2)

  #calculate daily percentiles over the year:
  dp<-calcPercentile(dt,
                     pollutant="PM25",
                     avg.time="year",
                     percentile=c(0,10,25,50,75,90,95,98,99,99.5,99.9,100))

  #count daily exceedances of level A (15), level B (25), and level C (30) objectives:
  da<-nrow(subset(dt,dt[,2]>=oa+0.5))
  db<-nrow(subset(dt,dt[,2]>=ob+0.5))
  dc<-nrow(subset(dt,dt[,2]>=oc+0.5))

  #calculate daily % of exceedances
  pda<-round((da/nd)*100,2)
  pdb<-round((db/nd)*100,2)
  pdc<-round((dc/nd)*100,2)

  #calculate number of monitoring days each month
  dm<-timeAverage(dt,
                  avg.time="month",
                  statistic="frequency")

  #calculate number of monitoring days each quarter
  dq<-timeAverage(dt,
                  avg.time="3 month",
                  statistic="frequency")

  #calculate total no. days each quarter
  alld<-timeAverage(pm25sub,avg.time="day")
  allq<-c(sum(monthDays(dm$date)[1:3]),sum(monthDays(dm$date)[4:6]),
          sum(monthDays(dm$date)[7:9]),sum(monthDays(dm$date)[10:12]))

  #calculate quarterly data capture (%)
  q<-round((dq[,2]/allq)*100,0)


  #Change dm from 12 observations of 2 variables to 1 observation of 12 variables for making summary (sas) below
  dm<-dm %>% 
    mutate(date=format(date,"%m")) %>%
    spread(key=date,value=PM25)
  
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
                  Hr_Mean=round(mean(pm25sub$PM25,na.rm=T),2),
                  Hr_StDev=round(sd(pm25sub$PM25,na.rm=T),2),
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
                  Roll3Yr=NA,
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

  # names(sas)
  
}

# # # TESTING # # # 

# data<-feather::read_feather("unverifiedData.feather")
# 
# dataBackup<-data
# data<-dataBackup
# 
# data<-data %>%
#   dplyr::filter(PARAMETER %in% "PM25")
# 
# pm25sas<-purrr::map_dfr((data %>%
#               dplyr::filter(PARAMETER %in% "PM25") %>%
#               distinct(STATION_NAME) %>%
#               arrange(STATION_NAME))$STATION_NAME,
# 
#            function(station){
# 
#              #for testing
#              # allStations<-(data %>%
#              #                 dplyr::filter(PARAMETER %in% "PM25") %>%
#              #                 distinct(STATION_NAME) %>%
#              #                 arrange(STATION_NAME))$STATION_NAME
#              #
#              # station<-(data %>%
#              #   dplyr::filter(PARAMETER %in% "PM25") %>%
#              #   dplyr::filter(STATION_NAME==allStations[17]) %>%
#              #     distinct(STATION_NAME))$STATION_NAME
#              #
#              # end testing
# 
# 
#              station<-data %>%
#                dplyr::filter(PARAMETER %in% "PM25" &
#                         STATION_NAME %in% station)
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
#                  #   dplyr::filter(INSTRUMENT %in% instrument[1])
# 
#                  #END TESTING
# 
#                  instrument<-station %>%
#                    dplyr::filter(INSTRUMENT %in% instrument)
# 
#                  pm25SASFcn(data=instrument)
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
# # subset for a single station and param for testing the function
# data<-data %>%
#   dplyr::filter(STATION_NAME=="Golden Helipad" &
#            INSTRUMENT=="PM25 SHARP5030i")
# 
# pm25SASFcn(data)


