#### SETUP ####
library(tidyverse)
library(magrittr)

#### rcaaqs issue to file - throws an error when all NA's ####


data<-rcaaqs::pm25_sample_data %>% 
  dplyr::filter(lubridate::year(date_time)==2012)

data %>%
  dplyr::summarise(p50 = rcaaqs:::quantile2(value,
    probs = 0.5,
    na.rm = TRUE,
    type = "caaqs"))

#### 2022 Sparwood ####
# has fewer days of valid data compared to stats summaries
data<-envair::importBC_data(parameter_or_station = c("pm25",
                                                     "pm10"),
                            years=2022,
                            use_openairformat = FALSE) %>%
  dplyr::filter(STATION_NAME=="Sparwood Centennial Square" &
                  lubridate::year(DATE_PST)==2022)
  

# throwing an error - filed issue on github
# captures<-envair::get_captures(param=data,
#                                years = 2022)

# use rcaaqs to get data capture since envair::get_captures() isn't working
test<-rcaaqs:::pm_annual_caaqs(
  data,
  dt="DATE_PST",
  val="RAW_VALUE",
  by="PARAMETER"
)

# calculate number of valid days/month
test$daily_avg %>%
  dplyr::filter(valid==TRUE) %>%
  dplyr::group_by(PARAMETER,
                  month=lubridate::month(date,
                                   label=TRUE)) %>%
  dplyr::summarise(n=n()) %>% utils::View(.)

# calculate number of valid days/year
test$daily_avg %>%
  dplyr::filter(valid==TRUE) %>%
  dplyr::group_by(PARAMETER,
                  year=lubridate::year(date)) %>%
  dplyr::summarise(n=n()) %>% utils::View(.)

#### envair hours in year (plus one) ####

data<-envair::importBC_data(parameter_or_station = "PM10",
                            years = 2022,
                            use_openairformat = FALSE) %>%
  dplyr::filter(STATION_NAME=="Golden Helipad")


data %>% 
  dplyr::filter(!is.na(RAW_VALUE)) %>%
  dplyr::summarise(n=n())

data %>% 
  dplyr::filter(!is.na(RAW_VALUE) &
                  lubridate::year(DATE_PST)==2022) %>%
  dplyr::summarise(n=n())

data %>%
  dplyr::filter(lubridate::year(DATE_PST)!=2022)

#### wind roses in Trail ####

envair::list_parameters()

data<-envair::importBC_data(parameter_or_station = c("wspd_sclr",
                                                     "wdir_vect"),
                            years=2016:2022,
                            use_openairformat = FALSE) %>%
  
  dplyr::filter(STATION_NAME %in% c("Trail Butler Park",
                                    "Trail Columbia Gardens Airport",
                                    "Birchbank Golf Course") &
                  lubridate::year(DATE_PST) %in% 2016:2022)

# two different owner fields (INDUSTRY and NA)
data %>%
  
  dplyr::filter(STATION_NAME=="Trail Butler Park" & 
                  OWNER=="INDUSTRY") %>%
  
  dplyr::group_by(STATION_NAME,
                  YEAR=lubridate::year(DATE_PST),
                  PARAMETER,
                  VALIDATION_STATUS,
                  OWNER) %>%
  
  dplyr::summarise(
    n_total=n(),
    na=sum(is.na(RAW_VALUE))) %>% utils::View(.)


windData<- data %>%
  
  dplyr::filter(STATION_NAME=="Trail Butler Park" &
                  is.na(OWNER)) %>%
                  # OWNER=="INDUSTRY") %>%
  
  dplyr::select(date=DATE_PST,
                STATION_NAME,
                PARAMETER,
                RAW_VALUE) %>%
  
  tidyr::pivot_wider(.,
                     names_from = PARAMETER,
                     values_from = RAW_VALUE) %>%
  
  # dplyr::slice(1:10) %>%
  
  
  dplyr::rename(ws = WSPD_SCLR,
                wd = WDIR_VECT) %>%
  
  
  dplyr::mutate(ws = ifelse(is.na(wd), NA_real_, ws),
                wd = ifelse(is.na(ws), NA_real_, wd))


purrr::map(unique(lubridate::year(windData$date)) %>% sort,
            
            function(year){
              
              # TESTING
              
               # year<-2016
              # 
              # windDataBackup<-windData
              # 
              # windData<-windDataBackup
              
              # END TESTING
              
              station<-unique(windData$STATION_NAME)
              
              windData %<>%
                dplyr::filter(lubridate::year(date) %in% year)
              
              #number of calm hours
              ncalm <- windData %>%
                dplyr::filter(ws < 0.5) %>%
                dplyr::summarise(calms = n())
              
              #total number of valid ws hours
              ntotal <- windData %>%
                dplyr::filter(!is.na(ws)) %>%
                dplyr::summarise(total = n())
              
              #percentage calm (ws<0.5 m/s)
              pcalm <- round(100 * ncalm / ntotal,
                             digits = 2)
              
              #filter out calm windData
              roseData <- windData %>%
                dplyr::filter(ws >= 0.5)
              
              if(nrow(windData)==sum(is.na(windData$ws)) | nrow(roseData)==0){
                
                paste(year,
                      ": there is either no paired wind data at",
                      station,
                      "or there is no paired wind data above the 0.5 m/s threshold.")
                
              } else {
                
                
                
                #windRose
                openair::windRose(
                  roseData,
                  annotate = FALSE,
                  breaks = c(0.5, 1.5, 3.3, 5.5, 7.9, 10.7, 13.8, 17.1),
                  #Beaufort scale with 0.5 as lowest cut point.
                  sub = paste("Calms (=<0.5m/s)=", 
                              pcalm, 
                              "% (",
                              ntotal$total,
                              " valid paired observations)"),
                  key.position = "right",
                  main = stringr::str_c(year," Wind Rose at ",station),
                  angle = 360 / 16,
                  #16 spokes
                  cols = "jet",
                  paddle = FALSE
                ) 
                
              } # end else
              
            } #end function(station)
            
)

#### columbia gardens airport so2 ####

data<-envair::importBC_data(parameter_or_station="Trail Columbia Gardens Airport",
                            years=2022,
                            use_openairformat = FALSE) %>%
  dplyr::filter(lubridate::year(DATE_PST)==2022)

data %<>% tibble::as_tibble(.)

caaqs<-rcaaqs::so2_3yr_caaqs(data %>%
  dplyr::filter(PARAMETER=="SO2"),
  dt="DATE_PST",
  val="RAW_VALUE")

caaqs$yearly_99 %>% utils::View(.) # 42.4 for annual 99th percentile of d1hm

d1hm<-openair::timeAverage(mydata=data %>%
                             dplyr::filter(PARAMETER=="SO2") %>%
                             dplyr::select(date=DATE_PST,
                                           value=RAW_VALUE),
                           avg.time="day",         #averaging period
                           statistic="max")

#calculate the annual 99th percentile of d1hm
d1hm_p99<-openair::timeAverage(d1hm,
                               avg.time="year",
                               statistic="percentile",
                               percentile=99) %>%
  select(value) # 40.0 

d1hm_p99<-d1hm %>%
  dplyr::summarise(p99=rcaaqs:::quantile2(value,
                                         probs=0.99,
                                         na.rm=TRUE,
                                         type="caaqs")) # 42.4

