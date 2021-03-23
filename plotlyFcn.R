# works on ftp data. created list of plotly objects. filters by parameter provided, 
# loops though each STATION_NAME in data and creates a plotly object coloured by instrument.

# parameter<-"PM25"

plotlyFcn<-function(data, # hourly data
                    allData, # hourly and daily data in one tidy tibble
                    parameter){

  library(tidyverse)
  
  purrr::map((data %>%
                dplyr::filter(PARAMETER %in% parameter) %>%
                dplyr::distinct(STATION_NAME) %>%
                dplyr::arrange(STATION_NAME))$STATION_NAME,
             
             function(station){
               # FOR TESTING #
               # station<-(data %>%
               #             filter(PARAMETER %in% parameter) %>%
               #             distinct(STATION_NAME) %>%
               #             arrange(STATION_NAME))$STATION_NAME
               
               # station<-"Golden Helipad"
               
               # END TESTING
               
               plotData<-allData %>%
                 dplyr::filter(PARAMETER %in% parameter &
                          STATION_NAME %in% station) %>% 
                 dplyr::mutate(TIME_AVG=factor(TIME_AVG,
                                               levels = c("Daily",
                                                          "Hourly")))
               
               
               p<- ggplot2::ggplot(plotData,
                               aes(x=DATE_PST,
                                   y=ROUNDED_VALUE,
                                   linetype=TIME_AVG,
                                   color=INSTRUMENT))+
                 geom_line() +
                 
                 labs(title=paste(station,
                                  "Hourly and Daily",
                                  parameter),
                      x="",
                      y="Concentration") +
                        
                     scale_color_viridis_d() +
                 
                 scale_linetype_manual(values=c("solid",
                                                "dotted")) +
                 
                 theme_bw()
                   
                   
               
               
                 plotly::ggplotly(p)
                 
               
             }#STATION LOOP
             
             )#map 

}

# plotlyFcn(data,
#           "PM25"
#           )
