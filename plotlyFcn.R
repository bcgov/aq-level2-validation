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
                                               levels = c("Hourly",
                                               "24-HR Running Ave",
                                                 "Daily")))
               
               
               p<- ggplot2::ggplot(plotData,
                               aes(x=DATE_PST,
                                   y=ROUNDED_VALUE,
                                   color=TIME_AVG,
                                   linetype=INSTRUMENT))+
                 geom_line(alpha=1.0) +
                 
                 # geom_point(alpha=0.8,
                 #            pch=21)+
                 
                 labs(title=paste(station,
                                  "Hourly and Daily",
                                  parameter),
                      x="",
                      y=parameter) +
                 
                 scale_color_brewer(palette = "Set1")+
                        
                     # scale_color_viridis_d(option = "D") +
                 
                 # scale_linetype_manual(values=c("solid",
                 #                                "dotted")) +
                 
                 scale_y_continuous(breaks = seq(0,
                                                 max(plotData$ROUNDED_VALUE,
                                                     na.rm = TRUE)*1.05,
                                                 20)#,
                                    # minor_breaks = seq(10,
                                    #                    max(plotData$ROUNDED_VALUE,
                                    #                        na.rm = TRUE)*1.05,
                                    #                    20)
                                    )+
                 
                 scale_x_datetime(date_breaks = "2 weeks",
                                  date_labels = "%b %d")+
                 
                 theme_bw()+
                 
                 theme(axis.text.x = element_text(angle = 90))
                   
                   
               
               
                 plotly::ggplotly(p)
                 
               
             }#STATION LOOP
             
             )#map 

}

# plotlyFcn(data,
#           "PM25"
#           )
