# works on ftp data. created list of plotly objects. filters by parameter provided, 
# loops though each STATION_NAME in data and creates a plotly object coloured by instrument.

# parameter<-"PM25"

plotlyFcn<-function(data,parameter){

  library(tidyverse)
  
  purrr::map((data %>%
                filter(PARAMETER %in% parameter) %>%
                distinct(STATION_NAME) %>%
                arrange(STATION_NAME))$STATION_NAME,
             
             function(station){
               
               # station<-(data %>%
               #             filter(PARAMETER %in% parameter) %>%
               #             distinct(STATION_NAME) %>%
               #             arrange(STATION_NAME))$STATION_NAME
               
               station<-data %>%
                 filter(PARAMETER %in% parameter &
                          STATION_NAME %in% station)
               
               plotly::plot_ly(
                 data= station,
                 x=~DATE_PST,
                 y=~RAW_VALUE,
                 color=~INSTRUMENT,
                 colors = "Set1",
                 type = "scatter",
                 mode="lines+markers") %>%
                 plotly::layout(title = (station %>% distinct(STATION_NAME))$STATION_NAME,
                        xaxis=list(title=""),
                        yaxis=list(title=stringr::str_c("Hourly",parameter,sep = " ")),
                        showlegend=TRUE,
                        legend = list(x = 0.1, y = 0.9))
                 
               
             }#STATION LOOP
             
             )#map 

}

# plotlyFcn(data,
#           "PM25"
#           )
