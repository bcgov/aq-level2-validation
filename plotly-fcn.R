# works on ftp data. created list of plotly objects. filters by parameter provided, 
# loops though each STATION_NAME in data and creates a plotly object coloured by instrument.

# parameter<-"O3"

plotly_fcn<-function(data, # hourly data
                    all_data, # hourly and daily data in one tidy tibble
                    parameter){

  library(tidyverse)
  
  #loop through each STATION_NAME (should only be one)
  purrr::map((data |>
                dplyr::filter(PARAMETER %in% parameter) |>
                dplyr::distinct(STATION_NAME) |>
                dplyr::arrange(STATION_NAME))$STATION_NAME,
             
             function(station){
               # FOR TESTING #
               # station<-(data |>
               #             filter(PARAMETER %in% parameter) |>
               #             distinct(STATION_NAME) |>
               #             arrange(STATION_NAME))$STATION_NAME

               # station<-"Kitimat Riverlodge"
               # parameter<-"PM25"
               
               # END TESTING
               
               
               if(parameter!="O3"){
               #### for all parameters except ozone:####
               plot_data<-all_data |>
                 dplyr::filter(PARAMETER %in% parameter &
                          STATION_NAME %in% station) |> 
                 dplyr::mutate(TIME_AVG=factor(TIME_AVG,
                                               levels = c("Hourly",
                                               "24-HR Running Ave",
                                                 "Daily")))
               
               p<- ggplot2::ggplot(plot_data,
                                   ggplot2::aes(x=DATE_PST,
                                                y=RAW_VALUE,
                                                color=TIME_AVG,
                                                linetype=INSTRUMENT))+
                 ggplot2::geom_line(alpha=1.0) +
                 
                 ggplot2::geom_point(alpha=0.8,
                                     
                                     pch=21)+
                 
                 ggplot2::labs(title=paste(station,
                                           "Hourly and Daily",
                                           parameter),
                               x="",
                               y=parameter) +
                 
                 ggplot2::scale_color_brewer(palette = "Set1")+
                 
                 # scale_color_viridis_d(option = "D") +
                 
                 # scale_linetype_manual(values=c("solid",
                 #                                "dotted")) +
                 
                 ggplot2::scale_y_continuous(breaks = seq(0,
                                                          max(plot_data$RAW_VALUE,
                                                              na.rm = TRUE)*1.05,
                                                          20)#,
                                             # minor_breaks = seq(10,
                                             #                    max(plot_data$RAW_VALUE,
                                             #                        na.rm = TRUE)*1.05,
                                             #                    20)
                 )+
                 
                 ggplot2::scale_x_datetime(date_breaks = "2 weeks",
                                           date_labels = "%b %d")+
                 
                 ggplot2::theme_bw()+
                 
                 ggplot2::theme(axis.text.x = element_text(angle = 90))
               
               } else{
               
               #### for ozone (8-hour running average): ####
               plot_data<-data |>
                 dplyr::filter(PARAMETER %in% parameter &
                                 STATION_NAME %in% station) |>
                 # rename to date for openair
                 dplyr::rename(date=DATE_PST) %>%
                 dplyr::group_by(STATION_NAME,PARAMETER,INSTRUMENT) %>%
                 dplyr::group_modify(~ openair::rollingMean(.x,
                                                            pollutant="RAW_VALUE",
                                                            width=8,              #8 hour rolling mean
                                                            new.name="ROLL_MEAN_8HR", #column heading
                                                            data.thresh=75,       #>=18 hr. for rolling mean
                                                            align="right")
                 ) %>%
                 ungroup %>%
                 dplyr::rename(DATE_PST=date)
               
               
               p<- ggplot2::ggplot(plot_data,
                               ggplot2::aes(x=DATE_PST,
                                   y=ROLL_MEAN_8HR,
                                   # color=TIME_AVG,
                                   linetype=INSTRUMENT))+
                 ggplot2::geom_line(alpha=1.0,
                                    color="orange") +
                 
                 ggplot2::geom_point(alpha=0.8,
                            pch=21,
                            color="orange")+
                 
                 ggplot2::labs(title=paste(station,
                                  "Rolling 8-hour",
                                  parameter),
                      x="",
                      y=parameter) +
                 
                 ggplot2::scale_color_brewer(palette = "Set1")+
                        
                     # scale_color_viridis_d(option = "D") +
                 
                 # scale_linetype_manual(values=c("solid",
                 #                                "dotted")) +
                 
                 ggplot2::scale_y_continuous(breaks = seq(0,
                                                 max(plot_data$ROLL_MEAN_8HR,
                                                     na.rm = TRUE)*1.05,
                                                 20)#,
                                    # minor_breaks = seq(10,
                                    #                    max(plot_data$ROLL_MEAN_8HR,
                                    #                        na.rm = TRUE)*1.05,
                                    #                    20)
                                    )+
                 
                 ggplot2::scale_x_datetime(date_breaks = "2 weeks",
                                  date_labels = "%b %d")+
                 
                 ggplot2::theme_bw()+
                 
                 ggplot2::theme(axis.text.x = element_text(angle = 90))
                 
               }
                 
                 plotly::ggplotly(p) |>
                   plotly::layout(legend = list(orientation = 'h',y = -0.5))

               
             }#STATION LOOP
             
             )#map 

}

# plotly_fcn(data=data,
#            all_data=all_data,
#           parameter="PM25"
#           )
