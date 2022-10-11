station<-"Trail Butler Park"

stationData<-windData %>%
  dplyr::filter(STATION_NAME %in% station)

pdf(file = "./_!2020 Kootenay Data Validation/2009-2020_WindRoses_TrailButlerPark.pdf",onefile = TRUE)


purrr::walk(lubridate::year(stationData$date) %>% unique %>% sort,
           
           function(year){
             
             # TESTING
             # 
             # year<-2020
             # 
             # # END TESTING
             
             windRoseData <- stationData %>%
               dplyr::filter(lubridate::year(date) %in% year)
             
             if(nrow(windRoseData)==0) {
               "There is no (paired) wind data for this year."} else {
                 
                 #number of calm hours
                 ncalm <- windRoseData %>%
                   dplyr::filter(ws < 0.5) %>%
                   dplyr::summarise(calms = n())
                 
                 #total number of valid ws hours
                 ntotal <- windRoseData %>%
                   dplyr::filter(!is.na(ws)) %>%
                   dplyr::summarise(total = n())
                 
                 #percentage calm (ws<0.5 m/s)
                 pcalm <- round(100 * ncalm / ntotal,
                                digits = 2)
                 
                 #filter out calm windRoseData
                 windRoseData %<>%
                   dplyr::filter(ws >= 0.5)
                 
                 saveRDS(tibble::tibble(
                   Year=year,
                   Station=station,
                   `% Calm`=pcalm$calms
                 ),
                 file = stringr::str_c("./_!2020 Kootenay Data Validation/percentCalm_",
                                       station,
                                       year,".rds"))
                 
                 #windRose
                 openair::windRose(
                   windRoseData,
                   annotate = FALSE,
                   breaks = c(0.5, 1.5, 3.3, 5.5, 7.9, 10.7, 13.8, 17.1),
                   #Beaufort scale with 0.5 as lowest cut point.
                   sub = paste("Calms (=<0.5m/s)=", pcalm, "%"),
                   key.position = "right",
                   main = stringr::str_c(year," Wind Rose at ",station),
                   angle = 360 / 16,
                   #16 spokes
                   cols = "jet",
                   paddle = FALSE,
                   type="year"
                 )
                 
                } # end else
             
             
             
           } #end function(station)
           
) # map

dev.off()



pc<-purrr::map_dfr(stringr::str_subset(dir("./_!2020 Kootenay Data Validation"),
                                       "percentCalm_Trail Butler Park"),
                   
                   function(pc){
                     
                     readr::read_rds(file.path("./_!2020 Kootenay Data Validation",pc)) 
                     
                     
                     
                   }
)


ggplot2::ggplot(pc,
                aes(x=Year,
                    y=`% Calm`)) +
  
  geom_line()+
  geom_point() +
  theme_bw() +
  labs(title="Trail Butler Park")

ggsave("./_!2020 Kootenay Data Validation/percentCalmsTrailButlerPark.png",
       width=8,
       height=5,
       units="in")
