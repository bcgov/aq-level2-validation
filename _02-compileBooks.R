# compile annual data validation reports for each file in preppedDAta:
root<-stringr::str_remove(dir("./preppedData"),
                        ".rds") 

utils::View(root)

yearToValidate<-2022

# need a list of pm10 stations in 2022 (to know which reports to re-run)
data<-envair::importBC_data(parameter_or_station = "pm10",
                            years = 2022,
                            use_openairformat = FALSE) %>%
  dplyr::filter(lubridate::year(DATE_PST)==2022) %>%
  tidyr::tibble(.)

stations<-data %>%
  dplyr::distinct(STATION_NAME)

stations %<>%
  dplyr::slice(-18) %>%
  dplyr::add_row(STATION_NAME="Kelowna KLO Road") %>%
  dplyr::arrange(STATION_NAME)

stations %<>%
  dplyr::slice(-28) %>%
  dplyr::add_row(STATION_NAME="Quesnel Johnston Avenue") %>%
  dplyr::arrange(STATION_NAME)

stations %<>%
  dplyr::slice(-29) %>%
  dplyr::add_row(STATION_NAME="Smithers Muheim Memorial") %>%
  dplyr::arrange(STATION_NAME)


purrr::walk(
  #compile reports for stations without bugs
  stations$STATION_NAME[32:length(stations$STATION_NAME)],
  
            function(r){
              
              # testing
              
              # (r<-root[1])
              
              # End testing
              
             
             bookdown::render_book(".",# this will compile all the rmd files listed in _bookdown.yml
                                    "bookdown::gitbook",
                                    output_dir = stringr::str_c("_",r,sep = ""),
                                    params = list(
                                      preppedData = stringr::str_c("./preppedData/",
                                                                   r,
                                                                   ".rds", 
                                                                   collapse = ""),
                                      year = yearToValidate
                                    )
                
                                    
              ) 
              
            }
            
            )

# need to delete all files 404.html - don't know why these are being generated
filesToDelete<-list.files(path = ".",
           pattern = "404",
           recursive = TRUE,
           full.names = TRUE)

file.remove(filesToDelete)

# list files in the reports
filesToCopy <- dir(
  file.path(".",
            stringr::str_c("_",
                           rootNoBugs)),
  all.files = TRUE,
  full.names = TRUE,
  recursive = TRUE
)

#copy files to shared drive
destination<-"R:\\WANSHARE\\EPD\\EPD_SHARED\\MAS\\Air Quality Section\\aq-level2-validation"

file.copy(from = filesToCopy,
          to=destination,
          overwrite = TRUE,
          recursive = TRUE)