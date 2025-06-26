# _06-compile-reports.R

source("_00-setup.R")

# compile annual data validation reports for each file in preppedDAta:

root<-stringr::str_remove(dir("./prepped-data"),
                        ".rds") 

utils::View(root)

# stations that won't compile:

#indices in roots that won't compile: 
bugs<-c(112) #warfield elementary

root_no_bugs<-root[!(6:length(root) %in% bugs)]

utils::View(root_no_bugs)

purrr::walk(#root[1:length(root)],
  #compile reports for stations without bugs
  # root_no_bugs[1:length(root_no_bugs)], #%>% utils::View(.),
  
  #compile reports for specific stations:
  root[52],
            function(r){
              
              # testing
              
              # (r<-root[1])
              
              # End testing
              print(paste0("Compiling for ",
                           r,
                           " ..."))
             
             bookdown::render_book(".",# this will compile all the rmd files listed in _bookdown.yml
                                    "bookdown::gitbook",
                                    output_dir = stringr::str_c("_",r,sep = ""),
                                    params = list(
                                      prepped_data = stringr::str_c("./prepped-data/",
                                                                   r,
                                                                   ".rds", 
                                                                   collapse = ""),
                                      year = year_to_validate
                                    )
                
                                    
              ) 
              
            }
            
            )

# need to delete all files 404.html - don't know why these are being generated
files_to_delete <- list.files(
  #path = ".",
  path = "R:\\WANSHARE\\EPD\\EPD_SHARED\\MAS\\Air Quality Section\\aq-level2-validation",
  pattern = "404",
  recursive = TRUE,
  full.names = TRUE
)

file.remove(files_to_delete)

# list files in the reports
files_to_copy <- dir(
  file.path(".",
            stringr::str_c("_",
                           root_no_bugs)),
  all.files = TRUE,
  full.names = TRUE,
  recursive = TRUE
)

#copy files to shared drive
destination<-"R:\\WANSHARE\\EPD\\EPD_SHARED\\MAS\\Air Quality Section\\aq-level2-validation"

file.copy(from = files_to_copy,
          to=destination,
          overwrite = TRUE,
          recursive = TRUE)