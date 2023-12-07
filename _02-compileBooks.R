# compile annual data validation reports for each file in preppedDAta:

root<-stringr::str_remove(dir("./preppedData"),
                        ".rds") 

utils::View(root)

yearToValidate<-2022

#indices in roots that won't compile: 
bugs<-c(110, # Vanderhoof Courthouse
        114, # Warfield Haley Park
        117, # Willow Creek Mine
        118 # Willow Creek Compressor Station 2
        )

rootNoBugs<-root[!(1:length(root) %in% bugs)]

purrr::walk(
  #compile reports for stations without bugs
  # rootNoBugs[1:length(rootNoBugs)], #%>% utils::View(.),
  root[110],
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