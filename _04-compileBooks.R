# compile annual data validation reports for each file in preppedDAta:

root<-stringr::str_remove(dir("./preppedData"),
                        ".rds") 

utils::View(root)

yearToValidate<-2023

#indices in roots that won't compile: 
bugs<-c(20, # courtenay elementary school -> issue with wind rose
        24, # duncan college street issue with wind rose
        )

rootNoBugs<-root[!(1:length(root) %in% bugs)]

utils::View(rootNoBugs)

purrr::walk(#root[3:length(root)],
  #compile reports for stations without bugs
  rootNoBugs[1:length(rootNoBugs)], #%>% utils::View(.),
  
  #compile reports for specific stations: butler park, columbia gardens airport, birchbank,
  # warfield elementary and warfield haley park
  # root[47],
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