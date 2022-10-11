# compile annual data validation reports for each file in preppedDAta:

root<-stringr::str_remove(dir("./preppedData"),
                          ".rds")

yearToValidate<-2021

purrr::walk(root[1:length(root)],
            
            function(r){
              
              # testing
              
              (r<-root[1])
              
              # End testing
              
             
             bookdown::render_book(".", # this will compile all the rmd files listed in _bookdown.yml
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





