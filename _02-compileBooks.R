# compile annual data validation reports for each file in preppedDAta:

root<-stringr::str_remove(dir("./preppedData"),
                          ".rds")

# golden helipad
root<-root[32]

purrr::walk(root,
            
            function(r){
              
              # testing
              
              # (r<-root[1])
              
              # End testing
              
              bookdown::render_book("_AnnualDataValidation.Rmd", 
                                    "bookdown::gitbook",
                                    output_dir = stringr::str_c("_",r,sep = ""),
                                    params = list(
                                      preppedData = stringr::str_c("./preppedData/",
                                                                   r,
                                                                   ".rds", 
                                                                   collapse = ""),
                                      year = 2020
                                    )
                
                                    
              ) 
              
            }
            
            )





