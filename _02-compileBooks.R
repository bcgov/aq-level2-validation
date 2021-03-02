# compile annual data validation reports for all files in regionalPreppedData:

#THIS ISN'T WORKING, DATA ISN'T CHANGING BETWEEN DIFFERENT REGIONS/FILES.

root<-stringr::str_remove(dir("./regionalPreppedData"),
                          ".rds")

purrr::walk(root,
            
            function(r){
              
              bookdown::render_book("_AnnualDataValidation.Rmd", 
                                    "bookdown::gitbook",
                                    output_dir = stringr::str_c("_",r,sep = ""),
                                    params = list(
                                      preppedData = stringr::str_c("./regionalPreppedData/",
                                                                   r,
                                                                   ".rds", sep = ""),
                                      year = 2019
                                    )
                                    
              ) 
              
            }
            
            )





