# compile annual data validation reports for all files in regionalPreppedData:

root<-stringr::str_remove(dir("./regionalPreppedData"),
                          ".rds")

purrr::walk(root[1],
            
            function(root){
              
              bookdown::render_book("_AnnualDataValidation.Rmd", 
                                    "bookdown::gitbook",
                                    output_dir = stringr::str_c("_",root,sep = ""),
                                    params = list(
                                      preppedData = stringr::str_c("./regionalPreppedData/",
                                                                   root,
                                                                   ".rds", sep = ""),
                                      year = 2019
                                    )
                                    
              ) 
              
            }
            
            )





