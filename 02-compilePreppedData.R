# compile annual data validation reports for all files in regionalPreppedData:

purrr::walk(stringr::str_remove(dir("./regionalPreppedData"),
                                ".rds"),
            
            function(root){
              
              rmarkdown::render(
                "AnnualDataValidation.Rmd",
                params = list(
                  preppedData = stringr::str_c("./regionalPreppedData/",
                                               root,
                                               ".rds", sep = ""),
                  year = 2019
                ),
                output_file = stringr::str_c("./reports/AnnualDataValidation_",
                                             root,
                                             ".html",
                                             sep="")
              ) 
              
            }
            
            )


