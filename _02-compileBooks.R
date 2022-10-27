# compile annual data validation reports for each file in preppedDAta:

root<-stringr::str_remove(dir("./preppedData"),
                          ".rds") 

utils::View(root)


yearToValidate<-2021

# # # ISSUES WITH THESE STATIONS IN ROOT # # # 

# duplicates identified at: (these stations are outstanding - need to compile reports)
# 17 colwood city hall (wspd_sclr, wdir_vect)
# 76 prince george exploration place (humidity, wdir_vect, wspd_sclr)
# 108 vanderhoof courthouse (wdir_vect, wspd_sclr)

#21 crofton elementary - report compiles but lacking northerly winds
#22 crofton substation - no wind speed data
#23 duncan college street - duplicates in STATION_NAME, PARAMETER, DATE_PST (Duncan college street_60 has instrument=UNSPECIFIED and all the data is "")
#83 prince rupert roosevelt park school - compiled but no wdir_vect

#104 valemount - issue with wind data metadata


# new root with flagged stations removed
root<-root[which(!root %in%  c("Valemount",
                 "Vanderhoof Courthouse"))]

utils::View(root)


purrr::walk("Vanderhoof Courthouse",#root[109],#:length(root)],
  
            function(r){
              
              # testing
              
              # (r<-root[1])
              
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





