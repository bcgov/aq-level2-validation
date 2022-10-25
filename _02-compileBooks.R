# compile annual data validation reports for each file in preppedDAta:

root<-stringr::str_remove(dir("./preppedData"),
                          ".rds") 

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

# utils::View(root)

# root<-root[which(!root %in%  c("Colwood City Hall", #this and stations below awaiting fix to ftp from ecms
#                  "Prince George Exploration Place",
#                  "Vanderhoof Courthouse"))]

purrr::walk(root[85:103],
  # 
  # root[which(!root %in%  c("Colwood City Hall", #this and stations below awaiting fix to ftp from ecms
  #                                   "Prince George Exploration Place",
  #                                   "Valemount",
  #                                   "Vanderhoof Courthouse"))],
            
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





