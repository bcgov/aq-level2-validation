# compile annual data validation reports for each file in preppedDAta:

root<-stringr::str_remove(dir("./preppedData"),
                        ".rds") 

utils::View(root)

yearToValidate<-2022

#indices in roots that won't compile: 
bugs<-c(54,#Langdale Elementary
        57, #Merritt Nicola Ave MAML
        73, #Port Edward Sunset Drive
        90, # Quesnel Kinchant St MAML
        111, # Vancouver International Airport #2
        113, # Vanderhoof Courthouse
        117, # Warfield Haley Park
        119, # Williams Lake Colunmeetza School
        120, # Willow Creek Mine
        121 # Willow Creek Compressor Station 2
        )

purrr::walk(
  #compile reports for stations without bugs
  root[!(1:length(root) %in% bugs)] %>% utils::View(.),
  
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

# copy compiled reports to shared drive
filesToCopy<-list.files(path = ".",
           pattern="sas-statistics.html",
           recursive=TRUE,
           full.name=TRUE)

(filesToCopy<-filesToCopy[which(!(filesToCopy %in% c("./sas-statistics.html",
                                                     "./_book-output/sas-statistics.html")
                                  )
                                )
                          ]
  )

utils::View(filesToCopy)

(destinations<-file.path(
  "R:\\WANSHARE\\EPD\\EPD_SHARED\\MAS\\Air Quality Section\\AnnualDataValidation",
  stringr::str_remove(filesToCopy,"./")))

file.copy(filesToCopy[1],
          destinations[1],
          overwrite = TRUE)


purrr::map(1:length(filesToCopy),
  
           function(index){
                      
           file.copy(filesToCopy[index],
                     destinations[index],
                     overwrite = TRUE)
           
           })


