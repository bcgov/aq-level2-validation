# _04-prev-years-wind-checks.R

source("_00-setup.R")

#### check - campbell loggers ####
prev_yrs_wind<-readr::read_rds("prev-yrs-wind.rds")
# issue: when datalogger is switched to campbell ""_60|_MET_60| MET_60" is appended to STATION_NAME_FULL. we want same id's for same station
#fix: strip "_60|_MET_60| MET_60" from station names

# testing to confirm regex and stringr package work as intended
prev_yrs_wind |>
  dplyr::distinct(STATION_NAME,
                  STATION_NAME_FULL) |>
  dplyr::filter(stringr::str_detect(STATION_NAME_FULL,
                                    "_60|_MET_60| MET_60")) |>
  dplyr::mutate(STATION_NAME_FULL=stringr::str_remove(STATION_NAME_FULL,
                                                      "_60|_MET_60| MET_60")
                
  ) |>
  dplyr::distinct(STATION_NAME,
                  STATION_NAME_FULL)


# change the data in prev_yrs_wind
prev_yrs_wind %<>%
  dplyr::mutate(STATION_NAME_FULL=stringr::str_remove(STATION_NAME_FULL,
                                                      "_60|_MET_60| MET_60")
  )

# have a look
prev_yrs_wind |> 
  dplyr::distinct(STATION_NAME,
                  STATION_NAME_FULL)

#### checks - NA checks ####
# check if there are NA's for STATION_NAME_FULL and for VALIDATION_STATUS
prev_yrs_wind |>
  dplyr::filter(is.na(STATION_NAME_FULL) & is.na(VALIDATION_STATUS)) #0 rows

# if it's the same number as is.na(STATION_NAME_FULL) | is.na(VALIDATION_STATUS),
# then it's always both of them that are NA
prev_yrs_wind |>
  dplyr::filter(is.na(STATION_NAME_FULL) | is.na(VALIDATION_STATUS)) #0 rows
# the above shows that STATION_NAME_FULL is always NA when VALIDATION_STATUS is NA

# summary of when these NA's occur: (only run if above is non-zero)
prev_yrs_wind |>
  dplyr::filter(is.na(STATION_NAME_FULL) & is.na(VALIDATION_STATUS)) %>%
  dplyr::group_by(year=lubridate::year(DATE_PST),
                  STATION_NAME_FULL,
                  STATION_NAME,
                  PARAMETER) %>%
  dplyr::summarise(`# NA's`=n()) 

# filter out these NA's (only run if above is non-zero)
prev_yrs_wind %<>%
  dplyr::filter(!(is.na(STATION_NAME_FULL) & is.na(VALIDATION_STATUS)))

#### check - validation status ####
# years prev_yrs_wind should only have 
# validation status =  Level 2. check this

#this summarises data that has more than one validation status in a single year
prev_yrs_wind |>
  dplyr::group_by(YEAR=lubridate::year(DATE_PST),
                  STATION_NAME_FULL,
                  STATION_NAME,
                  PARAMETER,
                  VALIDATION_STATUS,
                  INSTRUMENT) |>
  dplyr::summarise(n=n()) |> 
  dplyr::ungroup() |>
  dplyr::group_by(YEAR,
                  STATION_NAME_FULL,
                  STATION_NAME,
                  PARAMETER,
                  INSTRUMENT) |>
  dplyr::filter(n()>1) %>% utils::View(.) # shows that many stations have 1 hour of level 2 validation status and the rest are level 0

#easier to do this graphically:
STATION_NAME_FULL<-prev_yrs_wind %>% dplyr::distinct(STATION_NAME_FULL) %>% dplyr::pull(STATION_NAME_FULL)

plotList<-purrr::map(STATION_NAME_FULL, 
                     function(x) {
                       
                       ggplot2::ggplot(
                         prev_yrs_wind %>%
                           # dplyr::mutate(year=lubridate::year(DATE_PST)) %>%
                           dplyr::filter(STATION_NAME_FULL == x),
                         ggplot2::aes(
                           x = DATE_PST,
                           y = VALIDATION_STATUS,
                           color = VALIDATION_STATUS,
                           shape = INSTRUMENT
                         )
                       ) +
                         ggplot2::geom_point(alpha = 0.6) +
                         ggplot2::facet_grid(rows=vars(PARAMETER)) +
                         ggplot2::theme_bw() +
                         ggplot2::labs(title = x) +
                         ggplot2::scale_color_viridis_d()
                       
                     })

pdf(file="prev-yrs-wind-validation-status.pdf")

purrr::walk(plotList,print)

dev.off()

# band-aid fix - make all data prevYrWind have "Level 2" VALIDATION_STATUS
prev_yrs_wind %<>%
  dplyr::mutate(VALIDATION_STATUS="Level 2")

#### check - find duplicates ####
duplicates <- prev_yrs_wind |>
  dplyr::group_by(DATE_PST,
                  STATION_NAME_FULL,
                  STATION_NAME,
                  PARAMETER,
                  VALIDATION_STATUS,
                  INSTRUMENT) |> 
  dplyr::summarise(n = dplyr::n(),
                   .groups = "drop") |>
  dplyr::filter(n > 1L) |>
  dplyr::left_join(
    x=_,
    prev_yrs_wind,
    by = c(
      "DATE_PST",
      "STATION_NAME_FULL",
      "STATION_NAME",
      "PARAMETER",
      "VALIDATION_STATUS",
      "INSTRUMENT"
    )
  ) |>
  
  dplyr::arrange(DATE_PST,
                 STATION_NAME,
                 STATION_NAME_FULL,
                 PARAMETER) |>
  dplyr::select(
    DATE_PST,
    STATION_NAME,
    STATION_NAME_FULL,
    PARAMETER,
    INSTRUMENT,
    RAW_VALUE,
    everything())

# no duplicates!

#### check - fix duplicates ####
# (not applicable if above didn't identify any duplicates
# DESCRIPTION OF THE ISSUE: there should be a single data set of wind speed and direction for each unique station name, but sometimes wind speed data get split between, for example STATION_NAME AND STATION_NAME_60, or between two different but arbitrary INSTRUMENT fields. CAUSES ISSUES WHEN GENERATING WIND ROSES.

# have a closer look at station_name_full
summary_stnNameFull<-prevYrWind %>%
  dplyr::group_by(STATION_NAME,
                  STATION_NAME_FULL,
                  PARAMETER,INSTRUMENT) %>%
  dplyr::summarise(start=min(DATE_PST),
                   end=max(DATE_PST),
                   n=n(),
                   `values`=sum(!is.na(RAW_VALUE)),
                   `NAs`=sum(is.na(RAW_VALUE)),
                   `allNAs`=n()==sum(is.na(RAW_VALUE)))

utils::View(summary_stnNameFull)

# find combo's that are all NA's and remove them from prevYrWind:
prevYrWind<-summary_stnNameFull %>%
  dplyr::filter(allNAs==TRUE) %>% #utils::View(.)
  dplyr::select(STATION_NAME,
                STATION_NAME_FULL,
                PARAMETER,
                INSTRUMENT) %>%
  dplyr::anti_join(
    prevYrWind,
    .,
    by=c("STATION_NAME",
         "STATION_NAME_FULL",
         "PARAMETER",
         "INSTRUMENT")
  )

#read windDuplicates
windDuplicates<-readr::read_rds("wind-duplicates.rds")

windDuplicates<-prevYrWind %>%
  
  dplyr::select(DATE_PST,
                STATION_NAME,
                PARAMETER,
                INSTRUMENT,
                RAW_VALUE) %>%
  
  dplyr::group_by(DATE_PST, 
                  STATION_NAME, 
                  PARAMETER,
                  INSTRUMENT) %>%
  
  dplyr::summarise(n = dplyr::n(), 
                   .groups = "drop") %>%
  
  dplyr::filter(n > 1L) %>%
  
  dplyr::left_join(.,
                   prevYrWind,
                   by=c("DATE_PST",
                        "STATION_NAME", 
                        "PARAMETER",
                        "INSTRUMENT")) %>%
  
  dplyr::select(DATE_PST,
                STATION_NAME,
                STATION_NAME_FULL,
                PARAMETER,
                INSTRUMENT,
                RAW_VALUE) %>%
  
  dplyr::arrange(DATE_PST,
                 STATION_NAME,
                 PARAMETER,
                 INSTRUMENT,
                 STATION_NAME_FULL) %>%
  
  dplyr::group_by(DATE_PST,
                  STATION_NAME,
                  PARAMETER,
                  INSTRUMENT) %>%
  
  dplyr::mutate(result=dplyr::case_when(
    
    sum(is.na(RAW_VALUE))==0 ~ "2 values",
    
    sum(is.na(RAW_VALUE))==1 ~ "1 value, 1 NA",
    
    sum(is.na(RAW_VALUE))==2 ~ "2 NAs"
    
  )) %>%
  
  dplyr::ungroup(.)

#preview each case
utils::View(windDuplicates %>%
              dplyr::group_by(STATION_NAME,
                              result)%>%
              dplyr::slice(1:2))


#summarise distinct combinations
windDuplicates %>%
  dplyr::distinct(STATION_NAME,
                  STATION_NAME_FULL,
                  PARAMETER,
                  INSTRUMENT,
                  result) %>% utils::View(.)

#save windDuplicates
readr::write_rds(windDuplicates,
                 "wind-duplicates.rds")

# fix duplicates
#1. fix duplicates (two cases: 1 value, 1 NA or 2 NAs)
#2. replace data in prevYrWind with fixed data
# Note: whatever is done for Duncan (Cairnsmore and College Street) will have to be repeated in data

windFixes<- windDuplicates %>%
  
  # dplyr::slice(1:2) %>%
  
  dplyr::group_by(DATE_PST,
                  STATION_NAME,
                  PARAMETER,
                  INSTRUMENT,
                  result) %>%
  # this will pull desired value for both cases (the value when there is one, and an NA otherwise)
  dplyr::arrange(RAW_VALUE) %>%
  
  dplyr::slice(1) %>%
  
  dplyr::ungroup(.)

#save windDuplicates
readr::write_rds(windFixes,
                 "wind-fixes.rds")                     

# GOT TO HERE - now need to replace data in prevYrWind with windFixes.
# 1. filter out windDuplicates using antijoin
# 2. bind_rows with windFixes

prevYrWind<-dplyr::anti_join(prevYrWind,
                             windDuplicates,
                             by=c("DATE_PST",
                                  "STATION_NAME",
                                  "STATION_NAME_FULL",
                                  "PARAMETER",
                                  "INSTRUMENT",
                                  "RAW_VALUE")) %>%
  
  dplyr::select(DATE_PST,
                STATION_NAME,
                STATION_NAME_FULL,
                PARAMETER,
                INSTRUMENT,
                RAW_VALUE) %>%
  
  dplyr::bind_rows(.,
                   windFixes %>%
                     dplyr::select(-result))

#check whether there are any duplicates remaining in prevYrWind by rerunning code above that generates windDuplicates -> result was tibble with 0 rows i.e. no more duplicates!

#### save prev_yrs_wind ####
readr::write_rds(prev_yrs_wind,
                 "prev-yrs-wind-checked.rds")
