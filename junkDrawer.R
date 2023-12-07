test<-readr::read_rds("./preppedData/Port Edward Sunset Drive.rds")

min(test$DATE_PST)

head(test)

test %>% 
  dplyr::filter(PARAMETER=="PM25") %>%
  dplyr::arrange(DATE_PST) %>%
  dplyr::slice(1:24)

test %>%
  dplyr::group_by(PARAMETER,INSTRUMENT) %>%
  dplyr::summarise(sum(!is.na(RAW_VALUE)),
                   sum(is.na(RAW_VALUE)),
                   n())
# BUNCH OF INSTRUMENT=NA'S

portEd<-envair::importBC_data(parameter_or_station = "Port Edward Sunset Drive",
                              years=2022,
                              use_openairformat = FALSE)

portEd %>%
  dplyr::group_by(PARAMETER,INSTRUMENT) %>%
  dplyr::summarise(sum(!is.na(RAW_VALUE)),
                   sum(is.na(RAW_VALUE)),
                   n())