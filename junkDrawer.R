test<-readr::read_rds("./preppedData/Merritt Nicola Ave MAML.rds")

min(test$DATE_PST)

head(test)

test %>% 
  dplyr::filter(PARAMETER=="PM25") %>%
  dplyr::arrange(DATE_PST) %>%
  dplyr::slice(1:24)
