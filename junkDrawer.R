# from index


```{r dailyData}
(day<-data %>%
    # rename to date for openair
    dplyr::rename(date=DATE_PST) %>%
    dplyr::group_by(STATION_NAME,PARAMETER,INSTRUMENT) %>%
    dplyr::group_modify(~ openair::timeAverage(.x,
                                               pollutant="PARAMETER",
                                               avg.time = "day",
                                               data.thresh = 75)
    ) %>%
    ungroup %>%
    dplyr::rename(DATE_PST=date))

```


```{r hourlyAndDaily}

# combine hourly and day data sets in one tidy tibble
allData<-data %>%
  dplyr::select(names(day)) %>%
  dplyr::mutate(TIME_AVG = "Hourly") %>%
  dplyr::bind_rows(.,
                   day %>%
                     dplyr::mutate(TIME_AVG = "Daily"))

# allData %>%
#   dplyr::filter(TIME_AVG=="Hourly")
# 
# allData %>%
#   dplyr::filter(TIME_AVG=="Daily")
```

# hourly plots

plotlyFcn(data,
          allData,
          parameter=toupper("pm25"))