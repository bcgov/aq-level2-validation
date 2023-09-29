# Skookumchuk Farstad Way

data %>%
  dplyr::mutate(ROUNDED_VALUE=round(RAW_VALUE,1)) %>%
  dplyr::filter(PARAMETER=="TRS" &
                  ROUNDED_VALUE>5) %>% utils::View()
