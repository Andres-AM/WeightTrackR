
source("libraries.R");

data <- read_csv2("Data/data_test.csv",show_col_types = FALSE) %>%
  group_by(n_week) %>%
  summarise(
    across(c(body_mass, fat_mass, lean_mass, fat_perc, lean_perc, prot_g, kcal),
           \(x) mean(x, na.rm = TRUE)
    ))

floor_week <- function(date = date,lim_lwr = lim_lwr ){
  
  date <- (as.double(date(date)) - as.double(lim_lwr)) %/% 7
  
  return(date)
  
}

lim_lwr <- date("2022-08-01")

dates <- stringr::str_split_1("2022-08-08;2023-08-01;2023-10-16",pattern = ";")

dates <- map_dbl(.x = dates,.f = floor_week, lim_lwr)

data1 <- data %>% 
  mutate(phase_finder = if_else(n_week %in% dates ,1,0), phase = cumsum(phase_finder)) %>% 
  group_by(phase) %>% 
  filter(n_week == max(n_week)  )

