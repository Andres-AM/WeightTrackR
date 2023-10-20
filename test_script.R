
source("libraries.R");source("FUN.R")

data <- read_csv2("Data/data_table_test.csv",show_col_types = FALSE) %>%
  group_by(n_week) %>%
  summarise(
    across(c(body_mass, fat_mass, lean_mass, fat_perc, lean_perc, prot_g, kcal),
           \(x) mean(x, na.rm = TRUE)
    ))

lim_lwr <- date("2022-08-08")

dates <- str_split_1("2022-08-08;2022-08-22;2023-08-14;2023-10-16",pattern = ";")   

dates_out <- c( map_dbl(.x = dates,.f = date_to_n_week, lim_lwr) , max(data$n_week));dates_out

n_week_to_date(n_week = dates_out, lim_lwr = lim_lwr)

data1 <- data %>% 
  mutate(
    phase_finder = if_else(n_week %in% dates_out,1,0), 
    phase = cumsum(phase_finder)
    ) %>% 
  group_by(phase) %>% 
  filter(n_week == max(n_week)) %>%
  ungroup() 


table_add_delta(data = data1,lim_lwr = lim_lwr)
