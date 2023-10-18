
source("libraries.R");source("FUN.R")

data <- read_csv2("Data/data_test.csv",show_col_types = FALSE) %>%
  group_by(n_week) %>%
  summarise(
    across(c(body_mass, fat_mass, lean_mass, fat_perc, lean_perc, prot_g, kcal),
           \(x) mean(x, na.rm = TRUE)
    ))

lim_lwr <- date("2022-08-08")

date_to_n_week <- function(date = date,lim_lwr = lim_lwr ){
  
  date <- (as.double(date(date) - date(lim_lwr))) %/% 7
  
  return(date)
  
}

# n_week2 <- function(date = date,lim_lwr = lim_lwr ){
#   
#   date <- week(date) - week(lim_lwr)
#   
#   return(date)
#   
# }

n_week_to_date <- function(n_week = n_week,lim_lwr = lim_lwr ){
  
  date <- lim_lwr + weeks((n_week))
  
  return(date)
  
}

dates <- str_split_1("2022-08-08;2022-08-22;2023-08-14;2023-10-16",pattern = ";")   

dates_out <- map_dbl(.x = dates,.f = date_to_n_week, lim_lwr) ;dates_out

dates_out <- c(dates_out, max(data$n_week));dates_out

n_week_to_date(n_week = dates_out, lim_lwr = lim_lwr)

data1 <- data %>% 
  mutate(
    date = n_week_to_date(n_week,lim_lwr),
    phase_finder = if_else(n_week %in% dates_out,1,0), 
    phase = cumsum(phase_finder)
    ) %>% 
  group_by(phase) %>% 
  filter(n_week == max(n_week)) %>%
  ungroup() %>% 
  mutate(delta_body_mass = body_mass - lag(body_mass, default = first(body_mass)),
         delta_lean_mass = lean_mass - lag(lean_mass, default = first(lean_mass)),
         delta_fat_mass = fat_mass - lag(fat_mass, default = first(fat_mass)))  %>%
  # Select specific columns and arrange them in descending order of date
  select(date, phase,body_mass, lean_mass, fat_mass, fat_perc, delta_body_mass, delta_lean_mass, delta_fat_mass) %>% 
  arrange(desc(date))


hist()
