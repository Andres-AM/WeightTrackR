
t_interval <- as.double(upr_lim-lwr_lim)

dr_1 <- read_delim("Data/body_mass.txt", delim = ",", show_col_types = F)
dr_2 <- read_delim("Data/fat_perc.txt", delim = ",", show_col_types = F)
dr_3 <- read_delim("Data/lean.txt", delim = ",", show_col_types = F)
dr_4 <- read_delim("Data/calories.txt", delim = ",", show_col_types = F)  
dr_7 <- read_delim("Data/prot.txt", delim = ",", show_col_types = F)  

## Data import and merging 
dr <- reduce(list(dr_1,dr_2,dr_3),full_join, by = "Date") %>% 
  mutate( Date = date(Date), n_day = as.double(Date) - as.double(lwr_lim)) %>% 
  reduce(.x = list(.,dr_4,dr_7), .f = full_join, by = "Date") %>%
  
  ## Renaming variables for better clarity
  rename( body_mass = `Body mass(kg)`, kcal = `Energy consumed(kcal)`, lean_mass =`Lean body mass(kg)`,
          prot_g = `Protein(g)`,fat_perc = `Body fat percentage(%)`) %>% 
  
  ## Data cleaning and data manipulation 
  mutate( fat_perc = fat_perc*100, fat_mass = fat_perc/100 * body_mass,
          fat_perc = replace(fat_perc, Date >= date("2022-11-01") & Date <= date("2022-11-10"),NA),   ## bug correction from data set
          lean_mass = replace(lean_mass, Date >= date("2022-11-01") & Date <= date("2022-11-10"),NA), ## bug correction from data set
          lean_perc = (body_mass - (body_mass * (fat_perc/100)))/body_mass * 100,
          n_week =  n_day %/%7  ) %>% 
  
  ## Filtering missing values    
  filter(!(is.na(body_mass)) & Date > lwr_lim) %>% 
  select( Date,n_day,n_week,everything()) %>% 

## Summarizing by date 
  group_by(n_week,n_day) %>%
  summarise( across(c(body_mass, fat_mass, lean_mass, fat_perc, lean_perc,prot_g), 
                    mean, na.rm = TRUE),
             kcal = mean(kcal[kcal > 1600], na.rm = TRUE)) %>%
  group_by(n_week) %>%
  summarise(across(c(body_mass, fat_mass, lean_mass, fat_perc, lean_perc,prot_g,kcal),
                   mean, na.rm = TRUE))

## Predictive model for Body Mass and Fat Percentage
model_bodymass <- lm( body_mass ~  poly(x = n_week,degree = ord,raw = T), dr[dr$n_week > last(dr$n_week) - w_mod,])
model_fatperc  <- lm( fat_perc  ~  poly(x = n_week,degree = ord,raw = T), dr[dr$n_week > last(dr$n_week) - w_mod,])

model_data <- dr %>% 
  data_grid( n_week = seq(last(n_week)-w_mod,t_interval %/%7 )) %>%  
  add_predictions( model = model_bodymass , var = "bodymass_pred") %>% 
  add_predictions( model = model_fatperc , var = "fatperc_pred") 

## Summarizing by date 
df_clean <- dr %>% 
  full_join(model_data,by =  "n_week") %>% 
  rename(date = n_week) %>%
  select(date,body_mass,bodymass_pred,fat_perc, fatperc_pred,kcal,lean_perc, prot_g, fat_mass, lean_mass)
