
t_interval <- as.double(upr_lim-lwr_lim)

dr_1 <- read_delim("Data/body_mass.txt", delim = ",", show_col_types = F)
dr_2 <- read_delim("Data/fat_perc.txt", delim = ",", show_col_types = F)
dr_3 <- read_delim("Data/lean.txt", delim = ",", show_col_types = F)
dr_4 <- read_delim("Data/calories.txt", delim = ",", show_col_types = F)  
dr_7 <- read_delim("Data/prot.txt", delim = ",", show_col_types = F)  

## Data import and merging 
dr <- reduce(list(dr_1,dr_2,dr_3),full_join, by = "Date") %>% 
  mutate( Date = date(Date), date = as.double(Date) - as.double(lwr_lim)) %>% 
  reduce(.x = list(.,dr_4,dr_7), .f = full_join, by = "Date") %>%
  rename( body_mass = `Body mass(kg)`, kcal = `Energy consumed(kcal)`, lean_mass =`Lean body mass(kg)`,
          prot_g = `Protein(g)`,fat_perc = `Body fat percentage(%)`) %>% 
  mutate( fat_perc = fat_perc*100, fat_mass = fat_perc/100 * body_mass,
          fat_perc = replace(fat_perc, Date >= date("2022-11-01") & Date <= date("2022-11-10"),NA),   ## Correction bug de Eufy App
          lean_mass = replace(lean_mass, Date >= date("2022-11-01") & Date <= date("2022-11-10"),NA), ## Correction bug de Eufy App
          lean_perc = (body_mass - (body_mass * (fat_perc/100)))/body_mass * 100,
          week = paste0(year(Date),"_",week(Date))) %>% 
  filter(!(is.na(body_mass)) & Date > lwr_lim) %>% 
  select( date,everything())

## Predictive model for Body Mass and Fat Percentage
model_bodymass <- lm( body_mass ~  poly(x = date,degree = ord,raw = T), dr[dr$Date > last(dr$Date) - weeks(5),])
model_fatperc  <- lm( fat_perc  ~  poly(x = date,degree = ord,raw = T), dr[dr$Date > last(dr$Date) - weeks(5),])

model_data <- dr %>% 
  data_grid( date = seq(first(dr$date),t_interval)) %>%  
  add_predictions( model = model_bodymass , var = "bodymass_pred") %>% 
  add_predictions( model = model_fatperc , var = "fatperc_pred") 

## Summarizing by date 
df <- dr %>% 
  group_by(date) %>%
  summarise( body_mass = mean(body_mass,na.rm = T), 
             fat_mass = mean(fat_mass,na.rm = T), 
             lean_mass = mean(lean_mass, na.rm = T), 
             fat_perc = mean(fat_perc,na.rm = T), 
             lean_perc = mean(lean_perc,na.rm = T), 
             kcal = mean( kcal,na.rm = T), 
             prot_g = mean(prot_g, na.rm = T),
             week , Date
  ) %>% 
  full_join(model_data,by = "date") %>% 
  mutate( n_week =  date %/%7) %>% 
  group_by(n_week)  %>%
  summarise( body_mass = mean(body_mass,na.rm = T), 
             fat_mass = mean(fat_mass,na.rm = T), 
             lean_mass = mean(lean_mass, na.rm = T), 
             
             fat_perc = mean(fat_perc,na.rm = T), 
             lean_perc = mean(lean_perc,na.rm = T), 
             
             kcal = mean( kcal[kcal > 1600],na.rm = T), 
             bodymass_pred = mean(bodymass_pred, na.rm = T),
             fatperc_pred = mean(fatperc_pred, na.rm = T),
             prot_g = mean(prot_g, na.rm = T)
             ) %>% 
  rename(date = n_week) %>% 
  select(date,body_mass,bodymass_pred,fat_perc, fatperc_pred,kcal,lean_perc, prot_g, fat_mass, lean_mass)

# # ## Predictive model for Body Mass and Fat Percentage
# model_bodymass <- lm( body_mass ~  poly(x = date,degree = ord,raw = T), dr1)
# model_fatperc <- lm( fat_perc  ~  poly(x = date,degree = ord,raw = T), dr1)

