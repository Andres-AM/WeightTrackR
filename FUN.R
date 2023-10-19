
## Function to transform input date to number of week from start date (lim_lwr in this function)
## Floors the date to week
date_to_n_week <- function(date = date,lim_lwr = lim_lwr ){
  
  date <- (as.double(date(date) - date(lim_lwr))) %/% 7
  
  return(date)
  
}

n_week_to_date <- function(n_week = n_week,lim_lwr = lim_lwr ){
  
  date <- lim_lwr + weeks((n_week))
  
  return(date)
  
}

data_tidy <- function(
    lim_lwr = "2022-08-01",
    lim_upr = "2023-12-01",
    lim_lwr_mod = "2023-09-01",
    lim_upr_mod = "2023-09-30",
    target_fp = 15,
    target_bm = 69
    ){
  
  ## Range for data filtering 
  lim_lwr <- date(lim_lwr)
  lim_upr <- date(lim_upr)
  
  lim_upr_week <- date_to_n_week(lim_upr,lim_lwr)
  
  lim_lwr_mod <-  date_to_n_week(lim_lwr_mod,lim_lwr)
  lim_upr_mod <-  date_to_n_week(lim_upr_mod,lim_lwr)
  
  ## Data is available in the data folder under different text files
  dr_1 <- read_delim("Data/body_mass.txt", delim = ",", show_col_types = F)
  dr_2 <- read_delim("Data/fat_perc.txt", delim = ",", show_col_types = F)
  dr_3 <- read_delim("Data/lean.txt", delim = ",", show_col_types = F)
  dr_4 <- read_delim("Data/calories.txt", delim = ",", show_col_types = F)  %>%  mutate( Date = date(Date)) 
  dr_7 <- read_delim("Data/prot.txt", delim = ",", show_col_types = F) %>%  mutate( Date = date(Date)) 

  ## Data import, merging and setting the start date to zero
  raw_data <- reduce(list(dr_1, dr_2, dr_3), full_join, by = "Date") %>%
    mutate(Date = date(Date),lim_lwr = lim_lwr,  n_day = as.double(Date) - as.double(lim_lwr)) %>%
    reduce(.x = list(., dr_4, dr_7), .f = full_join, by = "Date") %>%
    # Renaming variables for better clarity
    # Data cleaning and data manipulation
    mutate(
      body_mass = `Body mass(kg)`,
      lean_mass = `Lean body mass(kg)`,
      fat_perc = `Body fat percentage(%)`,
      kcal = `Energy consumed(kcal)`,
      prot_g = `Protein(g)`,
      body_mass = as.double(body_mass),
      fat_perc = as.double(fat_perc),
      fat_perc = fat_perc * 100,
      fat_mass = fat_perc / 100 * body_mass,
      ## bug correction from data set
      fat_perc = replace( fat_perc, Date >= date("2022-11-01") & Date <= date("2022-11-10"), NA),
      lean_mass = replace( lean_mass, Date >= date("2022-11-01") & Date <= date("2022-11-10"), NA),
      lean_perc = (body_mass - (body_mass * (fat_perc / 100))) / body_mass * 100,
      n_week = n_day %/% 7
    ) %>%
    ## Filtering missing values
    filter(!(is.na(body_mass)) & Date >= lim_lwr)  %>%
    select(Date, n_day, n_week, everything()) %>%
    ## Summarizing by date
    group_by(n_week, n_day, Date) %>%
    summarise(
      across(c(body_mass, fat_mass, lean_mass, fat_perc, lean_perc, prot_g),
             \(x) mean(x, na.rm = TRUE)
      ),
      kcal = mean(kcal[kcal > 1600], na.rm = TRUE)
    )

  ## Grouping and averaging the raw data per week in a single value on each Monday
  raw_data_week <- raw_data %>%
    group_by(n_week) %>%
    summarise(
      across(c(body_mass, fat_mass, lean_mass, fat_perc, lean_perc, prot_g, kcal),
             \(x) mean(x, na.rm = TRUE)
      ))
  
  ## Predictive model for Body Mass and Fat Percentage
  model_bodymass <- lm(
    body_mass ~ n_week,
    raw_data_week[lim_lwr_mod <= raw_data_week$n_week & raw_data_week$n_week <= lim_upr_mod, ]
  )
  
  model_fatperc <- lm(
    fat_perc ~ n_week,
    raw_data_week[lim_lwr_mod <= raw_data_week$n_week & raw_data_week$n_week <= lim_upr_mod, ]
  )
  
  ## Adding confidence intervals
  model_data <- raw_data_week %>%
    data_grid(n_week = seq(lim_lwr_mod, lim_upr_week)) %>%
    data.frame(
      predict(model_bodymass, ., interval = "confidence"),
      predict(model_fatperc, ., interval = "confidence")
    ) %>%
    as_tibble()
  
  names(model_data) <- c(
    "n_week",
    "bodymass_pred", "bodymass_pred_lwr", "bodymass_pred_upr",
    "fatperc_pred", "fatperc_pred_lwr", "fatperc_pred_upr"
  )
  
  ## Adding the prediction to the main data set and renaming the week variable to date
  plot_data <- raw_data_week %>%
    full_join(model_data, by = "n_week") %>%
    select(
      n_week, body_mass, bodymass_pred, bodymass_pred_lwr, bodymass_pred_upr,
      fat_perc, fatperc_pred, fatperc_pred_lwr, fatperc_pred_upr, kcal,
      lean_perc, prot_g, fat_mass, lean_mass
    )
  
  
  ## function on this to do 
  # Calculate delta values for body_mass, lean_mass, and fat_mass based on lagged values
  table_data <- raw_data_week %>%
    mutate(delta_body_mass = body_mass - lag(body_mass, default = first(body_mass)),
           delta_lean_mass = lean_mass - lag(lean_mass, default = first(lean_mass)),
           delta_fat_mass = fat_mass - lag(fat_mass, default = first(fat_mass)),
           date = lim_lwr + weeks(n_week)
    )   %>%
    # Select specific columns and arrange them in descending order of date
    select(date, body_mass, lean_mass, fat_mass, fat_perc, delta_body_mass, delta_lean_mass, delta_fat_mass) %>% 
    arrange(desc(date))
  
  return(
    list( 
      raw_data = raw_data,
      table_data = table_data, 
      plot_data = plot_data,
      model_fatperc = model_fatperc,
      model_bodymass = model_bodymass
    )
  )
}
