
data_tidy <- function(
    lim_lwr = "2022-08-01",
    lim_upr = "2023-12-01",
    lim_lwr_mod = "2023-09-01",
    lim_upr_mod = "2023-09-30",
    target_fp = 15,
    target_bm = 69
    ){
  
  lim_upr_week <- (as.double(ymd(lim_upr)) - as.double(lim_lwr)) %/% 7
  lim_lwr <- ymd(lim_lwr)
  lim_upr <- ymd(lim_upr)
  
  lim_lwr_mod <- (as.double(ymd(lim_lwr_mod)) - as.double(lim_lwr)) %/% 7
  lim_upr_mod <- (as.double(ymd(lim_upr_mod)) - as.double(lim_lwr)) %/% 7
  
  ## Range of data visualization in days, number of days between lim_upr and lim_lwr
  t_interval <- as.double(lim_upr - lim_lwr)
  
  # t_interval <- as.double(lim_upr_mod - lim_lwr_mod)
  
  ## Data is available in the data folder under different text files
  dr_1 <- read_delim("Data/body_mass.txt", delim = ",", show_col_types = F)
  dr_2 <- read_delim("Data/fat_perc.txt", delim = ",", show_col_types = F)
  dr_3 <- read_delim("Data/lean.txt", delim = ",", show_col_types = F)
  dr_4 <- read_delim("Data/calories.txt", delim = ",", show_col_types = F)
  dr_7 <- read_delim("Data/prot.txt", delim = ",", show_col_types = F)
  
  ## Data import, merging and setting the start date to zero
  dr <- reduce(list(dr_1, dr_2, dr_3), full_join, by = "Date") %>%
    mutate(Date = date(Date), n_day = as.double(Date) - as.double(lim_lwr)) %>%
    reduce(.x = list(., dr_4, dr_7), .f = full_join, by = "Date") %>%
    ## Renaming variables for better clarity
    rename(
      body_mass = `Body mass(kg)`,
      kcal = `Energy consumed(kcal)`, lean_mass = `Lean body mass(kg)`,
      prot_g = `Protein(g)`, fat_perc = `Body fat percentage(%)`
    ) %>%
    ## Data cleaning and data manipulation
    mutate(
      fat_perc = fat_perc * 100, fat_mass = fat_perc / 100 * body_mass,
      ## bug correction from data set
      fat_perc = replace(
        fat_perc,
        Date >= date("2022-11-01") & Date <= date("2022-11-10"), NA
      ),
      lean_mass = replace(
        lean_mass,
        Date >= date("2022-11-01") & Date <= date("2022-11-10"), NA
      ),
      lean_perc = (body_mass - (body_mass * (fat_perc / 100))) / body_mass * 100,
      n_week = n_day %/% 7
    ) %>%
    ## Filtering missing values
    filter(!(is.na(body_mass)) & Date > lim_lwr) %>%
    select(Date, n_day, n_week, everything()) %>%
    ## Summarizing by date
    group_by(n_week, n_day) %>%
    summarise(
      across(c(body_mass, fat_mass, lean_mass, fat_perc, lean_perc, prot_g),
             \(x) mean(x, na.rm = TRUE)
      ),
      kcal = mean(kcal[kcal > 1600], na.rm = TRUE)
    ) 
  
  ## Grouping and averaging the raw data per week in a single value on each Monday
  dr_week <- dr %>%
    group_by(n_week) %>%
    summarise(
      across(c(body_mass, fat_mass, lean_mass, fat_perc, lean_perc, prot_g, kcal),
             \(x) mean(x, na.rm = TRUE)
      ))
  
  ## Predictive model for Body Mass and Fat Percentage
  model_bodymass <- lm(
    body_mass ~ n_week,
    dr_week[lim_lwr_mod <= dr_week$n_week & dr_week$n_week <= lim_upr_mod, ]
  )
  model_fatperc <- lm(
    fat_perc ~ n_week,
    dr_week[lim_lwr_mod <= dr_week$n_week & dr_week$n_week <= lim_upr_mod, ]
  )
  
  ## Adding confidence intervals
  model_data <- dr_week %>%
    # data_grid(n_week = seq(last(n_week) - w_mod, t_interval %/% 7)) %>%
    # data_grid(n_week = seq(lim_lwr_mod, t_interval %/% 7)) %>%
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
  df_clean <- dr_week %>%
    full_join(model_data, by = "n_week") %>%
    rename(date = n_week) %>%
    select(
      date, body_mass, bodymass_pred, bodymass_pred_lwr, bodymass_pred_upr,
      fat_perc, fatperc_pred, fatperc_pred_lwr, fatperc_pred_upr, kcal,
      lean_perc, prot_g, fat_mass, lean_mass
    )
  
  ## Calculating the predicted values
  tf_bm <- lim_lwr + ((target_bm - model_bodymass$coefficients[[1]]) / (model_bodymass$coefficients[[2]]))*7
  tf_fp <- lim_lwr + (target_fp - model_fatperc$coefficients[[1]]) /  (model_fatperc$coefficients[[2]])*7
  
  # Calculate delta values for body_mass, lean_mass, and fat_mass based on lagged values
  recap_table <- df_clean %>%
    mutate(delta_body_mass = body_mass - lag(body_mass, default = first(body_mass)),
           delta_lean_mass = lean_mass - lag(lean_mass, default = first(lean_mass)),
           delta_fat_mass = fat_mass - lag(fat_mass, default = first(fat_mass)),
           date = lim_lwr + weeks(date), empty = ""
    )  %>% 
    # Remove rows with NA values for delta_body_mass
    filter(!is.na(delta_body_mass)) %>% 
    # Select specific columns and arrange them in descending order of date
    select(date, body_mass, lean_mass, fat_mass, fat_perc, delta_body_mass, delta_lean_mass, delta_fat_mass) %>% 
    arrange(desc(date))
  
  
  return(list( recap_table = recap_table, 
               df_clean = df_clean,
               model_fatperc = model_fatperc,
               model_bodymass = model_bodymass,
               dr = dr
  )
  )
}
