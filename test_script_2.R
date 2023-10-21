
source("libraries.R");source("FUN.R")

# df <- output_tidy()$fun_output$plot_data
# write_csv2(df,file ="Data/data_plot_test.csv")
data_plot <- read_csv2("Data/data_plot_test.csv",show_col_types = FALSE)  
models <- output_tidy()$fun_output

lim_lwr = date("2022-08-01")
target_fp <- 19
models$model_fatperc

table_to_plot <- function(
    data_plot = data_plot,
    lim_lwr = lim_lwr,
    var = var,
    var_pred = var_pred,
    var_pred_upr = var_pred_upr,
    var_pred_lwr = var_pred_lwr,
    target_var = target_var,
    model_var = model_var,
    color = color ){
  
  tf_var <-  lim_lwr + (target_var - model_var$coefficients[[1]]) / ( model_var$coefficients[[2]])*7
  
  plot_base <-  data_plot %>%
    mutate(Date = lim_lwr + weeks(n_week)) %>%                                          
    ggplot(aes(x = Date)) +
    # Format x-axis labels as abbreviated month and year, with breaks at every month
    scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +                                  
    theme(axis.text.x = element_text(angle = 0)) +      
    # Predictions and CI
    geom_line(aes(y = !!sym(var_pred)), na.rm = T, col = "grey", linetype = 3) +         
    geom_line(aes(y = !!sym(var_pred_upr)), linewidth = 0.1) +   
    geom_line(aes(y = !!sym(var_pred_lwr)), linewidth = 0.1) +
    geom_hline(yintercept = target_var, linetype = 2, col = "grey") +         
    geom_point(aes(tf_var, target_var), shape = 3, color = "red") +
    # Plotting the values above the predictions
    geom_point(aes(y = !!sym(var)), na.rm = T, col = color, size = 0.75) +                            
    geom_line(aes(y = !!sym(var)), na.rm = T, col = color) +                                          
    scale_y_continuous(n.breaks = 10)
  
}

p <- table_to_plot(
  data_plot = data_plot,
  lim_lwr = lim_lwr,
  var = "fat_perc",
  var_pred = "fatperc_pred", 
  var_pred_upr = "fatperc_pred_upr", 
  var_pred_lwr = "fatperc_pred_lwr", 
  target_var = target_fp, 
  model_var = "",
  color = "red"
) + labs(y = "Fat percentage (%)", x = "Date",title  = paste0("Target: ",target_fp,"% the ",format(tf_fp, "%B %d, %Y")))
