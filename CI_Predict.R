

get_stability <- function (data, grouping_variable, value, AD, percentage = T) {
  
  # data overview
  data_2 <- data %>% group_by(!!sym(grouping_variable)) %>%
    summarise(n = n(),  mean = mean(!!sym(value)), group = !!sym(grouping_variable))
  
  # Running model
  mod <- lm(mean ~ group, data_2)
  
  # get model parameters
  coef <- summary(mod)$coefficients
  slope <- coef[[2]]
  Yint <- coef[[1]]
  SE_slope <- coef[[4]]
  Slope_tstat <- coef[[6]]
  p_Value <- coef[[8]]
  name <- value
  r2 = summary(lm(mean ~ group, data_2))$r.squared
  
  # table of model parameters
  data_recap <- data.frame(Sample = name, slope = slope, Yint = Yint,
                           SE_slope = SE_slope, Slope_tstat = Slope_tstat, p_Value = p_Value,
                           significant = if_else(abs(p_Value) < 0.05, "Yes", "No"), R2 = r2)
  
  # if the allowable drift is given in percentage
  if (percentage == T)
  {upper_lim = data_2$mean[[1]] * (1 + AD/100)
  lower_lim = data_2$mean[[1]] * (1 - AD/100)}
  
  # if the allowable drift is given in absolute value
  if (percentage == F)
  {upper_lim = data_2$mean[[1]] +  AD
  lower_lim = data_2$mean[[1]] - AD}
  
  # results model interpretation: if regression significant (< 0.05)
  if (data_recap$significant[[1]] == "Yes") {
    
    # if positive slope
    if (data_recap$slope[1] >= 0) {
      predslm = predict(mod, interval = "confidence")
      predslm <- cbind(predslm, day = data_2$group)
      predslm <- as.data.frame(predslm)
      predslm <- predslm %>%
        arrange(desc(day)) %>% mutate(diff = upr - upper_lim) %>% filter(diff <= 0) %>%
        filter(diff == max(diff))
      
      # stability claim
      claim = paste("The stability duration for the sample is ",
                    max(predslm$day))
      
      # plot
      plot = ggplot(data, aes(x = !!sym(grouping_variable),  y = !!sym(value))) + geom_point() +
        geom_smooth(method = lm ,  formula = y~x, color = "red", fill = "blue") +
        geom_hline(aes(yintercept = lower_lim, linetype = "Lower_limit"), color = "green") +
        geom_hline(aes(yintercept = upper_lim, linetype = "Upper_limit"), color = "blue") +
        scale_linetype_manual(name = "limit", values = c(2, 2),
                              guide = guide_legend(override.aes = list(color = c("green", "blue")))) +
        theme(legend.position = "bottom")}
    
    #  if regression significant (< 0.05) and negative slope
    else if (data_recap$slope[1] < 0) {
      predslm = predict(mod, interval = "confidence")
      predslm <- cbind(predslm, day = data_2$group)
      predslm <- as.data.frame(predslm)
      predslm <- predslm %>% arrange(desc(day)) %>% mutate(diff = lower_lim - lwr) %>%
        filter(diff <= 0) %>% filter(diff ==  max(diff))
      
      # stability claim
      claim = paste("The stability duration for the sample is ",
                    max(predslm$day))
      
      # plot
      plot = ggplot(data, aes(x = !!sym(grouping_variable), y = !!sym(value))) +
        geom_point() +
        geom_smooth(method = lm,  formula = y~x,color = "red", fill = "blue") +
        geom_hline(aes(yintercept = lower_lim, linetype = "Lower_limit"), color = "green") +
        geom_hline(aes(yintercept = upper_lim, linetype = "Upper_limit"), color = "blue") +
        scale_linetype_manual(name = "limit", values = c(2, 2),
                              guide = guide_legend(override.aes = list(color = c("green", "blue")))) +
        theme(legend.position = "bottom")}
  }
  
  # if the regression is not significant
  else {
    
    claim = paste0("The stability duration for the sample is ",
                   max(data[grouping_variable]))
    
    # plot
    plot = ggplot(data, aes(x = !!sym(grouping_variable), y = !!sym(value))) +
      geom_point() +
      geom_smooth(method = lm, formula = y~x, color = "red", fill = "blue") +
      geom_hline(aes(yintercept = lower_lim, linetype = "Lower_limit"), color = "green") +
      geom_hline(aes(yintercept = upper_lim, linetype = "Upper_limit"), color = "blue") +
      scale_linetype_manual(name = "limit", values = c(2, 2),
                            guide = guide_legend(override.aes = list(color = c("green", "blue")))) +
      theme(legend.position = "bottom")}
  
  results = list(data_recap = data_recap, plot = plot, claim = claim,
                 upper_lim = upper_lim, lower_lim = lower_lim)
  
  return(results)
  
}
