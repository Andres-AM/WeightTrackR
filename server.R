
source("libraries.R")
source("FUN.R")

server <- function(input, output) {
  
  output$dates <- renderPrint({
    
    stringr::str_split_1(input$phase_date,pattern = ";")
    
  })
  
  ## Reactive output from function
  output_tidy <- reactive({ 
    
    # Adjust the date to always start on Mondays, start of the week
    lim_lwr <- floor_date(date(input$date_lim[1]),unit = "week",week_start = 1)
    lim_upr <- floor_date(date(input$date_lim[2]),unit = "week",week_start = 1)
    
    # Input values to data_tidy function
    fun_output <- data_tidy(
      lim_lwr      = lim_lwr,
      lim_upr      = lim_upr,
      lim_lwr_mod  = input$date_phase[1],
      lim_upr_mod  = input$date_phase[2],
      target_fp    = input$target_fp,
      target_bm    = input$target_bm)
    
    # Recap table as the main table output 
    table_data <- fun_output$data_week %>% 
      filter( filtering_pred == F) %>% 
      arrange(desc(date)) %>% 
      mutate( 
        date =  format(date, "%B %d, %Y"), 
        fat_perc = paste0("<b>",round(fat_perc,input$round_value)," <b>"," %"),
        BM = as.character(paste0("(",if_else(delta_body_mass > 0,"+",""), round(delta_body_mass,input$round_value),")"," <b>",round(body_mass,input$round_value),"<b>")),
        LM = as.character(paste0("(",if_else(delta_lean_mass > 0,"+",""),round(delta_lean_mass,input$round_value),")"," <b>",round(lean_mass,input$round_value),"<b>")),
        FM = as.character(paste0("(",if_else(delta_fat_mass > 0,"+",""), round(delta_fat_mass,input$round_value),")"," <b>",round(fat_mass,input$round_value),"<b>")),
        ## Scores and ratios
        score_bulk = paste0("target: ",round((delta_lean_mass + delta_fat_mass + delta_body_mass)/2/input$bulk_val*100, input$round_value)," %",
                            " ratio: ", round(delta_lean_mass/((delta_lean_mass + delta_fat_mass + delta_body_mass)/2)*100, input$round_value), " %"),
        score_cut = paste0("target: ",round((delta_lean_mass + delta_fat_mass + delta_body_mass)/2/(-input$cut_val)*100, input$round_value)," %",
                           " ratio: ", round(delta_fat_mass/((delta_lean_mass + delta_fat_mass + delta_body_mass)/2)*100, input$round_value), " %")
      ) %>% 
      select(date, fat_perc, BM, LM, FM,score_bulk, score_cut)
    
    raw_data_day <- fun_output$raw_data_day %>%
      ungroup() %>%
      mutate(
        body_mass = round(body_mass,input$round_value),
        fat_mass  = round(fat_mass ,input$round_value),
        lean_mass = round(lean_mass,input$round_value),
        fat_perc  = round(fat_perc ,input$round_value),
      ) %>%
      dplyr::arrange(desc(date))
    
    results <- list(fun_output = fun_output, 
                    raw_data_day = raw_data_day, 
                    data_week = fun_output$data_week, 
                    table_data = table_data, 
                    lim_lwr = lim_lwr, 
                    lim_upr = lim_upr)
    
  })
  
  output$raw_data_day <- DT::renderDataTable(
    
    output_tidy()$raw_data_day  %>%
      select(date, fat_perc,body_mass,lean_mass,fat_mass) %>%
      datatable(
        colnames = c('Date (days)',
                     "Fat percentage (%)",
                     "Body Mass (kg)",
                     "Lean Mass (kg)",
                     "Fat Mass (kg)"
        ),
        options = list(paging = T, 
                       pageLength = 20,
                       dom = "t",
                       scrollY = TRUE),
        rownames = FALSE,
        filter = 'bottom',
        escape = F) 
    
  )
  
  output$your_table <- DT::renderDataTable(
    
    if(input$phase_type == "Bulking") { 
      
      output_tidy()$table_data  %>%
        select(date,fat_perc,BM,LM,FM,score_bulk) %>%
        datatable(
          colnames = c('Date (weeks)',
                       "Fat percentage (%)",
                       "Body Mass (kg)",
                       "Lean Mass (kg)",
                       "Fat Mass (kg)",
                       "Bulking Score"
          ),
          options = list(paging = T,
                         pageLength = 20,
                         dom = "t",
                         scrollX = TRUE),
          rownames = FALSE,
          filter = 'bottom',
          escape = F)
      
    } else if(input$phase_type == "Cutting") { 
      
      output_tidy()$table_data %>%
        select(date,fat_perc,BM,LM,FM,score_cut) %>% 
        datatable(
          colnames = c('Date (Week)',
                       "Fat percentage (%)",
                       "Body Mass (kg)",
                       "Lean Mass (kg)",
                       "Fat Mass (kg)",
                       "Cutting Score"
          ),
          options = list(paging = T, 
                         pageLength = 20,
                         dom = "t",
                         scrollX = TRUE),
          rownames = FALSE,
          filter = 'bottom',
          escape = F) 
      
    } 
  )
  
  your_plot <- reactive({
    
    arguments <-list( data_plot = output_tidy()$data_week,
                      lim_lwr = output_tidy()$lim_lwr)
    
    list_plots <- mapply(FUN = table_to_plot, 
                         var = c("fat_perc","body_mass","lean_mass" ),
                         var_pred = c("fatperc_pred", "bodymass_pred", "leanmass_pred"),
                         var_pred_upr = c("fatperc_pred_upr", "bodymass_pred_upr","leanmass_pred_upr"),
                         var_pred_lwr = c("fatperc_pred_lwr", "bodymass_pred_lwr","leanmass_pred_lwr"),
                         target_var = c(input$target_fp, input$target_bm,input$target_lm),  
                         model_var = list(output_tidy()$fun_output$model_fatperc,
                                          output_tidy()$fun_output$model_bodymass,
                                          output_tidy()$fun_output$model_leanmass),
                         color =c("red","blue","grey"),
                         y_axis_name = c("Fat percentage (%)","Body Mass (kg)","Lean Mass (kg)"),
                         unit_var = c("%","kg","kg"),
                         MoreArgs = arguments, 
                         SIMPLIFY = F
    )
    
    return(
      if (input$plot_choice == "Fat Percentage (%)") { list_plots[[1]] } 
      else if(input$plot_choice == "Body Mass (kg)") { list_plots[[2]] } 
      else if(input$plot_choice == "Lean Mass (kg)") { list_plots[[3]] }
    )
    
  })
  
  output$SelectedPlot <- renderPlotly(
    
    ggplotly(your_plot()) %>%
      layout(xaxis = list(autorange = TRUE),
             yaxis = list(autorange = TRUE))
    
  )
  
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(25, "%"), "Progress", icon = icon("list"),
      color = "blue"
    )
  })
  
}
