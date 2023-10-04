
library(shiny)
source("libraries.R")
source("FUN.R")


### UI part of the application
ui <- fluidPage(theme = shinytheme("flatly"),
  tabsetPanel(
    tabPanel('Overview',
             titlePanel("Health Dashboard"),
             sidebarLayout(
               sidebarPanel(
                 
                 h3("Input Parameters"),
                 
                 selectInput( inputId = "phase_type",
                              label = "Phase Type",
                              choices = c("Bulking",
                                          "Cutting"
                                          # ,
                                          # "maintaining"
                              )
                 ),
                 
                 conditionalPanel(condition = "input.phase_type == 'Bulking'",
                                  
                                  numericInput(inputId = "bulk_val",
                                               label = "Bulking target, BM in kg/week",
                                               value = "0.5",
                                               min = 0,
                                  ),
                                  
                                  helpText("Note: gaining 500g/week of body mass should be the maximum value, 
                                         with a 50% gain ratio of lean mass and 50% fat mass (250g/week for each). 
                                         This totals for 1.8kg/month of body mass.")
                 ),
                 
                 conditionalPanel(condition = "input.phase_type == 'Cutting'",
                                  
                                  numericInput(inputId = "cut_val",
                                               label = "Cutting target, BM in kg/week",
                                               value = "0.7",
                                               min = 0,
                                  ),
                                  
                                  helpText("Note: loosing 700g/week should be the maximum value, 
                                         with a 70% loss ratio of fat mass and 30% lean mass (500g/week of fat mass and 250g of lean mass).
                                         This totals for 3kg/month of body mass"),
                 ),
                 
                 hr(),

                 h3("Prediction"),
                 
                 dateRangeInput(inputId = "date_lim",
                                label = "Date range",
                                start = "2022-08-01",
                                end = "2023-12-01",
                                min = "2022-08-01"),
                 
                 dateRangeInput(inputId = "date_phase",
                                label = "Date interval for mod",
                                start = "2022-08-01",
                                end = "2023-12-01",
                                min = "2022-08-01"),
                 
                 hr(),
                 
                 sliderInput(inputId = "target_fp",
                             label = "Target Fat Percentage (%)",
                             min = 10,
                             max = 20, 
                             value = 19,
                             step = 0.5
                 ),
                 
                 sliderInput("target_bm",
                             label = "Target Body Mass (kg)",
                             min = 68,
                             max = 82,
                             value = 69
                 ),
                 
                 hr(),
                 
                 sliderInput(inputId = "w_mod",
                             label = "weeks for mod",
                             min = 3,
                             max = 20,
                             value = 12
                 ),
               ),
               
               mainPanel(
                 fluidPage(
                   
                   selectInput( inputId = "plot_choice",
                                label = "Graph type",
                                choices = c("Fat Percentage (%)",
                                            "Body Mass (kg)",
                                            "Lean Mass (kg)")),
                   
                   plotlyOutput(outputId = "SelectedPlot",width = "100%",height = "100%"),
                   
                   hr(), 
                   
                   dataTableOutput(outputId = "your_table", width = "100%", height = "auto", fill = TRUE),
                   
                 ),
               )
             )
    ),
    tabPanel('Raw Data',
             
             dataTableOutput(outputId = "raw_data", width = "100%", height = "auto", fill = TRUE),
             
    ),
    tabPanel('Options',
             titlePanel("Parameters"),
             
             numericInput(inputId = "round_value",
                          label = "round value",
                          value = "1",
                          min = 0,
             )
    )
  )
)


### Server part of the Shiny App
server <- function(input, output) {
  
  floor_date <- reactive({
    
    lim_lwr <- input$date_lim[1] + days(8- wday(input$date_lim[1]))
    lim_upr <- input$date_lim[2] + days(8- wday(input$date_lim[2]))
    
    results <- list(lim_lwr = lim_lwr,
                    lim_upr = lim_upr)
    
  })
  
  ## Reactive output from function
  output_tidy <- reactive({ 
    
    fun_output <- data_tidy(
      lim_lwr      = floor_date()$lim_lwr,
      lim_upr      = floor_date()$lim_upr,
      lim_lwr_mod  = input$date_phase[1],
      lim_upr_mod  = input$date_phase[2],
      w_mod        = input$w_mod,
      target_fp    = input$target_fp,
      target_bm    = input$target_bm)
    
    recap_table_tidy <- fun_output$recap_table %>% 
      mutate( 
        date =  format(date, "%B %d, %Y"), 
        fat_perc = paste0("<b>",round(fat_perc,input$round_value)," <b>"," %"),
        BM = as.character(paste0("(",if_else(delta_body_mass > 0,"+",""), round(delta_body_mass,input$round_value),")"," <b>",round(body_mass,input$round_value),"<b>")),
        LM = as.character(paste0("(",if_else(delta_lean_mass > 0,"+",""),round(delta_lean_mass,input$round_value),")"," <b>",round(lean_mass,input$round_value),"<b>")),
        FM = as.character(paste0("(",if_else(delta_fat_mass > 0,"+",""), round(delta_fat_mass,input$round_value),")"," <b>",round(fat_mass,input$round_value),"<b>")),
        
        score_bulk = paste0("target: ",round((delta_lean_mass + delta_fat_mass + delta_body_mass)/2/input$bulk_val*100, input$round_value)," %",
                            " ratio: ", round(delta_lean_mass/((delta_lean_mass + delta_fat_mass + delta_body_mass)/2)*100, input$round_value), " %"),
        score_cut = paste0("target: ",round((delta_lean_mass + delta_fat_mass + delta_body_mass)/2/(-input$cut_val)*100, input$round_value)," %",
                           " ratio: ", round(delta_fat_mass/((delta_lean_mass + delta_fat_mass + delta_body_mass)/2)*100, input$round_value), " %")
      )
    
    results <- list(fun_output = fun_output, recap_table_tidy = recap_table_tidy )
    
  })
  
  output$raw_data <- DT::renderDataTable(
    
    output_tidy()$fun_output$dr %>% 
      ungroup() %>%  
      mutate( 
        date = date(input$date_lim[1] + n_day),
        body_mass = round(body_mass,input$round_value),
        fat_mass  = round(fat_mass ,input$round_value),
        lean_mass = round(lean_mass,input$round_value),
        fat_perc  = round(fat_perc ,input$round_value),
      ) %>% 
      dplyr::arrange(desc(date)) %>%
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
      
      output_tidy()$recap_table_tidy  %>%
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
      
      output_tidy()$recap_table_tidy  %>%
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
  
  
  plot_FP <- reactive({ 
    
    tf_fp <- floor_date()$lim_lwr + (input$target_fp - output_tidy()$fun_output$model_fatperc$coefficients[[1]]) / (output_tidy()$fun_output$model_fatperc$coefficients[[2]])*7
    
    output_tidy()$fun_output$df_clean %>%
      mutate(Date = floor_date()$lim_lwr + weeks(date)) %>%
      ggplot(aes(x = Date)) +
      geom_point(aes(y = fat_perc), na.rm = T, col = "red", size = 0.75) +  # Add red points with 'fat_perc' on y-axis
      geom_line(aes(y = fat_perc), na.rm = T, col = "red") +  # Connect points with red lines
      
      # Predictions and CI
      geom_line(aes(y = fatperc_pred), na.rm = T, col = "grey", linetype = 3) +  # Add grey dashed line based on 'fatperc_pred' values
      geom_hline(yintercept = input$target_fp, linetype = 2, col = "grey") +  # Add grey horizontal line at 'target_fp'
      geom_point(aes(tf_fp, input$target_fp), shape = 3, color = "red") +  # Add red point at coordinates ('tf_fp', 'target_fp')
      geom_line(
        # data = subset(output_tidy()$fun_output$df_clean, fatperc_pred_upr > input$target_fp - 1),
                aes(y = fatperc_pred_upr), linewidth = 0.1) +  # Add thin line for 'fatperc_pred_upr' values above 'target_fp - 1'
      geom_line(
        # data = subset(output_tidy()$fun_output$df_clean, fatperc_pred_lwr > input$target_fp - 1),
                aes(y = fatperc_pred_lwr), linewidth = 0.1) +  # Add thin line for 'fatperc_pred_lwr' values above 'target_fp - 1'

      scale_y_continuous(n.breaks = 10) +  # Set y-axis limits and breaks
      scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +  # Format x-axis labels as abbreviated month and year, with breaks at every month
      theme(axis.text.x = element_text(angle = 0)) +  # Rotate x-axis labels by 45 degrees
      labs(y = "Fat percentage (%)", x = "Date",title  = paste0("Target: ",input$target_fp," % the ",format(tf_fp, "%B %d, %Y")))  # Set y-axis and x-axis labels
    
  })
  
  plot_BM <- reactive({ 
    
    tf_bm <- floor_date()$lim_lwr + (input$target_bm - output_tidy()$fun_output$model_bodymass$coefficients[[1]]) / (output_tidy()$fun_output$model_bodymass$coefficients[[2]])*7
    
    output_tidy()$fun_output$df_clean %>%
      mutate(Date = floor_date()$lim_lwr + weeks(date)) %>%
      ggplot(aes(x = Date)) +
      geom_point(aes(y = body_mass),na.rm = T, col = "blue", size= 0.75) +
      geom_line(aes(y = body_mass),na.rm = T, col = "blue") +
      
      scale_y_continuous(n.breaks = 15) +
      scale_x_date(date_labels = "%b %y", date_breaks = "1 month" ) +
      theme(axis.text.x = element_text(angle = 45))+
      labs( y = "Body Mass (kg)", x = "Date")
    
  })
  
  plot_LM <- reactive({ 
    
    output_tidy()$fun_output$df_clean %>%
      mutate(Date = floor_date()$lim_lwr + weeks(date)) %>%
      ggplot(aes(x = Date)) +
      geom_point(aes(y = lean_mass),na.rm = T, col = "grey", size= 0.75) +
      geom_line(aes(y = lean_mass),na.rm = T, col = "grey") +
      
      scale_y_continuous(n.breaks = 10) +
      scale_x_date(date_labels = "%b %y", date_breaks = "1 month" ) +
      theme(axis.text.x = element_text(angle = 45)) +
      labs( y = "Lean Body Mass (kg)",  x = "Date")
    
  })
  
  your_plot <- reactive({
    
    if (input$plot_choice == "Fat Percentage (%)") { plot_FP() } 
    else if(input$plot_choice == "Body Mass (kg)") { plot_BM() } 
    else if(input$plot_choice == "Lean Mass (kg)") { plot_LM() }
    
  })
  
  output$SelectedPlot <- renderPlotly(
    
    ggplotly(your_plot()) %>%
      layout(xaxis = list(autorange = TRUE),
             yaxis = list(autorange = TRUE))
    
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

############# Others 


### Fat % pred graph

# Predictions and CI
# geom_line(aes(y = fatperc_pred), na.rm = T, col = "grey", linetype = 3) +  # Add grey dashed line based on 'fatperc_pred' values
# geom_hline(yintercept = input$target_fp, linetype = 2, col = "grey") +  # Add grey horizontal line at 'target_fp'
# geom_point(aes(tf_fp, input$target_fp), shape = 3, color = "red") +  # Add red point at coordinates ('tf_fp', 'target_fp')
# geom_line(data = subset(output_tidy()$df_clean, fatperc_pred_upr > input$target_fp - 1),
#           aes(y = fatperc_pred_upr), linewidth = 0.1) +  # Add thin line for 'fatperc_pred_upr' values above 'target_fp - 1'
# geom_line(data = subset(output_tidy()$df_clean, fatperc_pred_lwr > input$target_fp - 1),
#           aes(y = fatperc_pred_lwr), linewidth = 0.1) +  # Add thin line for 'fatperc_pred_lwr' values above 'target_fp - 1'


### BM pred graph


## Predictions and CI 
# geom_line(aes(y = bodymass_pred), col = "grey",na.rm = T, linetype = 3) +
# geom_hline(yintercept = input$target_bm, linetype = 2, col = "grey") +
# geom_point(aes(tf_bm,input$target_bm), shape = 3, color = "red")+
# geom_line(data = subset(output_tidy()$df_clean,bodymass_pred_upr > input$target_bm - 1),
#           aes(y = bodymass_pred_upr),linewidth = 0.1)+
# geom_line(data = subset(output_tidy()$df_clean,bodymass_pred_lwr > input$target_bm - 1),
#           aes(y = bodymass_pred_lwr),linewidth = 0.1) +