
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
                                            label = "Phase Type",selected = "Cutting",
                                            choices = c("Bulking",
                                                        "Cutting" 
                                            )
                               ),
                               
                               conditionalPanel(condition = "input.phase_type == 'Bulking'",
                                                
                                                numericInput(inputId = "bulk_val",
                                                             label = "Bulking target, BM in kg/week",
                                                             value = "0.5",
                                                             step = 0.1,
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
                                                             step = 0.1,
                                                             min = 0,
                                                ),
                                                
                                                helpText("Note: loosing 700g/week should be the maximum value, 
                                                         with a 70% loss ratio of fat mass and 30% lean mass (500g/week of fat mass and 250g of lean mass).
                                                         This totals for 3kg/month of body mass"),
                               ),
                               
                               hr(),
                               
                               h3("Prediction"),
                               
                               dateRangeInput(inputId = "date_phase",
                                              label = "Date interval for prediction:",
                                              min = "2022-08-01",
                                              start = "2023-08-01",
                                              end = "2023-10-16"
                               ),
                               
                               
                               conditionalPanel(condition = "input.plot_choice == 'Fat Percentage (%)'",
                                                
                                                sliderInput(inputId = "target_fp",
                                                            label = "Target Fat Percentage (%)",
                                                            min = 12,
                                                            max = 20, 
                                                            value = 19,
                                                            step = 0.5
                                                ),
                                                
                                                helpText("Note: test"),
                               ),
                               
                               conditionalPanel(condition = "input.plot_choice == 'Body Mass (kg)'",
                                                
                                                sliderInput("target_bm",
                                                            label = "Target Body Mass (kg)",
                                                            min = 65,
                                                            max = 78,
                                                            value = 76
                                                ),
                                                
                                                helpText("Note: test"),
                               ),
                               
                               conditionalPanel(condition = "input.plot_choice == 'Lean Mass (kg)'",
                
                                                sliderInput("target_lm",
                                                            label = "Target Lean Mass (kg)",
                                                            min = 60,
                                                            max = 64,
                                                            value = 62,step = 0.2
                                                ),
                                                
                                                helpText("Note: test"),
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
                           
                           dateRangeInput(inputId = "date_lim",
                                          label = "Date range for plots",
                                          min   = "2022-08-01",
                                          start = "2022-08-01",
                                          end   = "2023-11-01"
                           ),
                           
                           numericInput(inputId = "round_value",
                                        label = "round value",
                                        value = "1",
                                        min = 0,
                           ),
                           textInput(inputId = "phase_date",
                                     label ="phase_date",
                                     value = "2022-08-01;2023-08-01;2023-10-16"),
                           
                           verbatimTextOutput(outputId = "dates")
                  )
                )
)


### Server part of the Shiny App
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
    table_data <- fun_output$table_data %>% 
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
      )
    
    raw_data <- fun_output$raw_data %>%
      ungroup() %>%
      mutate(
        date = Date,
        body_mass = round(body_mass,input$round_value),
        fat_mass  = round(fat_mass ,input$round_value),
        lean_mass = round(lean_mass,input$round_value),
        fat_perc  = round(fat_perc ,input$round_value),
      ) %>%
      dplyr::arrange(desc(date))
    
    results <- list(fun_output = fun_output, 
                    raw_data = raw_data, 
                    plot_data = fun_output$plot_data, 
                    table_data = table_data, 
                    lim_lwr = lim_lwr, 
                    lim_upr = lim_upr)
    
  })

  output$raw_data <- DT::renderDataTable(
    
    output_tidy()$raw_data  %>%
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
    
    plot_FP  <- table_to_plot(
      data_plot = output_tidy()$plot_data,
      lim_lwr = output_tidy()$lim_lwr,
      var = "fat_perc",
      var_pred = "fatperc_pred", 
      var_pred_upr = "fatperc_pred_upr", 
      var_pred_lwr = "fatperc_pred_lwr", 
      target_var = input$target_fp, 
      model_var = output_tidy()$fun_output$model_fatperc,
      color = "red", 
      y_axis_name = "Fat percentage (%)",
      unit_var = "%"
    ) 
    
    plot_BM  <- table_to_plot(
      data_plot = output_tidy()$plot_data,
      lim_lwr = output_tidy()$lim_lwr,
      var = "body_mass",
      var_pred = "bodymass_pred", 
      var_pred_upr = "bodymass_pred_upr", 
      var_pred_lwr = "bodymass_pred_lwr", 
      target_var = input$target_bm, 
      model_var = output_tidy()$fun_output$model_bodymass,
      color = "blue",
      y_axis_name = "Body Mass (kg)",
      unit_var = "kg"
      ) 
    
    plot_LM  <- table_to_plot(
      data_plot = output_tidy()$plot_data,
      lim_lwr = output_tidy()$lim_lwr,
      var = "lean_mass",
      var_pred = "leanmass_pred", 
      var_pred_upr = "leanmass_pred_upr", 
      var_pred_lwr = "leanmass_pred_lwr", 
      target_var = input$target_lm, 
      model_var = output_tidy()$fun_output$model_leanmass,
      color = "grey", 
      y_axis_name = "Lean Mass (kg)",
      unit_var = "kg"
      ) 

    return(
      if (input$plot_choice == "Fat Percentage (%)") { plot_FP } 
      else if(input$plot_choice == "Body Mass (kg)") { plot_BM } 
      else if(input$plot_choice == "Lean Mass (kg)") { plot_LM }
    )
    
  })
  
  output$SelectedPlot <- renderPlotly(
    
    ggplotly(your_plot()) %>%
      layout(xaxis = list(autorange = TRUE),
             yaxis = list(autorange = TRUE))
    
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

