

ui <- dashboardPage(
  
  dashboardHeader(title = "WeightTrackR" ),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    fluidRow(
      box(
        title = "Input Parameters",
        
        selectInput( inputId = "phase_type",
                     label = "Phase Type",
                     selected = "Cutting",
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
        
        # hr(),
        
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
                         
                         # helpText("Note: test"),
        ),
        
        conditionalPanel(condition = "input.plot_choice == 'Body Mass (kg)'",
                         
                         sliderInput("target_bm",
                                     label = "Target Body Mass (kg)",
                                     min = 65,
                                     max = 78,
                                     value = 76
                         ),
                         
                         # helpText("Note: test"),
        ),
        
        conditionalPanel(condition = "input.plot_choice == 'Lean Mass (kg)'",
                         
                         sliderInput("target_lm",
                                     label = "Target Lean Mass (kg)",
                                     min = 60,
                                     max = 64,
                                     value = 62,step = 0.5
                         ),
                         
                         # helpText("Note: test"),
        ),
        
      ),
      box(
        
        selectInput( inputId = "plot_choice",
                     label = "Graph type",
                     choices = c("Fat Percentage (%)",
                                 "Body Mass (kg)",
                                 "Lean Mass (kg)")),
        
        plotlyOutput(outputId = "SelectedPlot",width = "100%",height = "100%"),
        
        hr(), 
        
        DT::dataTableOutput(outputId = "your_table", width = "100%", height = "auto", fill = TRUE)
        
      )
      
    )
    
    
    
  )
  
)