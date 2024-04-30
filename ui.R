
source("libraries.R")
source("FUN.R")

ui <- dashboardPage(
  
  dashboardHeader(title = "WeightTrackR" ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Parameters", tabName = "dashboard", icon = icon("dashboard"), startExpanded = T,
               
               
               
               
               dateRangeInput(inputId = "date_lim",
                              label = "Date range for plots",
                              min   = "2022-08-01",
                              start = "2023-08-01",
                              end   = "2023-09-01"
               ),
               
               hr(), 
               
               
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
                                # tabItem("about",
                                #         h1("Note: gaining 500g/week of body mass should be the maximum value,
                                #                          with a 50% gain ratio of lean mass and 50% fat mass (250g/week for each).
                                #                          This totals for 1.8kg/month of body mass.")),
                                
                                # helpText("Note: gaining 500g/week of body mass should be the maximum value,
                                #                          with a 50% gain ratio of lean mass and 50% fat mass (250g/week for each).
                                #                          This totals for 1.8kg/month of body mass.")
               ),
               
               conditionalPanel(condition = "input.phase_type == 'Cutting'",
                                
                                numericInput(inputId = "cut_val",
                                             label = "Cutting target, BM in kg/week",
                                             value = "0.7",
                                             step = 0.1,
                                             min = 0,
                                ),
                                
                                # p("Note: loosing 700g/week should be the maximum value,
                                #                          with a 70% loss ratio of fat mass and 30% lean mass (500g/week of fat mass and 250g of lean mass).
                                #                          This totals for 3kg/month of body mass"),
               ),
               
               hr(),
               
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
                                
                                # p("Note: Prediction"),
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
               )
               
               
               
               ),
      menuItem("Options", icon = icon("th"), tabName = "widgets",
               

               
               numericInput(inputId = "round_value",
                            label = "round value",
                            value = "1",
                            min = 0,
               ),
               textInput(inputId = "phase_date",
                         label ="phase_date",
                         value = "2022-08-01;2023-08-01;2023-10-16w"),
               
               verbatimTextOutput(outputId = "dates")
               
               
               
               ) 
      
    )
    
  ),
  
  dashboardBody(
    fluidRow(
      box(width = 8,   

                 selectInput( inputId = "plot_choice",
                              label = "Graph type",
                              choices = c("Fat Percentage (%)",
                                          "Body Mass (kg)",
                                          "Lean Mass (kg)")),
                 
                 plotlyOutput(outputId = "SelectedPlot")

      ),
      
      box(width = 4, "other informations"),
      
      tabBox(width = 12, 
             tabPanel("Table",
                      
                      DT::dataTableOutput(outputId = "your_table")
             ),
             tabPanel("Raw Data",
                      
                      DT::dataTableOutput(outputId = "raw_data_day"),
                      
             )
      )
    )
  )
)
