
library(shiny)
source("libraries.R")
source("FUN.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Health Dashboard"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      h3("Input Parameters"),
      
      hr(),
      
      actionButton(inputId = "action_button",
                   label = "Data Refresh",
                   width = "100%"),
      
      hr(),
      
      radioButtons(inputId = "phase_type",
                   label = "Phase Type",
                   inline = T,
                  
                   choices = c("bulking","cutting","maintaining")),
      
      hr(),
      
      dateRangeInput(inputId = "date_lim",
                     label = "Date range",
                     start = "2022-08-01",
                     end = "2023-12-01",
                     min = "2022-08-01"),
      
      dateRangeInput(inputId = "date_phase",
                     label = "Date phase",
                     start = "2022-08-01",
                     end = "2023-12-01",
                     min = "2022-08-01"),
      
      hr(),
      
      sliderInput(inputId = "target_fp",
                  label = "Target Fat Percentage [%]",
                  min = 10,
                  max = 20, 
                  value = 14,
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
    
    # Show a plot of the generated distribution
    mainPanel(
      
      fluidPage(
        
        dataTableOutput(outputId = "recap_table", width = "100%", height = "auto", fill = TRUE),
        
        plotOutput(outputId = "plot_FP")
        
      ),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$recap_table <- DT::renderDataTable( 
    
    data_tidy(
      lim_lwr   = input$date_lim[1],
      lim_upr   = input$date_lim[2],
      w_mod     = input$w_mod,
      target_fp = input$target_fp,
      target_bm = input$target_bm)$recap_table %>% 
      datatable(options = list(paging = T, pageLength = 20),
                rownames = FALSE,
                filter = 'bottom'  ) %>%
      formatRound(
        columns = c(
          "body_mass", 
          "lean_mass", 
          "fat_mass", 
          "delta_body_mass", 
          "delta_lean_mass", 
          "delta_fat_mass"
        ), 
        digits=2),
    
  )
  
  output$plot_FP <- renderPlotly(
    
    df_clean %>% 
      ggplot(aes(x = lim_lwr + weeks(date))) +  # Set x-axis values based on 'lim_lwr' and 'date'
      geom_point(aes(y = fat_perc), na.rm = T, col = "red", size = 0.75) +  # Add red points with 'fat_perc' on y-axis
      geom_line(aes(y = fat_perc), na.rm = T, col = "red") +  # Connect points with red lines
      geom_line(aes(y = fatperc_pred), na.rm = T, col = "grey", linetype = 3) +  # Add grey dashed line based on 'fatperc_pred' values
      geom_hline(yintercept = target_fp, linetype = 2, col = "grey") +  # Add grey horizontal line at 'target_fp'
      geom_point(aes(tf_fp, target_fp), shape = 3, color = "red") +  # Add red point at coordinates ('tf_fp', 'target_fp')
      theme(axis.text.x = element_text(angle = 45)) +  # Rotate x-axis labels by 45 degrees
      geom_line(data = subset(df_clean, fatperc_pred_upr > target_fp - 1), aes(y = fatperc_pred_upr), linewidth = 0.1) +  # Add thin line for 'fatperc_pred_upr' values above 'target_fp - 1'
      geom_line(data = subset(df_clean, fatperc_pred_lwr > target_fp - 1), aes(y = fatperc_pred_lwr), linewidth = 0.1) +  # Add thin line for 'fatperc_pred_lwr' values above 'target_fp - 1'
      # geom_area(data = subset(df_clean, fatperc_pred_lwr > target_fp - 1), aes(ymin = fatperc_pred_lwr, ymax = fatperc_pred_upr), alpha = 0.1) +  # Add shaded area between 'fatperc_pred_lwr' and 'fatperc_pred_upr'
      scale_y_continuous(n.breaks = 10, limits = c(target_fp - 1, 23)) +  # Set y-axis limits and breaks
      scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +  # Format x-axis labels as abbreviated month and year, with breaks at every month
      labs(y = "Fat percentage", x = "Date")  # Set y-axis and x-axis labels
    
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
