# LIBRARIES ----
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)

library(plotly)
library(tidyverse)


# UI ----
ui <- navbarPage(
    title = "Reactive UI",
    inverse = FALSE,
    collapsible = TRUE,
    
    tabPanel(
        title = "Reactive Plot",
        
        div(
            id = "year_input",
            numericInput(
                inputId = "years",
                label = "Select Years",
                value = 3,
                min = 1,
                max = 10,
                step = 1
            )
        ),
        
        div(
            id = "conditional_values",
            class = "well",
            uiOutput(
                outputId = "input_values"
            )
        ),
        
        
        div(
            id = "table_and_plot_output",
            
            div( 
                id = "table_output",
                column(
                    width = 4,
                    tableOutput("table")
                )
            ),
            
            div(
                id = "plot_output",
                column(
                    width = 8,
                    plotOutput(
                        outputId = "years_plot"
                    )
                )
            )
            
            
        )
        
        
        
        
        
        
    )
    
)

# SERVER ----
server <- function(input, output, session) {
    
    # Reactive Plot ----
    
    ## Render Value UI ----
    output$input_values <- renderUI({
        
        years <- as.integer(input$years)
        
        lapply(1:years, function(i) {
            numericInput(
                inputId = paste0("input_value_", i),
                label = "Insert Value",
                value = 3,
                min = 1,
                max = 10,
                step = 1
            )
        }
        )
    })
    
    ## Create Output Table ----
    output$table <- renderTable({
        
        years <- 1:as.integer(input$years)
        
        values <- lapply(years, function(i) {
            input[[paste0('input_value_', i)]]  
        }
        )
        
        
        
        
        data.frame(Year = years, Values = unlist(values))
    })
    
    ## Create Output plot ----
    output$years_plot <- renderPlot({
        
        years <- 1:as.integer(input$years)
        
        values <- lapply(years, function(i) {
            input[[paste0('input_value_', i)]]  
        }
        )
        
        
        df <- data.frame(Year = as.integer(years), Value = unlist(values))
        
        df %>%
            ggplot(mapping = aes(x = as.factor(Year), y = Value, group = 1)) +
            geom_line() +
            geom_point() +
            labs(x = "Year",
                 y = "Value",
                 title = "Reactive Plot") + 
            theme(plot.title = element_text(hjust = 0.5, face = "bold"))
        
    })
    
    
    
    
    
    
}

# RUN APP ----
shinyApp(ui = ui, server = server)