#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(plotly)
library(tidytuesdayR)

# tt <- tt_load("2021-10-05")
# 
# nurses <- tt$nurses %>% 
#   clean_names() 

unique_states <- fct_reorder(nurses$state,
                             nurses$total_employed_rn,
                             .desc = TRUE,
                             na.rm = TRUE) %>%
                 levels()



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Registered Nurses"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("states",
                          "States:",
                          choices = unique_states ,
                          selected = c("New York", "California",
                                       "Texas", "Pennsylvania",
                                       "Washington"),
                          multiple = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("wages_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$wages_plot <- renderPlotly({
      req(input$states)
        # generate bins based on input$bins from ui.R
      g <- nurses %>% 
        filter(state %in% input$states) %>% 
        filter(!is.na(hourly_wage_median)) %>% 
        ggplot(aes(year, hourly_wage_median, color = state)) +
        geom_line() +
        expand_limits(y = 0) +
        scale_y_continuous(labels = dollar_format()) 
      
      ggplotly(g)
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
