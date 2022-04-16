#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(fpp3)
library(tsibble)
library(readr)
library(tidyverse)
library(plotly)
library(magrittr)
library(DT)
library(shiny)
library(shinyWidgets)

options(shiny.maxRequestSize = 30*1024^2)

state_data <- readr::read_csv("clean_state.csv")



# Define UI for application that draws a histogram
ui <- shinybootstrap2::withBootstrap2({fluidPage(
  
  # Application title
  titlePanel("Forecasting Prices of Homes on state level"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select State:", choices = unique(as.character(state_data$state)))
     
    ),
    # Show a plot of the generated distribution
    sidebarPanel(
      airMonthpickerInput(
        inputId = "date",
        label = "Select Month:"
      ),
    ),
  )
)})

# Define server logic required to draw a histogram
server <- function(input, output) {
  getDataset <- reactive({
    return(state_data)
    
  })
  output$dcompPlot <- renderPlot({
    ds_ts <- STL(state_data)
    autoplot(ds_ts)
  })
  observeEvent(
    input$allInput,
    updateSelectInput(session,
                      inputId = "stateInput",
                      'State',
                      choices = sort(unique(state_data$state)),
                      selected = c(unique(state_data$state))),
    ignoreInit = T
  )
  filtered <- reactive({
    if (input$allInput){
      state_data %>% 
        filter(month_date_yyyymm >= input$yearInput[1],
               month_date_yyyymm <= input$yearInput[2]
        )
    } else {
      state_data %>% 
        filter(month_date_yyyymm >= input$yearInput[1],
               month_date_yyyymm <= input$yearInput[2],
               region %in% input$stateInput)
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
