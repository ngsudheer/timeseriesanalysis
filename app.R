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
library(ggplot2)
library(tsibbledata)
library(forecast)
library(stringr)
options(shiny.maxRequestSize = 30*1024^2)

state_data <- readr::read_csv("clean_state.csv")
new_state_data_ts <- state_data %>% mutate(Month = yearmonth(month_date_yyyymm))
new_state_data_ts <- as_tsibble(new_state_data_ts, index = 'Month', key = 'state')


# Define UI for application that draws a histogram
ui <- shinybootstrap2::withBootstrap2({fluidPage(
  
  # Application title
  titlePanel("Forecasting Prices of Homes on state level"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select State:", choices = unique(as.character(sort(state_data$state))))
     
    ),
    # Show a plot of the generated distribution
    sidebarPanel(
      selectInput("month", "Choose a month:",
                  c("1 Month" = 2,
                    "3 Month" =3,
                    "6 Month" =6,
                    "9 Month" =9,
                    "1 Year" =12,
                    "2 Year" =24,
                    "3 Year" =36)),
    ),
    
   
   
  ),
  textOutput("value"),
  textOutput("result"),
  tabsetPanel(
              tabPanel("Seasonal Plot", plotOutput(outputId = "final_cast")),
              tabPanel("Forecasts", plotOutput(outputId = "arima")),
              
  ),
  
  
)})

# Define server logic required to draw a histogram
server <- function(input, output) {
 
  output$result <- renderText({
    paste("You selected", tolower(input$state))
  })
  month_selected = 12
  output$final_cast <- renderPlot({
    price_increases <- ts(state_data["price_increased_count"], frequency = as.numeric(input$month), start=c(2017,01,01), end=c(2021,12,1))
    
    forecast::ggseasonplot(price_increases, year.labels=TRUE, year.labels.left=TRUE) +
      ylab("price_increased_count") +
      ggtitle("Seasonal plot:increase in price")
  })
 
 
  
 
  
  output$arima <- renderPlot({
    fit <- new_state_data_ts %>%
      filter(state== input$state) %>%
      model(ARIMA(median_listing_price ~ 1 + pdq(1, 1, 0)))
    #fit %>% ggseasonplot()
    print(input$state)
    ca_state_data_ts <- new_state_data_ts %>% filter(state == tolower(input$state))
    print(input$state)
    output$text4 <- renderText({ 
      paste(input$state)
    })
    new_prices <- scenarios(
      "data" = new_data(ca_state_data_ts, n=as.numeric(input$month))
    )
    forecast_price <- forecast(fit, new_data = new_prices)
    ca_state_data_ts %>%
      autoplot(median_listing_price) +
      autolayer(forecast_price) +
      labs(title = 'Forecast price for next year', y = 'Median Price')
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
