library(shiny)

shinyServer(function(input, output) {
  
  source("pattern_identifier.R")
  # stocklist <- read.csv(url("http://www.nseindia.com/content/equities/EQUITY_L.csv"))$SYMBOL
  output$breakoutTable <- renderDataTable(breakoutMaker(input$stockSymbol)[c("timestamp", "open_high", "open_low", "forecast_horizon_high", "forecast_horizon_low", "pass")])

  
})
