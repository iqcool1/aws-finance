library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Open High Low Pattern"),
  
  # Sidebar with input for symbol
  sidebarLayout(
    sidebarPanel(
      selectInput("stockSymbol",
                "Enter Stock Symbol",
                read.csv(url("http://www.nseindia.com/content/equities/EQUITY_L.csv"))$SYMBOL,
                selected = NULL,
                selectize = T
                ),
      submitButton("Submit")
    ),
    
    # Show a table of the breakouts and successes
    mainPanel(
      dataTableOutput("breakoutTable")
    )
  )
))