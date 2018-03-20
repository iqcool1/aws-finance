#packages
list.of.packages <- c( "dplyr","shiny")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, require, character.only = TRUE)



#API
function_name <- "TIME_SERIES_INTRADAY"
stock_ticker <- "BANKNIFTY"
period <- "15min"
my_api_key <- "JIHFA9VL858HGIF4"
my_data_type <- "csv"
my_outputsize <- "full"

api_call <- paste0("https://www.alphavantage.co/query?function=", 
                   function_name, 
                   "&symbol=", 
                   stock_ticker, 
                   "&interval=", 
                   period, 
                   "&apikey=", 
                   my_api_key, 
                   "&datatype=", 
                   my_data_type,
                   "&outputsize=",
                   my_outputsize)

df <- read.csv(url(api_call))
df$timestamp <-  as.POSIXct(as.character(df$timestamp))
df$timestamp <- df$timestamp + (9*60*60) + (30*60)


#constants
runtime <- Sys.time()

open_interval <- as.POSIXct(as.character(as.Date(runtime))) + (9*60*60) + (15*60)
close_interval <- open_interval + (6*60*60) + (15*60)
current_interval <- as.POSIXct(as.character(runtime)) - (as.numeric(as.POSIXct(as.character(runtime))) %% (15*60))
previous_interval <- current_interval - (15*60)