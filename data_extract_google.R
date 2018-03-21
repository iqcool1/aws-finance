#packages
list.of.packages <- c( "dplyr","httr", "zoo", "readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, require, character.only = TRUE)



#API
source("intialisation.R")
response<-httr::GET(paste0("https://www.google.com/finance/getprices?f=d%2Cc%2Ch%2Cl%2Co%2Cv",
                           "&i=", api_interval,
                           "&p=", api_history,
                           "&q=", api_stock_symbol,
                           "&x=", api_exchange))
response_data <- content(response, type = "text/csv", skip = 4)
response_data <- response_data[-c(1,2),]
names(response_data)[1] <- "response_date"
names(response_data) <- tolower(names(response_data))
df <- 
  response_data %>% 
  mutate(timestamp = ifelse(substr(response_date,1,1)=="a",
                            as.POSIXct(as.numeric(substr(response_date,2,20)), origin = "1970-01-01"),
                            response_date
                            )) #%>% 
for(i in 1:nrow(df)){
  df[i, "timestamp"] <- ifelse(substr(df[i, "response_date"],1,1)=="a",
                               df[i, "timestamp"],
                               as.numeric(df[i-1, "timestamp"])# + (as.numeric(df[i, "response_date"]) * api_interval)
                               )
}

df$timestamp <- ifelse(substr(df$response_date,1,1)=="a",
                       as.numeric(df$timestamp),
                       as.numeric(df$timestamp) + (as.numeric(df$response_date) * api_interval)
                       )
                       
df$timestamp <- as.POSIXct(as.numeric(df$timestamp), origin = "1970-01-01")
df <- 
  df %>%
  mutate(open = as.numeric(open),
         high = as.numeric(high),
         low = as.numeric(low),
         close = as.numeric(close)
         ) %>% 
  select(timestamp, open, high, low, close, volume)


# constants
# runtime <- Sys.time()
# 
# open_interval <- as.POSIXct(as.character(as.Date(runtime))) + (9*60*60) + (15*60)
# close_interval <- open_interval + (6*60*60) + (15*60)
# current_interval <- as.POSIXct(as.character(runtime)) - (as.numeric(as.POSIXct(as.character(runtime))) %% (15*60))
# previous_interval <- current_interval - (15*60)