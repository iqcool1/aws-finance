#packages
list.of.packages <- c( "dplyr","httr", "zoo", "readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org", lib = "/usr/local/lib/R/site-library")
lapply(list.of.packages, require, character.only = TRUE)


breakoutMaker <- function(api_stock_symbol) {
  
  # initialisation.R
  api_interval <- 15*60
  api_history <- "180d"
  # api_stock_symbol <- "YESBANK"
  api_exchange <- "NSE"
  forecast_horizon <- 8
  
  # data_extract_google.R
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
  
  # high_low_overlap
  patterns <-
    df %>% 
    arrange(timestamp) %>% 
    filter(as.numeric(timestamp) %% (24*60*60) != (9*60*60 + 15*60) - (5*60*60 + 30*60)) %>% 
    group_by(date = as.Date(timestamp)) %>% 
    mutate(open_high = ifelse(timestamp == min(timestamp), high, -1),
           open_low = ifelse(timestamp == min(timestamp), low,-1)
    )
  for(i in 1:nrow(patterns)){
    if(patterns[i, "open_high"] == -1) {
      patterns[i, "open_high"] <- patterns[i-1, "open_high"]
      patterns[i, "open_low"] <- patterns[i-1, "open_low"]
    } 
  }
  
  patterns <-
    patterns %>% 
    group_by(date) %>% 
    mutate(open_high = as.numeric(open_high),
           open_low = as.numeric(open_low),
           overlap_flag = ifelse(high > open_high & low < open_low & open_high != open_low, 1, 0),
           pattern_flag = ifelse(lag(overlap_flag) == 1 & (lag(high) < high | lag(low) > low), 1, 0),
           pattern_type = ifelse(pattern_flag == 1 & lag(high) < high, "H", ifelse(pattern_flag == 1 & lag(low) > low, "L", "N"))) %>% 
    arrange(desc(timestamp)) %>%  
    as.data.frame(stringsAsFactors=F) %>% 
    mutate(forecast_horizon_high = rollapplyr(high, forecast_horizon*(6.25*60*60)/api_interval, max, fill = "NA", partial = T),
           forecast_horizon_low = rollapplyr(low, forecast_horizon*(6.25*60*60)/api_interval, min, fill = "NA", partial = T)) %>%  
    arrange(timestamp)
  
  breakouts <- 
    patterns %>% 
    filter(pattern_type %in% c("H", "L")) %>% 
    mutate(pass = ifelse((pattern_type == "H" & forecast_horizon_high >= high + 2*(open_high-open_low))
                         | (pattern_type == "L" & forecast_horizon_low <= low - 2*(open_high-open_low)), 1, 0),
           stock_symbol = api_stock_symbol)
  
  return(breakouts)
  
  
  
}