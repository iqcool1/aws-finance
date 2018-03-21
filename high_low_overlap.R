source("data_extract_google.R")
#inputs = df, api_interval, forecast_horizon

patterns <-
  df %>% 
  arrange(timestamp) %>% 
  filter(as.numeric(timestamp) %% (24*60*60) != (9*60*60 + 15*60) - (5*60*60 + 30*60)) %>% 
  group_by(date = as.Date(timestamp)) %>% 
  mutate(open_high = ifelse(timestamp == min(timestamp), high, "X"),
         open_low = ifelse(timestamp == min(timestamp), low, "X")
         )
for(i in 1:nrow(patterns)){
  if(patterns[i, "open_high"] == "X") {
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
