#all stocks
source("pattern_identifier.R")
stocklist <- read.csv(url("http://www.nseindia.com/content/equities/EQUITY_L.csv"))$SYMBOL
breakouts <- NULL

for(i in 1:length(stocklist)) {
  
  breakouts <- rbind(breakouts, breakoutMaker(stocklist[i]))
  
}

breakout_summary <-
  breakouts %>% 
  group_by(stock_symbol) %>% 
  summarise(occurences = n(),
            success = sum(pass),
            accuracy = success/occurences) %>% 
  arrange(desc(success))
