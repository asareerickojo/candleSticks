#https://bookdown.org/kochiuyu/technical-analysis-with-r-second-edition/engulfing-patterns.html
#___________________Setting up environment_________________
library(renv)
init()
snapshot()

#______________________packages________________________
library(quantmod)
library(tidyverse)
library(tidyquant)   #https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ02-quant-integrations-in-tidyquant.html

#_______________________data_______________________
getSymbols("MSFT")
MSFT <- tq_get("MSFT") %>% column_to_rownames(var = "date") %>%
  mutate(
    #characterising a candle stick
    median = (open+close)/2,
    upper = high - pmax(open,close),
    lower = pmin(open, close) - low,
    whole = high - low,
    body = abs(open - close),
    
    #identifying size of a candle stick
    small = if_else(0.1 * whole > body, 1,0),   #small candle is when 10% of the length whole gt body length
    large = if_else(0.9 * whole < body, 1,0),
    
    #movement: is it trending up or down?
    up = if_else(close > open, 1,0),
    down = if_else(close < open, 1,0),
    
    #measure how fast price moves: use inter-day gap
    gap_up = if_else(pmin(open,close) > pmax(lag(open), lag(close)), 1,0), #candle body day 2 higher than day 1
    gap_down = if_else(pmax(open,close) < pmin(lag(open), lag(close)), 1,0)  #candle body day 2 lower than day 1
    
    #can use SMA or EMA to capture longterm trend
  )
head(MSFT)
view(MSFT %>% select(open, close, up, gap_up, down, gap_down))

write.csv(MSFT, "MSFT.csv")







