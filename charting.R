#______________________packages________________________
library(quantmod)
library(tidyverse)
library(tidyquant)   #https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ02-quant-integrations-in-tidyquant.html

#sourcing custom functions
source("functions/functions.R")    #got to put all candle stick pattern functions into a package

#_______________________data_______________________
tsll <- stock_data(sticker = "TSLL", start_date = "2022-08-09")

#graphing
tq_get("TSLL", from = "2022-08-09") %>%
 ggplot(aes(x = date, y = close)) +
 geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
 labs(title = "AAPL Candlestick Chart", y = "Closing Price", x = "") +
 theme_tq()