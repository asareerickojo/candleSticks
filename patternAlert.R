#https://bookdown.org/kochiuyu/technical-analysis-with-r-second-edition/engulfing-patterns.html
#___________________Setting up environment_________________
library(renv)
init()
snapshot()

#______________________packages________________________
library(quantmod)
library(tidyverse)
library(tidyquant)   #https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ02-quant-integrations-in-tidyquant.html
library(telegram.bot)

#sourcing custom functions
source("functions/functions.R")    #got to put all candle stick pattern functions into a package

#alert
library(telegram.bot)

# Initiate the bot session using the token from the environment variable.
bot = Bot(token = bot_token('arbot_bot'))

#check for pattern and send message to Telegram
stock_price_check(stock ="TSLL", from = "2022-08-09")

