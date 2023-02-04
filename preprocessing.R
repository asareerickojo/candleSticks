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

# Initiate the bot session using the token from the environment variable.
bot = Bot(token = bot_token('arbot_bot'))

#alert
library(telegram.bot)
# Initiate the bot session using the token from the environment variable.
bot = Bot(token = bot_token('arbot_bot'))

stock_price_check = function(){
  
  # setting up a counter. 
  i = 0 

  while(i < 2){ 
    
    # Step 1. Fetching stock price
    df <- stock_data(sticker = "TSLL", start_date = "2022-08-09")
    df <- tail(df, 1)
    
    # Stp 3. check if there is a hammer pattern
    df <- df %>% dplyr::filter(hammer==1 | inverted_hammer==1)
    
    #if number of row is grt 0 then the pattern exist
    if(nrow(df) > 0){
      # Send message if hammer exit
      message=sprintf('hammer or inverted hammer pattern', 0.99)
      send_msg(text = message)
      #message(msg)
    }
    
    # Step 5. Waiting for certain interval (60 seconds)
    Sys.sleep(60)
    
    # Step Drop: Incrementing counter
    i = i + 1 # comment out this line when you run. I had to put an increment to make sure the while loop stops after 4 execusions
  }
}


stock_price_check()

