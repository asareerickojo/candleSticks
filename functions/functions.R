
#functions to define candlestick
median_body <- function(open,close){
  return((open+close)/2)
}

upper_shadow <- function(high, open, close){
  return(abs(high - pmax(open,close)))
}

lower_shadow <- function(low, open, close){
  return(abs(pmin(open, close) - low))
}

whole_body <- function(high, low){
  return(abs(high - low))
}

candle_body <- function(open, close){
  return(abs(open-close))
}

small_body <- function(high, low, open, close) {
  whole <- abs(high - low)
  body <- abs(open-close)
  if(0.1 * whole > body){1}else{0}
}

large_body <- function(high, low, open, close) {
  whole <- abs(high - low)
  body <- abs(open-close)
  if(0.9 * whole < body){1}else{0}
}

up_trend <- function(open, close) {
  if(close > open){1}else{0}
}

down_trend <- function(open, close) {
  if(close < open){1}else{0}
}

gap_up <- function(open, close) {
  if(isTRUE(pmin(open,close) > pmax(lag(open, n=1L), lag(close, n=1L))) == TRUE) {1}else{0}
}

gap_down <- function(open, close) {
  if(isTRUE(pmax(open,close) < pmin(lag(open, n=1L), lag(close, n=1L))) == TRUE){1}else{0}
}

#candleStick patterns
doji_candle <- function(high, low, open, close){
  whole <- abs(high - low)
  body <- abs(open-close)
  if(isTRUE(0.1 * whole > body) == TRUE){1}else{0}
}

dragonfly_candle <- function(high, low, open, close){
  whole <- abs(high - low)
  body <- abs(open-close)
  upper <- abs(high - pmax(open,close))
  
  if(isTRUE(0.1 * whole > body & 0.1*whole > upper) == TRUE){1}else{0}
}

gravestone_candle <- function(high, low, open, close){
  whole <- abs(high - low)
  body <- abs(open-close)
  lower <- abs(pmin(open,close) - low)
  
  if(isTRUE(0.1 * whole > body & 0.1*whole > lower) == TRUE){1}else{0}
}

#____________hammer
hammer_candle <- function(high, low, open, close){
  whole <- abs(high - low)
  body <- abs(open-close)
  upper <- abs(high - pmax(open,close))
  lower <- abs(pmin(open,close) - low)
  
  if(isTRUE(0.1 * whole > upper & 0.7*whole < lower) == TRUE){1}else{0}
}

invertedhammer_candle <- function(high, low, open, close){
  whole <- abs(high - low)
  body <- abs(open-close)
  upper <- abs(high - pmax(open,close))
  lower <- abs(pmin(open,close) - low)
  
  if(isTRUE(0.7 * whole < upper & 0.1*whole > lower) == TRUE){1}else{0}
}
#__________Engulfing patterns________
#*it is a 2 day pattern: real body of day 1 is with that if day 2


bullish_engulf <- function(down, up, open, close) {
  #*Bullish engulfing: bearish day 1; bullish day 2; real body day 1 within that of day 2
  #*more of a signal reversal esp when preceded by 4 or more bearish candles
  #*so dont look at only the two candles
  
  if(isTRUE(lag(down, n=1L) > 0 & up > 0 & open <= lag(close, n=1L) & close >= lag(open, n = 1L)) == TRUE){1}else{0}
}

bearish_engulf <- function(down, up, open, close) {
  #*Bullish engulfing: bullish day 1; bearish day 2; real body day 1 within that of day 2
  #*more of a signal reversal esp when preceded by 4 or more bearish candles
  #*so dont look at only the two candles
  
  if(isTRUE(lag(up, n=1L) > 0 & down > 0 & lag(open, n=1L) >= close & lag(close, n=1L) <= open) == TRUE){1}else{0}
}


#_____________________________Harami_________________________
#* 2 day pattern
#* mirrors engulfing: but real body of day2 completely contained in day1

bullish_harami <- function(down, up, open, close) {
  #*Bullish engulfing: large bearish day 1; small bullish day 2; real body day 2 within that of day 1
  #*more of a signal reversal 
  #*so dont look at only the two candles
  
  if(isTRUE(lag(down, n=1L) > 0 & up > 0 & open >= lag(close, n=1L) & close <= lag(open, n=1L))==TRUE){1}else{0}
}

bearish_harami <- function(down, up, open, close) {
  #*Bearish engulfing: large bullish day 1; small bearish day 2; real body day 2 within that of day 1
  #*large candle moves in dxn of trend and small candle is like a doji
  #*more of a signal reversal
  #*so dont look at only the two candles
  
  if(isTRUE(lag(up, n=1L) > 0 & down > 0 & close >= lag(open, n=1L) & open <= lag(close, n=1L)) ==TRUE){1}else{0}
}


#____________Mean reversal______________________________
#* 2 day pattern

piercing_line <- function(median, down, up, close, open) {
  #*bullish signal: bearish on day1; bullish on day2; close on day2 > midpoint of day 1 body
  
  if(isTRUE(lag(down, n=1L) > 0 & up > 0 & close > lag(median, n=1L))==TRUE){1}else{0}
}

dark_cloud <- function(median, down, up, close, open) {
  #*bearish signal: bullish on day1; bearish on day2; close on day2 < midpoint of day 1 body
  
  if(isTRUE(lag(up, n=1L) > 0 & down > 0 & close < lag(median, n=1L))==TRUE){1}else{0}
}

#__________________Two in a row__________________________________________
#*2 day pattern
#*completely opp engulfing or harami patterns: real bodies are completely disjoint

kicking_up <- function(down, up, close, open){
  #* bullish signal: bearish day1; bullish day2; gap up between day1 and day2
  
  if(isTRUE(lag(down, n=1L) > 0 & up > 0 & open > lag(close, n=1L))==TRUE){1}else{0}
}


kicking_down <- function(down, up, close, open){
  #* bearish signal: bullish day1; bearish day2; gap down between day1 and day2
  
  if(isTRUE(lag(up, n=1L) > 0 & down > 0 & open < lag(open, n=1L))==TRUE){1}else{0}
}

#_______________Three in a row___________________________
#* three day pattern

threeWhiteSoldiers <- function(up, open, close){
  #*bullish signal: large bullish for 3 days
  #*open prices are rising for 3 days
  #*close prices are rising for 3 days
  
  if(isTRUE(up > 0   & lag(up, n=1L) > 0 & lag(up, n=2L) > 0 &          #bullish signal: large bullish for 3 days
     open > lag(open, n=1L) & lag(open, n=1L) > lag(open, n=2L) &      #open prices are rising for 3 days
     close > lag(close, n=1L) & lag(close, n=1L) > lag(close, n=2L))==TRUE){   #close prices are rising for 3 days
    1}else{0}
}

threeBlackCrows <- function(down, open, close){
  #*Bearish signal: large bearish for 3 days
  #*open prices are dropping for 3 days
  #*close prices are dropping for 3 days
  
  if(isTRUE(down > 0   & lag(down, n=1L) > 0 & lag(down, n=2L) > 0 &          #bearish signal: large bullish for 3 days
     open < lag(open, n=1L) & lag(open, n=1L) < lag(open, n=2L) &      #open prices are dropping for 3 days
     close < lag(close, n=1L) & lag(close, n=1L) < lag(close, n=2L))==TRUE){   #close prices are dropping for 3 days
    1}else{0}
}

#____________________________Star_____________________________________________
#three day pattern; day 2 features doji while day1 and day3 features large candle

morningStar <- function(up, down, close, open, doji, large_body, small_body){
  #*bullish signal
  #*large bearish day1, small candle day2, large bullish day3
  #*gap down between day1 and day2
  #*gap up between day 2 and day3
  
  if(isTRUE(lag(down, n=2L) > 0 & lag(large_body, n=2L) > 0 &       #large bearish candle in day 1
     lag(doji, n=1L) > 0 | lag(small_body, n=1L) > 0 &       # doji or small candle in day 2
     up > 0 & large_body > 0 &                          # large bullish candle in day3
     lag(open, n=1L) > lag(close, n=2L)  &              # gap down between day 1 and day 2
     open > lag(open, n=1L)                             # gap up between day 2 and day3 
  )==TRUE){1}else{0}
}


eveningStar <- function(up, down, close, open, doji, large_body, small_body){
  #*bearish signal
  #*large bullish day1, small candle day2, large bearish day3
  #*gap up between day1 and day2
  #*gap down between day 2 and day3
  
  if(isTRUE(lag(up, n=2L) > 0 & lag(large_body, n=2L) > 0 &        #large bullish candle in day 1
     lag(doji, n=1L) > 0 | lag(small_body, n=1L) > 0 &   # doji or small candle in day 2
     down > 0 & large_body > 0 &                    # large bearish candle in day3
     lag(open, n=1L) > lag(open, n=2L)  &           # gap up between day 1 and day 2
     open < lag(open, n=1L)                         # gap down between day 2 and day3 
  )==TRUE){1}else{0}
}

#_______________________Three Methods___________________________
# 5 day pattern

risingThree <- function(low, high, up, down, large_body, small_body){
  #*bullish signal
  #*large bullish d1, small bearish for next 3 days;large bullish on day 5
  #*day 1 low is lowest
  #*day 5 high is highest
  
  if(isTRUE(lag(up, n=4L) > 0 & lag(large_body, n=4L) > 0 &                                    #large bullish candle on day1
    lag(down, n =3L) > 0 & lag(small_body, n=3L) > 0 &                                 #small bearish candle on day 2 
    lag(down, n =2L) > 0 & lag(small_body, n=2L) > 0 &                                 #small bearish candle on day 3
    lag(down, n =1L) > 0 & lag(small_body, n=1L) > 0 &                                 #small bearish candle on day 4
    low < pmin(lag(low, n=1L), lag(low, n=2L), lag(low, n=3L), lag(low, n=4L)) &       # day 1 low is lowest
    lag(high, n=4L) > pmax(high, lag(high, n=1L), lag(high, n=2L), lag(high, n=3L))    # day 1 high is highest
  )==TRUE){1}else{0}
}


fallingThree <- function(low, high, up, down, large_body, small_body){
  #*bearish signal
  #*large bearish d1, small bullish for next 3 days;large bearish on day 5
  #*day 1 high is highest
  #*day 5 low is lowest
  
  if(isTRUE(lag(down, n=4L) > 0 & lag(large_body, n=4L) > 0 &                                  #large bearish candle on day1
    lag(up, n =3L) > 0 & lag(small_body, n=3L) > 0 &                                 #small bullish candle on day 2 
    lag(up, n =2L) > 0 & lag(small_body, n=2L) > 0 &                                 #small bullish candle on day 3
    lag(up, n =1L) > 0 & lag(small_body, n=1L) > 0 &                                 #small bullish candle on day 4
    high < pmax(lag(high, n=1L), lag(high, n=2L), lag(high, n=3L), lag(high, n=4L)) &       # day 1 high is highest
    lag(low, n=4L) > pmin(low, lag(low, n=1L), lag(low, n=2L), lag(low, n=3L))    # day 5 low is lowest
  )==TRUE){1}else{0}
}


# all together: data gathering function
## Get stock prices for multiple stocks
#mult_stocks <- tq_get(c("FB", "AMZN"),
#  get  = "stock.prices",
# from = "2016-01-01",
# to   = "2017-01-01")
stock_data <- function(sticker, start_date){
  
  df <- tq_get(sticker, from = start_date) %>% column_to_rownames(var = "date") %>%
    mutate(
      #characterizing a candle stick
      median = as.numeric(unlist(pmap_dbl(list(open, close), median_body))),
      upper = as.numeric(unlist(pmap_dbl(list(high, open, close), upper_shadow))),
      lower = as.numeric(unlist(pmap_dbl(list(high, open, close), lower_shadow))),
      whole_body = as.numeric(unlist(pmap_dbl(list(low, high), whole_body))),
      candle_body = as.numeric(unlist(pmap_dbl(list(open, close), candle_body))),
      
      #identifying size of a candle stick
      small_body = as.numeric(unlist(pmap_dbl(list(high, low, open, close), small_body))),  
      large_body = as.numeric(unlist(pmap_dbl(list(high, low, open, close), large_body))),
      
      #movement: is it trending up or down?
      up = as.numeric(unlist(pmap_dbl(list(open, close), up_trend))),
      down = as.numeric(unlist(pmap_dbl(list(open, close), down_trend))),
      
      #measure how fast price moves: use inter-day gap
      gap_up = as.numeric(unlist(pmap_dbl(list(open, close), gap_up))), #candle body day 2 higher than day 1
      gap_down = as.numeric(unlist(pmap_dbl(list(open, close), gap_down))), #candle body day 2 lower than day 1
      
      #_______________candle stick patterns
      #a. One day candle patterns
      doji = as.numeric(unlist(pmap_dbl(list(high, low, open, close), doji_candle))), 
      dragonfly = as.numeric(unlist(pmap_dbl(list(high, low, open, close), dragonfly_candle))),
      gravestone = as.numeric(unlist(pmap_dbl(list(high, low, open, close), gravestone_candle))), 
      hammer = as.numeric(unlist(pmap_dbl(list(high, low, open, close), hammer_candle))), 
      inverted_hammer = as.numeric(unlist(pmap_dbl(list(high, low, open, close), invertedhammer_candle))),
      
      # 2-day patterns
      bullish_engulfing = as.numeric(unlist(pmap_dbl(list(down, up, open, close), bullish_engulf))), 
      bearish_engulfing = as.numeric(unlist(pmap_dbl(list(down, up, open, close), bearish_engulf))), 
      bullish_harami = as.numeric(unlist(pmap_dbl(list(down, up, open, close), bullish_harami))), 
      bearish_harami = as.numeric(unlist(pmap_dbl(list(down, up, open, close), bearish_harami))), 
      piercing_line = as.numeric(unlist(pmap_dbl(list(median, down, up, open, close), piercing_line))), 
      dark_cloud = as.numeric(unlist(pmap_dbl(list(median, down, up, open, close), dark_cloud))), 
      kicking_up = as.numeric(unlist(pmap_dbl(list(down, up, open, close), kicking_up))), 
      kicking_down = as.numeric(unlist(pmap_dbl(list(down, up, open, close), kicking_down))), 
      
      #3-days patterns
      three_white_soldiers = as.numeric(unlist(pmap_dbl(list(up, open, close), threeWhiteSoldiers))), 
      three_black_crows = as.numeric(unlist(pmap_dbl(list(up, open, close), threeBlackCrows))), 
      morning_star = as.numeric(unlist(pmap_dbl(list(up, down, close, open, doji, large_body, small_body),
                                                morningStar))), 
      evening_star = as.numeric(unlist(pmap_dbl(list(up, down, close, open, doji, large_body, small_body),
                                                eveningStar))),
      
      #5-day patterns
      falling_three = as.numeric(unlist(pmap_dbl(list(low, high, up, down, large_body, small_body), 
                                                 fallingThree))),
      rising_three = as.numeric(unlist(pmap_dbl(list(low, high, up, down, large_body, small_body), 
                                                risingThree)))
    )
  
  return(df)
  
}


#messaging function
send_msg = function(text){
  bot$sendMessage(
    chat_id = 5631402921, 
    text = text
  )
}


stock_price_check = function(stock, from){
  
  # setting up a counter. 
  i = 0 
  
  while(i < 2){ 
    
    # Step 1. Fetching stock price
    df <- stock_data(sticker = stock, start_date = from)
    df <- tail(df, 1)
    # Stp 3. check if there is a hammer pattern
    df <- df %>% dplyr::filter(hammer==1 | inverted_hammer==1 | bullish_engulfing ==1 |
                              bearish_engulfing ==1 | bullish_harami ==1 | bearish_harami==1|piercing_line==1 |
                              dark_cloud ==1|kicking_up ==1|kicking_down==1|three_white_soldiers==1 |
                              three_black_crows ==1| morning_star==1|evening_star==1|falling_three==1|rising_three==1)
    #if number of row is grt 0 then the pattern exist
    if(nrow(df) > 0){
      # Send message if hammer exit
      message=sprintf('trading opportunity for %01s', stock)
      send_msg(text = message)
      #message(msg)
    }
    
    # Step 5. Waiting for certain interval (60 seconds)
    Sys.sleep(60)
    
    # Step Drop
    i = i + 1 
  }
}
