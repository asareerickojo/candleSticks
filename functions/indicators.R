
#functions to define candlestick
median_body <- function(open,close){
  return((open+close)/2)
}

upper_shadow <- function(high, open, close){
  return(high - pmax(open,close))
}

lower_shadow <- function(high, open, close){
  return(pmin(open, close) - low)
}

whole_body <- function(high, low){
  return(high - low)
}

candle_body <- function(open, close){
  return(abs(open-close))
}

small_body <- function(high, low, open, close) {
  whole <- high - low
  body <- abs(open-close)
  if(0.1 * whole > body){1}else{0}
}

large_body <- function(high, low, open, close) {
  whole <- high - low
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
  if(pmin(open,close) > pmax(lag(open, n=1L), lag(close, n=1L))){1}else{0}
}

gap_down <- function(open, close) {
  if(pmax(open,close) < pmin(lag(open, n=1L), lag(close, n=1L))){1}else{0}
}

#candleStick patterns
MSFT <- read.csv("MSFT.csv") %>%
  mutate(
    doji = if_else(small ==1, 1,0) # if candle has a small body (0.1 * whole > body, 1,0),
  )
view(MSFT)

doji_candle <- function(high, low, open, close){
  whole <- high - low
  body <- abs(open-close)
  if(0.1 * whole > body){1}else{0}
}

dragonfly_condle <- function(high, low, open, close){
  whole <- high - low
  body <- abs(open-close)
  upper <- high - pmax(open,close)
  
  if(0.1 * whole > body & 0.1*whole > upper){1}else{0}
}

gravestone_candle <- function(high, low, open, close){
  whole <- high - low
  body <- abs(open-close)
  lower <- pmin(open,close) - low
  
  if(0.1 * whole > body & 0.1*whole > lower){1}else{0}
}

#____________hammer
hammer_candle <- function(high, low, open, close){
  whole <- high - low
  body <- abs(open-close)
  upper <- high - pmax(open,close)
  lower <- pmin(open,close) - low
  
  if(0.1 * whole > upper & 0.7*whole < lower){1}else{0}
}

invertedhammer_candle <- function(high, low, open, close){
  whole <- high - low
  body <- abs(open-close)
  upper <- high - pmax(open,close)
  lower <- pmin(open,close) - low
  
  if(0.7 * whole < upper & 0.1*whole > lower){1}else{0}
}
#__________Engulfing patterns________
#*it is a 2 day pattern: real body of day 1 is with that if day 2


bullish_engulf <- function(down, up, open, close) {
  #*Bullish engulfing: bearish day 1; bullish day 2; real body day 1 within that of day 2
  #*more of a signal reversal esp when preceded by 4 or more bearish candles
  #*so dont look at only the two candles
  
  if(lag(down, n=1L) > 0 & up > 0 & open <= lag(close, n=1L) & close >= lag(open, n = 1L)){
    1}
    else{
    0}
}

bearish_engulf <- function(down, up, open, close) {
  #*Bullish engulfing: bullish day 1; bearish day 2; real body day 1 within that of day 2
  #*more of a signal reversal esp when preceded by 4 or more bearish candles
  #*so dont look at only the two candles
  
  if(lag(up, n=1L) > 0 & down > 0 & lag(open, n=1L) >= close & lag(close, n=1L) <= open){
    1}
  else{
    0}
}


#_____________________________Harami_________________________
#* 2 day pattern
#* mirrors engulfing: but real body of day2 completely contained in day1

bullish_harami <- function(down, up, open, close) {
  #*Bullish engulfing: large bearish day 1; small bullish day 2; real body day 2 within that of day 1
  #*more of a signal reversal 
  #*so dont look at only the two candles
  
  if(lag(down, n=1L) > 0 & up > 0 & open >= lag(close, n=1L) & close <= lag(open, n=1L)){
    1}
  else{
    0}
}

bearish_harami <- function(down, up, open, close) {
  #*Bearish engulfing: large bullish day 1; small bearish day 2; real body day 2 within that of day 1
  #*large candle moves in dxn of trend and small candle is like a doji
  #*more of a signal reversal
  #*so dont look at only the two candles
  
  if(lag(up, n=1L) > 0 & down > 0 & close >= lag(open, n=1L) & open <= lag(close, n=1L)){
    1}
  else{
    0}
}


#____________Mean reversal______________________________
#* 2 day pattern

piercing_line <- function(median, down, up, close, open) {
  #*bullish signal: bearish on day1; bullish on day2; close on day2 > midpoint of day 1 body
  
  if(lag(down, n=1L) > 0 & up > 0 & close > lag(median, n=1L)){
    1
  }else{
    0
  }
}


piercing_line <- function(median, down, up, close, open) {
  #*bullish signal: bearish on day1; bullish on day2; close on day2 > midpoint of day 1 body
  
  if(lag(down, n=1L) > 0 & up > 0 & close > lag(median, n=1L)){
    1
  }else{
    0
  }
}

dark_crowd <- function(median, down, up, close, open) {
  #*bearish signal: bullish on day1; bearish on day2; close on day2 < midpoint of day 1 body
  
  if(lag(up, n=1L) > 0 & down > 0 & close < lag(median, n=1L)){
    1
  }else{
    0
  }
}

#__________________Two in a row__________________________________________
#*2 day pattern
#*completely opp engulfing or harami patterns: real bodies are completely disjoint

kicking_up <- function(down, up, close, open){
  #* bullish signal: bearish day1; bullish day2; gap up between day1 and day2
  
  if(lag(down, n=1L) > 0 & up > 0 & open > lag(close, n=1L)){
    1
  }else{
    o
  }
}


kicking_down <- function(down, up, close, open){
  #* bearish signal: bullish day1; bearish day2; gap down between day1 and day2
  
  if(lag(up, n=1L) > 0 & down > 0 & open < lag(open, n=1L)){
    1
  }else{
    o
  }
}

#_______________Three in a row___________________________
#* three day pattern

threeWhiteSoldiers <- function(up, open, close){
  #*bullish signal: large bullish for 3 days
  #*open prices are rising for 3 days
  #*close prices are rising for 3 days
  
  if(up > 0   & lag(up, n=1L) > 0 & lag(up, n=2L) > 0 &          #bullish signal: large bullish for 3 days
     open > lag(open, n=1L) & lag(open, n=1L) > lag(open, n=2L) &      #open prices are rising for 3 days
     close > lag(close, n=1L) & lag(close, n=1L) > lag(close, n=2L)){   #close prices are rising for 3 days
    1
  }else{
    0
  }
}


threeBlackCrows <- function(up, open, close){
  #*Bearish signal: large bearish for 3 days
  #*open prices are dropping for 3 days
  #*close prices are dropping for 3 days
  
  if(down > 0   & lag(down, n=1L) > 0 & lag(down, n=2L) > 0 &          #bearish signal: large bullish for 3 days
     open < lag(open, n=1L) & lag(open, n=1L) < lag(open, n=2L) &      #open prices are dropping for 3 days
     close < lag(close, n=1L) & lag(close, n=1L) < lag(close, n=2L)){   #close prices are dropping for 3 days
    1
  }else{
    0
  }
}

#____________________________Star_____________________________________________
#three day pattern; day 2 features doji while day1 and day3 features large candle

morningStar <- function(up, down, close, open, doji, large_body, small_body){
  #*bullish signal
  #*large bearish day1, small candle day2, large bullish day3
  #*gap down between day1 and day2
  #*gap up between day 2 and day3

  if(lag(down, n=2L) > 0 & lag(large, n=2L) > 0 &       #large bearish candle in day 1
     lag(doji, n=1L) > 0 | lag(small, n=1L) > 0 &       # doji or small candle in day 2
     up > 0 & large_body > 0 &                          # large bullish candle in day3
     lag(open, n=1L) > lag(close, n=2L)  &              # gap down between day 1 and day 2
     open > lag(open, n=1L)                             # gap up between day 2 and day3 
  ){
    1
  }else{
    0
  }
}


eveningStar <- function(up, down, close, open, doji, large_body, small_body){
  #*bearish signal
  #*large bullish day1, small candle day2, large bearish day3
  #*gap up between day1 and day2
  #*gap down between day 2 and day3
  
  if(lag(up, n=2L) > 0 & lag(large, n=2L) > 0 &        #large bullish candle in day 1
        lag(doji, n=1L) > 0 | lag(small, n=1L) > 0 &   # doji or small candle in day 2
        down > 0 & large_body > 0 &                    # large bearish candle in day3
        lag(open, n=1L) > lag(open, n=2L)  &           # gap up between day 1 and day 2
        open < lag(open, n=1L)                         # gap down between day 2 and day3 
  ){1}else{
    0
  }
}

#_______________________Three Methods___________________________
# 5 day pattern

risingThree <- function(low, high, up, down, large_body, small_body){
  #*bullish signal
  #*large bullish d1, small bearish for next 3 days;large bullish on day 5
  #*day 1 low is lowest
  #*day 5 high is highest
  
  if(
    lag(up, n=4L) > 0 & lag(large_body, n=4L) > 0 &                                    #large bullish candle on day1
    lag(down, n =3L) > 0 & lag(small_body, n=3L) > 0 &                                 #small bearish candle on day 2 
    lag(down, n =2L) > 0 & lag(small_body, n=2L) > 0 &                                 #small bearish candle on day 3
    lag(down, n =1L) > 0 & lag(small_body, n=1L) > 0 &                                 #small bearish candle on day 4
    low < pmin(lag(low, n=1L), lag(low, n=2L), lag(low, n=3L), lag(low, n=4L)) &       # day 1 low is lowest
    lag(high, n=4L) > pmax(high, lag(high, n=1L), lag(high, n=2L), lag(high, n=3L))    # day 1 high is highest
  ){
    1
  }else{
    0
  }
}


fallingThree <- function(low, high, up, down, large_body, small_body){
  #*bearish signal
  #*large bearish d1, small bullish for next 3 days;large bearish on day 5
  #*day 1 high is highest
  #*day 5 low is lowest
  
  if(
    lag(down, n=4L) > 0 & lag(large_body, n=4L) > 0 &                                  #large bearish candle on day1
    lag(up, n =3L) > 0 & lag(small_body, n=3L) > 0 &                                 #small bullish candle on day 2 
    lag(up, n =2L) > 0 & lag(small_body, n=2L) > 0 &                                 #small bullish candle on day 3
    lag(up, n =1L) > 0 & lag(small_body, n=1L) > 0 &                                 #small bullish candle on day 4
    high < pmax(lag(high, n=1L), lag(high, n=2L), lag(high, n=3L), lag(high, n=4L)) &       # day 1 high is highest
    lag(low, n=4L) > pmin(low, lag(low, n=1L), lag(low, n=2L), lag(low, n=3L))    # day 5 low is lowest
  ){
    1
  }else{
    0
  }
}
  
  
  
  
  
  
  
  
  
  
  
















