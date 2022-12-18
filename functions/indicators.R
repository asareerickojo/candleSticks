
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
  if(pmin(open,close) > pmax(lag(open), lag(close))){1}else{0}
}

gap_down <- function(open, close) {
  if(pmax(open,close) < pmin(lag(open), lag(close))){1}else{0}
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
