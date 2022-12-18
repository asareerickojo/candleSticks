
#____________________________Doji__________________________________________________
#* It is a one day pattern: shows that open and close are virtually equal
#* signals indecision: Three types
#* 1. Doji or long legged doji
#* characteristics: 1. long candle stick (H>>L) and 2. narrow real body(close ~ open)

MSFT <- read.csv("MSFT.csv") %>%
  mutate(
    doji = if_else(small ==1, 1,0) # if candle has a small body (0.1 * whole > body, 1,0),
  )
view(MSFT)

#__________________________Dragonfly_______________________________________________
#* Dragonfly: bullish signal i.e resists downward pressure
#* 1. long candle stick and narrow real body (doji)
#* 2. short upper shadow (high ~close ~ open)

MSFT <- MSFT %>%
  mutate(
    dragonfly = if_else(doji ==1 & 0.1 * whole > upper, 1,0) 
  )

#_________________________Gravestone_______________________________________________
#* Dragonfly: bearish signal i.e resists upward pressure
#* 1. long candle stick and narrow real body (doji)
#* 2. short lower shadow (low ~close ~ open)

MSFT <- MSFT %>%
  mutate(
    gravestone = if_else(doji ==1 & 0.1 * whole > lower, 1,0)
  )





