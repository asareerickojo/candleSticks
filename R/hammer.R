
#* it is a 1 day pattern
#* similar to dragonfly and gravestone but real body not too short. 
#* often needs a confirmation: depending on bullish or bearish the subsequent candle must close above closing price of hammer 
#* Types

#________________________Hammer and hang man___________________________________
#* dragonfly style hammer: short upper shadow and long lower shadow
#* it is a reversal signal
#* hammer: bullish in a downside  
#* hangman: bearish in am upside

MSFT <- MSFT %>%
  mutate(
    hammer = if_else(0.1 * whole > upper & 0.7*whole < lower, 1,0)
  )


#________________________Inverted Hammer and Shooting Stars___________________________________
#* gravestone style hammer: short lower shadow and long upper shadow
#* it is a reversal signal
#* inverted hammer: bullish in a downside  
#* shooting: bearish in am upside

MSFT <- MSFT %>%
  mutate(
    inverted_hammer = if_else(0.7 * whole < upper & 0.1*whole > lower, 1,0)
  )

