library(tidyverse)

input <- readLines("05/input.txt")
input

convert.bin <- function(bin.str){
  decode <- bin.str %>% 
    str_replace_all("F", "0") %>% 
    str_replace_all("B", "1") %>% 
    str_replace_all("R", "1") %>% 
    str_replace_all("L", "0")
  return(decode)
}

seatids <- strtoi(convert.bin(input), base=2)
max(seatids)
setdiff(min(seatids):max(seatids), seat.ids)

## broken down solution
#### PART ONE SOLUTION
## convert string to binary, return separated int values
# find.seat <- function(bin.str){
#   ## convert to 0/1 for binary calculation
#   decode.str <- convert.bin(bin.str)
#   
#   row <- strtoi(substr(decode.str, 1, 7), base=2)
#   seat <- strtoi(substr(decode.str, 8, 10), base=2)
#   
#   return(list(row=row, seat=seat))
# }
# 
# ## calculate seat ID
# seatID <- function(seat.list){
#   return(seat.list$row * 8 + seat.list$seat)
# }
# 
# seats <- find.seat(input)
# seat.ids <- seatID(seats)
# max(seat.ids)
# 
# #### PART TWO SOLUTION
# sort(seat.ids) ## range of seats 100-861
# setdiff(100:861, seat.ids)
