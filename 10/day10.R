library(tidyverse)

input <- readLines("10/input.txt") %>% as.numeric()
## add internal adapter and outlet base
df <- data.frame(V1=c(0, max(input)+3, input))

#### PART ONE SOLUTION
arrange(df, V1) %>% 
  mutate(
    diff = lead(V1) - V1
  ) %>% 
  count(diff)
70*33

#### PART TWO SOLUTION
## 2^n which can be removed
initial.n <- nrow(df)
arrange(df, V1) %>% 
  mutate(
    diff.lead = lead(V1) - V1,
    diff.lag = V1 - lag(V1)
  ) %>% filter(!(diff.lead == 1 & diff.lag == 1)) %>% nrow

2^(104-55)
