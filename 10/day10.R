library(tidyverse)

input <- readLines("10/input.txt") %>% as.numeric()
## add internal adapter and outlet base
df <- data.frame(V1=c(0, max(input)+3, input))

out <- arrange(df, V1) %>% 
  mutate(
    diff = lead(V1) - V1
  ) %>% 
  count(diff)
out
