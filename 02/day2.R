library(tidyverse)

input <- data.table::fread("02/input.txt", header=FALSE)
input

## clean and rename columns
clean <- input %>% 
  mutate(
    letter=substr(V2,1,1)
  ) %>%   
  separate(V1, c("min.letter", "max.letter"), "-") %>% 
  mutate_at(vars(min.letter, max.letter), as.numeric) %>% 
  select(min.letter, max.letter, letter, password=V3)
head(clean)
str(clean)

#### PART ONE SOLUTION
clean %>% 
  mutate(
    ## count number of letters
    n.letter = str_count(password, letter)
  ) %>% 
  mutate(
    ## verify between values
    valid = n.letter >= min.letter & n.letter <= max.letter
  ) %>% 
  ## count number of valid passwords
  count(valid)

#### PART TWO SOLUTION
clean %>% 
  mutate(
    valid = xor(
      substr(password, min.letter, min.letter) == letter,
      substr(password, max.letter, max.letter) == letter
    )
  ) %>% 
  ## count number of valid passwords
  count(valid)
