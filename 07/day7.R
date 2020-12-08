library(tidyverse)

input <- readLines("07/input.txt")
input <- readLines("07/test.txt")
input

#### PART ONE SOLUTION
## this is recursion
find.bags <- function(color){
  ## find bags containing color
  bag.rule <- str_detect(input, paste("bags contain [:print:]+", color))
  ## extract bag names
  valid.bags <- str_extract(input[bag.rule], "^[:print:]+ bags contain") %>% 
    ## remove bags contain
    substr(., 1, nchar(.)-13)

  if(length(valid.bags)==0) return()
  
  print(valid.bags)
  
  out.bags <- list(valid.bags)
  for(i in 1:length(valid.bags)){
    out.bags[[length(out.bags)+1]] <- find.bags(valid.bags[i])
  }

  return(unique(unlist(out.bags)))
}

first.bag <- "shiny gold"
bag1 <- find.bags(first.bag)
bag1

#### PART TWO SOLUTION
## this is still recursion, just opposite direction
find.bag.count <- function(color, first.loop){
  ## find rule for color
  bag.rule <- input[str_detect(input, paste(color, "bags contain [:print:]+"))]
  ## parse contained bags
  bags.inside <- str_extract(bag.rule, "[:digit:]+ [:print:]+ bags") %>% 
    ## remove bags at end
    str_split(", ") %>% unlist
  
  if(is.na(bags.inside)) return(1)
  
  print(bags.inside)
  # return(bags.inside)
  # return(find.bag.count(bags.inside))
  
  ## calculate number of bags within for each
  bags.within <- ifelse(
    first.loop,
    str_extract(bags.inside, "\\d+") %>% as.integer %>%  sum(),
    0
  )
  for(i in 1:length(bags.inside)){
    ## check bag or bags
    bag.length <- ifelse(str_detect(bags.inside[i], "bags"), 5, 4)
    bag.color <- bags.inside[i] %>%
      ## extract all but digit
      str_extract("\\D+") %>% substr(., 2, nchar(.)-bag.length)
    ## extract just digit
    bag.count <- as.double(str_extract(bags.inside[i], "\\d+"))
    
    recurse <- find.bag.count(bag.color, F)
    print(bag.count*recurse)
    bags.within <- bags.within + bag.count * recurse
  }

  return(bags.within)
}

bags.out <- find.bag.count("shiny gold", T)

# ## calculate number of bags within for each
# bags.within <- 0
# for(i in 1:length(bags.inside)){
#   ## check bag or bags
#   bag.length <- ifelse(str_detect(bags.inside[i], "bags"), 5, 4)
#   bag.color <- bags.inside[i] %>%
#     ## extract all but digit
#     str_extract("\\D+") %>% substr(., 2, nchar(.)-bag.length)
#   ## extract just digit
#   bag.count <- as.double(str_extract(bags.inside[i], "\\d+"))
# 
#   # bags.within <- bags.within + bag.count*find.bag.count(bag.color)
# }
# bag.count
# str(bag.count)
