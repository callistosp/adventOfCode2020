library(tidyverse)

input <- readLines("11/input.txt") 
# input <- readLines("11/test.txt") 
input

numr <- length(input) ## rows=90
numc <- nchar(input) %>% unique ## cols=91

## flatten the matrix
tmp <- input %>% 
  strsplit(split=NULL) %>% unlist
  # do.call(rbind,.) #%>% as.array(dim=c(90,91))
tmp

## @param .x index in array
## @param .input array
applyRules <- function(.x, .input){
  tmp <- .input[.x]
  ## don't do anything to empty locations
  if(tmp == ".") return(".")
  ## count adjacent seats
  ## middle case
  adj <- c(
    .x - c(numc + -1:1),
    .x + c(-1, 1),
    .x + c(numc + -1:1))
  ## left case
  if(.x %% numc == 1) adj <- c(
      .x - c(numc + -1:0),
      .x + c(1),
      .x + c(numc + 0:1)
    )
  ## right case
  if(.x %% numc == 0) adj <- c(
      .x - c(numc + 0:1),
      .x + c(-1),
      .x + c(numc + -1:0)
    )
  ## top/bottom case
  adj <- adj %>% subset(between(., 1, length(.input)))
  occ.count <- sum(.input[adj] == "#")
  ## apply rules and return new state
  out <- case_when(
    tmp == "L" & occ.count == 0 ~ "#",
    tmp == "#" & occ.count >= 4 ~ "L",
    TRUE ~ tmp
  )
  return(out)
}

old.state <- tmp
loop.n <- 0
repeat{
  new.state <- character()
  for(i in 1:length(old.state)){
    new.state[i] <- applyRules(i, old.state)
  }
  print(glue::glue("------------------- loop {(loop.n <- loop.n + 1)} ------------------------"))
  cat(new.state)
  ## if same, exit
  if(all(old.state == new.state)){break}
  ## else, store for next loop
  old.state <- new.state
}

sum(new.state == "#")

