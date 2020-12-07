library(tidyverse)

input <- readLines("06/input.txt")
input
## append "" to final line to make it a complete case
input[length(input) + 1] <- ""

## collapse to single
input.list <- list()
tmp <- ""
for(i in 1:length(input)){
  ## stop and save when reach line break
  if(input[i] == ""){
    input.list[[length(input.list) + 1]] <- tmp
    tmp <- ""
  }
  
  ## if not a new person, append to existing line
  tmp <- ifelse(tmp == "",
                ## first entry
                input[i],
                ## subsequent entry
                paste0(tmp, input[i]))
}
input.list

#### PART ONE SOLUTION
## remove duplicate letters from strings
clean.string <- function(str){
  unique.letters <- ""
  for(i in 1:nchar(str)){
    this.letter <- substr(str,i,i)
    if(!str_detect(unique.letters, this.letter)){unique.letters <- c(unique.letters, this.letter)}
  }
  ## alphabetize for return
  return(intersect(letters, unique.letters))
}

clean.input <- map(input.list, clean.string)
clean.input
unlist(clean.input) %>% length()

#### PART TWO SOLUTION
## loop through input, keep only if in previous
output.list <- list()
tmp <- ""
group.start <- T
for(i in 1:length(input)){
  ## stop and save when reach line break
  if(input[i] == ""){
    output.list[[length(output.list) + 1]] <- tmp
    tmp <- ""
    group.start <- T
    next
  }
  
  ## if tmp empty, continue until next person
  if(length(tmp) == 0) next

  ## if first of new group, keep all
  if(group.start){
    tmp <- strsplit(input[i], split=NULL) %>% unlist()
    group.start <- F
    next
  }

  ## if not first of a group, only keep letters in tmp contained in input[i]
  tmp <- intersect(tmp, strsplit(input[i], split=NULL) %>% unlist())
}
output.list
unlist(output.list) %>% length()




