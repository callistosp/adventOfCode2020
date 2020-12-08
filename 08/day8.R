library(tidyverse)

input <- data.table::fread("08/input.txt", header=F) %>% 
  ## add row to count number of times each line encountered
  mutate(runs = 0) %>% 
  rename(cmd = V1, line=V2)

input

#### PART ONE SOLUTION
traverse <- function(input.loop, return.command = F){
  ## start with empty accumulator on line 1
  accumulator <- 0
  i <- 1
  commands <- list()
  while(!any(input.loop$runs>1)){
    ## if reach end of file, print and quit (added for part two)
    if(i > nrow(input.loop)){ 
      print("SUCCESS")
      ## set accumulator to negative value to escape loop
      accumulator <- accumulator * -1
      break
    }
    ## capture commands encountered, don't return by default
    commands[[length(commands)+1]] <- list(mutate(input.loop[i],number=i))
    ## if line encountered, increase number of times run by 1
    input.loop$runs[i] <- input.loop$runs[i] + 1
    if(input.loop$cmd[i] == "acc"){
      accumulator <- accumulator + input.loop$line[i]
      i <- i + 1
      next
    } 
    if(input.loop$cmd[i] == "nop"){
      i <- i + 1
      next
    } 
    if(input.loop$cmd[i] == "jmp"){
      i <- i + input.loop$line[i]
      next
    }
  }
  if(return.command){
    ## instead of returning accumulator value, return all commands
    ## encountered so far in the run
    return(commands)
  }
  return(accumulator)
}

traverse(input)

#### PART TWO SOLUTION
## select from any nop/jmp encountered in regular run
commands <- traverse(input, T)
commands
clean.out <- do.call(bind_rows, commands) %>% 
  filter(cmd != "acc")
clean.out

## brute force: try all commands in order to find solution
for(j in 1:nrow(clean.out)){
  tmp <- input %>% 
    ## add row numbers for select
    mutate(number = row_number()) %>% 
    mutate(
      cmd = case_when(
        cmd == "nop" & number == clean.out$number[j] ~ "jmp",
        cmd == "jmp" & number == clean.out$number[j] ~ "nop",
        TRUE ~ cmd
      )
    )
  out <- traverse(tmp)
  print(out)
  ## if encounter negative accumulator, quit the loop
  if(out<0) break
}
