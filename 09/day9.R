library(tidyverse)

input <- readLines("09/input.txt") %>% as.double()
input

#### PART ONE SOLUTION
## construct matrix of sums
## arr ALWAYS 25 long array
construct.mat <- function(arr){
  out <- list()
  for(i in 1:(length(arr)-1)){
    tmp <- arr[(i+1):length(arr)]
    out[[length(out)+1]] <- arr[i] + tmp
  }
  return(unlist(out))
}

## check values in matrix
## arr.n ALWAYS 26 long array (includes test at end)
check.array <- function(arr.n){
  arr <- construct.mat(arr.n[1:(length(arr.n)-1)])
  return(any(arr == arr.n[length(arr.n)]))
}

## step through input
for(j in 1:length(input)){
  last.val <- j + 25
  if(!check.array(input[j:last.val])){
    print(input[last.val])
    print("Value not detected")
    break
  }
  print(paste("j =", last.val))
}

#### PART TWO SOLUTION
for(j in 1:length(input)){
  
}
