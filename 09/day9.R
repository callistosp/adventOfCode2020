library(tidyverse)

input <- readLines("09/input.txt") %>% as.double()
input

#### PART ONE SOLUTION
## construct half-matrix of sums
construct.mat <- function(arr){
  out <- list()
  for(i in 1:(length(arr)-1)){
    tmp <- arr[(i+1):length(arr)]
    out[[length(out)+1]] <- arr[i] + tmp
  }
  return(unlist(out))
}

## check values in matrix (includes test at end)
## ALWAYS TRUE: length(arr.n) = length(arr) + 1
## value returns T if last entry in vector included in the
## half-matrix of sums from entries 1-25 of the vector
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
find.sum <- function(.input, .ref){
  for(j in 1:length(.input)){
    for(k in (j+1):length(.input)){
      tmp <- sum(input[j:k])
      if(tmp == .ref) return(sum(min(input[j:k]),max(input[j:k])))
      ## if sum greater than ref, move on to next j
      if(tmp > .ref) break
    }
  }
}

ref <- 26796446
find.sum(input, ref)
