library(tidyverse)

input <- data.table::fread("01/input.txt", header = F)
input

max.n <- nrow(input)

## PART ONE SOLUTION
for(i in 1:max.n){
  tmp <- as.integer(input[i])
  next.n <- i + 1
  if(next.n > max.n) break
  
  sums <- input[next.n:max.n] + as.integer(tmp)
  if(any(sums == 2020)){
    cat(i)
    break
  }
}

which(sums$V1 == 2020)
paste(input[i], ", ", input[i+which(sums$V1 == 2020)])
1743 + 277
1743 * 277

## PART TWO SOLUTION
for(i in 1:max.n){
  next.n <- i + 1
  if(next.n > max.n) break
  
  tmpi <- as.integer(input[i])
  
  for(j in next.n:max.n){
    tmpj <- as.integer(input[j])
    
    sums2 <- input[next.n:max.n] + tmpi + tmpj
    if(any(sums2 == 2020)){
      print(i)
      print(j)
      break
    }
  }
}

which(sums2$V1 == 2020)
paste(input[i], ", ", input[j], ", ", input[i+which(sums$V1 == 2020)])
1067 + 691 + 262
1067 * 691 * 262




### ONELINE SOLUTION FROM TWITTER
input <- pull(input, V1)
prod(input[input %in% (2020 - input)])
prod(input[input %in% (2020 - outer(input, input, "+"))])
