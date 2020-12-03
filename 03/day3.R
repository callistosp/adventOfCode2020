library(tidyverse)

input <- data.table::fread("03/input.txt", header=FALSE)
input

## how wide is our map?
width.map <- nchar(input$V1[1])
length.map <- nrow(input)

#### PART ONE SOLUTION

## loop through until we reach the bottom
tree.count <- 0
position <- list(x=1, y=1)

while(position$y < length.map){
  ## traverse down 1 and right 3
  position$y <- position$y + 1
  position$x <- ifelse(
    position$x + 3 <= width,
    position$x + 3,
    ## if we go off the map, reset location so we can reuse single map
    position$x + 3 - width
  )
  
  print(paste0("row ", position$y, " column ", position$x))
  
  tmp <- substr(input$V1[position$y], position$x, position$x)
  print(tmp)
  tree <- tmp == "#"
  
  tree.count <- ifelse(
    tree,
    tree.count + 1,
    tree.count
  )
}

print(tree.count)

#### PART TWO SOLUTION

slopes <- list(
  list(x=1, y=1),
  list(x=3, y=1),
  list(x=5, y=1),
  list(x=7, y=1),
  list(x=1, y=2)
)
trees <- list()

for(i in 1:length(slopes)){
  tree.count <- 0
  position <- list(x=1, y=1)
  
  while(position$y < length.map){
    ## traverse down slope.y and right slope.x
    position$y <- position$y + slopes[[i]]$y
    position$x <- ifelse(
      position$x + slopes[[i]]$x <= width,
      position$x + slopes[[i]]$x,
      ## if we go off the map, reset location so we can reuse single map
      position$x + slopes[[i]]$x - width
    )
    
    # print(paste0("row ", position$y, " column ", position$x))
    
    tmp <- substr(input$V1[position$y], position$x, position$x)
    # print(tmp)
    tree <- tmp == "#"
    
    tree.count <- ifelse(
      tree,
      tree.count + 1,
      tree.count
    )
  }
  
  trees[[i]] <- tree.count
}

trees
prod(unlist(trees))
