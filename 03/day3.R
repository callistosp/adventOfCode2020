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
    position$x + 3 <= width.map,
    position$x + 3,
    ## if we go off the map, reset location so we can reuse single map
    position$x + 3 - width.map
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
traverseMap <- function(.slope, .width.map, .length.map){
  ## set start position
  tree.count <- 0
  position <- list(x=1, y=1)
  
  while(position$y < .length.map){
    ## traverse down slope.y and right slope.x
    position$y <- position$y + .slope$y
    position$x <- ifelse(
      position$x + .slope$x <= .width.map,
      position$x + .slope$x,
      ## if we go off the map, reset location so we can reuse single map
      position$x + .slope$x - .width.map
    )

    tmp <- substr(input$V1[position$y], position$x, position$x)
    tree <- tmp == "#"
    
    tree.count <- ifelse(
      tree,
      tree.count + 1,
      tree.count
    )
  }
  return(tree.count)
}

slopes <- list(
  list(x=1, y=1),
  list(x=3, y=1),
  list(x=5, y=1),
  list(x=7, y=1),
  list(x=1, y=2)
)
trees <- map(slopes, ~ traverseMap(.x, width.map, length.map))

trees
prod(unlist(trees))
