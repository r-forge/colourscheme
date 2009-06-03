
### some generic methods and functions

schemeData <- function(object){
  UseMethod("schemeData")
}

print.colourScheme <- function(x,...){
  cat(attr(x,"type"),"\n")
  print(schemeData(x))
}

