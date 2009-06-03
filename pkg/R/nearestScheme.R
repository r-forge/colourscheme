
nearestScheme <- function(valueColours){
### valueColours is a data frame with $value and $col columns
  force(valueColours)

  f = function(data){
    colours = rep(NA,length(data))
    for(i in 1:length(data)){
      d=data[i]
      diffs = abs(valueColours$value-d)
      colours[i]=as.character(valueColours[which.min(diffs),'col'])

    }
    return(colours)
  }
  class(f) <- c("nearestScheme","colourScheme","function")
  attr(f,"type") <- "nearest colour scheme"
  return(f)
}

schemeData.nearestScheme <- function(object){
  return(get("valueColours",env=environment(object)))
}
