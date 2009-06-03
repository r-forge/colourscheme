factorScheme <- function(fac,colours){
  fac = as.factor(fac)
  levels = levels(fac)
  if(length(levels)!=length(colours)){
    stop(paste("Can't have ",length(levels)," levels in a factor for ",length(colours)," colours"))
  }
  names(colours) <- as.character(levels)
  force(levels)
  force(colours)
  f = function(data){
    data=as.character(data)
    return(colours[data])
  }
  class(f) <- c("factorScheme","colourScheme","function")
  attr(f,"type") <- "factor colour scheme"
  return(f)
}

schemeData.factorScheme <- function(object){
  return(list(
              colours = get("colours",env=environment(object)),
              fac = get("levels",env=environment(object))
              )
         )
}
