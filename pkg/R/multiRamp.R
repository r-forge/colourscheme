multiRamp <- function(ranges,rampSpec,...){
  ## ranges is Nx2
  ## rampSpec is list(N)
  ramps = list()
  for(i in 1:length(rampSpec)){
    ramps[[i]] = rampInterpolate(ranges[i,],rampSpec[[i]])
#    cat("Colour = ",col,"\n")
#    browser()
  }
  force(ranges)
  force(rampSpec)
  force(ramps)
  f = function(data){
    colours = rep(NA,length(data))
    for(i in 1:length(data)){
      d=data[i]
      inRange = d>=ranges[,1] & d <= ranges[,2]
      if(any(inRange)){
        pos = (1:length(ranges[,1]))[inRange][1]
        colours[i] <- ramps[[pos]](d)
      }else{
        colours[i] <- NA
      }
    }
    return(colours)
  }
  class(f) <- c("multiRamp","colourScheme","function")
  attr(f,"type") <- "multiple-ramp colour scheme"
  return(f)
}

schemeData.multiRamp <- function(object){
  return(list(ranges=get("ranges",env=environment(object)),
              rampSpec=get("rampSpec",env=environment(object))
              )
         )
}
