rampInterpolate <- function(limits,ramp,...){
  
  rampFunction = colorRamp(ramp,...)
  force(list(...))
  force(ramp)
  force(rampFunction)
  force(limits)
  f=function(data){
    t=(data-min(limits))/(max(limits)-min(limits))
    outside = t < 0.0 | t > 1.0
    okColours = rgb(rampFunction(t[!outside])/255)
    colours = rep(NA,length(data))
    colours[!outside] <- okColours
    return(colours)
  }
  class(f) <- c("rampInterpolate","colourScheme","function")
  attr(f,"type") <- "interpolated colour ramp scheme"
  return(f)
}

schemeData.rampInterpolate <- function(object){
  return(list(
              limits = get("limits",env=environment(object)),
              ramp = get("ramp",env=environment(object))
              )
         )
}

