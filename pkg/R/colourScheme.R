
schemeData <- function(object){
  UseMethod("schemeData")
}

print.colourScheme <- function(x,...){
  cat(attr(x,"type"),"\n")
  print(schemeData(x))
}

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

continuousScheme <- function(valueColours){
  ## assert column 1 is sorted increasing
    
}

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

