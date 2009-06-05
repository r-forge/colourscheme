
rgbScheme = function(d3,equalScale=FALSE){
  return(f3Scheme(d3,rgb,"rgb scheme",equalScale))
}

hsvScheme = function(d3,equalScale=FALSE){
  return(f3Scheme(d3,hsv,"hsv scheme",equalScale))
}

f3Scheme <- function(d3, f3, schemeText, equalScale=FALSE){
  force(f3)
  force(equalScale)
  force(d3)
  dataranges = apply(d3,2,range)
  if(equalScale){
    ranges = matrix(range(dataranges),2,3)
  }else{
    ranges = dataranges
  }
  force(dataranges)
  ranges = t(ranges)
  force(ranges)
  ranges[,2]=ranges[,2]-ranges[,1]
  force(ranges)
  f = function(data){
    data = t(data)
    rgbs = t((data-ranges[,1])/ranges[,2])
    return(f3(rgbs[,1],rgbs[,2],rgbs[,3]))
  }
  class(f) <- c("f3scheme","colourScheme","function")
  attr(f,"type") <- schemeText
  return(f)
}

schemeData.f3scheme <- function(object){
  return(list(d3 = get("dataranges",env=environment(object)),
              equalScale = get("equalScale",env=environment(object))
              )
         )
}

