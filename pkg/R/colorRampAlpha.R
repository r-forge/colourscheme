.colorRampAlpha <- function (colors, bias = 1, space = c("rgb", "Lab"), interpolate = c("linear", 
                                                         "spline")) 
{
###
### Version of colorRamp that ramps alpha transparency as well if given.
###
  if (bias <= 0) 
    stop("'bias' must be positive")
  rgba = t(col2rgb(colors,alpha=TRUE)/255)
  colors <- rgba[,1:3]
  alpha = rgba[,4]
  space <- match.arg(space)
  interpolate <- match.arg(interpolate)
  if (space == "Lab") {
    colors <- convertColor(colors, from = "sRGB", to = "Lab")
  }
  interpolate <- switch(interpolate, linear = stats::approxfun, 
                        spline = stats::splinefun)
  x <- seq.int(0, 1, length.out = nrow(colors))^{bias}
  palette <- c(interpolate(x, colors[, 1]),
               interpolate(x, colors[, 2]),
               interpolate(x, colors[, 3]),
               interpolate(x, alpha))
  roundcolor <- function(rgb) {
    rgb[rgb < 0] <- 0
    rgb[rgb > 1] <- 1
    rgb
  }
  if (space == "Lab") {
    function(x) {
      cp = cbind(palette[[1L]](x), palette[[2L]](x), palette[[3L]](x))
      cc = convertColor(cp, from="Lab", to="sRGB")
      cb = cbind(cc,palette[[4L]](x))
      return(roundcolor(cb)*255)
      roundcolor(cbind(convertColor(cbind(palette[[1L]](x), palette[[2L]](x), 
                                    palette[[3L]](x)), from = "Lab", to = "sRGB"),palette[[4L]](x)) * 255)
    }
  }
  else {
    function(x) {
      roundcolor(cbind(palette[[1L]](x),
                       palette[[2L]](x), 
                       palette[[3L]](x),
                       palette[[4L]](x))) * 255
    }
  }
}
