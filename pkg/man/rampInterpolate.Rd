\name{rampInterpolate}
\Rdversion{1.1}
\alias{rampInterpolate}

\title{
Interpolated ramp colour scheme
}
\description{
Returns a colour scheme that interpolates colours between values.
}
\usage{
rampInterpolate(limits, ramp, ...)
}
\arguments{
  \item{limits}{A vector, the min and max of which specify the ramp limits.}
  \item{ramp}{A vector of colour values that are passed to colorRamp}
  \item{\dots}{Further arguments passed to colorRamp}
}

\details{
The rampInterpolate colour scheme maps from the min and max of the
limits argument to a colour ramp generated by the standard R colorRamp
function. 
}

\value{
A colourscheme function that interpolates in a colour ramp.
}

\examples{
rs = rampInterpolate ( limits =c(-2 , 2),
          ramp = c("red", " yellow ", " blue "))
rs(0)
### show the difference between linear and spline interp:
rs2 = rampInterpolate ( limits =c(-2 , 2),
          ramp = c("red", " yellow ", " blue "),
          interpolate="spline")
rs(1)
rs2(1)
}

\author{
Barry Rowlingson <b.rowlingson@lancaster.ac.uk>
}

\keyword{color}
  

