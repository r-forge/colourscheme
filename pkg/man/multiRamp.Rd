\name{multiRamp}
\Rdversion{1.1}
\alias{multiRamp}
\title{
A multiple-ramp colour scheme
}
\description{
A colour scheme that combines a number of interpolated ramps.
}
\usage{
multiRamp(ranges, rampSpec, ...)
}
\arguments{
  \item{ranges}{A two-column matrix, each row being the min and max of
    the ramp}
  \item{rampSpec}{A list of vectors of colours for each ramp}
  \item{\dots}{Currently ignored.}
}

\details{
This function returns a colour scheme that consists of a number of ramps
such as created by rampInterpolate. It is useful when your colour scheme
has a number of discrete parts, for example when colouring elevations on
a map to show water, land, and ice, or when you want to show a definite
horizon at a specific value.
}

\value{
A colourscheme function.
}

\author{
Barry Rowlingson <b.rowlingson@lancaster.ac.uk>
}

\examples{
# topological colour scheme - water, land, ice:
tramp = multiRamp(rbind(c(-2000,0),c(0,1000),c(1000,9000)),
 list(c("black","blue"),c("green","brown"),c("gray70","gray70"))
 )
}


\keyword{color}
  

