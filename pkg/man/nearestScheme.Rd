\name{nearestScheme}
\Rdversion{1.1}
\alias{nearestScheme}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Use the nearest colour to a set of value-colour pairs.
}
\description{
The nearest scheme takes a data frame of values and colours and produces a
colour scheme function that maps data values to the colour corresponding to
the nearest value in the data frame.
}
\usage{
nearestScheme(valueColours)
}
\arguments{
  \item{valueColours}{a data frame with at least two columns, named
    'col' and 'values'. The 'col' column must contain colour values and
    the 'values' column must be numeric.}
}

\details{
  The numbers in the 'values' column should be unique, the returned colour
  is not clearly defined otherwise. 
}

\value{
A colourscheme function.
}

\author{
Barry Rowlingson <b.rowlingson@lancaster.ac.uk>
}

\examples{
z=nearestScheme(data.frame(col=c("red","white","blue"),values=c(1,2,3)))
z(seq(0,4,len=6))
}

\keyword{color}
  

