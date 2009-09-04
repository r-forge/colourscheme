\name{factorScheme}
\Rdversion{1.1}
\alias{factorScheme}

\title{
Generate colour schemes for factor data.
}
\description{
Return a colour scheme that generates a colour for each level of
a factor.
}
\usage{
factorScheme(fac, colours)
}
\arguments{
  \item{fac}{A factor}
  \item{colours}{The colours for the levels of the factor}
}

\value{
A colourscheme function.
}

\author{
Barry Rowlingson <b.rowlingson@lancaster.ac.uk>
}

\examples{
f = factor (c("a","a","b","a","c"))
fs = factorScheme (f,c("red"," blue "," green "))
fs(f)
g = factor (c("c","b","a"))
fs(g)
}


\keyword{color}
  

