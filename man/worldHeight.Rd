% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/world-functions.R
\docType{methods}
\name{worldHeight}
\alias{worldHeight}
\alias{worldHeight,worldNLR-method}
\title{World height}
\usage{
worldHeight(world)

\S4method{worldHeight}{worldNLR}(world)
}
\arguments{
\item{world}{WorldMatrix or worldArray object.}
}
\value{
Integer.
}
\description{
Report the height of the \code{world} in patch number.
}
\examples{
w1 <- createWorld()
worldHeight(w1)


}
\author{
Sarah Bauduin
}
\references{
Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
            Center for Connected Learning and Computer-Based Modeling,
            Northwestern University. Evanston, IL.
}
\seealso{
\url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#world-dim}
}

