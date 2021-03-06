% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/turtle-functions.R
\docType{methods}
\name{patchRight}
\alias{patchRight}
\alias{patchRight,worldNLR,agentMatrix,numeric,numeric-method}
\title{Patches on the right}
\usage{
patchRight(world, turtles, dist, angle, torus = FALSE)

\S4method{patchRight}{worldNLR,agentMatrix,numeric,numeric}(world, turtles,
  dist, angle, torus = FALSE)
}
\arguments{
\item{world}{WorldMatrix or worldArray object.}

\item{turtles}{AgentMatrix object representing the moving agents.}

\item{dist}{Numeric. Vector of distances from the \code{turtles}. \code{dist} must be
of length 1 or of length \code{turtles}.}

\item{angle}{Numeric. Vector of angles in degrees by which the \code{turtle}'s
headings should rotate to locate the patches. Must be of length 1 or of
length \code{turtles}.}

\item{torus}{Logical to determine if the \code{world} is wrapped. Default is
\code{torus = FALSE}.}
}
\value{
Matrix (ncol = 2) with the first column "pxcor" and the second
        column "pycor" representing the coordinates of the patches at \code{dist}
        distances of the \code{turtles} and \code{angle} to the right of their headings.
        The order of the patches follows the order of the \code{turtles}.
}
\description{
Report the coordinates of the patches at the given distances of the \code{turtles}
and given \code{angle} right of their headings.
}
\details{
If a given \code{dist} value is negative, then the turtle would look backward.
         If a given \code{angle} value is negative, then the turtle would
         look to the left.

         If \code{torus = FALSE} and the patch at distance \code{dist} of a turtle
         and \code{angle} degrees to the right of its heading is outside the
         \code{world}'s extent, \code{NA}
         are returned for the patch coordinates. If \code{torus = TRUE}, the patch
         coordinates from a wrapped \code{world} are returned.
}
\examples{
w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
t1 <- createTurtles(n = 1, coords = cbind(xcor = 2, ycor = 2), heading = 90)
patchRight(world = w1, turtles = t1, dist = 2, angle = 90)


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
\url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch-lr-and-ahead}
}

