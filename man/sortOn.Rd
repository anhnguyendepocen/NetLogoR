% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agentset-functions.R
\docType{methods}
\name{sortOn}
\alias{sortOn}
\alias{sortOn,agentMatrix,missing,character-method}
\alias{sortOn,matrix,worldArray,character-method}
\alias{sortOn,matrix,worldMatrix,missing-method}
\title{Sort agents}
\usage{
sortOn(agents, world, var)

\S4method{sortOn}{matrix,worldMatrix,missing}(agents, world)

\S4method{sortOn}{matrix,worldArray,character}(agents, world, var)

\S4method{sortOn}{agentMatrix,missing,character}(agents, var)
}
\arguments{
\item{agents}{Matrix (ncol = 2) with the first column "pxcor" and the second
              column "pycor" representing the patches coordinates, or

              AgentMatrix object representing the moving agents.}

\item{world}{WorldMatrix or worldArray object.}

\item{var}{Character. The name of the selected \code{agents} variable.
If \code{agents} are patches and the \code{world} is a
worldMatrix object, \code{var} must not be provided. If
\code{agents} are patches and the \code{world} is a worldArray
object, \code{var} is the name of the layer to use to define the patches
values. If \code{agents} are turtles, \code{var} is one of
the turtles' variable and can be equal to \code{"xcor"},
\code{"ycor"}, any of the variables created when turtles were created,
as well as any variable created using \code{turtlesOwn()}.}
}
\value{
Matrix (ncol = 2) with the first column "pxcor" and the second column
        "pycor" representing the coordinates of the patches sorted according to
        their values, if \code{agents}
        are patches, or

        AgentMatrix representing the turtles sorted according
        to their \code{var} values, if \code{agents} are
        turtles.
}
\description{
Return the \code{agents} sorted according to their value.
}
\details{
\code{world} must not be provided if \code{agents} are turtles.

         The sorting of the \code{agents} is done in a increasing order.
}
\examples{
# Patches
w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4,
                          data = sample(1:5, size = 25, replace = TRUE))
plot(w1)
p1 <- sortOn(agents = patches(w1), world = w1)

# Turtles
t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
sortHeadingT1 <- sortOn(agents = t1, var = "heading")


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
\url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#sort-on}
}

