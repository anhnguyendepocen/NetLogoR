% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/turtle-functions.R
\docType{methods}
\name{turtleSet}
\alias{turtleSet}
\alias{turtleSet,agentMatrix-method}
\title{Create a turtle agenset}
\usage{
turtleSet(...)

\S4method{turtleSet}{agentMatrix}(...)
}
\arguments{
\item{...}{AgentMatrix objects representing the moving agents.}
}
\value{
AgentMatrix object containing all the unique turtles.
}
\description{
Report a turtle agentset containing all unique turtles provided in the inputs.
}
\details{
Duplicated turtles are identified based only on their who numbers.
         The turtle chosen for a who number is the first one given in the inputs.
         To keep all turtles from the inputs, use NLset() to
         reassign who numbers in some of the inputs, prior using
         turtleSet(), to avoid turtles with duplicated who numbers.
}
\examples{
w1 <- createWorld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10), breed = "sheep")
t2 <- createTurtles(n = 2, coords = randomXYcor(w1, n = 2), breed = "wolf")
t2 <- NLset(turtles = t2, agents = t2, var = "who", val = c(10, 11))
t3 <- createTurtles(n = 1, coords = randomXYcor(w1, n = 1), breed = "sheperd")
t3 <- NLset(turtles = t3, agents = t3, var = "who", val = 12)
t4 <- turtleSet(t1, t2, t3)


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
\url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#turtle-set}
}

