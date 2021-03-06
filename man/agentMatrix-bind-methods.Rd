% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Classes.R
\docType{methods}
\name{cbind}
\alias{cbind}
\alias{cbind.agentMatrix}
\alias{rbind}
\alias{rbind.agentMatrix}
\title{Combine R Objects by Rows or Columns}
\usage{
\method{cbind}{agentMatrix}(..., deparse.level)

\method{rbind}{agentMatrix}(..., deparse.level = 1)
}
\arguments{
\item{...}{Two agentMatrix objects}

\item{deparse.level}{See \code{\link[base]{cbind}}}
}
\description{
Take a sequence of agentMatrix arguments and combine by columns or rows, respectively.
This will take the coordinates of the first argument and remove the coordinates
of the second object.
}

