################################################################################
#' The \code{NLworld} class
#'
#' Behaves the same as a Raster* object except:
#'
#' A \code{NLworld} is a grid composed of squared patches of resolution 1.
#' Patches have two coordinates \code{pxcor} and \code{pycor}, representing their center.
#' \code{pxcor} and \code{pycor} are always integer.
#' \code{pxcor} and \code{pycor} can be negative if there are patches to the left
#' or below the patch \code{[0,0]}.
#' When creating a \code{NLworld}, the number of patches is equal to
#' \code{(maxPxcor - minPxcor + 1) * (maxPycor - minPycor + 1)}.
#' \code{pxcor} can be seen as a column number and \code{pycor} can be seen as a
#' row number but increasing as you move up.
#' The use of \code{[]} to extract \code{Raster*} cells by row and column numbers has
#' been redefined to extract \code{NLworld} patches using the patches' coordinates
#' \code{[pxcor,pyxor]}.
#'
#' @inheritParams raster
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @author Sarah Bauduin, Eliot McIntire, and Alex Chubaty
#'
setClass(
  "NLworld",
  contains = C("RasterLayer", "RasterStack")
)

