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
#' @exportClass NLworld
#'
setClass(
  "NLworld",
  contains = c("RasterLayer", "RasterStack"),
  slots = c(
    maxPxcor = "numeric", # the functions maxPxcor, minPxcor, maxPycor, minPycor ...
    minPxcor = "numeric", # ... need to be renamed into getMaxPxcor, getMinPxcor, etc.
    maxPycor = "numeric", # maxPxcor, minPxcor, maxPycor, minPycor are single values...
    minPycor = "numeric", # ... defining the NLworld extent
    pxcor = "numeric", # pxcor and pycor are patch coordinates...
    pycor = "numeric", # one value per patch
    pacthSize = "numeric"
  ),
  prototype = c(
    pxcor = (minPxcor + colFromCell(NLworld, 1:(NLworld@nrows * NLworld@ncols))) - 1, # define the patch coordinates with the raster row and column numbers
    pycor = (maxPycor - rowFromCell(NLworld, 1:(NLworld@nrows * NLworld@ncols))) + 1,
    patchSize = 1
  )
)


#' @export
#' @importFrom NetLogoR getMaxPycor
#' @importFrom NetLogoR getMinPxcor
#' @name [
#' @docType methods
#' @rdname NLworld-class
setMethod(
  "[",
  signature(worldRaster = "NLworld", pxcor = "numeric", pycor = "numeric"),
  definition = function(worldRaster, pxcor, pycor) {

    maxPycor <- getMaxPycor(worldRaster = worldRaster)
    minPxcor <- getMinPxcor(worldRaster = worldRaster)

    nrow_R<- (maxPycor - pycor) + 1 # define raster row and column numbers with pxcor and pycor
    ncol_R<- (pxcor - minPxcor) + 1
    cellsExtract <- worldRaster[nrow_R, ncol_R]

    return(cellsExtract)
})


#' @export
#' @importFrom NetLogoR getMaxPycor
#' @importFrom NetLogoR getMinPxcor
#' @name [<-
#' @rdname NLworld-class
setReplaceMethod(
  "[",
  signature(worldRaster = "NLworld", pxcor = "numeric", pycor = "numeric", value = "numeric"),
  definition = function(worldRaster, pxcor, pycor, value) {

    worldRaster[pxcor, pycor] <- value

    validObject(worldRaster)
    return(worldRaster)
})
