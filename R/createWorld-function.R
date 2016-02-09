################################################################################
#' Create a world
#'
#' Create empty patches at the extent and resolution given for the world.
#'
#' @param minPxcor  Minimum x coordinate (raster left border).
#'                  Default value \code{-16}, as in NetLogo.
#'
#' @param maxPxcor  Maximum x coordinate (raster right border).
#'                  Default value \code{16}, as in NetLogo.
#'
#' @param minPycor  Minimum y coordinate (raster bottom border).
#'                  Default value \code{-16}, as in NetLogo.
#'
#' @param maxPycor  Maximum y coordinate (raster top border).
#'                  Default value \code{16}, as in NetLogo.
#'
#' @param patchSize Numeric vector of length 1 or 2 to set the raster resolution.
#'                  Default value \1.
#'
#' @param ...       Additional arguments, see Details
#'
#' @details Arguments of the \code{raster} function from the raster package may be added.
#'
#' @return A RasterLayer object with extent and resolution as given by the parameters.
#'         Cells value are NA.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples # Create a square world of 25 patches.
#'           world <- createWorld(minPxcor = 0, maxPxcor = 5, minPycor = 0, maxPycor = 5)
#'           # Give the patches random values between 0 and 10.
#'           world[] <- runif(n = 25, min = 0, max = 10)
#'           plot(world)
#'
#' @export
#' @importFrom raster raster
#' @docType methods
#' @rdname createWorld
#'
#' @author Sarah Bauduin
#'
setGeneric("createWorld", function(minPxcor = -16, maxPxcor = 16, minPycor = -16, maxPycor = 16, patchSize = 1, ...){
  standardGeneric("createWorld")
})

#' @export
#' @rdname createWorld
setMethod(
  "createWorld",
  #signature = c("numeric", "numeric", "numeric", "numeric", "numeric"), # not working
  #signature = c("numeric", "numeric", "numeric", "numeric", "numeric", ...), # not working
  definition = function(minPxcor = -16, maxPxcor = 16, minPycor = -16, maxPycor = 16, patchSize = 1, ...){
    worldRaster<-raster(xmn=minPxcor, xmx=maxPxcor, ymn=minPycor, ymx=maxPycor, res=patchSize, ...)
    return(worldRaster)
  }
)
