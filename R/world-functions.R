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
#' @examples
#' # Create a square world of 25 patches.
#' world <- createWorld(minPxcor = 0, maxPxcor = 5, minPycor = 0, maxPycor = 5)
#' # Give the patches random values between 0 and 10.
#' world[] <- runif(n = 25, min = 0, max = 10)
#' plot(world)
#'
#' @export
#' @docType methods
#' @rdname createWorld
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "createWorld",
  function(minPxcor = -16, maxPxcor = 16, minPycor = -16, maxPycor = 16,
           patchSize = 1, ...) {
  standardGeneric("createWorld")
})

#' @export
#' @rdname createWorld
setMethod(
  "createWorld",
  signature = c("numeric", "numeric", "numeric", "numeric", "numeric"),
  definition = function(minPxcor = -16, maxPxcor = 16, minPycor = -16,
                        maxPycor = 16, patchSize = 1, ...) {
    worldRaster <- raster(xmn = minPxcor, xmx = maxPxcor, ymn = minPycor,
                          ymx = maxPycor, res = patchSize, ...)
    return(worldRaster)
  }
)

#' @export
#' @rdname createWorld
setMethod(
  "createWorld",
  signature = c("numeric", "numeric", "numeric", "numeric", "missing"),
  definition = function(minPxcor = -16, maxPxcor = 16, minPycor = -16,
                        maxPycor = 16, ...) {
    worldRaster <- raster(xmn = minPxcor, xmx = maxPxcor, ymn = minPycor,
                          ymx = maxPycor, res = 1, ...)
    return(worldRaster)
  }
)

#' @export
#' @rdname createWorld
setMethod(
  "createWorld",
  signature = c("missing", "missing", "missing", "missing", "missing"),
  definition = function(...) {
    worldRaster <- raster(xmn = -16, xmx = 16, ymn = -16, ymx = 16,
                          res = 1, ...)
    return(worldRaster)
  }
)


################################################################################
#' Maximum pxcor
#'
#' Report the maximum x-coordinate for patches.
#'
#' @param worldRaster A \code{Raster} or \code{RasterStack} object.
#'
#' @return A numeric value
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create a world with the default settings.
#' world <- createWorld()
#' maxPxcor(worldRaster = world)
#'
#' @export
#' @docType methods
#' @rdname maxPxcor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "maxPxcor",
  function(worldRaster) {
    standardGeneric("maxPxcor")
  })

#' @export
#' @rdname maxPxcor
setMethod(
  "maxPxcor",
  signature = "RasterLayer",
  definition = function(worldRaster) {
    maxPxcorRaster <- worldRaster@extent@xmax
    return(maxPxcorRaster)
  }
)

#' @export
#' @rdname maxPxcor
setMethod(
  "maxPxcor",
  signature = "RasterStack",
  definition = function(worldRaster) {
    worldRaster_l <- worldRaster[[1]] # take the 1st layer of the stack
    maxPxcorRaster <- maxPxcor(worldRaster = worldRaster_l)
    return(maxPxcorRaster)
  }
)


################################################################################
#' Maximum pycor
#'
#' Report the maximum y-coordinate for patches.
#'
#' @param worldRaster A \code{Raster} or \code{RasterStack} object.
#'
#' @return A numeric value
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create a world with the default settings.
#' world <- createWorld()
#' maxPycor(worldRaster = world)
#'
#' @export
#' @docType methods
#' @rdname maxPycor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "maxPycor",
  function(worldRaster) {
    standardGeneric("maxPycor")
  })

#' @export
#' @rdname maxPycor
setMethod(
  "maxPycor",
  signature = "RasterLayer",
  definition = function(worldRaster) {
    maxPycorRaster <- worldRaster@extent@ymax
    return(maxPycorRaster)
  }
)

#' @export
#' @rdname maxPycor
setMethod(
  "maxPycor",
  signature = "RasterStack",
  definition = function(worldRaster) {
    worldRaster_l <- worldRaster[[1]] # take the 1st layer of the stack
    maxPycorRaster <- maxPycor(worldRaster = worldRaster_l)
    return(maxPycorRaster)
  }
)
