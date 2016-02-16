################################################################################
#' Create a NLworld
#'
#' Create an empty grid of patches of class \code{NLworld}.
#'
#' @param minPxcor  \code{pxcor} for patches on the left border of the NLworld.
#'                  Default value = \code{-16}, as in NetLogo.
#'
#' @param maxPxcor  \code{pxcor} for patches on the right border of the NLworld.
#'                  Default value = \code{16}, as in NetLogo.
#'
#' @param minPycor  \code{pycor} for patches at the bottom of the NLworld.
#'                  Default value \code{-16}, as in NetLogo.
#'
#' @param maxPycor  \code{pycor} for patches at the top of the NLworld.
#'                  Default value \code{16}, as in NetLogo.
#'
#' @details See \code{help("NLworld)} for more details on the NLworld.
#'
#' @return A NLworld object composed of \code{(maxPxcor - minPxcor + 1) * (maxPycor - minPycor + 1)}
#'         patches. Patches value are NA.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create a square world of 25 patches.
#' world <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' # Give the patches random values between 0 and 10.
#' world[] <- runif(n = 25, min = 0, max = 10)
#' plot(world)
#'
#' @export
#' @docType methods
#' @rdname createNLworld
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "createNLworld",
  function(minPxcor = -16, maxPxcor = 16, minPycor = -16, maxPycor = 16) {
  standardGeneric("createNLworld")
})

#' @export
#' @rdname createNLworld
setMethod(
  "createNLworld",
  signature = c("numeric", "numeric", "numeric", "numeric"),
  definition = function(minPxcor = -16, maxPxcor = 16, minPycor = -16,
                        maxPycor = 16) {

    world <- new("NLworld",
                 minPxcor = minPxcor, maxPxcor = maxPxcor,
                 minPycor = minPycor, maxPycor = maxPycor)

    # define the raster coordinates with the NLworld extent
    world@extent@xmin <- minPxcor
    world@extent@xmax <- maxPxcor + 1
    world@extent@ymin <- minPycor
    world@extent@ymax <- maxPycor + 1
    res(world) <- 1

    # define the patch coordinates with the raster row and column numbers
    world@pxcor = (minPxcor + colFromCell(world, 1:(world@nrows * world@ncols))) - 1
    world@pycor = (maxPycor - rowFromCell(world, 1:(world@nrows * world@ncols))) + 1

    return(world)
  }
)


#' @export
#' @rdname createNLworld
setMethod(
  "createNLworld",
  signature = c("missing", "missing", "missing", "missing"),
  definition = function() {

    world <- new("NLworld",
                 minPxcor = -16, maxPxcor = 16,
                 minPycor = -16, maxPycor = 16)

    # define the raster coordinates with the NLworld extent
    world@extent@xmin <- -16
    world@extent@xmax <- 16 + 1
    world@extent@ymin <- -16
    world@extent@ymax <- 16 + 1
    res(world) <- 1

    # define the patch coordinates with the raster row and column numbers
    world@pxcor = (-16 + colFromCell(world, 1:(world@nrows * world@ncols))) - 1
    world@pycor = (16 - rowFromCell(world, 1:(world@nrows * world@ncols))) + 1

    return(world)
  }
)
