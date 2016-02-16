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
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' # Give the patches random values between 0 and 10.
#' w1[] <- runif(n = 25, min = 0, max = 10)
#' plot(w1)
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


################################################################################
#' Get maximum pxcor
#'
#' Report the patches maximum \code{pxcor} in a NLworld.
#'
#' @param world A \code{NLworld} object representing the world.
#'
#' @return A numeric value
#'
#' @details Does the same as accessing the slot with \code{NLworld@maxPxcor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create a world with the default settings.
#' w1 <- createNLworld()
#' getMaxPxcor(world = w1)
#'
#' @export
#' @docType methods
#' @rdname getMaxPxcor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "getMaxPxcor",
  function(world) {
    standardGeneric("getMaxPxcor")
  })

#' @export
#' @rdname getMaxPxcor
setMethod(
  "getMaxPxcor",
  signature = "NLworld",
  definition = function(world) {
    return(world@maxPxcor)
  }
)


################################################################################
#' Get maximum pycor
#'
#' Report the patches maximum \code{pycor} in a NLworld.
#'
#' @param world A \code{NLworld} object representing the world.
#'
#' @return A numeric value
#'
#' @details Does the same as accessing the slot with \code{NLworld@maxPycor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create a world with the default settings.
#' w1 <- createNLworld()
#' getMaxPycor(world = w1)
#'
#' @export
#' @docType methods
#' @rdname getMaxPycor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "getMaxPycor",
  function(world) {
    standardGeneric("getMaxPycor")
  })

#' @export
#' @rdname getMaxPycor
setMethod(
  "getMaxPycor",
  signature = "NLworld",
  definition = function(world) {
    return(world@maxPycor)
  }
)


################################################################################
#' Get minimum pxcor
#'
#' Report the patches minimum \code{pxcor} in a NLworld.
#'
#' @param world A \code{NLworld} object representing the world.
#'
#' @return A numeric value
#'
#' @details Does the same as accessing the slot with \code{NLworld@minPxcor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create a world with the default settings.
#' w1 <- createNLworld()
#' getMinPxcor(world = w1)
#'
#' @export
#' @docType methods
#' @rdname getMinPxcor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "getMinPxcor",
  function(world) {
    standardGeneric("getMinPxcor")
  })

#' @export
#' @rdname getMinPxcor
setMethod(
  "getMinPxcor",
  signature = "NLworld",
  definition = function(world) {
    return(world@minPxcor)
  }
)


################################################################################
#' Get minimum pycor
#'
#' Report the patches minimum \code{pycor} in a NLworld.
#'
#' @param world A \code{NLworld} object representing the world.
#'
#' @return A numeric value
#'
#' @details Does the same as accessing the slot with \code{NLworld@minPycor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create a world with the default settings.
#' w1 <- createNLworld()
#' getMinPycor(world = w1)
#'
#' @export
#' @docType methods
#' @rdname getMinPycor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "getMinPycor",
  function(world) {
    standardGeneric("getMinPycor")
  })

#' @export
#' @rdname getMinPycor
setMethod(
  "getMinPycor",
  signature = "NLworld",
  definition = function(world) {
    return(world@minPycor)
  }
)


################################################################################
#' NLworld width
#'
#' Report the width of the \code{NLworld} in patch number.
#'
#' @param world A \code{NLworld} object representing the world.
#'
#' @return A numeric value
#'
#' @details It equals \code{NLworld@maxPxcor - NLworld@minPxcor + 1}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create a world with the default settings.
#' w1 <- createNLworld()
#' worldWidth(world = w1)
#'
#' @export
#' @docType methods
#' @rdname worldWidth
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "worldWidth",
  function(world) {
    standardGeneric("worldWidth")
  })

#' @export
#' @rdname worldWidth
setMethod(
  "worldWidth",
  signature = "NLworld",
  definition = function(world) {
    return(world@maxPxcor - world@minPxcor + 1)
  }
)


################################################################################
#' NLworld height
#'
#' Report the height of the \code{NLworld} in patch number.
#'
#' @param world A \code{NLworld} object representing the world.
#'
#' @return A numeric value
#'
#' @details It equals \code{NLworld@maxPycor - NLworld@minPycor + 1}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create a world with the default settings.
#' w1 <- createNLworld()
#' worldHeight(world = w1)
#'
#' @export
#' @docType methods
#' @rdname worldHeight
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "worldHeight",
  function(world) {
    standardGeneric("worldHeight")
  })

#' @export
#' @rdname worldHeight
setMethod(
  "worldHeight",
  signature = "NLworld",
  definition = function(world) {
    return(world@maxPycor - world@minPycor + 1)
  }
)
