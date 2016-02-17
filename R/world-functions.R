################################################################################
#' Create a \code{NLworld}
#'
#' Create an empty grid of patches of class \code{NLworld}.
#'
#' @param minPxcor  \code{pxcor} for patches on the left border of the \code{NLworld}
#'                  Default value = \code{-16}, as in NetLogo.
#'
#' @param maxPxcor  \code{pxcor} for patches on the right border of the \code{NLworld}
#'                  Default value = \code{16}, as in NetLogo.
#'
#' @param minPycor  \code{pycor} for patches at the bottom of the \code{NLworld}
#'                  Default value \code{-16}, as in NetLogo.
#'
#' @param maxPycor  \code{pycor} for patches at the top of the \code{NLworld}
#'                  Default value \code{16}, as in NetLogo.
#'
<<<<<<< HEAD
#' @details See \code{help("NLworld")} for more details on the \code{NLworld}
#'
#' @return A \code{NLworld} object composed of \code{(maxPxcor - minPxcor + 1) * (maxPycor - minPycor + 1)}
#'         patches. Patches value are NA.
=======
#' @details See \code{help("NLworld")} for more details on the NLworld.
#'
#' @return A NLworld object composed of \code{(maxPxcor - minPxcor + 1) * (maxPycor - minPycor + 1)}
#'         patches. Patch value are \code{NA}.
>>>>>>> origin/master
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

     createNLworld(-16, 16, -16, 16)

  }
)


################################################################################
#' Convert to a \code{NLworld}
#'
#' Convert a \code{Raster*} object into a \code{NLworld}.
#'
#' @param raster A \code{RasterLayer} or a \code{RasterStack} object.
#'
#' @details See \code{help("NLworld")} for more details on the \code{NLworld}.
#'
#' @return A \code{NLworld} or \code{NLworldStack} object depending on the input.
#'         Patches value are retained from the \code{Raster*} object.
#'
#' @examples
#'
#' @export
#' @docType methods
#' @rdname convertNLworld
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "convertNLworld",
  function(raster) {
    standardGeneric("convertNLworld")
  })


#' @export
#' @rdname convertNLworld
setMethod(
  "convertNLworld",
  signature = c("RasterLayer"),
  definition = function(raster) {

    world <- as(raster, "NLworld")

    # find the patch intersecting x = 0 and/or y = 0 to start the patches coordinates
    # otherwise the left patches have their pxcor = 0
    # and the bottom patches have their pycor = 0.
    patch0y <- colFromX(object = world, x = 0)
    patchx0 <- rowFromY(object = world, y = 0)

    if(!is.na(patch0y) & !is.na(patchx0)){

      world@minPxcor <- -(patch0y - 1)
      world@maxPxcor <- world@ncols - patch0y
      world@minPycor <- -(world@nrows - patchx0)
      world@maxPycor <- patchx0 - 1

    } else if(!is.na(patch0y)){

      world@minPxcor <- -(patch0y - 1)
      world@maxPxcor <- world@ncols - patch0y
      world@minPycor <- 0
      world@maxPycor <- world@nrows - 1

    } else if(!is.na(patchx0)){

      world@minPxcor <- 0
      world@maxPxcor <- world@ncols - 1
      world@minPycor <- -(world@nrows - patchx0)
      world@maxPycor <- patchx0 - 1

    } else {

      world@minPxcor <- 0
      world@maxPxcor <- world@ncols - 1
      world@minPycor <- 0
      world@maxPycor <- world@nrows - 1

    }

    world@pxcor = (world@minPxcor + colFromCell(world, 1:(world@nrows * world@ncols))) - 1
    world@pycor = (world@maxPycor - rowFromCell(world, 1:(world@nrows * world@ncols))) + 1

    return(world)
})


#' @export
#' @rdname convertNLworld
setMethod(
  "convertNLworld",
  signature = c("RasterStack"),
  definition = function(raster) {

    worldStack <- as(raster, "NLworldStack")

    for(i in 1:nlayers(worldStack)){
      worldStack[[i]] <- convertNLworld(raster = worldStack[[i]])
    }

    return(worldStack)
})


################################################################################
#' Maximum pxcor
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
#' maxPxcor(world = w1)
#'
#' @export
#' @docType methods
#' @rdname maxPxcor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "maxPxcor",
  function(world) {
    standardGeneric("maxPxcor")
  })

#' @export
#' @rdname maxPxcor
setMethod(
  "maxPxcor",
  signature = "NLworld",
  definition = function(world) {
    return(world@maxPxcor)
  }
)


################################################################################
#' Maximum pycor
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
#' maxPycor(world = w1)
#'
#' @export
#' @docType methods
#' @rdname maxPycor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "maxPycor",
  function(world) {
    standardGeneric("maxPycor")
  })

#' @export
#' @rdname maxPycor
setMethod(
  "maxPycor",
  signature = "NLworld",
  definition = function(world) {
    return(world@maxPycor)
  }
)


################################################################################
#' Minimum pxcor
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
#' minPxcor(world = w1)
#'
#' @export
#' @docType methods
#' @rdname minPxcor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "minPxcor",
  function(world) {
    standardGeneric("minPxcor")
  })

#' @export
#' @rdname minPxcor
setMethod(
  "minPxcor",
  signature = "NLworld",
  definition = function(world) {
    return(world@minPxcor)
  }
)


################################################################################
#' Minimum pycor
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
#' minPycor(world = w1)
#'
#' @export
#' @docType methods
#' @rdname minPycor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "minPycor",
  function(world) {
    standardGeneric("minPycor")
  })

#' @export
#' @rdname minPycor
setMethod(
  "minPycor",
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
