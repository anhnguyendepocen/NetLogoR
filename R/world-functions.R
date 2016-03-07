################################################################################
#' Create a \code{NLworld} object.
#'
#' Create an empty grid of patches of class \code{NLworld}.
#'
#' @param minPxcor  \code{pxcor} for patches on the left border of the \code{NLworld}.
#'                  Default value is \code{-16}, as in NetLogo.
#'
#' @param maxPxcor  \code{pxcor} for patches on the right border of the \code{NLworld}.
#'                  Default value is \code{16}, as in NetLogo.
#'
#' @param minPycor  \code{pycor} for patches at the bottom of the \code{NLworld}.
#'                  Default value is \code{-16}, as in NetLogo.
#'
#' @param maxPycor  \code{pycor} for patches at the top of the \code{NLworld}.
#'                  Default value is \code{16}, as in NetLogo.
#'
#' @details See \code{help("NLworld*")} for more details on the \code{NLworld} class.
#'
#' @return A \code{NLworld} object composed of \code{(maxPxcor - minPxcor + 1) * (maxPycor - minPycor + 1)}
#'         patches. Patches value are \code{NA}.
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
    world@extent@xmin <- minPxcor - 0.5
    world@extent@xmax <- maxPxcor + 0.5
    world@extent@ymin <- minPycor - 0.5
    world@extent@ymax <- maxPycor + 0.5
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
#' Convert to a \code{NLworld*} object.
#'
#' Convert a \code{RasterLayer} object into a \code{NLworld} object or a \code{RasterStack}
#' into a {NLworldStack} object.
#'
#' @param raster A \code{RasterLayer} or a \code{RasterStack} object.
#'
#' @return A \code{NLworld} or a \code{NLworldStack} object depending on the input.
#'         Patches value are retained from the \code{Raster*} object.
#'
#' @details See \code{help("NLworld*")} for more details on the \code{NLworld*} classes.
#'          The \code{Raster*} is resampled to match the coordinates system and
#'          resolution of a \code{NLworld*} using a bilinear interpolation. The
#'          extent will be bigger by 1 on the width and on the height.
#'
#' @examples
#' r <- raster(system.file("external/test.grd", package="raster")) # from the raster package
#' plot(r)
#' extent(r)
#' world <- convertNLworld(raster = r)
#' plot(world)
#' extent(world)
#' minPxcor(world)
#' maxPxcor(world)
#' minPycor(world)
#' maxPycor(world)
#'
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

    minPxcor <- round(raster@extent@xmin)
    maxPxcor <- round(raster@extent@xmax)
    minPycor <- round(raster@extent@ymin)
    maxPycor <- round(raster@extent@ymax)
    world <- createNLworld(minPxcor = minPxcor, maxPxcor = maxPxcor, minPycor = minPycor, maxPycor = maxPycor)

    worldR <- resample(raster, world)

    worldNL <- as(worldR, "NLworld")
    worldNL@minPxcor <- minPxcor
    worldNL@maxPxcor <- maxPxcor
    worldNL@minPycor <- minPycor
    worldNL@maxPycor <- maxPycor
    worldNL@pxcor <- world@pxcor
    worldNL@pycor <- world@pycor

    return(worldNL)
})


#' @export
#' @rdname convertNLworld
setMethod(
  "convertNLworld",
  signature = c("RasterStack"),
  definition = function(raster) {

    worldStack <- new("NLworldStack")

    for(i in 1:nlayers(raster)){
      world <- convertNLworld(raster = raster[[i]])
      worldStack <- addLayer(worldStack, world)
    }

    return(worldStack)
})


################################################################################
#' Maximum pxcor
#'
#' Report the patches maximum \code{pxcor} in a \code{NLworld*}.
#'
#' @param world A \code{NLworld*} object representing the world.
#'
#' @return A numeric value
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

#' @export
#' @rdname maxPxcor
setMethod(
  "maxPxcor",
  signature = "NLworldStack",
  definition = function(world) {
    world_l <- world[[1]]
    maxPxcor(world = world_l)
  }
)

################################################################################
#' Maximum pycor
#'
#' Report the patches maximum \code{pycor} in a \code{NLworld*}.
#'
#' @param world A \code{NLworld*} object representing the world.
#'
#' @return A numeric value
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

#' @export
#' @rdname maxPycor
setMethod(
  "maxPycor",
  signature = "NLworldStack",
  definition = function(world) {
    world_l <- world[[1]]
    maxPycor(world = world_l)
  }
)

################################################################################
#' Minimum pxcor
#'
#' Report the patches minimum \code{pxcor} in a \code{NLworld*}.
#'
#' @param world A \code{NLworld*} object representing the world.
#'
#' @return A numeric value
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

#' @export
#' @rdname minPxcor
setMethod(
  "minPxcor",
  signature = "NLworldStack",
  definition = function(world) {
    world_l <- world[[1]]
    minPxcor(world = world_l)
  }
)

################################################################################
#' Minimum pycor
#'
#' Report the patches minimum \code{pycor} in a \code{NLworld*}.
#'
#' @param world A \code{NLworld*} object representing the world.
#'
#' @return A numeric value
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

#' @export
#' @rdname minPycor
setMethod(
  "minPycor",
  signature = "NLworldStack",
  definition = function(world) {
    world_l <- world[[1]]
    minPycor(world = world_l)
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


################################################################################
#' Clear turtles
#'
#' Kill all \code{turtles}.
#'
#' @param turtles A \code{SpatialPointsDataFrame} object representing the turtles.
#'
#' @param envir   The R environment wher the turtles are.
#'                Default value is \code{parent.frame(n = 1)}
#'
#' @details Remove the \code{turtles} object from the R environment.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create one turtle.
#' t1 <- SpatialPointsDataFrame(coords = matrix(c(1,2), nrow = 1, ncol = 2), data = data.frame(NA))
#' t1
#' clearTurtles(turtles = t1)
#' t1 # does not work
#'
#' @export
#' @docType methods
#' @rdname clearTurtles
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "clearTurtles",
  function(turtles, envir = parent.frame(n = 1)) {
    standardGeneric("clearTurtles")
  }
)

#' @export
#' @rdname clearTurtles
setMethod(
  "clearTurtles",
  signature = c("SpatialPointsDataFrame"),
  definition = function(turtles, envir) {
    rm(list = deparse(substitute(turtles)), envir = envir)
  }
)


################################################################################
#' Clear patches
#'
#' Reset all patches value to \code{NA}.
#'
#' @param world A \code{NLworld} or \code{NLworldStack} object.
#'
#' @return An empty \code{NLworld} object.
#'
#' @details Similar as setValues(world, NA).
#'          The name of the layer is reset to "".
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create a world with the default settings.
#' w1 <- createNLworld()
#' w1[] <- runif(n = 1089)
#' w1_val <- values(w1)
#' summary(w1_val)
#'
#' w1 <- clearPatches(world = w1)
#' w1_val <- values(w1)
#' summary(w1_val)
#'
#' @export
#' @docType methods
#' @rdname clearPatches
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "clearPatches",
  function(world) {
    standardGeneric("clearPatches")
  }
)

#' @export
#' @rdname clearPatches
setMethod(
  "clearPatches",
  signature = c("NLworld"),
  definition = function(world) {
    worldNA <- setValues(world, NA)
    worldNA@data@names <- ""
    return(worldNA)
  }
)

#' @export
#' @rdname clearPatches
setMethod(
  "clearPatches",
  signature = c("NLworldStack"),
  definition = function(world) {
    world_l <- world[[1]]
    clearPatches(world = world_l)
  }
)


################################################################################
#' Resize NLworld
#'
#' Change the size of the \code{NLworld}. Previous patches and turtles are lost.
#'
#' @param world     A \code{NLworld} or \code{NLworldStack} object representing the current world.
#'
#' @param minPxcor  \code{pxcor} for patches on the left border of the new \code{NLworld}
#'                  Default value = \code{-16}, as in NetLogo.
#'
#' @param maxPxcor  \code{pxcor} for patches on the right border of the new \code{NLworld}
#'                  Default value = \code{16}, as in NetLogo.
#'
#' @param minPycor  \code{pycor} for patches at the bottom of the new \code{NLworld}
#'                  Default value \code{-16}, as in NetLogo.
#'
#' @param maxPycor  \code{pycor} for patches at the top of the new \code{NLworld}
#'                  Default value \code{16}, as in NetLogo.
#'
#' @param turtles  A \code{SpatialPointsDataFrame} object representing the turtles.
#'
#' @return A \code{NLworld} object composed of \code{(maxPxcor - minPxcor + 1) * (maxPycor - minPycor + 1)}
#'         patches. Patch value are \code{NA}.
#'
#' @details By using \code{resizeWorld}, the previous \code{NLworld}, patches and
#'          turtles are deleted.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create a world with the default settings.
#' w1 <- createNLworld()
#' w1[] <- runif(n = 1089)
#' # Create one turtle
#' t1 <- SpatialPointsDataFrame(coords = matrix(c(1,2), nrow = 1, ncol = 2), data = data.frame(NA))
#' w2 <- resizeWorld(world = w1, minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9, turtles = t1)

#' w2[] <- runif(n = 100)
#' plot(w1) # does not work, w1 does not exist anymore
#' plot(w2)
#'
#' @export
#' @importFrom sp SpatialPointsDataFrame
#' @docType methods
#' @rdname resizeWorld
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "resizeWorld",
  function(world, minPxcor, maxPxcor, minPycor, maxPycor, turtles) {
    standardGeneric("resizeWorld")
  })

#' @export
#' @rdname resizeWorld
setMethod(
  "resizeWorld",
  signature = c("NLworld", "numeric", "numeric", "numeric", "numeric", "SpatialPointsDataFrame"), # works
  definition = function(world, minPxcor, maxPxcor, minPycor, maxPycor, turtles) {
    rm(list = deparse(substitute(world)), envir = parent.frame(n = 1))
    rm(list = deparse(substitute(turtles)), envir = parent.frame(n = 1)) # I could not make clearTurles works here
    newWorld <- createNLworld(minPxcor = minPxcor, maxPxcor = maxPxcor,
                              minPycor = minPycor, maxPycor = maxPycor)
  }
)

#' @export
#' @rdname resizeWorld
setMethod(
  "resizeWorld",
  signature = c("NLworld", "numeric", "numeric", "numeric", "numeric", "missing"), # does not work (old world still there)
  definition = function(world, minPxcor, maxPxcor, minPycor, maxPycor) {
    rm(list = deparse(substitute(world)), envir = parent.frame(n = 1)) # why this doesn't work here?
    newWorld <- createNLworld(minPxcor = minPxcor, maxPxcor = maxPxcor,
                              minPycor = minPycor, maxPycor = maxPycor)
  }
)

#' @export
#' @rdname resizeWorld
setMethod(
  "resizeWorld",
  signature = c("NLworld", "missing", "missing", "missing", "missing", "SpatialPointsDataFrame"), # does not work (old world and old turtles still there)
  definition = function(world, turtles) {
    resizeWorld(world = world, minPxcor = -16, maxPxcor = 16, minPycor = -16,
                maxPycor = 16, turtles = turtles)
  }
)

#' @export
#' @rdname resizeWorld
setMethod(
  "resizeWorld",
  signature = c("NLworld", "missing", "missing", "missing", "missing", "missing"), # does not work (old world still there)
  definition = function(world) {
    resizeWorld(world = world, minPxcor = -16, maxPxcor = 16, minPycor = -16,
                maxPycor = 16)
  }
)

#' @export
#' @rdname resizeWorld
setMethod(
  "resizeWorld",
  signature = c("NLworldStack", "numeric", "numeric", "numeric", "numeric", "SpatialPointsDataFrame"), # works
  definition = function(world, minPxcor, maxPxcor, minPycor, maxPycor, turtles) {
    rm(list = deparse(substitute(world)), envir = parent.frame(n = 1))
    rm(list = deparse(substitute(turtles)), envir = parent.frame(n = 1))
    newWorld <- createNLworld(minPxcor = minPxcor, maxPxcor = maxPxcor,
                              minPycor = minPycor, maxPycor = maxPycor)
  }
)

#' @export
#' @rdname resizeWorld
setMethod(
  "resizeWorld",
  signature = c("NLworldStack", "numeric", "numeric", "numeric", "numeric", "missing"), # does not work (old world still there)
  definition = function(world, minPxcor, maxPxcor, minPycor, maxPycor) {
    rm(list = deparse(substitute(world)), envir = parent.frame(n = 1))
    newWorld <- createNLworld(minPxcor = minPxcor, maxPxcor = maxPxcor,
                              minPycor = minPycor, maxPycor = maxPycor)
  }
)

#' @export
#' @rdname resizeWorld
setMethod(
  "resizeWorld",
  signature = c("NLworldStack", "missing", "missing", "missing", "missing", "SpatialPointsDataFrame"), # does not work (old world and old turtles still there)
  definition = function(world, turtles) {
    resizeWorld(world = world, minPxcor = -16, maxPxcor = 16, minPycor = -16,
                maxPycor = 16, turtles = turtles)
  }
)

#' @export
#' @rdname resizeWorld
setMethod(
  "resizeWorld",
  signature = c("NLworldStack", "missing", "missing", "missing", "missing", "missing"), # does not work (old world still there)
  definition = function(world) {
    resizeWorld(world = world, minPxcor = -16, maxPxcor = 16, minPycor = -16,
                maxPycor = 16)
  }
)
