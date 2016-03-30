################################################################################
#' Create a world
#'
#' Create an empty grid of patches of class NLworld.
#'
#' @inheritParams fargs
#'
#' @return NLworld object composed of \code{(maxPxcor - minPxcor + 1) * (maxPycor - minPycor + 1)}
#'         patches. Patches value are \code{NA}.
#'
#' @details If no parameters value are provided, default values are: \code{minPxcor = -16},
#'          \code{maxPxcor = 16}, \code{minPycor = -16}, and \code{maxPycor = 16}.
#'
#'          See \code{help("NLworld-class")} for more details on the NLworld class.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- runif(length(w1))
#' plot(w1)
#'
#'
#' @export
#' @docType methods
#' @rdname createNLworld
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "createNLworld",
  function(minPxcor, maxPxcor, minPycor, maxPycor) {
  standardGeneric("createNLworld")
})

#' @export
#' @rdname createNLworld
setMethod(
  "createNLworld",
  signature = c("numeric", "numeric", "numeric", "numeric"),
  definition = function(minPxcor, maxPxcor, minPycor, maxPycor) {

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
#' Convert a Raster* object into a NLworlds object
#'
#' Convert a RasterLayer object into a NLworld object or a RasterStack object
#' into a NLworldStack object.
#'
#' @param raster RasterLayer or RasterStack object.
#'
#' @return NLworld or NLworldStack object depending on the input.
#'         Patches value are retained from the Raster* object.
#'
#' @details See \code{help("NLworlds-class")} for more details on the NLworlds
#'          classes.
#'
#'          The \code{raster} is resampled to match the coordinates system and
#'          resolution of a NLworlds using the nearest neighbor method. The
#'          extent will be bigger by 1 on the width and on the height.
#'
#' @examples
#' r <- raster(nrows = 21, ncols = 21, xmn = 0, ymn = 0, res = 5)
#' r[] <- runif(length(r))
#' plot(r)
#' extent(r)
#'
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

    worldR <- resample(raster, world, method = "ngb")

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
#' Report the patches maximum pxcor in the \code{world}.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#max-pcor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld()
#' maxPxcor(w1)
#'
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
#' Report the patches maximum pycor in the \code{world}.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#max-pcor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld()
#' maxPycor(w1)
#'
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
#' Report the patches minimum pxcor in the \code{world}.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#min-pcor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld()
#' minPxcor(w1)
#'
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
#' Report the patches minimum pycor in the \code{world}.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#min-pcor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld()
#' minPycor(w1)
#'
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
#' World width
#'
#' Report the width of the \code{world} in patch number.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#world-dim}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld()
#' worldWidth(w1)
#'
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
  signature = "NLworlds",
  definition = function(world) {
    w_width <- maxPxcor(world) - minPxcor(world) + 1
    return(w_width)
  }
)


################################################################################
#' World height
#'
#' Report the height of the \code{world} in patch number.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#world-dim}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld()
#' worldHeight(w1)
#'
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
  signature = "NLworlds",
  definition = function(world) {
    w_height <- maxPycor(world) - minPycor(world) + 1
    return(w_height)
  }
)


################################################################################
#' Clear turtles
#'
#' Kill all the turtles.
#'
#' @inheritParams fargs
#'
#' @param envir   The R environment where \code{turtles} are located.
#'                Default value is \code{envir = parent.frame(n = 1)}.
#'
#' @details Removes the \code{turtles} object from the R environment. If some of
#'          the agents in \code{turtles} are present in other agentsets, it does
#'          not remove them from these other agentsets. This function just remove
#'          the given \code{turtles} agentset.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#clear-turtles}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld()
#' t1 <- createTurtles(n = 10, world = w1)
#' t1 # show the object containing the turtles
#' clearTurtles(t1)
#' t1 # 't1' not found
#'
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
#' Clear world's patches
#'
#' Reset all patches values to \code{NA}.
#'
#' @inheritParams fargs
#'
#' @return NLworld object with \code{NA} values for all patches.
#'
#' @details The name of the layer is set to \code{""}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#clear-patches}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld()
#' w1[] <- runif(length(w1))
#' w1Val <- values(w1)
#' summary(w1Val)
#'
#' w1 <- clearPatches(w1)
#' w1Val <- values(w1)
#' summary(w1Val)
#'
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
#' Resize a world
#'
#' Change the size of the \code{world} and clear the patches and given \code{turtles}.
#'
#' @inheritParams fargs
#'
#' @return NLworld object composed of \code{(maxPxcor - minPxcor + 1) * (maxPycor - minPycor + 1)}
#'         patches. Patch value are \code{NA}.
#'
#' @details The given \code{world} and \code{turtles} are deleted, as well as the
#'          patches values contained in the \code{world}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#resize-world}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld()
#' w1[] <- runif(length(w1))
#' t1 <- createTurtles(n = 10, world = w1)
#' w2 <- resizeWorld(world = w1, minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9, turtles = t1)
#' w2[] <- runif(length(w2))
#' plot(w1) # does not work, 'w1' does not exist anymore
#' plot(w2)
#'
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
  signature = c("NLworlds", "numeric", "numeric", "numeric", "numeric", "SpatialPointsDataFrame"), # works
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
  signature = c("NLworlds", "numeric", "numeric", "numeric", "numeric", "missing"), # does not work (old world still there)
  definition = function(world, minPxcor, maxPxcor, minPycor, maxPycor) {
    rm(list = deparse(substitute(world)), envir = parent.frame(n = 1)) # why this doesn't work here?
    newWorld <- createNLworld(minPxcor = minPxcor, maxPxcor = maxPxcor,
                              minPycor = minPycor, maxPycor = maxPycor)
  }
)
