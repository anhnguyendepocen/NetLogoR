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
#' w1 <- set(world = w1, agents = patches(w1), val = runif(count(patches(w1))))
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
#' w1 <- set(world = w1, agents = patches(w1), val = runif(count(patches(w1))))
#' w1Val <- of(world = w1, agents = patches(w1))
#' summary(w1Val)
#'
#' w1 <- clearPatches(w1)
#' w1Val <- of(world = w1, agents = patches(w1))
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

