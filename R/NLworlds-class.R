################################################################################
#' Create a NLworldStack
#'
#' Stack multiple NLworld objects.
#'
#' @param ... NLworld objects.
#'
#' @return NLworldStack object with the NLworld stacked as layers.
#'
#' @details The NLworld objects must have the same extents and cannot be empty
#'          (i.e., the patches values must be different than \code{NA}).
#'
#' @examples
#' w1 <- createNLworld()
#' w1 <- set(world = w1, agents = patches(w1), val = runif(count(patches(w1))))
#' w2 <- createNLworld()
#' w2 <- set(world = w2, agents = patches(w2), val = runif(count(patches(w2))))
#' w3 <- NLstack(w1, w2)
#'
#' library(SpaDES)
#' clearPlot()
#' Plot(w3)
#'
#'
#' @export
#' @importClassesFrom NetLogoRClasses NLworldStack
#' @importFrom raster addLayer
#' @importFrom SpaDES objectNames
#' @docType methods
#' @rdname NLstack
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "NLstack",
  signature = "...",
  function(...) {
    standardGeneric("NLstack")
  })

#' @export
#' @rdname NLstack
setMethod(
  "NLstack",
  signature = "NLworld",
  definition = function(...) {

    rasterS <- stack(...)

    layerNames <- objectNames("NLstack") # get object names
    names(rasterS) <- sapply(layerNames, function(x) x$objs)

    worldStack <- as(rasterS, "NLworldStack")

    return(worldStack)
  }
)


################################################################################
#' Create a world
#'
#' Create a world of patches of class NLworldMatrix.
#'
#' @inheritParams fargs
#'
#' @param data Vector of length \code{(maxPxcor - minPxcor + 1) * (maxPycor - minPycor + 1)}.
#'             Default is \code{NA}.
#'
#' @return NLworldMatrix object.
#'
#' @export
#' @importFrom raster extent
#' @importClassesFrom NetLogoRClasses NLworldMatrix
#' @docType methods
#' @rdname createNLworldMatrix
#'
#' @author Sarah Bauduin, Eliot McIntire, and Alex Chubaty
#'
setGeneric(
  "createNLworldMatrix",
  function(minPxcor, maxPxcor, minPycor, maxPycor, data = NA) {
    standardGeneric("createNLworldMatrix")
  })

#' @export
#' @rdname createNLworldMatrix
setMethod(
  "createNLworldMatrix",
  signature = c(minPxcor = "numeric", maxPxcor = "numeric", minPycor = "numeric", maxPycor = "numeric"),
  definition = function(minPxcor, maxPxcor, minPycor, maxPycor, data) {

    numX <- (maxPxcor - minPxcor + 1)
    numY <- (maxPycor - minPycor + 1)
    data <- matrix(ncol = numX,
                    nrow = numY, data = data, byrow = TRUE) # byrow = TRUE to be similar as a raster when assigning data

    world <- new("NLworldMatrix",
                 .Data = data,
                 minPxcor = minPxcor, maxPxcor = maxPxcor, minPycor = minPycor, maxPycor = maxPycor,
                 extent = extent(minPxcor - 0.5, maxPxcor + 0.5, minPycor - 0.5, maxPycor + 0.5),
                 res = c(1, 1),
                 pCoords = cbind(pxcor = rep_len(minPxcor:maxPxcor, length.out = numX * numY),
                                 pycor = rep(maxPycor:minPycor, each = numX))
                 )

    return(world)
})

#' @export
#' @rdname createNLworldMatrix
setMethod(
  "createNLworldMatrix",
  signature = c("missing", "missing", "missing", "missing", "missing"),
  definition = function() {
    createNLworldMatrix(-16, 16, -16, 16, data = NA)
  }
)


################################################################################
#' Create a NLworldArray
#'
#' Stack multiple NLworldMatrix data in an array.
#'
#' @param ... NLworldMatrix objects.
#'
#' @return NLworldArray object.
#'
#' @export
#' @importClassesFrom NetLogoRClasses NLworldArray
#' @importFrom SpaDES updateList
#' @importFrom abind abind
#' @docType methods
#' @rdname NLworldArray
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "NLworldArray",
  signature = "...",
  function(...) {
    standardGeneric("NLworldArray")
  })

#' @export
#' @rdname NLworldArray
setMethod(
  "NLworldArray",
  signature = "NLworldMatrix",
  definition = function(...) {

    NLwMs <- list(...)

    if(length(unique(lapply(NLwMs, FUN = function(x){x@extent}))) == 1) { # similar dimensions can have different extent
      out <- abind::abind(NLwMs@.Data, along = 3)
    } else {
      stop("NLworldMatrix extents must all be equal")
    }
    objNames <- as.character(substitute(deparse(...))[-1])
    dimnames(out) <- list(NULL, NULL, objNames)

    world <- new("NLworldArray",
                 .Data = out,
                 minPxcor = NLwMs[[1]]@minPxcor, maxPxcor = NLwMs[[1]]@maxPxcor,
                 minPycor = NLwMs[[1]]@minPycor, maxPycor = NLwMs[[1]]@maxPycor,
                 extent = NLwMs[[1]]@extent,
                 res = c(1, 1),
                 pCoords = NLwMs[[1]]@pCoords
    )

    return(world)
})



################################################################################
#' Cells numbers from patches coordinates
#'
#' Report the cells numbers as defined for a Raster* object given the patches
#' coordinates \code{pxcor} and \code{pycor}.
#'
#' @param world NLworlds or NLworldMs object.
#' @inheritParams fargs
#'
#' @return Numeric. Vector of cells number.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' cellFromPxcorPycor(world = w1, pxcor = 0, pycor = 9)
#'
#'
#' @export
#' @docType methods
#' @rdname cellFromPxcorPycor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "cellFromPxcorPycor",
  function(world, pxcor, pycor) {
    standardGeneric("cellFromPxcorPycor")
  })

#' @export
#' @rdname cellFromPxcorPycor
setMethod(
  "cellFromPxcorPycor",
  signature = c("NLworlds", "numeric", "numeric"),
  definition = function(world, pxcor, pycor) {
    cellNum <- cellFromXY(world, cbind(x = pxcor, y = pycor))
    return(cellNum)
  }
)

#' @export
#' @rdname cellFromPxcorPycor
setMethod(
  "cellFromPxcorPycor",
  signature = c("NLworldMs", "numeric", "numeric"),
  definition = function(world, pxcor, pycor) {
    j <- pxcor - world@minPxcor + 1
    i <- world@maxPycor - pycor + 1
    (i-1)*ncol(world@.Data)+j # Faster
  }
)

################################################################################
#' Patches coordinates from cells numbers
#'
#' Report the patches coordinates given the cells numbers as defined for a Raster* object.
#'
#' @param world NLworlds or NLworldMs object.
#' @inheritParams fargs
#'
#' @param cellNum Integer. Vector of cells number.
#'
#' @return Matrix (ncol = 2) with the first column "pxcor" and the second
#'         column "pycor" in the order of the given \code{cellNum}.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' cellNum <- cellFromPxcorPycor(world = w1, pxcor = 0, pycor = 9)
#' PxcorPycorFromCell(world = w1, cellNum = cellNum)
#'
#'
#' @export
#' @docType methods
#' @rdname PxcorPycorFromCell
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "PxcorPycorFromCell",
  function(world, cellNum) {
    standardGeneric("PxcorPycorFromCell")
  })

#' @export
#' @rdname PxcorPycorFromCell
setMethod(
  "PxcorPycorFromCell",
  signature = c("NLworlds", "numeric"),
  definition = function(world, cellNum) {
    XY <- xyFromCell(world, cellNum)
    pCoords <- cbind(pxcor = XY[,1], pycor = XY[,2])
    return(pCoords)
  }
)

#' @export
#' @rdname PxcorPycorFromCell
setMethod(
  "PxcorPycorFromCell",
  signature = c("NLworldMs", "numeric"),
  definition = function(world, cellNum) {
    pCoords <- world@pCoords[cellNum,,drop = FALSE]
    return(pCoords)
  }
)
