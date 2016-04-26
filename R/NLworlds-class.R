################################################################################
#' The NLworld class
#'
#'
#' A \code{NLworld} object is a grid composed of squared patches (i.e., pixels)
#' that behaves mostly the same as a \code{RasterLayer} object.
#' Patches have two coordinates \code{pxcor} and \code{pycor}, representing the
#' location of their center. \code{pxcor} and \code{pycor} are always integer
#' and increment by 1. \code{pxcor} increases as you move right and \code{pycor}
#' increases as you move up.  \code{pxcor} and \code{pycor} can be negative if
#' there are patches to the left or below the patch \code{[pxcor = 0, pycor = 0]}.
#'
#' When creating a \code{NLworld} object, its extent is equal to \code{xmin = minPxcor - 0.5},
#' \code{xmax = maxPxcor + 0.5}, \code{ymin = minPycor - 0.5}, and \code{ymax = maxPycor + 0.5}.
#' The number of patches created is then equal to
#' \code{((maxPxcor - minPxcor) + 1) * ((maxPycor - minPycor) + 1)}.
#'
#' \code{[]} can be used to extract values from a \code{NLworld} object by using
#' the patch coordinates \code{[pxcor, pyxor]}. When multiple coordinates are provided,
#' the order of the values returned matches the order of the cell numbers as defined
#' for a \code{RasterLayer}. \code{[] <-} can be used to replace patch values in a
#' \code{NLworld} object. Similarly, when replacing values of several patches, the
#' values should be given in the order of the cells numbers as defined for a \code{RasterLayer}.
#'
#' @inheritParams raster::RasterLayer
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @aliases NLworld
#' @name NLworld-class
#' @rdname NLworld-class
#' @author Sarah Bauduin, Eliot McIntire, and Alex Chubaty
#' @exportClass NLworld
#'
setClass(
  "NLworld",
  contains = c("RasterLayer"),
  representation (
    minPxcor = "numeric",
    maxPxcor = "numeric",
    minPycor = "numeric",
    maxPycor = "numeric",
    pxcor = "numeric",
    pycor = "numeric"
  )
)

#' @export
#' @name [
#' @docType methods
#' @rdname NLworld-class
setMethod(
  "[",
  signature("NLworld", "numeric", "numeric", "ANY"),
  definition = function(x, i, j, drop) {

    cells <- which(x@pxcor %in% i & x@pycor %in% j, TRUE) # cell number(s)
    xValues <- values(x)
    cellValues <- xValues[cells]

    return(cellValues)
  }
)

#' @export
#' @name [<-
#' @rdname NLworld-class
setReplaceMethod(
  "[",
  signature("NLworld","numeric","numeric","numeric"),
  definition = function(x, i, j, value) {
    cells <- which(x@pxcor %in% i & x@pycor %in% j, TRUE) # cell number(s)
    x@data@values[cells] <- value

    validObject(x)
    return(x)
  }
)




#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @aliases NLworldMatrix
#' @name NLworldMatrix
#' @rdname NLworldMatrix
#' @author Sarah Bauduin, Eliot McIntire, and Alex Chubaty
#' @exportClass NLworldMatrix
setOldClass("NLworldMatrix")

createNLworldMatrix <- function(data, minPxcor, maxPxcor, minPycor, maxPycor) {
  # define the patch coordinates with the raster row and column numbers
  numX <- (maxPxcor - minPxcor + 1)
  numY <- (maxPycor - minPycor + 1)
  world <- matrix(ncol=numY,
                 nrow=numX,data = data)
  attr(world, "xmin") <- minPxcor
  attr(world, "xmax") <- maxPxcor
  attr(world, "ymin") <- minPycor
  attr(world, "ymax") <- maxPycor
  attr(world, "res") <- 1
  class(world) <- c("NLworldMatrix", "matrix", "array", "mMatrix", "structure", "vector")
  return(world)
}

#' @export
#' @name [
#' @docType methods
#' @rdname NLworld-class
setMethod(
  "[",
  signature("NLworldMatrix", "numeric", "numeric", "ANY"),
  definition = function(x, i, j, drop) {

    cells <- which(x@pxcor %in% i & x@pycor %in% j, TRUE) # cell number(s)
    xValues <- values(x)
    cellValues <- xValues[cells]

    return(cellValues)
  }
)

#' @export
#' @name [<-
#' @rdname NLworld-class
setReplaceMethod(
  "[",
  signature("NLworldMatrix","numeric","numeric","numeric"),
  definition = function(x, i, j, value) {

    cells <- which(x[,"pxcor"] %in% i & x[,"pycor"] %in% j, TRUE) # cell number(s)
    x@data@values[cells] <- value

    validObject(x)
    return(x)
  }
)



#
#   contains = c("matrix"),
#   representation (
#     minPxcor = "numeric",
#     maxPxcor = "numeric",
#     minPycor = "numeric",
#     maxPycor = "numeric",
#     pxcor = "numeric",
#     pycor = "numeric"
#   )



################################################################################
#' The NLworldStack class
#'
#'
#' A \code{NLworldStack} object is similar to a \code{RasterStack} object, it is
#' a collection of \code{NLworld} objects with the same extent.
#'
#' @inheritParams raster::RasterStack
#'
#' @aliases NLworldStack
#' @name NLworldStack-class
#' @rdname NLworldStack-class
#' @author Sarah Bauduin
#' @exportClass NLworldStack
#'
setClass(
  "NLworldStack",
  contains = "RasterStack"
)

#' @export
#' @name [
#' @docType methods
#' @rdname NLworldStack-class
setMethod(
  "[",
  signature("NLworldStack", "numeric", "numeric", "ANY"),
  definition = function(x, i, j, drop) {

    x_l <- x[[1]]
    cells <- which(x_l@pxcor %in% i & x_l@pycor %in% j, TRUE) # same cell number(s) for all layers
    xValues <- values(x)
    cellValues <- xValues[cells,]

    if(class(cellValues) != "matrix"){
      cellValues <- matrix(cellValues, ncol = nlayers(x), byrow = TRUE, dimnames = list(NULL, names(cellValues)))
    }

    return(cellValues)
  }
)

#' @export
#' @name [<-
#' @rdname NLworldStack-class
setReplaceMethod(
  "[",
  signature("NLworldStack","numeric","numeric","matrix"),
  definition = function(x, i, j, value) {

    x_l <- x[[1]]
    cells <- which(x_l@pxcor %in% i & x_l@pycor %in% j, TRUE) # same cell number(s) for all layers
    xValues <- values(x)
    xValues[cells,] <- value

    for(k in 1:nlayers(x)){
      x <- setValues(x, values = xValues[,k], layer = k) # replace values of each layer
    }

    validObject(x)
    return(x)
  }
)

#' @export
#' @name [<-
#' @rdname NLworldStack-class
setReplaceMethod(
  "[",
  signature("NLworldStack","numeric","numeric","numeric"),
  definition = function(x, i, j, value) {

    value <- matrix(data = value, nrow = 1, ncol = length(value))

    # Copy/paste of above. Putting :
    # x[i,j] <- value
    # did not work

    x_l <- x[[1]]
    cells <- which(x_l@pxcor %in% i & x_l@pycor %in% j, TRUE) # same cell number(s) for all layers
    xValues <- values(x)
    xValues[cells,] <- value

    for(k in 1:nlayers(x)){
      x <- setValues(x, values = xValues[,k], layer = k) # replace values of each layer
    }

    validObject(x)
    return(x)

    # end copy/paste from above

  }
)


################################################################################
#' The \code{NLworlds} class
#'
#'
#' The \code{NLworlds} class is the union of the \code{NLworld} and \code{NLworldStack}
#' classes. Mostly used for building function purposes.
#'
#' @slot members  NLworld, NLworldStack
#'
#' @aliases NLworlds
#' @name NLworlds-class
#' @rdname NLworlds-class
#' @author Sarah Bauduin
#' @exportClass NLworlds
setClassUnion(name="NLworlds",
              members=c("NLworld", "NLworldStack", "NLworldMatrix")
)


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
#' Cells numbers from patches coordinates
#'
#' Report the cells numbers as defined for a Raster* object given the patches
#' coordinates \code{pxcor} and \code{pycor}.
#'
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
    browser()
    cellNum <- cellFromXY(world, cbind(x = pxcor, y = pycor))
    return(cellNum)
  }
)

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

################################################################################
#' Patches coordinates from cells numbers
#'
#' Report the patches coordinates given the cells numbers as defined for a Raster* object.
#'
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

