################################################################################
#' The NLworld class
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
  representation(
    minPxcor = "numeric",
    maxPxcor = "numeric",
    minPycor = "numeric",
    maxPycor = "numeric",
    pxcor = "numeric",
    pycor = "numeric"
  )
)

#' @include Classes.R
#' @export
#' @name [
#' @aliases [,NLworld,numeric,numeric,ANY-method
#' @docType methods
#' @rdname extract-methods
setMethod(
  "[",
  signature("NLworld", "numeric", "numeric", "ANY"),
  definition = function(x, i, j, ..., drop) {
    cells <- which(x@pxcor %in% i & x@pycor %in% j, TRUE) # cell number(s)
    xValues <- values(x)
    cellValues <- xValues[cells]
    return(cellValues)
})

#' @export
#' @name [<-
#' @aliases [<-,NLworld,numeric,numeric,ANY-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("NLworld", "numeric", "numeric", "ANY"),
  definition = function(x, i, j, value) {
    cells <- which(x@pxcor %in% i & x@pycor %in% j, TRUE) # cell number(s)
    x@data@values[cells] <- value
    validObject(x)
    return(x)
})

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
#' @aliases [,NLworldStack,numeric,numeric,ANY-method
#' @docType methods
#' @rdname extract-methods
setMethod(
  "[",
  signature("NLworldStack", "numeric", "numeric", "ANY"),
  definition = function(x, i, j, ..., drop) {
    x_l <- x[[1]]
    cells <- which(x_l@pxcor %in% i & x_l@pycor %in% j, TRUE) # same cell number(s) for all layers
    xValues <- values(x)
    cellValues <- xValues[cells,]

    if (class(cellValues) != "matrix") {
      cellValues <- matrix(cellValues, ncol = nlayers(x), byrow = TRUE,
                           dimnames = list(NULL, names(cellValues)))
    }

    return(cellValues)
  }
)

#' @export
#' @name [<-
#' @aliases [<-,NLworldStack,numeric,numeric,matrix-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("NLworldStack", "numeric", "numeric", "matrix"),
  definition = function(x, i, j, value) {

    x_l <- x[[1]]
    cells <- which(x_l@pxcor %in% i & x_l@pycor %in% j, TRUE) # same cell number(s) for all layers
    xValues <- values(x)
    xValues[cells,] <- value

    for (k in 1:nlayers(x)) {
      x <- setValues(x, values = xValues[,k], layer = k) # replace values of each layer
    }

    validObject(x)
    return(x)
  }
)

# #' @export
# #' @name [<-
# #' @aliases [<-,NLworldStack,numeric,numeric,numeric-method
# #' @rdname extract-methods
# setReplaceMethod(
#   "[",
#   signature("NLworldStack", "numeric", "numeric", "numeric"),
#   definition = function(x, i, j, value) {
#
#     value <- matrix(data = value, nrow = 1, ncol = length(value))
#
#     # Copy/paste of above. Putting :
#     # x[i,j] <- value
#     # did not work
#
#     x_l <- x[[1]]
#     cells <- which(x_l@pxcor %in% i & x_l@pycor %in% j, TRUE) # same cell number(s) for all layers
#     xValues <- values(x)
#     xValues[cells,] <- value
#
#     for (k in 1:nlayers(x)) {
#       x <- setValues(x, values = xValues[,k], layer = k) # replace values of each layer
#     }
#
#     validObject(x)
#     return(x)
#
#     # end copy/paste from above
#
#   }
# )

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
setClassUnion(name = "NLworlds",
              members = c("NLworld", "NLworldStack")
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
#' w1 <- set(world = w1, agents = patches(w1), val = runif(NLcount(patches(w1))))
#' w2 <- createNLworld()
#' w2 <- set(world = w2, agents = patches(w2), val = runif(NLcount(patches(w2))))
#' w3 <- NLstack(w1, w2)
#'
#' #clearPlot()
#' #Plot(w3)
#'
#' @export
#' @importFrom raster addLayer
#' @importFrom SpaDES objectNames
#' @docType methods
#' @rdname NLstack
#' @aliases stack
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
})

################################################################################
#' The NLworlds class
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
setClassUnion(name = "NLworlds",
              members = c("NLworld", "NLworldStack")
)

################################################################################
#' The NLworldMatrix class
#'
#' This is similar to \code{NLworld}, but it is an s3 class extension of
#' \code{matrix}.
#'
#' Careful: The methods \code{[]} and \code{[] <-} retrieve or assign values for
#' the patches (i.e., matrix cells) in the given order. When using \code{NLworlds}
#' objects, these methods retrive or assign values for the patches in the order
#' of the cell numbers as defined in \code{Raster*} objects, independently of the
#' given order of the patches coordinates inside the brackets.
#' When no patches coordinates are provided, the values retrieved or assigned
#' is done in the order of the cell numbers as defined in in \code{Raster*} objects.
#'
#' @aliases NLworldMatrix
#' @seealso NLworld
#' @name NLworldMatrix-class
#' @rdname NLworldMatrix-class
#' @author Sarah Bauduin, Eliot McIntire, and Alex Chubaty
#' @exportClass NLworldMatrix
#' @importClassesFrom raster Extent
#'
setClass(
  "NLworldMatrix",
  representation(
    .Data = "matrix",
    minPxcor = "numeric",
    maxPxcor = "numeric",
    minPycor = "numeric",
    maxPycor = "numeric",
    extent = "Extent",
    res = "numeric",
    pCoords = "matrix"
  )
)

#' @export
#' @name [
#' @aliases [,NLworldMatrix,numeric,numeric,ANY-method
#' @docType methods
#' @rdname extract-methods
setMethod(
  "[",
  signature("NLworldMatrix", "numeric", "numeric", "ANY"),
  definition = function(x, i, j, ..., drop) {

    # cellNum <- cellFromPxcorPycor(world = x, pxcor = i, pycor = j)
    # allValues <- as.numeric(t(x)) # t() to retrieve the values by rows
    # cellValues <- allValues[cellNum]
    colMat <- i - x@minPxcor + 1
    rowMat <- x@maxPycor - j + 1
    cellValues <- x[cbind(rowMat, colMat)]

    return(cellValues)
})

#' @export
#' @name [
#' @aliases [,NLworldMatrix,missing,missing,ANY-method
#' @rdname extract-methods
setMethod(
  "[",
  signature("NLworldMatrix", "missing", "missing", "ANY"),
  definition = function(x, ..., drop) {
    return(as.numeric(t(x@.Data)))
})

#' @export
#' @name [<-
#' @aliases [<-,NLworldMatrix,numeric,numeric,ANY-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("NLworldMatrix", "numeric", "numeric", "ANY"),
  definition = function(x, i, j, value) {
    # matj <- i - attr(x, "minPxcor") + 1
    # mati <- attr(x, "maxPycor") - j + 1
    # x[cbind(mati, matj)] <- value
    colMat <- i - x@minPxcor + 1
    rowMat <- x@maxPycor - j + 1
    x@.Data[cbind(rowMat, colMat)] <- value

    validObject(x)
    return(x)
})

#' @export
#' @name [<-
#' @aliases [<-,NLworldMatrix,missing,missing,ANY-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("NLworldMatrix", "missing", "missing", "ANY"),
  definition = function(x, i, j, value) {
    nCell <- dim(x@.Data)[1] * dim(x@.Data)[2]
    if (length(value) != nCell) {
      value <- rep(value, nCell)
    }
    x@.Data <- matrix(data = value, ncol = dim(x@.Data)[2], byrow = TRUE)
    validObject(x)
    return(x)
})

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
})

################################################################################
#' The NLworldArray class
#'
#' This is similar to \code{NLworldStack}, but it is an s4 class extension of
#' \code{array}.
#'
#' @seealso NLworldStack
#' @name NLworldArray-class
#' @rdname NLworldArray-class
#' @author Sarah Bauduin, Eliot McIntire, and Alex Chubaty
#' @exportClass NLworldArray
#' @importClassesFrom raster Extent
#'
setClass(
  "NLworldArray",
  representation(
    .Data = "array",
    minPxcor = "numeric",
    maxPxcor = "numeric",
    minPycor = "numeric",
    maxPycor = "numeric",
    extent = "Extent",
    res = "numeric",
    pCoords = "matrix"
  )
)

#' @export
#' @name [
#' @aliases [,NLworldArray,numeric,numeric,ANY-method
#' @docType methods
#' @rdname extract-methods
setMethod(
  "[",
  signature("NLworldArray", "numeric", "numeric", "ANY"),
  definition = function(x, i, j, ..., drop) {
    colMat <- i - x@minPxcor + 1
    rowMat <- x@maxPycor - j + 1
    pCoords <- cbind(rowMat, colMat)
    cellValues <- unlist(lapply(1:dim(x)[3], function(z){
      as.numeric(t(x@.Data[cbind(pCoords, z)]))
    }))
    dim(cellValues) <- c(NROW(pCoords), 2L)
    colnames(cellValues) <- dimnames(x@.Data)[[3]]
    return(cellValues)
})

#' @export
#' @name [
#' @aliases [,NLworldArray,missing,missing,ANY-method
#' @docType methods
#' @rdname extract-methods
setMethod(
  "[",
  signature("NLworldArray", "missing", "missing", "ANY"),
  definition = function(x, ..., drop) {
    cellValues <- unlist(lapply(1:dim(x)[3], function(z){as.numeric(t(x@.Data[,,z]))}))
    dim(cellValues) <- c(dim(x)[1] * dim(x)[2], dim(x)[3])
    colnames(cellValues) <- dimnames(x@.Data)[[3]]
    return(cellValues)
})

#' @export
#' @name [<-
#' @aliases [<-,NLworldArray,numeric,numeric,matrix-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("NLworldArray", "numeric", "numeric", "matrix"),
  definition = function(x, i, j, value) {
    colMat <- i - x@minPxcor + 1
    rowMat <- x@maxPycor - j + 1
    coords <- cbind(rowMat, colMat)
    for (k in 1:dim(x)[3]) {
      x@.Data[cbind(coords, k)] <- value[,k]
    }
    validObject(x)
    return(x)
})

#' @export
#' @name [<-
#' @aliases [<-,NLworldArray,missing,missing,matrix-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("NLworldArray", "missing", "missing", "matrix"),
  definition = function(x, i, j, value) {
    nCell <- dim(x@.Data)[1] * dim(x@.Data)[2]
    if (NROW(value) != nCell) { # assuming value has one row
      value <- value[rep(1, nCell),]
    }
    for (k in 1:dim(x)[3]) {
      x@.Data[,,k] <- matrix(data = value[, k], ncol = dim(x@.Data)[2], byrow = TRUE)
    }
    validObject(x)
    return(x)
})

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
    # similar dimensions can have different extent
    if (length(unique(lapply(NLwMs, FUN = function(x){x@extent}))) == 1) {
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
#' The NLworldMs class
#'
#'
#' The \code{NLworldMs} class is the union of the \code{NLworldMatrix} and \code{NLworldArray}
#' classes. Mostly used for building function purposes.
#'
#' @slot members  NLworldMatrix, NLworldArray
#'
#' @aliases NLworldMs
#' @name NLworldMs-class
#' @rdname NLworldMs-class
#' @author Sarah Bauduin, and Eliot McIntire
#' @exportClass NLworldMs
setClassUnion(name = "NLworldMs",
              members = c("NLworldMatrix", "NLworldArray")
)

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
})

#' @export
#' @rdname cellFromPxcorPycor
setMethod(
  "cellFromPxcorPycor",
  signature = c("NLworldMs", "numeric", "numeric"),
  definition = function(world, pxcor, pycor) {
    j <- pxcor - world@minPxcor + 1
    i <- world@maxPycor - pycor + 1
    (i - 1) * ncol(world) + j # Faster
})

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
})

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

################################################################################
#' Convert NLWorldMatrix indices to vector indices
#'
#' This can be used to convert indices that would work with an R \code{RasterLayer},
#' \code{matrix}, or \code{vector} to NLWorldMatrix indices.
#'
#' @param world NLworldMatrix object.
#'
#' @param cellNum Integer. Vector of cells number.
#'
#' @return Numeric vector with indices that will work with an NLWorldMatrix.
#'
#' @export
#' @docType methods
#' @rdname NLWorldIndex
#'
#' @author Eliot McIntire
#'
#' @examples
#' w1 <- createNLworldMatrix(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' w1[] <- 1:100
#' w1Ras <- world2raster(w1)
#' index <- 24
#' pxpy <- PxcorPycorFromCell(world = w1, cellNum = index)
#'
#' rasValue <- as.integer(unname(w1Ras[index]))
#' # Not correct index:
#' identical(w1[index], rasValue)
#'
#' # Correct index
#' identical(w1[NLWorldIndex(w1, index)], rasValue)
#'
setGeneric(
  "NLWorldIndex",
  function(world, cellNum) {
    standardGeneric("NLWorldIndex")
})

#' @export
#' @rdname NLWorldIndex
setMethod(
  "NLWorldIndex",
  signature = c("NLworldMatrix", "numeric"),
  definition = function(world, cellNum) {
    b <- dim(world)
    floor((cellNum - 1) / b[2])  + seq.int(from = 1, to = prod(b), by = b[1])[(cellNum - 1) %% b[2] + 1]
  }
)
