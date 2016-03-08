################################################################################
#' The \code{NLworld} class
#'
#' A \code{NLworld} object is a grid composed of squared patches that behaves
#' mostly the same as a \code{RasterLayer} object.
#' Patches have two coordinates \code{pxcor} and \code{pycor}, representing the
#' coordinates of their center. \code{pxcor} and \code{pycor} are always integer
#' and increment by 1. When creating a \code{NLworld}, the extent of the \code{NLworld}
#' is \code{xmin = minPxcor - 0.5}, \code{xmax = maxPxcor + 0.5},
#' \code{ymin = minPycor - 0.5}, and \code{ymax = maxPycor + 0.5}. The number of
#' patches is equal to \code{((maxPxcor - minPxcor) + 1) * ((maxPycor - minPycor) + 1)}.
#' \code{pxcor} can be seen as column numbers, increasing as you move right and
#' \code{pycor} can be seen as row numbers but increasing as you move up. However,
#' \code{pxcor} and \code{pycor} can be negative if there are patches to the left
#' or below the patch \code{[0,0]}.
#'
#' The use of \code{[]} to extract \code{Raster*} cell values by row and column numbers
#' has been redefined to extract \code{NLworld*} patches using the patches' coordinates
#' \code{[pxcor,pyxor]}. When multiple coordinates are provided, the order of the
#' values returned matches the order of the cell numbers as defined for a \code{Raster*}.
#' Similarly, when replacing values of several patches using \code{[]<-}, the values
#' will be given in the order of the cell numbers as defined for a \code{Raster*}.
#'
#' @inheritParams RasterLayer
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
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


################################################################################
#' The \code{NLworldStack} class
#'
#' A \code{NLworldStack} object is similar to a \code{RasterStack} object as it is
#' a collection of \code{NLworld} objects.
#'
#' @inheritParams RasterStack
#'
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
#' Creating a \code{NLworldStack}
#'
#' Stacking several \code{NLworld} together to obtain a single \code{NlworldStack}
#' which contains different layers for the different \code{NLworld} values.
#'
#' @param ... \code{NLworld} objects.
#'
#' @return A \code{NLworldStack} object with the \code{NLworld} stacked as layers.
#'
#' @examples
#' # Create 2 worlds with the default settings but different values.
#' w1 <- createNLworld()
#' w2 <- createNLworld()
#' w1[] <- runif(n = 1089)
#' w2[] <- runif(n = 1089)
#' # Stack the 2 world together.
#' w3 <- NLstack(w1, w2)
#' plot(w3)
#'
#' @export
#' @importFrom raster addLayer
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
#' @importFrom SpaDES objectNames
#' @rdname NLstack
setMethod(
  "NLstack",
  signature = "NLworld",
  definition = function(...) {

    dots <- list(...)

    # Get object names:
    layerNames <- objectNames("NLstack")
    if(any(duplicated(sapply(layerNames, function(x) x$objs)))) {
      stop("Please use unique layer names")
    }

    worldStack <- new("NLworldStack")

    for (i in seq_along(dots)) {
      world <- dots[[i]]
      if(world@data@names == ""){
        world@data@names <- list(as.name(layerNames[[i]]$objs))  # name the layer with the object name
      }
      worldStack <- addLayer(worldStack, world)
    }
    return(worldStack)
  }
)


################################################################################
#' The \code{NLworlds} class
#'
#' This class is the union of the \code{NLworld} and \code{NLworldStack} classes.
#'
#' @slot members  NLworld, NLworldStack
#'
#' @aliases NLworlds
#' @name NLworlds-class
#' @rdname NLworlds-class
#' @author Sarah Bauduin
#' @exportClass NLworlds
setClassUnion(name="NLworlds",
              members=c("NLworld", "NLworldStack")
)


################################################################################
#' Cells number from patches coordinates
#'
#' Report the cells number as defined for a \code{Raster*} object with the patches
#' coordinates \code{pxcor} and \code{pycor}.
#'
#' @param world A \code{NLworld*} object.
#'
#' @param pxcor A vector of \code{pxcor} coordinates.
#'
#' @param pycor A vector of \code{pycor} coordinates.
#'
#' @return A vector of cells number.
#'
#' @examples
#' # Create a world.
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' cellFromPxcorPycor(world = w1, pxcor = 0, pycor = 9)
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
  signature = c("NLworld", "numeric", "numeric"),
  definition = function(world, pxcor, pycor) {

    pxcor_ <- world@pxcor
    pycor_ <- world@pycor
    cell_ <- 1:(world@nrows * world@ncols)

    df1 <- data.frame(pxcor_, pycor_, cell_)
    df2 <- data.frame(pxcor_ = pxcor, pycor_ = pycor, rank = 1:length(pxcor))
    df3 <- merge(df1, df2, all.y = TRUE)
    df4 <- df3[order(df3[,"rank"]),]

    return(as.numeric(df4$cell_))
  }
)

#' @export
#' @rdname cellFromPxcorPycor
setMethod(
  "cellFromPxcorPycor",
  signature = c("NLworldStack", "numeric", "numeric"),
  definition = function(world, pxcor, pycor) {

    world_l <- world[[1]]
    cellFromPxcorPycor(world = world_l, pxcor = pxcor, pycor = pycor)

  }
)


################################################################################
#' Patches coordinates from cells number
#'
#' Report the patches coordinates \code{pxcor} and \code{pycor} from the cells
#' number as defined for a \code{Raster*} object.
#'
#' @param world   A \code{NLworld*} object.
#'
#' @param cellNum A vector of cells number.
#'
#' @return A matrix (ncol = 2) with the first column \code{pxcor} and the second
#'         column \code{pycor} in the order of the cells number given.
#'
#' @examples
#' # Create a world.
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' cellNum <- cellFromPxcorPycor(world = w1, pxcor = 0, pycor = 9)
#' PxcorPycorFromCell( world = w1, cellNum = cellNum)
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
  signature = c("NLworld", "numeric"),
  definition = function(world, cellNum) {

    pxcor_ <- world@pxcor
    pycor_ <- world@pycor
    cell_ <- 1:(world@nrows * world@ncols)

    df1 <- data.frame(pxcor_, pycor_, cell_)
    df2 <- data.frame(cellNum, rank = 1:length(cellNum))
    df3 <- merge(df1, df2, by.x = "cell_", by.y = "cellNum", all.y = TRUE)
    df4 <- df3[order(df3[,"rank"]),]

    pCoords <- cbind(pxcor = df4$pxcor_, pycor = df4$pycor_)
    return(pCoords)
  }
)

#' @export
#' @rdname PxcorPycorFromCell
setMethod(
  "PxcorPycorFromCell",
  signature = c("NLworldStack", "numeric"),
  definition = function(world, cellNum) {

    world_l <- world[[1]]
    PxcorPycorFromCell(world = world_l, cellNum = cellNum)

  }
)
