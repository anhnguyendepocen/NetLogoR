################################################################################
#' The \code{NLworld} class
#'
#' Behaves the same as a \code{RasterLayer} object except:
#'
#' A \code{NLworld} is a grid composed of squared patches.
#' Patches have two coordinates \code{pxcor} and \code{pycor}, considered as the
#' coordinates of their center.
#' \code{pxcor} and \code{pycor} are always integer and increment by 1.
#' When creating a new \code{NLworld}, the extent of the \code{NLworld} is of
#' \code{xmin = minPxcor}, \code{xmax = maxPxcor}, \code{ymin = minPycor}, and
#' \code{ymax = maxPycor}. The number of patches is equal to
#' \code{((maxPxcor - minPxcor) + 1) * ((maxPycor - minPycor) + 1)}.
#' \code{pxcor} can be seen as a column number and \code{pycor} can be seen as a
#' row number but increasing as you move up. However, \code{pxcor} and \code{pycor}
#' can be negative if there are patches to the left or below the patch \code{[0,0]}.
#'
#' The use of \code{[]} to extract \code{Raster*} cell values by row and column numbers
#' has been redefined to extract \code{NLworld} patches using the patches' coordinates
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
#' Behaves the same as a \code{RasterStack} object except that it is a collection
#' of \code{NLworld} objects.
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

