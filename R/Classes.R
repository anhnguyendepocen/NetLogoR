#' @export
SpatialPoints2 <- function (coords, proj4string = CRS(as.character(NA)), bbox = NULL)
{
  #coords = coordinates(coords)
  colNames = dimnames(coords)[[2]]
  if (is.null(colNames))
    colNames = paste("coords.x", 1:(dim(coords)[2]), sep = "")
  rowNames = dimnames(coords)[[1]]
  dimnames(coords) = list(rowNames, colNames)
  if (is.null(bbox))
    bbox <- NetLogoR::.bboxCoords(coords)
  new("SpatialPoints", coords = coords, bbox = bbox, proj4string = proj4string)
}

#' Faster .bboxCoords
#'
#' This is a drop in replacement for .bboxCoords in raster package.
#'
#' @importFrom matrixStats colRanges
.bboxCoords <- function(coords) {
    stopifnot(nrow(coords) > 0)
    bbox = colRanges(coords)
    dimnames(bbox)[[2]] = c("min", "max")
    as.matrix(bbox)
}

#' @include NLworlds-class.R
#' @importFrom raster extent
setMethod(
  "extent",
  signature("NLworldMs"),
  definition = function (x, ...) {
    extent(cbind(c(attr(x, "xmin"), attr(x, "ymin")),
          c(attr(x, "xmax"), attr(x, "ymax"))))
  })

#' agentDataTable class
#'
#' This is incomplete.
#'
#'
#' @importClassesFrom data.table data.table
#' @name agentDataTable
#' @rdname agentDataTable
#' @author Eliot McIntire
#' @exportClass agentDataTable
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createTurtles(world = w1, n = 10, agent = TRUE)
#'
#' library(microbenchmark)
#' N = 1e5
#' microbenchmark(createTurtles(world = w1, n = N, agent = TRUE),
#'               createTurtles(world = w1, n = N, agent = FALSE))
agentDataTable <- function(coords, ..., coords.nrs = numeric(0), proj4string = CRS(as.character(NA)),
                           match.ID, bbox = NULL) {
  dt <- data.table(coords, ...)
  attr(dt, "proj4string") <- proj4string
  attr(dt, "bbox") <- bbox
  attr(dt, "coords.nrs") <- coords.nrs
  class(dt) <- c("agentDataTable", "data.table", "data.frame", "list", "oldClass", "vector")
  dt
}

setOldClass("agentDataTable")



#' agentMatrix class
#'
#' This is incomplete.
#'
#'
#' @name agentMatrix
#' @rdname agentMatrix
#' @author Eliot McIntire
#' @exportClass agentMatrix
#' @examples
#' newAgent <- new("agentMatrix",
#'       coords=cbind(pxcor=c(1,2,5),pycor=c(3,4,6)),
#'       char = letters[c(1,2,6)],
#'       nums2 = c(4.5, 2.6, 2343),
#'       char2 = LETTERS[c(4,24,3)],
#'       nums = 5:7)
#'
#' # compare speeds
#' microbenchmark(times = 499,
#'   spdf={
#'      SpatialPointsDataFrame(coords=cbind(pxcor=c(1,2,5),pycor=c(3,4,6)),
#'      data = data.frame(
#'        char = letters[c(1,2,6)],
#'        nums2 = c(4.5, 2.6, 2343),
#'        char2 = LETTERS[c(4,24,3)],
#'        nums = 5:7))},
#'   agentMat = { agentMatrix(
#'      coords=cbind(pxcor=c(1,2,5),
#'      pycor=c(3,4,6)),
#'      char = letters[c(1,2,6)],
#'      nums2 = c(4.5, 2.6, 2343),
#'      char2 = LETTERS[c(4,24,3)],
#'      nums = 5:7)},
#'   agentMatDirect = { new("agentMatrix",
#'      coords=cbind(pxcor=c(1,2,5),
#'      pycor=c(3,4,6)),
#'      char = letters[c(1,2,6)],
#'      nums2 = c(4.5, 2.6, 2343),
#'      char2 = LETTERS[c(4,24,3)],
#'      nums = 5:7)})
#'
setClass("agentMatrix", contains = "matrix",
         slots = c(x = "matrix", levels = "list", isFactor = "logical",
                   bbox = "matrix"),
         prototype = prototype(x = matrix(numeric()), levels = list(), isFactor = NA,
                               bbox = matrix(numeric()))
         )

#if(getRversion() >= "3.2.0") {
setMethod("initialize", "agentMatrix", function(.Object="agentMatrix", coords, ...)
  {


    otherCols <- list(pxcor=coords[,1],pycor=coords[,2], ...)
    charCols <- sapply(otherCols, is.character)
    numCols <- sapply(otherCols, is.numeric)
    facCols <- sapply(otherCols, is.factor)
    otherCols[charCols] <- lapply(otherCols[charCols], function(x) {
      if (is.character(x)) {
        factor(x, levels = sort(unique(x)))
      } else {
        x
      }})
    if(length(otherCols)>0) {
      .Object@x <- cbind(do.call(cbind,otherCols))
      .Object@levels <- sapply(otherCols, function(x) if(is.factor(x)) levels(x) else NULL)
      .Object@isFactor <- charCols

      .Object@bbox <- NetLogoR:::.bboxCoords(coords)
    }
    .Object

})


################################################################################
#' Create a new agentMatrix object
#'
#' This is a fast alternative to the SpatialPointsDataFrame. It is meant to replace
#' that functionality, though there are not as many methods (yet). The object is primarily
#' a numeric matrix. Any character column passed to ... will be converted to a numeric,
#' using \code{as.factor} internally, and stored as a numeric. Methods using this class
#' will automatically convert character queries to the correct numeric alternative.
#'
#' @param coords  A matrix with 2 columns representing x and y coordinates
#' @param ... Vectors or a data.frame or a matrix of extra columns to add to the coordinates
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#clear-turtles}
#'
#' @examples
#' agentMatrix()
#' newAgent <- agentMatrix(
#'       coords=cbind(pxcor=c(1,2,5),pycor=c(3,4,6)),
#'       char = letters[c(1,2,6)],
#'       nums2 = c(4.5, 2.6, 2343),
#'       char2 = LETTERS[c(4,24,3)],
#'       nums = 5:7)
#'
#' @export
#' @docType methods
#' @rdname agentMatrix
#'
#' @author Eliot McIntire
#'
setGeneric(
  "agentMatrix",
  function(coords, ...) {
    standardGeneric("agentMatrix")
  }
)

#' @export
#' @rdname agentMatrix
setMethod(
  "agentMatrix",
  signature = c(coords="matrix"),
  definition = function(coords, ...) {
    new("agentMatrix", coords = coords, ...)
  }
)

setMethod(
  "coordinates",
  signature("agentMatrix"),
  definition = function (obj, ...) {
    obj[,1:2]
  })


setAs("matrix", "agentMatrix",
      function(from) {
        tmp <- new("agentMatrix", coords = from[,1:2,drop=FALSE], from[,-(1:2), drop = FALSE])
        tmp
      })


#' @export
#' @name [
#' @docType methods
#' @rdname agentMatrix
setMethod(
  "[",
  signature(x="agentMatrix", "numeric", "numeric", "ANY"),
  definition = function(x, i, j, ..., drop) {

    x@x <- x@x[i,unique(c(1:2,j)),...,drop=drop]
    x@levels <- x@levels[j-2]
    x@isFactor <- x@isFactor[j-2]
    x@bbox <- .bboxCoords(x@x[i,1:2])
    x


  }
)

#' @export
#' @name [
#' @docType methods
#' @rdname agentMatrix
setMethod(
  "[",
  signature(x="agentMatrix", "missing", "character", "ANY"),
  definition = function(x, j, ..., drop) {

    cols <- match(j, colnames(x@x))
    x[,cols,...,drop=FALSE]

  }
)

#' @export
#' @name [
#' @docType methods
#' @rdname agentMatrix
setMethod(
  "[",
  signature(x="agentMatrix", "numeric", "character", "ANY"),
  definition = function(x, i, j, ..., drop) {

    cols <- match(j, colnames(x@x))
    x[i,cols,...,drop=FALSE]

  }
)

#' @export
#' @name [
#' @docType methods
#' @rdname agentMatrix
setMethod(
  "[",
  signature(x="agentMatrix", "missing", "numeric", "ANY"),
  definition = function(x, j, ..., drop) {



    x@x <- x@x[,unique(c(1:2,j)),...,drop=drop]
    x@levels <- x@levels[c(1:2,j)]
    x@isFactor <- x@isFactor[c(1:2,j)]
    x@bbox <- .bboxCoords(x@x[,1:2])
    x

  }
)


#' @export
#' @name [
#' @docType methods
#' @rdname agentMatrix
setMethod(
  "show",
  signature(object="agentMatrix"),
  definition = function(object) {

    if(NROW(object@x)>0) {
      tmp <- data.frame(object@x)
      colNames <- colnames(tmp[,object@isFactor,drop=FALSE])
      colLogical <- which(object@isFactor)
      tmp[,object@isFactor] <-
        sapply(seq_len(sum(object@isFactor)), function(x) {
          as.character(factor(tmp[,colLogical[x]],
                              sort(unique(tmp[,colLogical[x]])),
                              object@levels[[colNames[x]]]) )
        })
    } else {
      tmp <- object@x
    }
      show(tmp)

  }
)

#' Extract coordinates from agentDataTable
#'
#' This is incomplete
#'
#' @export
#' @rdname coordinates
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createTurtles(world = w1, n = 10, agent = TRUE)
#' coordinates(t1)
#'
#' library(microbenchmark)
#' N = 1e4
#' coords <-  cbind(xcor=runif(N, xmin(w1), xmax(w1)),
#'                  ycor = runif(N, ymin(w1), ymax(w1)))
#' turtlesDT <- createTurtles(coords = coords, n = N, agent = TRUE)
#' turtlesDF <- createTurtles(coords = coords, n = N, agent = FALSE)
#' microbenchmark(coordinates(turtlesDT), coordinates(turtlesDF))
#' microbenchmark(coordinates(turtlesDT) <- coords)
setMethod(
  "coordinates",
  signature("agentDataTable"),
  definition = function (obj, ...) {
    #cbind(x=obj$x, y=obj$y)
    obj[,list(xcor,ycor)]
  })

#' @importFrom data.table ':='
setReplaceMethod(
  "coordinates",
  signature("agentDataTable"),
  definition = function (object, value) {
    #cbind(x=obj$x, y=obj$y)
    object[,`:=`(xcor=value[,"xcor"],ycor=value[,"ycor"])]
  })
