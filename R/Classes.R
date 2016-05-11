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


#' @include NLworlds-class.R
#' @importFrom raster extent
setMethod(
  "extent",
  signature("NLworldMs"),
  definition = function (x, ...) {
    attr(x, "extent")
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




setMethod(
  "coordinates",
  signature("agentMatrix"),
  definition = function (obj, ...) {
    obj@.Data[,1:2]
  })



# @export
# @name [
# @docType methods
# @rdname agentMatrix
# setMethod(
#   "NROW",
#   signature(x="agentMatrix"),
#   definition = function(x) {
#     NROW(x@.Data)
#   }
# )

#' Methods for agentMatrix class objects
#'
#' These are NetLogoR specific methods.
#'
#' @export
#' @name [
#' @docType methods
#' @rdname agentMatrix
setMethod(
  "nrow",
  signature(x="agentMatrix"),
  definition = function(x) {
    nrow(x@.Data)
  }
)

#' @export
#' @name head
#' @docType methods
#' @rdname agentMatrix
head.agentMatrix <- function(x, n = 6L, ...) {
  x[seq_len(n),,drop=FALSE]
}

#' @export
#' @name tail
#' @docType methods
#' @rdname agentMatrix
tail.agentMatrix <- function(x, n = 6L, ...) {
  len <- NROW(x@.Data)
  ind <- (len - n + 1):len
  out <- x[ind,,drop=FALSE]
  rownames(out@.Data) <- ind
  out

}



#' @export
#' @name cbind
#' @docType methods
#' @rdname agentMatrix
#setMethod(
#  "cbind",
#  "agentMatrix",
#  definition = function(..., deparse.level) {
cbind.agentMatrix <- function(..., deparse.level) {
    tmp <- list(...)
    if(length(tmp) != 2) stop("cbind for agentMatrix is only defined for 2 agentMatrices")
    notAM <- sapply(tmp, function(x) all(is.na(x@.Data[,1:2])))

    if(NROW(tmp[[2]]@.Data) == 1) {
      tmp[[2]]@.Data <- tmp[[2]]@.Data[rep_len(1, length.out=NROW(tmp[[1]]@.Data)),]
    }

    if(any(colnames(tmp[[1]]@.Data)[-(1:2)] %in% colnames(tmp[[2]]@.Data)[-(1:2)])) {
      stop("There are duplicate columns in the two agentMatrix objects. Please remove duplicates.")
    }
    newMat <- fastCbind(tmp[[1]]@.Data, tmp[[2]]@.Data[,-(1:2),drop=FALSE])
    tmp[[1]]@.Data <- newMat
    colnames(newMat)
    tmp[[1]]@levels <- SpaDES::updateList(tmp[[2]]@levels, tmp[[1]]@levels)

    tmp[[1]]

  }


#' @export
#' @name [
#' @docType methods
#' @rdname agentMatrix
setMethod(
  "length",
  signature(x="agentMatrix"),
  definition = function(x) {
    length(x@.Data)
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
