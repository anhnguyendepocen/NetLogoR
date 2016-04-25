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

.bboxCoords <- function(coords) {
    stopifnot(nrow(coords) > 0)
    bbox = colRanges(coords)
    dimnames(bbox)[[2]] = c("min", "max")
    as.matrix(bbox)

}


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
