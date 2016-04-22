#' @importClassesFrom sp SpatialPoints SpatialPointsDataFrame
#' @name SpatialPointsDataTable
#' @rdname SpatialPointsDataTable
#' @author Eliot McIntire
#' @exportClass SpatialPointsDataTable
setClass("SpatialPointsDataTable",
         contains = "SpatialPointsDataFrame",
         slots = list(data = "data.table"),
         prototype = list(data = data.table())
)

setGeneric("SpatialPointsDataTable", function(coords, data,
                                              coords.nrs = numeric(0), proj4string = CRS(as.character(NA)),
                                              match.ID, bbox = NULL) {
  standardGeneric("SpatialPointsDataTable")
})

#' @export
#' @rdname SpatialPointsDataTable
setMethod(
  "SpatialPointsDataTable",
  signature(),
  definition = function (coords, data, coords.nrs = numeric(0), proj4string = CRS(as.character(NA)),
                         match.ID, bbox = NULL) {
    #if (!is(coords, "SpatialPoints"))
    #  coords = coordinates(coords)
    mtch = NULL
    cc.ID = row.names(coords)
    if (missing(match.ID)) {
      if (is.null(cc.ID))
        match.ID = FALSE
      else {
        mtch = match(cc.ID, row.names(data))
        match.ID = !any(is.na(mtch))
        if (match.ID && any(mtch != 1:nrow(data)))
          warning("forming a SpatialPointsDataFrame based on maching IDs, not on record order. Use match.ID = FALSE to match on record order")
      }
    }
    else if (is.character(match.ID)) {
      row.names(data) = data[, match.ID[1]]
      match.ID = TRUE
    }
    if (match.ID) {
      if (!is.null(cc.ID) && is(data, "data.frame")) {
        if (is.null(mtch))
          mtch = match(cc.ID, row.names(data))
        if (any(is.na(mtch)))
          stop("row.names of data and coords do not match")
        if (length(unique(mtch)) != nrow(data))
          stop("row.names of data and dimnames of coords do not match")
        data = data[mtch, , drop = FALSE]
      }
    }
    if (!is(coords, "SpatialPoints"))
      coords = SpatialPoints2(coords, proj4string = proj4string,
                             bbox = bbox)
    if (is.character(attr(data, "row.names")))
      dimnames(coords@coords)[[1]] = row.names(data)
    if(!is(data, "data.table")) data <- data.table(data)
    new("SpatialPointsDataTable", coords, data = data, coords.nrs = coords.nrs)
  })

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
#' t1 <- createTurtles(world = w1, n = 10)
#' t2 <- agentDataTable(coordinates(t1), t1@data)
agentDataTable <- function(coords, data, coords.nrs = numeric(0), proj4string = CRS(as.character(NA)),
                           match.ID, bbox = NULL) {
  dt <- data.table(coords, data)
  attr(dt, "proj4string") <- proj4string
  attr(dt, "bbox") <- bbox
  attr(dt, "coords.nrs") <- coords.nrs
  class(dt) <- c("agentDataTable", "data.table", "data.frame", "list", "oldClass", "vector")
  dt
}




