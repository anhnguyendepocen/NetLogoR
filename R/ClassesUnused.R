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
