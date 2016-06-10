if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("xcor", "ycor"))
}

#' SpatialPoints2
#'
#' needs documentation
#'
#' @param coords       needs documentation
#' @param proj4string  needs documentation
#' @param bbox         needs documentation
#'
#' @importFrom sp CRS
#' @export
SpatialPoints2 <- function(coords, proj4string = CRS(as.character(NA)), bbox = NULL) {
  #coords = sp::coordinates(coords)
  colNames = dimnames(coords)[[2]]
  if (is.null(colNames))
    colNames = paste("coords.x", 1:(dim(coords)[2]), sep = "")
  rowNames = dimnames(coords)[[1]]
  dimnames(coords) = list(rowNames, colNames)
  if (is.null(bbox))
    bbox <- .bboxCoords(coords)
  new("SpatialPoints", coords = coords, bbox = bbox, proj4string = proj4string)
}

#' Faster .bboxCoords
#'
#' This is a drop in replacement for .bboxCoords in raster package.
#'
#' @param coords documentation needed
#'
#' @importFrom matrixStats colRanges
#' @rdname bboxCoords
.bboxCoords <- function(coords) {
    stopifnot(length(coords) > 0)
    bbox = colRanges(coords)
    dimnames(bbox)[[2]] = c("min", "max")
    bbox
}

#' @include NLworlds-class.R
#' @importFrom raster extent
setMethod(
  "extent",
  signature("NLworldMs"),
  definition = function(x, ...) {
    attr(x, "extent")
})


#' The agentMatrix class
#'
#' Documentation needed.
#'
#' @name agentMatrix-class
#' @rdname agentMatrix-class
#' @author Eliot McIntire
#' @exportClass agentMatrix
#' @examples
#' newAgent <- new("agentMatrix",
#'       coords = cbind(pxcor = c(1, 2, 5), pycor = c(3, 4, 6)),
#'       char = letters[c(1, 2, 6)],
#'       nums2 = c(4.5, 2.6, 2343),
#'       char2 = LETTERS[c(4, 24, 3)],
#'       nums = 5:7)
#'
#' # compare speeds -- about 5x faster
#' if(require(microbenchmark)) {
#'   microbenchmark(times = 499,
#'     spdf = {SpatialPointsDataFrame(
#'       coords = cbind(pxcor = c(1, 2, 5), pycor = c(3, 4, 6)),
#'       data = data.frame(
#'           char = letters[c(1, 2, 6)],
#'           nums2 = c(4.5, 2.6, 2343),
#'           char2 = LETTERS[c(4, 24, 3)],
#'           nums = 5:7))},
#'     agentMat = {agentMatrix(
#'         coords = cbind(pxcor = c(1, 2, 5),
#'         pycor = c(3, 4, 6)),
#'         char = letters[c(1, 2, 6)],
#'         nums2 = c(4.5, 2.6, 2343),
#'         char2 = LETTERS[c(4, 24, 3)],
#'         nums = 5:7)},
#'     agentMatDirect = {new("agentMatrix",
#'         coords = cbind(pxcor = c(1, 2, 5),
#'         pycor = c(3, 4, 6)),
#'         char = letters[c(1, 2, 6)],
#'         nums2 = c(4.5, 2.6, 2343),
#'         char2 = LETTERS[c(4, 24, 3)],
#'         nums = 5:7)})
#' }
setClass("agentMatrix", contains = "matrix",
         slots = c(x = "matrix", levels = "list", bbox = "matrix"),
         prototype = prototype(
           x = matrix(numeric()), levels = list(), bbox = matrix(numeric())
         )
)

#' A meta class for agentMatrix and SpatialPointsDataFrame
#'
#' Both these types can be used by NetLogoR to describe turtle agents.
#'
#' @aliases agentClasses
#' @name agentClasses-class
#' @rdname agentClasses-class
#' @author Eliot McIntire
#' @importClassesFrom sp SpatialPixelsDataFrame SpatialPointsDataFrame
#' @exportClass agentClasses
setClassUnion(name = "agentClasses",
              members = c("agentMatrix", "SpatialPointsDataFrame", "SpatialPixelsDataFrame")
)

setMethod(
  "initialize",
  "agentMatrix",
  function(.Object="agentMatrix", coords, ..., levelsAM) {
    Coords <- TRUE
    if (missing(coords)) {
      coords <- NULL
    }
    if (is.null(coords)) {
      coords <- matrix(c(NA, NA), ncol = 2)
      Coords <- FALSE
    } else {
      coords <- unname(coords)
    }
    if (is.data.frame(coords)) {
      coords <- as.matrix(coords)
    }
    dotCols <- list(...)

    if (missing(levelsAM)) {
      if (all(sapply(dotCols, is.numeric))) {
        isMatrix <- sapply(dotCols, is.matrix)
        singleMatrix <- length(isMatrix)==1
        if (singleMatrix) {
          otherCols <- do.call(cbind, list(xcor = coords[,1],ycor = coords[,2], dotCols[[1]]))
        } else {
          otherCols <- append(list(xcor = coords[,1],ycor = coords[,2]), dotCols)
          otherCols <- do.call(cbind, otherCols)
        }
        if (length(otherCols) > 0) {
          .Object@.Data <- otherCols
          .Object@levels <- list(NULL) #rep(list(NULL), ncol(.Object@.Data))
          #names(.Object@levels) <- colnames(otherCols)
          if (Coords) {
            .Object@bbox <- .bboxCoords(coords)
          } else {
            .Object@bbox <- matrix(rep(NA_real_, 4), ncol = 2)
          }
        }
      } else {
        isDF <- sapply(dotCols, function(x) is(x, "data.frame"))
        if (any(names(dotCols) == "stringsAsFactors"))
          dotCols$stringsAsFactors <- NULL
        if (any(isDF)) {
          dotCols <- unlist(lapply(dotCols, as.list), recursive = FALSE)
        } else { #if (all(sapply(dotCols, is.matrix))) {
          # can't just do "do.call(cbind, dotCols)" because some may be numerics, others not... would coerce to all character
          dotCols <- unlist(lapply(seq_len(length(dotCols)), function(x) {
            isMat <- is.matrix(dotCols[[x]])
            if(isMat)  {
              innerMats <- lapply(seq_len(ncol(dotCols[[x]])), function(y) dotCols[[x]][,y])
              names(innerMats) <- colnames(dotCols[[x]])
            } else {
              innerMats <- dotCols[x]
            }
            return(innerMats)
          }), recursive = FALSE)

        }
        otherCols <- append(list(xcor = coords[,1],ycor = coords[,2]), dotCols)
        charCols <- sapply(otherCols, is.character)
        #charCols <- names(charCols)[charCols]
        numCols <- sapply(otherCols, is.numeric)
        facCols <- sapply(otherCols, is.factor)
        charCols <- facCols | charCols
        otherCols[charCols] <- lapply(otherCols[charCols], function(x) {
            factor(x, levels = sort(unique(x)))
          })

        if (length(otherCols[[1]]) == 1) names(otherCols[[1]]) <- 1
        if (length(otherCols) > 0) {
          .Object@.Data <- do.call(cbind,otherCols)
          .Object@levels <- lapply(otherCols[charCols], function(x) if (is.factor(x)) levels(x) else NULL)
          if (Coords) {
            .Object@bbox <- .bboxCoords(coords)
          } else {
            .Object@bbox <- matrix(rep(NA_real_, 4), ncol = 2)
          }
        }
      }
    } else {
      if ( (is.matrix(dotCols[[1]]) & is.numeric(dotCols[[1]])) | is(dotCols[[1]], "agentMatrix"))
        .Object@.Data <- cbind(coords, dotCols[[1]])
      else
        stop("if passing levelsAM, then ... must be a numeric matrix")
      .Object@levels <- levelsAM
      if (Coords) {
        .Object@bbox <- .bboxCoords(coords)
      } else {
        .Object@bbox <- matrix(rep(NA_real_, 4), ncol = 2)
      }
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
#' @param ... Vectors, a data.frame, or a matrix of extra columns to add to the coordinates,
#'            or a SpatialPointsDataFrame.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#clear-turtles}
#'
#' @examples
#' newAgent <- agentMatrix(
#'       coords = cbind(pxcor = c(1, 2, 5), pycor = c(3, 4, 6)),
#'       char = letters[c(1, 2, 6)],
#'       nums2 = c(4.5, 2.6, 2343),
#'       char2 = LETTERS[c(4, 24, 3)],
#'       nums = 5:7)
#'
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1 <- set(world = w1, agents = patches(w1), val = runif (count(patches(w1))))
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#'
#'
#' @export
#' @docType methods
#' @rdname agentMatrix
#'
#' @author Eliot McIntire
#'
setGeneric(
  "agentMatrix",
  function(..., coords) {
    standardGeneric("agentMatrix")
  }
)

#' @export
#' @rdname agentMatrix
setMethod(
  "agentMatrix",
  signature = c(coords = "matrix"),
  definition = function(..., coords) {
    new("agentMatrix", coords = coords, ...)
  }
)

#' @export
#' @importFrom sp coordinates
#' @rdname agentMatrix
setMethod(
  "agentMatrix",
  signature = c(coords = "missing"),
  definition = function(...) {
    if (is(..., "SpatialPointsDataFrame")) {
      dots <- list(...)
      new("agentMatrix", coords = sp::coordinates(dots[[1]]), dots[[1]]@data)
    } else {
      new("agentMatrix", coords = NULL, ...)
    }
  }
)

#' Set spatial coordinates
#'
#' @param obj  documentation needed
#' @param ...  additional arguments that may be used by particular methods
#'
#'  description needed
#'
#' @export
#' @rdname coordinates
setMethod(
  "coordinates",
  signature("agentMatrix"),
  definition = function(obj, ...) {
    obj@.Data[, 1:2, drop = FALSE]
})

#' @export
setAs("matrix", "agentMatrix",
      function(from) {
        tmp <- new("agentMatrix", coords = from[, 1:2, drop = FALSE], from[, -(1:2), drop = FALSE])
        tmp
})

#' @export
setAs("data.frame", "agentMatrix",
      function(from) {
        tmp <- new("agentMatrix", coords = from[, 1:2, drop = FALSE], from[, -(1:2), drop = FALSE])
        tmp
})

#' @export
setAs("agentMatrix", "data.frame",
      function(from) {
        tmp <- data.frame(from@.Data)
        rownames(tmp) <- seq_len(NROW(tmp))
        nam <- names(from@levels)
        tmp[,nam] <- lapply(nam, function(n) from@levels[[n]][tmp[,n]])

        tmp
})

#' Extract or Replace Parts of an Object
#'
#' Operators acting on vectors, matrices, arrays and lists to extract or replace parts.
#'
#' @note Extract methods for agentMatrix class will generally maintain the \code{agentMatrix} class.
#' This means that there will still be coordinates, character columns represented as numerics etc.
#' \code{$} is for extracting the raw columns and does not maintain the \code{agentMatrix} class.
#'
#' @param x     A \code{NLworld} object from which to extract element(s) or
#'                in which to replace element(s).
#' @param i     Indices specifying elements to extract or replace.
#' @param j     see \code{i}.
#' @param ...   other named arguments
#' @param drop  not implemented
#'
#' @export
#' @name [
#' @aliases [,agentMatrix,numeric,numeric,ANY-method
#' @docType methods
#' @rdname extract-methods
setMethod(
  "[",
  signature(x = "agentMatrix", "numeric", "numeric", "ANY"),
  definition = function(x, i, j, ..., drop) {
    colNames <- colnames(x@.Data)[j]
    levelInd <- match(colNames, names(x@levels))
    x@.Data <- x@.Data[i,unique(c(1:2, j)), ..., drop = FALSE]
    if (all(is.na(levelInd))) {
      x@levels <- list(NULL)
    } else {
      x@levels <- x@levels[colNames]
    }
    x@bbox <- .bboxCoords(x@.Data[, 1:2, drop = FALSE])
    x
  }
)

#' @export
#' @name [
#' @aliases [,agentMatrix,logical,missing,ANY-method
#' @docType methods
#' @rdname extract-methods
setMethod(
  "[",
  signature(x = "agentMatrix", "logical", "missing", "ANY"),
  definition = function(x, i, ..., drop) {
    x@.Data <- x@.Data[i, , drop = FALSE]
    if (length(x@.Data) > 0)
      x@bbox <- .bboxCoords(x@.Data[, 1:2, drop = FALSE])
    x
})

#' @export
#' @name [
#' @docType methods
#' @aliases [,agentMatrix,numeric,missing,ANY-method
#' @rdname extract-methods
setMethod(
  "[",
  signature(x = "agentMatrix", "numeric", "missing", "ANY"),
  definition = function(x, i, ..., drop) {
    x@.Data <- x@.Data[i, , drop = FALSE]
    if (length(x@.Data) > 0) {
      x@bbox <- .bboxCoords(x@.Data[, 1:2, drop = FALSE])
    }
    x
})

#' @export
#' @name [
#' @aliases [,agentMatrix,missing,missing,missing-method
#' @docType methods
#' @rdname extract-methods
setMethod(
  "[",
  signature(x = "agentMatrix", "missing", "missing", "missing"),
  definition = function(x, i, j, ..., drop) {
    x@.Data
})

#' @export
#' @name [
#' @aliases [,agentMatrix,missing,character,ANY-method
#' @docType methods
#' @rdname extract-methods
setMethod(
  "[",
  signature(x = "agentMatrix", "missing", "character", "ANY"),
  definition = function(x, j, ..., drop) {
    cols <- match(j, colnames(x@.Data))
    x[, cols, ..., drop = FALSE]
})

#' @export
#' @name [
#' @aliases [,agentMatrix,numeric,character,ANY-method
#' @docType methods
#' @rdname extract-methods
setMethod(
  "[",
  signature(x = "agentMatrix", "numeric", "character", "ANY"),
  definition = function(x, i, j, ..., drop) {
    cols <- match(j, colnames(x@.Data))
    x[i, cols, ..., drop = FALSE]
})

#' @export
#' @name [
#' @aliases [,agentMatrix,missing,numeric,ANY-method
#' @docType methods
#' @rdname extract-methods
setMethod(
  "[",
  signature(x = "agentMatrix", "missing", "numeric", "ANY"),
  definition = function(x, i, j, ..., drop) {
    colNames <- colnames(x@.Data)[j]
    levelInd <- match(colNames, names(x@levels))
    x@.Data <- x@.Data[, unique(c(1:2, j)), ..., drop = drop]
    if (all(is.na(levelInd))) {
      x@levels <- list(NULL)
    } else {
      #x@levels[[levelInd]][x@.Data[,j]]
      x@levels <- x@levels[colNames]
    }
    x@bbox <- .bboxCoords(x@.Data[, 1:2, drop = FALSE])
    x
})

#' @param value  Any R object
#'
#' @export
#' @name [<-
#' @aliases [<-,agentMatrix,numeric,numeric,numeric-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("agentMatrix", "numeric", "numeric", "numeric"),
  definition = function(x, i, j, value) {
    x@.Data[i,j] <- value
    validObject(x)
    return(x)
})

#' @export
#' @name [<-
#' @aliases [<-,agentMatrix,missing,numeric,numeric-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("agentMatrix", "missing", "numeric", "numeric"),
  definition = function(x, i, j, value) {
    x@.Data[,j] <- value
    validObject(x)
    return(x)
})

#' @export
#' @name [<-
#' @aliases [<-,agentMatrix,numeric,missing,numeric-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("agentMatrix", "numeric", "missing", "numeric"),
  definition = function(x, i, j, value) {
    x@.Data[i,] <- value
    validObject(x)
    return(x)
})

#' @export
#' @name [<-
#' @aliases [<-,agentMatrix,numeric,character,data.frame-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("agentMatrix", "numeric", "character", "data.frame"),
  definition = function(x, i, j, value) {
    colNums <- match(j, colnames(x))
    x[i,colNums] <- value
    x@.Data[i,colNums] <- value
    validObject(x)
    return(x)
})

#' @export
#' @name [<-
#' @aliases [<-,agentMatrix,numeric,numeric,character-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("agentMatrix", "numeric", "numeric", "character"),
  definition = function(x, i, j, value) {
    colNames <- colnames(x@.Data)[j]
    levelInd <- match(colNames, names(x@levels))
    levelExists <- all(value %in% x@levels[[levelInd]])
    if (!levelExists) {
      uniqueLevels <- unique(c(x@levels[[levelInd]], value))
      x@levels[[levelInd]] <- as.character(factor(x = uniqueLevels,
                                                  levels = uniqueLevels))
    }
    rmLevels <- c(unique(x@.Data[,j]),match(value, x@levels[[levelInd]]))
    if (length(unique(rmLevels)) < length(x@levels[[levelInd]])) {
      uniqueLevels <- x@levels[[levelInd]][unique(rmLevels)]
      x@levels[[levelInd]] <- as.character(factor(x = uniqueLevels,
                                                  levels = uniqueLevels))
    }

    x@.Data[i,j] <- match(value, x@levels[[levelInd]])
    validObject(x)
    return(x)
})

#' @export
#' @name [<-
#' @aliases [<-,agentMatrix,missing,numeric,character-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("agentMatrix", "missing", "numeric", "character"),
  definition = function(x, i, j, value) {
    x[seq_len(NROW(x)),j] <- value
    return(x)
})

#' @export
#' @name [<-
#' @aliases [<-,agentMatrix,missing,character,character-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("agentMatrix", "missing", "character", "character"),
  definition = function(x, i, j, value) {
    cols <- match(j, colnames(x@.Data))
    x[seq_len(NROW(x)),cols] <- value
    return(x)
})

#' @export
#' @name [<-
#' @aliases [<-,agentMatrix,numeric,character,character-method
#' @rdname extract-methods
setReplaceMethod(
  "[",
  signature("agentMatrix", "numeric", "character", "character"),
  definition = function(x, i, j, value) {
    cols <- match(j, colnames(x@.Data))
    x[i,cols] <- value
    return(x)
})

#' @param name  documentation needed
#'
#' @export
#' @docType methods
#' @rdname extract-methods
setMethod(
  "$",
  signature(x = "agentMatrix"),
  definition = function(x, name) {
    if (name %in% names(x@levels)) {
      x@levels[[name]][x@.Data[,name]]
    } else {
      x@.Data[,name]
    }
})

#' Relational Operators
#'
#' Binary operators which allow the comparison of values in an agentMatrix.
#'
#' @param e1  An \code{agentMatrix} object.
#' @param e2  atomic vector, symbol, call, or other object for which methods have been written.
#'
#' @export
#' @importFrom stats na.omit
#' @docType methods
#' @rdname agentMatrix-compare-methods
setMethod(
  "==",
  signature("agentMatrix", "character"),
  definition = function(e1, e2) {
    colNames <- colnames(e1@.Data)
    if (length(colNames) < 3) {
      warning("Coordinates are not characters, returning test for both coordinates")
      return(matrix(rep(FALSE, length(e1)), ncol = 2))
    }
    levelInd <- match(colNames, names(e1@levels))
    levelIndNoNA <- na.omit(levelInd)
    whInd <- which(!is.na(levelInd))
    if (all(is.na(levelInd))) {
      (e1@.Data == e2)[,-(1:2)]
    } else {
      logic <- e1@.Data == e2
      logic[,whInd] <- sapply(levelIndNoNA, function(z) {
        e1@levels[[z]][e1@.Data[, whInd[z]]]
      }) == e2
      logic[, -(1:2)]
    }
})

#' Show an object or get information about the object
#'
#' description needed
#'
#' @param object  An \code{agentMatrix} object.
#'
#' @export
#' @docType methods
#' @rdname agentMatrix-show-methods
setMethod(
  "show",
  signature(object = "agentMatrix"),
  definition = function(object) {
    if (NROW(object@.Data) > 0) {
      tmp <- data.frame(object@.Data)
      colNames <- colnames(tmp[, names(object@levels), drop = FALSE])
      tmp[,names(object@levels)] <-
        sapply(seq_along(names(object@levels)), function(x) {
          curLevels <- sort(unique(tmp[,names(object@levels)[x]]))
          as.character(factor(tmp[,names(object@levels)[x]],
                              curLevels,
                              object@levels[[colNames[x]]][curLevels]) )
        })
    } else {
      tmp <- object@.Data
    }
    show(tmp[,-(1:2),drop=FALSE])
})

# @export
# @name [
# @docType methods
# @rdname agentMatrix-show-methods
# setMethod(
#   "NROW",
#   signature(x="agentMatrix"),
#   definition = function(x) {
#     NROW(x@.Data)
#   }
# )

#' @param x  An \code{agentMatrix} object.
#'
#' @export
#' @docType methods
#' @rdname agentMatrix-show-methods
setMethod(
  "length",
  signature(x = "agentMatrix"),
  definition = function(x) {
    length(x@.Data)
})

#' @export
#' @docType methods
#' @rdname agentMatrix-show-methods
setMethod(
  "nrow",
  signature(x = "agentMatrix"),
  definition = function(x) {
    nrow(x@.Data)
})

#' @param n  documentation needed
#' @param ...  documentation needed
#'
#' @method head agentMatrix
#' @export head agentMatrix
#' @name head
#' @docType methods
#' @rdname agentMatrix-show-methods
head.agentMatrix <- function(x, n = 6L, ...) {
  x[seq_len(n), , drop = FALSE]
}

#' @method tail agentMatrix
#' @export tail agentMatrix
#' @name tail
#' @docType methods
#' @rdname agentMatrix-show-methods
tail.agentMatrix <- function(x, n = 6L, ...) {
  len <- NROW(x@.Data)
  ind <- (len - n + 1):len
  out <- x[ind, , drop = FALSE]
  rownames(out@.Data) <- ind
  out
}

#' Combine R Objects by Rows or Columns
#'
#' Take a sequence of agentMatrix arguments and combine by columns or rows, respectively.
#'
#' @param deparse.level  description needed
#' @param ... description needed
#'
#' @method cbind agentMatrix
#' @export
#' @name cbind
#' @docType methods
#' @rdname agentMatrix-bind-methods
cbind.agentMatrix <- function(..., deparse.level) {
    tmp <- list(...)
    if (length(tmp) != 2) stop("cbind for agentMatrix is only defined for 2 agentMatrices")
    notAM <- sapply(tmp, function(x) all(is.na(x@.Data[,1:2])))

    if (NROW(tmp[[2]]@.Data) == 1) {
      tmp[[2]]@.Data <- tmp[[2]]@.Data[rep_len(1, length.out = NROW(tmp[[1]]@.Data)),]
    }

    if (any(colnames(tmp[[1]]@.Data)[-(1:2)] %in% colnames(tmp[[2]]@.Data)[-(1:2)])) {
      stop("There are duplicate columns in the two agentMatrix objects. Please remove duplicates.")
    }
    newMat <- fastCbind(tmp[[1]]@.Data, tmp[[2]]@.Data[, -(1:2), drop = FALSE])
    tmp[[1]]@.Data <- newMat
    colnames(newMat)
    tmp[[1]]@levels <- SpaDES::updateList(tmp[[2]]@levels, tmp[[1]]@levels)

    tmp[[1]]
}

#' @method rbind agentMatrix
#' @export
#' @importFrom data.table rbindlist
#' @export
#' @name rbind
#' @docType methods
#' @rdname agentMatrix-bind-methods
rbind.agentMatrix <- function(..., deparse.level = 1) {
  dots <- list(...)
  levelsSame <- isTRUE(do.call(all.equal,lapply(dots, function(x) x@levels)))
  if (levelsSame) { # if same, then faster rbind of the matrices
    if (do.call(all.equal, lapply(dots, colnames))) {
      mat <- do.call(rbind, lapply(dots, function(x) x@.Data)) # Fastest option...
      #i.e., pass agentMatrix with known levels
    } else {
      mat <- as.matrix(do.call(rbindlist,
                               args = list(lapply(dots, function(x) as(x, "data.frame")),
                                           fill = TRUE)))
    }
    levels <- dots[[1]]@levels
    new("agentMatrix", coords = mat[,1:2],
        mat[,-(1:2)],
        levelsAM = levels)
  } else {
    # if levels are not the same, then need to take the "slow" option: convert to data.frame
    mat <- as.data.frame(do.call(rbindlist, args = list(lapply(dots, function(x) as(x, "data.frame")), fill = TRUE)))
    new("agentMatrix", coords = mat[,1:2], mat[,-(1:2)])
  }
}

