################################################################################
#' Function arguments
#'
#' @keywords integral
#'
#' @name fargs
#'
#'
#' @param n Integer.
#'
#' @param world NLworlds object.
#'
#' @param torus Logical to determine if the \code{world} is wrapped. Default is
#'              \code{torus = FALSE}.
#'
#' @param minPxcor Integer. Minimum pxcor for the patches (world's left border).
#'
#' @param maxPxcor Integer. Maximum pxcor for the patches (world's right border).
#'
#' @param minPycor Integer. Minimum pycor for the patches (world's bottom border).
#'
#' @param maxPycor Integer. Maximum pycor for the patches (world's top border).
#'
#' @param pxcor Integer. Vector of patches pxcor coordinates. Must be of length 1
#'              or of the same length as \code{pycor}.
#'
#' @param pycor Integer. Vector of patches pycor coordinates. Must be of length 1
#'              or of the same length as \code{pxcor}.
#'
#' @param pVar Character. If the \code{world} is a NLworldStack object, \code{pVar}
#'             is the name of the layer to use to define the patches values.
#'             \code{pVar} must not be provided if the \code{world} is a NLworld object.
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the moving agents.
#'
#' @param agents Matrix (ncol = 2) with the first column "pxcor" and the second
#'               column "pycor" representing the patches coordinates, or
#'
#'               SpatialPointsDataFrame created by \code{createTurtles()} or
#'               by \code{createOTurtles()} representing the moving agents.
#'
#' @param agents2 Matrix (ncol = 2) with the first column "pxcor" and the second
#'                column "pycor" representing the patches coordinates, or
#'
#'                SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the moving agents, or
#'
#'                Matrix (ncol = 2) with the first column "x and the second column
#'                "y" representing locations coordinates.
#'
#' @param nNeighbors Integer: 4 or 8. Represents the number of neihgbor patches
#'                   considered.
#'
#' @param dx Numeric. Vector of distances to the east (right) from the \code{agents}.
#'           If \code{dx} is negative, the distance to the west (left) is computed.
#'           \code{dx} must be of length 1 or of the same length as number of patches
#'           or turtles in \code{agents}.
#'
#' @param dy Numeric. Vector of distances to the north (up) from the \code{agents}.
#'           If \code{dy} is negative, the distance to the south is computed (down).
#'           \code{dy} must be of length 1 or of the same length as number of patches
#'           or turtles in \code{agents}.
#'
#'
NULL
