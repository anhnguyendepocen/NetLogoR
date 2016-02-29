################################################################################
#' Diffuse
#'
#' Each patch gives an equal share to its neighbor patches of a portion of its value.
#'
#' @param world      A \code{NLworld} or \code{NLworldStack} object.
#'
#' @param pVar       If the world is a \code{NLworldStack}, pVar is the name
#'                   (characters) of the layer used for the diffusion.
#'
#' @param share      A value between 0 and 1 representing the portion of the patch
#'                   value to be diffused among its neighbors.
#'
#' @param nNeighbors 4 or 8. Represent the number of neihgbor patches involved in
#'                   the diffusion process.
#'
#' @details What is given is lost for the patches.
#'          Patches on the sides of the world have less than 4 or 8 neighbors.
#'          Each neighbor still gets 1/4 or 1/8 of the shared amount and the diffusing
#'          patch keeps the leftover.
#'
#' @return A \code{NLworld} or \code{NLworldStack} object with updated values.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create a world of 25 patches.
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' # Give the patches random values between 0 and 10.
#' w1[] <- runif(n = 25, min = 0, max = 10)
#' plot(w1)
#' # Diffuse 50% of each patch value to its 8 neighbors
#' w2 <- diffuse(world = w1, share = 0.5, nNeighbors = 8)
#' plot(w2)
#'
#' @export
#' @docType methods
#' @rdname diffuse
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "diffuse",
  function(world, pVar, share, nNeighbors) {
    standardGeneric("diffuse")
  })

#' @export
#' @rdname diffuse
setMethod(
  "diffuse",
  signature = c("NLworld", "missing", "numeric", "numeric"),
  definition = function(world, share, nNeighbors) {

    worldVal_from <- values(world)
    cellNum <- 1:length(worldVal_from)
    toGive <- (worldVal_from * share) / nNeighbors
    df1 <- cbind.data.frame(cellNum, toGive)
    df2 <- as.data.frame(adjacent(world, cells = cellNum, directions = nNeighbors))
    df3 <- merge(df2, df1, by.x = "from", by.y = "cellNum", all = TRUE)
    loose <- tapply(df3$toGive, FUN = sum, INDEX = df3$from) # how much each patch give
    win <- tapply(df3$toGive, FUN = sum, INDEX = df3$to) # how much each patch receive
    new_worldVal <- worldVal_from - loose + win

    newWorld <- setValues(world, as.numeric(new_worldVal))
    return(newWorld)
  }
)

#' @export
#' @rdname diffuse
setMethod(
  "diffuse",
  signature = c("NLworldStack", "character", "numeric", "numeric"),
  definition = function(world, pVar, share, nNeighbors) {
    pos_l <- which(names(world) == pVar, TRUE) # find the layer
    world_l <- world[[pos_l]]
    newWorld <- diffuse(world = world_l, share = share, nNeighbors = nNeighbors)
    world[[pos_l]]@data@values <- values(newWorld)
    return(world)
  }
)


################################################################################
#' Distance in a \code{NLworld*}
#'
#' Reports the distance from agent(s) to other agent(s). Agents can be patches or
#' turtles.
#'
#' ## !!! Only implemented for the patches so far !!!
#'
#' @param world    A \code{NLworld*} object.
#'
#' @param from     A matrix (ncol = 2) with the first column \code{pxcor} and the
#'                 second column \code{pycor} representing the coordinates of the
#'                 patch(es) from which the distances will be computed.
#'
#' @param to       A matrix (ncol = 2) with the first column \code{pxcor} and the
#'                 second column \code{pycor} representing the coordinates of the
#'                 patch(es) to which the distances will be computed.
#'
#' @param torus    Logical to determine if the \code{NLworld*} is wrapped.
#'                 Default is \code{torus = FALSE}.
#'
#' @param allPairs Logical. Only relevant if the number of agents in \code{from}
#'                 and \code{to} is the same. If \code{FALSE}, the distance between
#'                 each point in \code{from} with the corresponding \code{to} is
#'                 returned. If \code{TRUE}, a full distance matrix is returned.
#'                 Default is \code{FALSE}.
#'
#' @details Distances from or to a patch is measured from the center of the patch.
#'          If the \code{NLworld*} is wrapped (\code{torus = TRUE}), the distance
#'          around the sides of the \code{NLworld*} is reported only if smaller than
#'          the one calculated with \code{torus = FALSE}.
#'
#' @return A vector of distances between patches if \code{from} and/or \code{to} is
#'         a single patch, or if \code{from} and \code{to} were of same length and
#'         \code{allPairs = FALSE}. Otherwise, a matrix of distances between each pair
#'         of patches between \code{from} and \code{to} with the rows representing the
#'         patches \code{from} and the columns the patches \code{to}.
#'         The order of the distances follow the order of the given patches coordinates.
#'
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create a NLworld
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' distCorner <- NLdist(world = w1, from = cbind(pxcor = 0, pycor = 0), to = cbind(pxcor = 9, pycor = 9))
#'
#' @export
#' @docType methods
#' @rdname NLdist
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "NLdist",
  function(world, from, to, torus = FALSE, allPairs = FALSE) {
    standardGeneric("NLdist")
  })

#' @export
#' @rdname NLdist
setMethod(
  "NLdist",
  signature = c(world = "NLworld", from = "matrix", to = "matrix"),
  definition = function(world, from, to, torus, allPairs) {

    dist <- pointDistance(p1 = from, p2 = to, lonlat = FALSE, allpairs = allPairs)

    if(torus == TRUE){
      # cannot understand what SpaDES wrap function does
    }
    return(dist)
  }
)


################################################################################
#' Is patch?
#'
#' Reports \code{TRUE} if the patch(es) exist, \code{FALSE} otherwise
#'
#' @param world A \code{NLworld*} object.
#'
#' @param pxcor A numeric value or vector of \code{pxcor} coordinate(s).
#'
#' @param pycor A numeric value or vector of \code{pycor} coordinate(s).
#'
#' @return \code{TRUE} or \code{FALSE} if the patch(es) exist(s) or no, in the
#'         order of the coordinates given.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create a NLworld
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' isPatch(world = w1, pxcor = -1, pycor = 2)
#'
#' @export
#' @docType methods
#' @rdname isPatch
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "isPatch",
  function(world, pxcor, pycor) {
    standardGeneric("isPatch")
  })

#' @export
#' @rdname NLdist
setMethod(
  "isPatch",
  signature = c("NLworld", "numeric", "numeric"),
  definition = function(world, pxcor, pycor) {
    pExist <- c()
    for(i in 1:length(pxcor)){
      pVal <- world[pxcor[i], pycor[i]]
      if(length(pVal) == 0){
        pExist[i] <- FALSE
      } else {
        pExist[i] <- TRUE
      }
    }
    return(pExist)
  }
)

#' @export
#' @rdname NLdist
setMethod(
  "isPatch",
  signature = c("NLworldStack", "numeric", "numeric"),
  definition = function(world, pxcor, pycor) {
    world_l <- world[[1]]
    isPatch(world = world_l, pxcor = pxcor, pycor = pycor)
  }
)


################################################################################
#' Neighbors
#'
#' Reports the 8 or 4 surrounding patches (neighbors) around patch(es) or turtle(s).
#'
#' ## !!! Only implemented for the patches so far !!!
#'
#' @param world      A \code{NLworld*} object.
#'
#' @param agent      A matrix (ncol = 2) with the first column \code{pxcor} and
#'                   the second column \code{pycor} representing the patch(es)
#'                   coordinates for which neighbors will be reported.
#'
#' @param nNeighbors 4 or 8 for the number of neighbor patches.
#'
#' @return
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create a NLworld
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' neighbors(world = w1, agent = cbind(pxcor = c(0, 4), pycor = c(0, 6)), nNeighbors = 4)
#'
#' @export
#' @docType methods
#' @rdname neighbors
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "neighbors",
  function(world, agent, nNeighbors) {
    standardGeneric("neighbors")
  })

#' @export
#' @rdname neighbors
setMethod(
  "neighbors",
  signature = c("NLworld", "matrix", "numeric"),
  definition = function(world, agent, nNeighbors) {

    cellNum <- cellFromPxcorPycor(world = world, pxcor = agent[,1], pycor = agent[,2])
    neighbors_df <- adjacent(world, cells = cellNum, directions = nNeighbors)
    pCoords <- PxcorPycorFromCell(world = world, cellNum = neighbors_df[,2])
    listAgent <- list()
    for(i in 1:length(cellNum)) {
      listAgent[[i]] <- pCoords[neighbors_df[,1] == cellNum[i],]
    }

    return(listAgent)
  }
)

#' @export
#' @rdname neighbors
setMethod(
  "neighbors",
  signature = c("NLworldStack", "matrix", "numeric"),
  definition = function(world, agent, nNeighbors) {
    world_l <- world[[1]]
    neighbors(world = world_l, agent = agent, nNeighbors = nNeighbors)
  }
)


################################################################################
#' Patch
#'
#' Reports the patch(es) coordinates \code{pxcor} and \code{pycor} at the given
#' \code{xcor} and \code{ycor} coordinates.
#'
#' @param world A \code{NLworld*} object.
#'
#' @param xcor  A vector of \code{xcor} coordinates.
#'
#' @param ycor  A vector of \code{ycor} coordinates.
#'
#' @param torus Logical to determine if the \code{NLworld*} is wrapped.
#'              Default is \code{torus = FALSE}.
#'
#' @return A matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the patch(es) coordinates. Each row represents
#'         a patch and the order is the one given in the coordinates \code{xcor} and
#'         \code{ycor}.
#'
#' @details If \code{torus == FALSE} or \code{torus == TRUE} and \code{xcor} and \code{ycor}
#'          are within the world's extent, this function is equivalent to round
#'          the values \code{xcor} and \code{ycor}.
#'          If \code{torus == FALSE} and \code{xcor} and \code{ycor} are outside
#'          the world's extent, \code{NA} is returned.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create a NLworld
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' patch(world = w1, xcor = 8.9, ycor = -0.1)
#' patch(world = w1, xcor = 0, ycor = 10, torus = TRUE)
#' patch(world = w1, xcor = 0, ycor = 10, torus = FALSE)
#'
#' @export
#' @importFrom SpaDES wrap
#' @docType methods
#' @rdname patch
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patch",
  function(world, xcor, ycor, torus = FALSE) {
    standardGeneric("patch")
  })

#' @export
#' @rdname patch
setMethod(
  "patch",
  signature = c("NLworld", "numeric", "numeric", "logical"),
  definition = function(world, xcor, ycor, torus) {

    pxcor_ori <- round(xcor)
    pycor_ori <- round(ycor)

    XYwrap <- wrap(cbind(x = pxcor_ori, y = pycor_ori), extent(world))
    pxcor_w <- XYwrap[,1]
    pycor_w <- XYwrap[,2]

    if(torus == FALSE){
      pxcor_w[pxcor_w != pxcor_ori] <- NA
      pycor_w[pycor_w != pycor_ori] <- NA
    }

    pCoords <- cbind(pxcor = pxcor_w, pycor = pycor_w)
    return(pCoords)
  }
)

#' @export
#' @rdname patch
setMethod(
  "patch",
  signature = c("NLworld", "numeric", "numeric", "missing"),
  definition = function(world, xcor, ycor) {
    patch(world = world, xcor = xcor, ycor = ycor, torus = FALSE)
  }
)

#' @export
#' @rdname patch
setMethod(
  "patch",
  signature = c("NLworldStack", "numeric", "numeric", "logical"),
  definition = function(world, xcor, ycor, torus) {
    world_l <- world[[1]]
    patch(world = world_l, xcor = xcor, ycor = ycor, torus = torus)
  }
)

#' @export
#' @rdname patch
setMethod(
  "patch",
  signature = c("NLworldStack", "numeric", "numeric", "missing"),
  definition = function(world, xcor, ycor) {
    patch(world = world, xcor = xcor, ycor = ycor, torus = FALSE)
  }
)


################################################################################
#' Other patches
#'
#' Reports all patches coordinates \code{pxcor} and \code{pycor} except for the one(s)
#' with coordinates \code{pxcor} and \code{pycor} given.
#'
#' @param world A \code{NLworld*} object.
#'
#' @param agent A matrix (ncol = 2) with the first column \code{pxcor} and the
#'              second column \code{pycor} representing the coordinates for the
#'              patches to be discarded.
#'
#' @param torus Logical to determine if the \code{NLworld*} is wrapped.
#'              Default is \code{torus = FALSE}.
#'
#' @return A matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the patches coordinates. Each row represents
#'         a patch and the order is the one of the cellnumbers as defined for a
#'         \code{Raster*} object.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create a NLworld
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9) # 100 patches
#' otherPatches <- other(world = w1, agent = cbind(pxcor = 0, pycor = 0))
#' nrow(otherPatches) ## 99 patches left
#'
#' @export
#' @importFrom SpaDES wrap
#' @docType methods
#' @rdname other
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "other",
  function(world, agent, torus = FALSE) {
    standardGeneric("other")
  })

#' @export
#' @rdname other
setMethod(
  "other",
  signature = c("NLworld", "matrix", "logical"),
  definition = function(world, agent, torus) {

    if(torus == TRUE){
      agent <- wrap(cbind(x = agent[,1], y = agent[,2]), extent(world))
    }

    allpCoords <- cbind(pxcor = world@pxcor, pycor = world@pycor)
    pCoords <- allpCoords[!duplicated(rbind(agent, allpCoords))[-(1:nrow(agent))],]

    return(pCoords)
  }
)

#' @export
#' @rdname other
setMethod(
  "other",
  signature = c("NLworld", "matrix", "missing"),
  definition = function(world, agent) {
    other(world = world, agent = agent, torus = FALSE)
  }
)

#' @export
#' @rdname other
setMethod(
  "other",
  signature = c("NLworldStack", "matrix", "logical"),
  definition = function(world, agent, torus) {
    world_l <- world[[1]]
    other(world = world_l, agent = agent, torus = torus)
  }
)

#' @export
#' @rdname other
setMethod(
  "other",
  signature = c("NLworldStack", "matrix", "missing"),
  definition = function(world, agent) {
    other(world = world, agent = agent, torus = FALSE)
  }
)
