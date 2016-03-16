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
#' Distance in a \code{NLworlds} object.
#'
#' Reports the distance from agent(s) to other agent(s) or to defined location(s)
#' (coordinates). Agents can be patches or turtles.
#'
#' @param world    A \code{NLworlds} object.
#'
#' @param from     A matrix (ncol = 2) with the first column \code{pxcor} and the
#'                 second column \code{pycor} representing the coordinates of the
#'                 patch(es) from which the distance(s) will be computed.
#'                 A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                 by \code{createOTurtles()} representing the turtle(s) from which
#'                 the distance(s) will be computed.
#'
#' @param to       A matrix (ncol = 2) with the first column \code{pxcor} and the
#'                 second column \code{pycor} representing the coordinates of the
#'                 patch(es) to which the distance(s) will be computed.
#'                 A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                 by \code{createOTurtles()} representing the turtle(s) to which
#'                 the distance(s) will be computed.
#'                 A matrix (ncol = 2) with the first column \code{xcor} and the
#'                 second column \code{ycor} representing the coordinates of the
#'                 location(s) to which the distance(s) will be computed.
#'
#' @param torus    Logical to determine if the \code{NLworlds} object is wrapped.
#'                 Default is \code{torus = FALSE}.
#'
#' @param allPairs Logical. Only relevant if the number of agents/locations in
#'                 \code{from} and \code{to} is the same. If \code{FALSE}, the
#'                 distance between each agent/location in \code{from} with the
#'                 corresponding \code{to} is returned. If \code{TRUE}, a full
#'                 distance matrix is returned. Default is \code{allPairs = FALSE}.
#'
#' @details Distances from or to a patch is measured from the center of the patch.
#'          If \code{torus = TRUE}, the distance around the sides of the world is
#'          reported only if smaller than the one across the world (i.e., as calculated
#'          with \code{torus = FALSE}).
#'          Coordinates (patches, turtles or locations) must be inside the world extent.
#'
#' @return A vector of distances if \code{from} and/or \code{to} is of length 1,
#'         or if \code{from} and \code{to} were of same length and \code{allPairs = FALSE}.
#'         The order of the distances follows the order of the agents.
#'         A matrix of distances between \code{from} (rows) and \code{to} (columns)
#'         if \code{from} and \code{to} are of different length or of same length
#'         but \code{allPairs = TRUE}.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create a NLworld
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' NLdist(world = w1, from = cbind(pxcor = 0, pycor = 0), to = cbind(pxcor = c(1,9), pycor = c(1,9)))
#' NLdist(world = w1, from = cbind(pxcor = 0, pycor = 0), to = cbind(pxcor = c(1,9), pycor = c(1,9)), torus = TRUE)
#' t1 <- createTurtles(n = 2, coords = randomXYcor(world = w1, n = 2))
#' NLdist(world = w1, from = t1, to = patch(world = w1, xcor = c(1,9), ycor = c(1,9)), allPairs = TRUE)
#'
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
  signature = c(world = "NLworlds", from = "matrix", to = "matrix"),
  definition = function(world, from, to, torus, allPairs) {

    if(min(from[,1]) < world@extent@xmin | max(from[,1]) > world@extent@xmax |
       min(from[,2]) < world@extent@ymin | max(from[,2]) > world@extent@ymax |
       min(to[,1]) < world@extent@xmin | max(to[,1]) > world@extent@xmax |
       min(to[,2]) < world@extent@ymin | max(to[,2]) > world@extent@ymax){
      stop("Given coordinates are outside the world extent.")
    }

    dist <- pointDistance(p1 = from, p2 = to, lonlat = FALSE, allpairs = allPairs)

    if(torus == TRUE){
      # Need to create coordinates for "to" in a wrapped world
      # For all the 8 possibilities of wrapping (to the left, right, top, bottom and 4 corners)
      to1 <- cbind(pxcor = to[,1] - (world@extent@xmax - world@extent@xmin), pycor = to[,2] + (world@extent@ymax - world@extent@ymin))
      to2 <- cbind(pxcor = to[,1], pycor = to[,2] + (world@extent@ymax - world@extent@ymin))
      to3 <- cbind(pxcor = to[,1] + (world@extent@xmax - world@extent@xmin), pycor = to[,2] + (world@extent@ymax - world@extent@ymin))
      to4 <- cbind(pxcor = to[,1] - (world@extent@xmax - world@extent@xmin), pycor = to[,2])
      to5 <- cbind(pxcor = to[,1] + (world@extent@xmax - world@extent@xmin), pycor = to[,2])
      to6 <- cbind(pxcor = to[,1] - (world@extent@xmax - world@extent@xmin), pycor = to[,2] - (world@extent@ymax - world@extent@ymin))
      to7 <- cbind(pxcor = to[,1], pycor = to[,2] - (world@extent@ymax - world@extent@ymin))
      to8 <- cbind(pxcor = to[,1] + (world@extent@xmax - world@extent@xmin), pycor = to[,2] - (world@extent@ymax - world@extent@ymin))

      dist1 <- pointDistance(p1 = from, p2 = to1, lonlat = FALSE, allpairs = allPairs)
      dist2 <- pointDistance(p1 = from, p2 = to2, lonlat = FALSE, allpairs = allPairs)
      dist3 <- pointDistance(p1 = from, p2 = to3, lonlat = FALSE, allpairs = allPairs)
      dist4 <- pointDistance(p1 = from, p2 = to4, lonlat = FALSE, allpairs = allPairs)
      dist5 <- pointDistance(p1 = from, p2 = to5, lonlat = FALSE, allpairs = allPairs)
      dist6 <- pointDistance(p1 = from, p2 = to6, lonlat = FALSE, allpairs = allPairs)
      dist7 <- pointDistance(p1 = from, p2 = to7, lonlat = FALSE, allpairs = allPairs)
      dist8 <- pointDistance(p1 = from, p2 = to8, lonlat = FALSE, allpairs = allPairs)

      dist <- pmin(dist, dist1, dist2, dist3, dist4, dist5, dist6, dist7, dist8)
    }
    return(dist)
  }
)

#' @export
#' @rdname NLdist
setMethod(
  "NLdist",
  signature = c(world = "NLworlds", from = "matrix", to = "SpatialPointsDataFrame"),
  definition = function(world, from, to, torus, allPairs) {
    NLdist(world = world, from = from, to = to@coords, torus = torus, allPairs = allPairs)
  }
)

#' @export
#' @rdname NLdist
setMethod(
  "NLdist",
  signature = c(world = "NLworlds", from = "SpatialPointsDataFrame", to = "matrix"),
  definition = function(world, from, to, torus, allPairs) {
    NLdist(world = world, from = from@coords, to = to, torus = torus, allPairs = allPairs)
  }
)

#' @export
#' @rdname NLdist
setMethod(
  "NLdist",
  signature = c(world = "NLworlds", from = "SpatialPointsDataFrame", to = "SpatialPointsDataFrame"),
  definition = function(world, from, to, torus, allPairs) {
    NLdist(world = world, from = from@coords, to = to@coords, torus = torus, allPairs = allPairs)
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
#' @rdname isPatch
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
#' @rdname isPatch
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
#' Reports the 4 or 8 surrounding patches (neighbors) around patch(es) or turtle(s).
#'
#' @param world      A \code{NLworlds} object.
#'
#' @param agents     A matrix (ncol = 2) with the first column \code{pxcor} and
#'                   the second column \code{pycor} representing the patch(es)
#'                   coordinates for which neighbors will be reported.
#'                   A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                   by \code{createOTurtles()} representing the turtles around
#'                   which the neighbors patches will be reported.
#'
#' @param nNeighbors 4 or 8 for the number of neighbor patches.
#'
#' @param torus      Logical to determine if the \code{NLworlds} object is wrapped.
#'                   Default is \code{torus = FALSE}.
#'
#' @return A list with each item being the patches coordinates of the neighbors for
#'         each of the agents. The list items follows the order of the agents.
#'
#' @details The patch around which the neighbors are identified if \code{agents}
#'          are patches or the patch on which the turtle is located if \code{agents}
#'          are turtles is not reported.
#'          If \code{torus = FALSE}, \code{agents} located on the edges of the world
#'          have less than \code{nNeighbors} patches. If \code{torus = FALSE}, agents
#'          located on the egdes of the world all have \code{nNeighbors} patches which
#'          some may be on the other side of the world.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create a NLworld
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' neighbors(world = w1, agents = cbind(pxcor = c(0, 4), pycor = c(0, 6)), nNeighbors = 8)
#' t1 <- createTurtles(n = 3, coords = randomXYcor(world = w1, n = 3))
#' neighbors(world = w1, agents = t1, nNeighbors = 4)
#'
#'
#' @export
#' @importFrom SpaDES adj
#' @docType methods
#' @rdname neighbors
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "neighbors",
  function(world, agents, nNeighbors, torus = FALSE) {
    standardGeneric("neighbors")
  })

#' @export
#' @rdname neighbors
setMethod(
  "neighbors",
  signature = c(world = "NLworlds", agents = "matrix", nNeighbors = "numeric"),
  definition = function(world, agents, nNeighbors, torus) {

    cellNum <- cellFromPxcorPycor(world = world, pxcor = agents[,1], pycor = agents[,2])
    neighbors_df <- adj(world, cells = cellNum, directions = nNeighbors, torus = torus)

    pCoords <- PxcorPycorFromCell(world = world, cellNum = neighbors_df[,2])
    listAgents <- list()
    for(i in 1:length(cellNum)) {
      listAgents[[i]] <- pCoords[neighbors_df[,1] == cellNum[i],]
    }

    return(listAgents)
  }
)

#' @export
#' @rdname neighbors
setMethod(
  "neighbors",
  signature = c(world = "NLworlds", agents = "SpatialPointsDataFrame", nNeighbors = "numeric"),
  definition = function(world, agents, nNeighbors, torus) {
    pTurtles <- patch(world = world, xcor = agents@coords[,1], ycor = agents@coords[,2], duplicate = TRUE)
    neighbors(world = world, agents = pTurtles, nNeighbors = nNeighbors, torus = torus)
  }
)


################################################################################
#' Patch
#'
#' Reports the patch(es) coordinates \code{pxcor} and \code{pycor} at the given
#' \code{xcor} and \code{ycor} coordinates.
#'
#' @param world      A \code{NLworlds} object.
#'
#' @param xcor       A vector of \code{xcor} coordinates.
#'
#' @param ycor       A vector of \code{ycor} coordinates.
#'
#' @param duplicate  Logical. If more than one set of coordinates \code{xcor, ycor}
#'                   fall into the same patch and \code{duplicate == TRUE}, the
#'                   patch coordinates are returned the number of times the coordinates.
#'                   If \code{duplicate == FALSE}, the patch coordinates are only
#'                   returned once.
#'                   Default is \code{duplicate == FALSE}.
#'
#' @param torus      Logical to determine if the \code{NLworlds} object is wrapped.
#'                   Default is \code{torus = FALSE}.
#'
#' @param out        Logical to determine if coordinates for patches outside of the
#'                   world should be returned. Default is \code{out = FALSE}.
#'
#' @return A matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the patch(es) coordinates at \code{xcor, ycor}.
#'
#' @details If \code{xcor} or \code{ycor} are outside the world's extent,
#'          \code{torus = FALSE} and \code{out = FALSE}, no patch coordinates are returned,
#'          if \code{out = TRUE} \code{NA} is returned for both patch coordinates.
#'          If \code{torus = TRUE}, the patch coordinates from the wrapped world are
#'          returned.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Create a NLworld
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' patch(world = w1, xcor = c(0, 9.1, 8.9, 5, 5.3), ycor = c(0, 0, -0.1, 12.4, 12.4))
#' patch(world = w1, xcor = c(0, 9.1, 8.9, 5, 5.3), ycor = c(0, 0, -0.1, 12.4, 12.4), duplicate = TRUE)
#' patch(world = w1, xcor = c(0, 9.1, 8.9, 5, 5.3), ycor = c(0, 0, -0.1, 12.4, 12.4), torus = TRUE)
#' patch(world = w1, xcor = c(0, 9.1, 8.9, 5, 5.3), ycor = c(0, 0, -0.1, 12.4, 12.4), torus = TRUE, duplicate = TRUE)
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
  function(world, xcor, ycor, duplicate = FALSE, torus = FALSE, out = FALSE) {
    standardGeneric("patch")
  })

#' @export
#' @rdname patch
setMethod(
  "patch",
  signature = c(world = "NLworlds", xcor = "numeric", ycor = "numeric"),
  definition = function(world, xcor, ycor, duplicate, torus, out) {

    pxcor_ <- round(xcor)
    pycor_ <- round(ycor)

    if(torus == TRUE){
      pCoords <- wrap(cbind(x = pxcor_, y = pycor_), extent(world))
      pxcor_ <- pCoords[,1]
      pycor_ <- pCoords[,2]
    }

    pxcorNA <- ifelse(pxcor_ < minPxcor(world) | pxcor_ > maxPxcor(world), NA, pxcor_)
    pycorNA <- ifelse(pycor_ < minPycor(world) | pycor_ > maxPycor(world), NA, pycor_)
    pxcorNA[is.na(pycorNA)] <- NA
    pycorNA[is.na(pxcorNA)] <- NA

    if(out == FALSE){
      pxcor = pxcorNA[!is.na(pxcorNA)]
      pycor = pycorNA[!is.na(pycorNA)]
    } else {
      pxcor = pxcorNA
      pycor = pycorNA
    }

    pCoords <- matrix(data = cbind(pxcor, pycor), ncol = 2, nrow = length(pxcor), dimnames = list(NULL, c("pxcor", "pycor")))

    if(duplicate == FALSE){
      pCoords <- unique(pCoords)
    }
    return(pCoords)
  }
)


################################################################################
#' Other patches
#'
#' Reports all patches coordinates \code{pxcor} and \code{pycor} except for the one(s)
#' with coordinates \code{pxcor} and \code{pycor} given.
#'
#' @param world  A \code{NLworld*} object.
#'
#' @param agents A matrix (ncol = 2) with the first column \code{pxcor} and the
#'               second column \code{pycor} representing the coordinates for the
#'               patches to be discarded.
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
#' otherPatches <- other(world = w1, agents = cbind(pxcor = 0, pycor = 0))
#' nrow(otherPatches) ## 99 patches left
#'
#' @export
#' @docType methods
#' @rdname other
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "other",
  function(world, agents) {
    standardGeneric("other")
  })

#' @export
#' @rdname other
setMethod(
  "other",
  signature = c("NLworld", "matrix"),
  definition = function(world, agents) {

    allpCoords <- cbind(pxcor = world@pxcor, pycor = world@pycor)
    pCoords <- allpCoords[!duplicated(rbind(agents, allpCoords))[-(1:nrow(agents))],]

    return(pCoords)
  }
)

#' @export
#' @rdname other
setMethod(
  "other",
  signature = c("NLworldStack", "matrix"),
  definition = function(world, agents) {
    world_l <- world[[1]]
    other(world = world_l, agents = agents)
  }
)


################################################################################
#' No patches
#'
#' Reports an empty patch agentset.
#'
#' @return An empty matrix (ncol = 2) with the first column \code{pxcor} and the
#'         second column \code{pycor}.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' p1 <- noPatches()
#' nrow(p1)
#'
#' @export
#' @docType methods
#' @rdname noPatches
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "noPatches",
  function(x) {
    standardGeneric("noPatches")
  })

#' @export
#' @rdname noPatches
setMethod(
  "noPatches",
  signature = "missing",
  definition = function() {
    return(matrix(, nrow = 0, ncol = 2, dimnames = list(NULL, c("pxcor", "pycor"))))
  }
)


################################################################################
#' Patch at
#'
#' Reports the patch(es) coordinates \code{pxcor} and \code{pycor} at \code{(dx, dy)}
#' distance of the agent(s).
#'
#' @param world  A \code{NLworlds} object.
#'
#' @param agents A matrix (ncol = 2) with the first column \code{pxcor} and the
#'               second column \code{pycor} representing the coordinates of the
#'               patches from wich \code{(dx, dy)} are computed.
#'               A SpatialPointsDataFrame created by \code{createTurtles()} or by
#'               \code{createOTurtles()} representing the turtles from wich
#'               \code{(dx, dy)} are computed.
#'
#' @param dx     Numeric. Distance to east from the agent. If \code{dx} is negative, the
#'               distance to the west is computed. \code{dx} must be a single value or
#'               of the length of \code{agents}.
#'
#' @param dy     Numeric. Distance to the north from the agent. If \code{dy} is negative,
#'               the distance to the south is computed. \code{dy} must be a single value or
#'               of the length of \code{agents}.
#'
#' @param torus  Logical to determine if the \code{NLworlds} object is wrapped.
#'               Default is \code{torus = FALSE}.
#'
#' @return A matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the patches coordinates. The order of the patches
#'         follows the order of the agents.
#'
#' @details If \code{torus = FALSE} and the patch at distance \code{(dx, dy)}
#'          of the agent is outside of the world's extent, \code{NA} is returned.
#'          If \code{torus = TRUE}, the patch coordinates from the wrapped world are
#'          returned.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' patchCorner <- patchAt(world = w1, agents = cbind(pxcor = 0, pycor = 0), dx = 1, dy = 1)
#' t1 <- createTurtles(n = 1, coords = cbind(xcor = 0, ycor = 0))
#' patchCorner <- patchAt(world = w1, agents = t1, dx = 1, dy = 1)
#'
#'
#' @export
#' @docType methods
#' @rdname patchAt
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patchAt",
  function(world, agents, dx, dy, torus = FALSE) {
    standardGeneric("patchAt")
  })

#' @export
#' @rdname patchAt
setMethod(
  "patchAt",
  signature = c(world = "NLworlds", agents = "matrix", dx = "numeric", dy = "numeric"),
  definition = function(world, agents, dx, dy, torus) {

    pxcor <- agents[,1] + dx
    pycor <- agents[,2] + dy
    pAt <- patch(world = world, xcor = pxcor, ycor = pycor, duplicate = TRUE, torus = torus, out = TRUE)

    return(pAt)
  }
)

#' @export
#' @rdname patchAt
setMethod(
  "patchAt",
  signature = c(world = "NLworlds", agents = "SpatialPointsDataFrame", dx = "numeric", dy = "numeric"),
  definition = function(world, agents, dx, dy, torus) {

    patchAt(world = world, agents = agents@coords, dx = dx, dy = dy, torus = torus)

  }
)


################################################################################
#' Patch at some distance and heading.
#'
#' Reports the patch(es) coordinates \code{pxcor} and \code{pycor} at certain
#' distance and heading of the agent(s).
#'
#' @param world  A \code{NLworlds} object.
#'
#' @param agents A matrix (ncol = 2) with the first column \code{pxcor} and the
#'               second column \code{pycor} representing the coordinates for the
#'               patches from wich \code{dist} is computed.
#'               A SpatialPointsDataFrame created by \code{createTurtles()} or by
#'               \code{createOTurtles()} representing the turtles from wich
#'               \code{dist} is computed.
#'
#' @param dist   Numeric. Distance from the agent. \code{dist} must be a single
#'               value or of the length of \code{agents}.
#'
#' @param head   Numeric. Absolute angle from the agent. \code{head} must be a
#'               single value or of the length of \code{agents}. Angle(s) are in
#'               degrees with 0 being North.
#'
#' @param torus  Logical to determine if the \code{NLworlds} object is wrapped.
#'               Default is \code{torus = FALSE}.
#'
#' @return A matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the patches coordinates. The order of the patches
#'         followa the order of the agents.
#'
#' @details If \code{torus = FALSE} and the patch at distance \code{dist} and
#'          heading \code{head} of the agent is outside the world's extent, \code{NA}
#'          is returned. If \code{torus = TRUE}, the patch coordinates from the
#'          wrapped world are returned.
#'          If \code{agents} are turtles, their heading is not taken into account; the
#'          given absolute heading \code{head} is used. To find a patch at certain
#'          distance from a turtle with the turtle's heading, look at \code{pacthAhead()},
#'          \code{pacthLeft()} or \code{pacthRight()}.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' patchCorner <- patchDistHead(world = w1, agents = cbind(pxcor = 0, pycor = 0), dist = 1, head = 45)
#' t1 <- createTurtles(n = 1, coords = cbind(xcor = 0, ycor = 0), heading = 315)
#' patchCorner <- patchDistHead(world = w1, agents = t1, dist = 1, head = 45)
#'
#'
#' @export
#' @importFrom CircStats rad
#' @docType methods
#' @rdname patchDistHead
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patchDistHead",
  function(world, agents, dist, head, torus = FALSE) {
    standardGeneric("patchDistHead")
  })

#' @export
#' @rdname patchDistHead
setMethod(
  "patchDistHead",
  signature = c(world = "NLworlds", agents = "matrix", dist = "numeric", head = "numeric"),
  definition = function(world, agents, dist, head, torus) {

    pxcor <- agents[,1] + sin(rad(head)) * dist
    pycor <- agents[,2] + cos(rad(head)) * dist
    pDistHead <- patch(world = world, xcor = pxcor, ycor = pycor, torus = torus, duplicate = TRUE, out = TRUE)

    return(pDistHead)
  }
)

#' @export
#' @rdname patchDistHead
setMethod(
  "patchDistHead",
  signature = c(world = "NLworlds", agents = "SpatialPointsDataFrame", dist = "numeric", head = "numeric"),
  definition = function(world, agents, dist, head, torus) {

    patchDistHead(world = world, agents = agents@coords, dist = dist, head = head, torus = torus)

    }
)


################################################################################
#' Patches of the world
#'
#' Reports the coordinates \code{pxcor} and \code{pycor} of all patches in the
#' \code{NLworld*}
#'
#' @param world A \code{NLworld*} object.
#'
#' @return A matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the patches coordinates. The order of the patches
#'         follows the order of the cellnumbers as defined for a \code{Raster*}.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9) # 100 patches
#' allPatches <- patches(world = w1)
#' nrow(allPatches)
#'
#'
#' @export
#' @docType methods
#' @rdname patches
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patches",
  function(world) {
    standardGeneric("patches")
  })

#' @export
#' @rdname patches
setMethod(
  "patches",
  signature = "NLworld",
  definition = function(world) {
    return(cbind(pxcor = world@pxcor, pycor = world@pycor))
  }
)

#' @export
#' @rdname patches
setMethod(
  "patches",
  signature = "NLworldStack",
  definition = function(world) {
    world_l <- world[[1]]
    patches(world = world_l)
  }
)


################################################################################
#' Patch set
#'
#' Reports a patch agentset as the the coordinates \code{pxcor} and \code{pycor}
#' of all patches in the inputs.
#'
#' @param ... Matrices (ncol = 2) of patch(es) coordinates with the first column
#'            \code{pxcor} and the second column \code{pycor}.
#'
#' @return A matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the patches coordinates.
#'
#' @details Duplicate patches among the inputs are removed in the return matrix.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' p1 <- patchAt(world = w1, agents = cbind(pxcor = c(0,1,2), pycor = c(0,0,0)), dx = 1, dy = 1)
#' p2 <- patchDistHead(world = w1, agents = cbind(pxcor = 0, pycor = 0), dist = 1, head = 45)
#' p3 <- patch(world = w1, xcor = 4.3, ycor = 8)
#' set1 <- patchSet(p1, p2, p3)
#'
#' @export
#' @docType methods
#' @rdname patchSet
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patchSet",
  function(...) {
    standardGeneric("patchSet")
  })

#' @export
#' @rdname patchSet
setMethod(
  "patchSet",
  signature = "matrix",
  definition = function(...) {

    dots <-list(...)
    pCoords <- unique(do.call(rbind, dots))
    return(pCoords)
  }
)


################################################################################
#' Random pxcor
#'
#' Reports random pxcor coordinate(s) between minPxcor and maxPxcor.
#'
#' @param world A \code{NLworld*} object.
#'
#' @param n     Integer. Represent how many random pxcor needs to be generated.
#'
#' @return A vector of integer.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' pxcor10 <- randPxcor(world = w1, n = 10)
#'
#'
#' @export
#' @docType methods
#' @rdname randPxcor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "randPxcor",
  function(world, n) {
    standardGeneric("randPxcor")
  })

#' @export
#' @rdname randPxcor
setMethod(
  "randPxcor",
  signature = c("NLworld", "numeric"),
  definition = function(world, n) {
    minPxcor <- world@minPxcor
    maxPxcor <- world@maxPxcor
    pxcor <- sample(minPxcor:maxPxcor, size = n, replace = TRUE)
    return(pxcor)
  }
)

#' @export
#' @rdname randPxcor
setMethod(
  "randPxcor",
  signature = c("NLworldStack", "numeric"),
  definition = function(world, n) {
    world_l <- world[[1]]
    randPxcor(world = world_l, n = n)
  }
)


################################################################################
#' Random pycor
#'
#' Reports random pycor coordinate(s) between minPycor and maxPycor.
#'
#' @param world A \code{NLworld*} object.
#'
#' @param n     Integer. Represent how many random pycor needs to be generated.
#'
#' @return A vector of integer.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' pycor10 <- randPycor(world = w1, n = 10)
#'
#'
#' @export
#' @docType methods
#' @rdname randPycor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "randPycor",
  function(world, n) {
    standardGeneric("randPycor")
  })

#' @export
#' @rdname randPycor
setMethod(
  "randPycor",
  signature = c("NLworld", "numeric"),
  definition = function(world, n) {
    minPycor <- world@minPycor
    maxPycor <- world@maxPycor
    pycor <- sample(minPycor:maxPycor, size = n, replace = TRUE)
    return(pycor)
  }
)

#' @export
#' @rdname randPycor
setMethod(
  "randPycor",
  signature = c("NLworldStack", "numeric"),
  definition = function(world, n) {
    world_l <- world[[1]]
    randPycor(world = world_l, n = n)
  }
)
