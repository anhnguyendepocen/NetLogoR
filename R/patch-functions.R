################################################################################
#' Diffuse values in a world
#'
#' Each patch gives an equal share to its neighbor patches of a portion of its value.
#'
#' @param world      \code{NLworlds} object.
#'
#' @param pVar       Characters. If the world is a \code{NLworldStack}, pVar is
#'                   the name of the layer used for the diffusion. Should not
#'                   be provided if world is a \code{NLworld}.
#'
#' @param share      Numeric. Value between 0 and 1 representing the portion of
#'                   the patch value to be diffused among its neighbors.
#'
#' @param nNeighbors Integer: 4 or 8. Represent the number of neihgbor patches
#'                   involved in the diffusion process.
#'
#' @return \code{NLworlds} object with updated values.
#'
#' @details What is given is lost for the patches.
#'
#'          Patches on the sides of the world have less than 4 or 8 neighbors.
#'          Each neighbor still gets 1/4 or 1/8 of the shared amount and the diffusing
#'          patch keeps the leftover.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#diffuse}
#'
#'          \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#diffuse4}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- runif(length(w1))
#' plot(w1)
#' # Diffuse 50% of each patch value to its 8 neighbors
#' w2 <- diffuse(world = w1, share = 0.5, nNeighbors = 8)
#' plot(w2)
#'
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
#' Distances between agents
#'
#' Report the distances betwenn agents \code{from} and agents or locations \code{to}.
#'
#' @param world    \code{NLworlds} object.
#'
#' @param from     Matrix (ncol = 2) with the first column \code{pxcor} and the
#'                 second column \code{pycor} representing the coordinates of the
#'                 patches from which the distances will be computed.
#'
#'                 SpatialPointsDataFrame created by \code{createTurtles()} or
#'                 by \code{createOTurtles()} representing the turtles from which
#'                 the distances will be computed.
#'
#' @param to       Matrix (ncol = 2) with the first column \code{pxcor} and the
#'                 second column \code{pycor} representing the coordinates of the
#'                 patches to which the distances will be computed.
#'
#'                 SpatialPointsDataFrame created by \code{createTurtles()} or
#'                 by \code{createOTurtles()} representing the turtles to which
#'                 the distances will be computed.
#'
#'                 Matrix (ncol = 2) with the first column \code{xcor} and the
#'                 second column \code{ycor} representing the coordinates of the
#'                 locations to which the distances will be computed.
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
#' @return Numeric. Vector of distances if \code{from} and/or \code{to} contained
#'         one agent/location, or if \code{from} and \code{to} contained the same
#'         number of agents/locations and \code{allPairs = FALSE}.
#'
#'         Matrix of distances between \code{from} (rows) and \code{to} (columns)
#'         if \code{from} and \code{to} are of different length, or of same length
#'         and \code{allPairs = TRUE}.
#'
#' @details Distances from/to a patch are measured from/to its center.
#'
#'          If \code{torus = TRUE}, a distances around the sides of the world is
#'          reported only if smaller than the one across the world (i.e., as calculated
#'          with \code{torus = FALSE}).
#'
#'          Coordinates of given \code{from} and \code{to} must be inside the world's extent.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#distance}
#'
#'          \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#distancexy}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' NLdist(world = w1, from = patch(w1, 0, 0), to = patch(w1, c(1, 9), c(1, 9)))
#' NLdist(world = w1, from = patch(w1, 0, 0), to = patch(w1, c(1, 9), c(1, 9)), torus = TRUE)
#' t1 <- createTurtles(n = 2, coords = randomXYcor(w1, n = 2))
#' NLdist(world = w1, from = t1, to = patch(w1, c(1,9), c(1,9)), allPairs = TRUE)
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
#' Do the patches exist?
#'
#' Report \code{TRUE} if the patches exist in the world's extent, report
#' \code{FALSE} otherwise.
#'
#' @param world \code{NLworlds} object.
#'
#' @param pxcor Integers. Vector of \code{pxcor} coordinates. Must be of the same
#'              length as \code{pycor}.
#'
#' @param pycor Integers. Vector of \code{pycor} coordinates. Must be of the same
#'              length as \code{pxcor}.
#'
#' @return Logicals. \code{TRUE} or \code{FALSE} if the patches exist inside the
#'         world's extent, in the order of the coordinates given.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' pExists(world = w1, pxcor = -1, pycor = 2)
#'
#'
#' @export
#' @docType methods
#' @rdname pExists
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "pExists",
  function(world, pxcor, pycor) {
    standardGeneric("pExists")
  })

#' @export
#' @rdname pExists
setMethod(
  "pExists",
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
#' @rdname pExists
setMethod(
  "pExists",
  signature = c("NLworldStack", "numeric", "numeric"),
  definition = function(world, pxcor, pycor) {
    world_l <- world[[1]]
    pExists(world = world_l, pxcor = pxcor, pycor = pycor)
  }
)


################################################################################
#' Find neighbors patches
#'
#' Report the 4 or 8 surrounding patches (neighbors) around patches or turtles.
#'
#' @param world      \code{NLworlds} object.
#'
#' @param agents     Matrix (ncol = 2) with the first column \code{pxcor} and
#'                   the second column \code{pycor} representing the patches
#'                   coordinates for which neighbors will be reported.
#'
#'                   SpatialPointsDataFrame created by \code{createTurtles()} or
#'                   by \code{createOTurtles()} representing the turtles around
#'                   which the neighbors patches will be reported.
#'
#' @param nNeighbors Integer: 4 or 8. The number of neighbor patches to identify.
#'
#' @param torus      Logical to determine if the \code{NLworlds} object is wrapped.
#'                   Default is \code{torus = FALSE}.
#'
#' @return List with each item being the patches coordinates of the neighbors for
#'         each agent. The list items follows the order of the \code{agents}.
#'
#' @details The patch or turtle around which the neighbors are identified is not
#'          returned.
#'
#'          If \code{torus = FALSE}, \code{agents} located on the edges of the world
#'          have less than \code{nNeighbors} patches. If \code{torus = FALSE}, agents
#'          located on the egdes of the world all have \code{nNeighbors} patches which
#'          some may be on the other side of the world.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#neighbors}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' neighbors(world = w1, agents = patch(w1, c(0,9), c(0,7)), nNeighbors = 8)
#' t1 <- createTurtles(n = 3, coords = randomXYcor(w1, n = 3))
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
#' Report the patches coordinates \code{pxcor} and \code{pycor} at the given
#' \code{xcor} and \code{ycor} coordinates.
#'
#' @param world      \code{NLworlds} object.
#'
#' @param xcor       Numeric. Vector of \code{xcor} coordinates. Must be of same
#'                   length as \code{ycor}.
#'
#' @param ycor       Numeric. Vector of \code{ycor} coordinates. Must be of same
#'                   length as \code{ycor}.
#'
#' @param duplicate  Logical. If more than one set of coordinates \code{xcor, ycor}
#'                   fall into the same patch and \code{duplicate == TRUE}, the
#'                   patch coordinates are returned the number of times the set of
#'                   coordinates. If \code{duplicate == FALSE}, the patch coordinates
#'                   are only returned once.
#'                   Default is \code{duplicate == FALSE}.
#'
#' @param torus      Logical to determine if the \code{NLworlds} object is wrapped.
#'                   Default is \code{torus = FALSE}.
#'
#' @param out        Logical to determine if coordinates for patches outside of the
#'                   world should be returned. Default is \code{out = FALSE}.
#'
#' @return Matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the patches coordinates at \code{xcor, ycor}.
#'
#' @details If \code{xcor} or \code{ycor} are outside the world's extent,
#'          \code{torus = FALSE} and \code{out = FALSE}, no patch coordinates are returned,
#'          if \code{out = TRUE} \code{NA} is returned for both patch coordinates.
#'          If \code{torus = TRUE}, the patch coordinates from the wrapped world are
#'          returned.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' patch(world = w1, xcor = c(0, 9.1, 8.9, 5, 5.3), ycor = c(0, 0, -0.1, 12.4, 12.4))
#' patch(world = w1, xcor = c(0, 9.1, 8.9, 5, 5.3), ycor = c(0, 0, -0.1, 12.4, 12.4), duplicate = TRUE)
#' patch(world = w1, xcor = c(0, 9.1, 8.9, 5, 5.3), ycor = c(0, 0, -0.1, 12.4, 12.4), torus = TRUE)
#' patch(world = w1, xcor = c(0, 9.1, 8.9, 5, 5.3), ycor = c(0, 0, -0.1, 12.4, 12.4), torus = TRUE, duplicate = TRUE)
#'
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
#' No patches
#'
#' Report an empty patch agentset.
#'
#' @return Matrix (ncol = 2, nrow = 0) with the first column \code{pxcor} and the
#'         second column \code{pycor}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#no-patches}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' p1 <- noPatches()
#' nrow(p1)
#'
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
#' Report the patches coordinates \code{pxcor} and \code{pycor} at \code{(dx, dy)}
#' distances of the \code{agents}.
#'
#' @param world  \code{NLworlds} object.
#'
#' @param agents Matrix (ncol = 2) with the first column \code{pxcor} and the
#'               second column \code{pycor} representing the coordinates of the
#'               patches from wich \code{(dx, dy)} are computed.
#'
#'               SpatialPointsDataFrame created by \code{createTurtles()} or by
#'               \code{createOTurtles()} representing the turtles from wich
#'               \code{(dx, dy)} are computed.
#'
#' @param dx     Numeric. Distances to east from the \code{agents}. If \code{dx} is
#'               negative, the distance to the west is computed. \code{dx} must be
#'               of length 1 or of the length of as number of \code{agents}.
#'
#' @param dy     Numeric. Distances to the north from the \code{agents}. If \code{dy}
#'               is negative, the distance to the south is computed. \code{dy} must
#'               be a single value or of the length as the number of \code{agents}.
#'
#' @param torus  Logical to determine if the \code{NLworlds} object is wrapped.
#'               Default is \code{torus = FALSE}.
#'
#' @return Matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the coordinates of the patches at \code{(dx, dy)}
#'         distances of the \code{agents}. The order of the patches follows the order
#'         of the \code{agents}.
#'
#' @details If \code{torus = FALSE} and the patch at distance \code{(dx, dy)}
#'          of the agent is outside of the world's extent, \code{NA} is returned.
#'          If \code{torus = TRUE}, the patch coordinates from the wrapped world are
#'          returned.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch-at}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' patchCorner <- patchAt(world = w1, agents = patch(w1, 0, 0), dx = 1, dy = 1)
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
#' Patches at certain distances and certain directions
#'
#' Report the patches coordinates \code{pxcor} and \code{pycor} at certain
#' distances and certain headings from the \code{agents}.
#'
#' @param world  \code{NLworlds} object.
#'
#' @param agents Matrix (ncol = 2) with the first column \code{pxcor} and the
#'               second column \code{pycor} representing the coordinates for the
#'               patches from which \code{dist} are computed.
#'
#'               SpatialPointsDataFrame created by \code{createTurtles()} or by
#'               \code{createOTurtles()} representing the turtles from which
#'               \code{dist} are computed.
#'
#' @param dist   Numeric. Distances from the \code{agents}. \code{dist} must be
#'               a single value or of the length as the number of \code{agents}.
#'
#' @param head   Numeric. Absolute angles from the \code{agents}. \code{head}
#'               must be a single value or of the length as the number of
#'               \code{agents}. Angles must be in degrees with 0 being North.
#'
#' @param torus  Logical to determine if the \code{NLworlds} object is wrapped.
#'               Default is \code{torus = FALSE}.
#'
#' @return Matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the patches coordinates at \code{dist} and \code{head}
#'         of \code{agents}. The order of the patches followa the order of the \code{agents}.
#'
#' @details If \code{torus = FALSE} and the patch at distance \code{dist} and
#'          heading \code{head} of an agent is outside the world's extent, \code{NA}
#'          are returned. If \code{torus = TRUE}, the patch coordinates from the
#'          wrapped world are returned.
#'
#'          If \code{agents} are turtles, their heading is not taken into account; the
#'          given absolute heading \code{head} is used. To find a patch at certain
#'          distance from a turtle with the turtle's heading, look at \code{pacthAhead()},
#'          \code{pacthLeft()} or \code{pacthRight()}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch-at-heading-and-distance}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' p1 <- patchDistHead(world = w1, agents = patch(w1, 0, 0), dist = 1, head = 45)
#' t1 <- createTurtles(n = 1, coords = cbind(xcor = 0, ycor = 0), heading = 315)
#' p2 <- patchDistHead(world = w1, agents = t1, dist = 1, head = 45)
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
#' All the patches in a world
#'
#' Report the coordinates \code{pxcor} and \code{pycor} for all patches in a
#' \code{NLworlds}.
#'
#' @param world \code{NLworlds} object.
#'
#' @return Matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the patches coordinates. The order of the patches
#'         follows the order of the cellnumbers as defined for a \code{Raster*}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patches}
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
#' Report a patch agentset as the coordinates \code{pxcor} and \code{pycor}
#' of all patches contained in the inputs.
#'
#' @param ... Matrices (ncol = 2) of patches coordinates with the first column
#'            \code{pxcor} and the second column \code{pycor}.
#'
#' @return Matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the patches coordinates.
#'
#' @details Duplicate patches among the inputs are removed in the returned matrix.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch-set}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' p1 <- patchAt(world = w1, agents = patch(w1, c(0,1,2), c(0,0,0)), dx = 1, dy = 1)
#' p2 <- patchDistHead(world = w1, agents = patch(w1, 0, 0), dist = 1, head = 45)
#' p3 <- patch(world = w1, xcor = 4.3, ycor = 8)
#' p4 <- patchSet(p1, p2, p3)
#'
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
#' Random \code{pxcor}
#'
#' Report random \code{pxcor} coordinates between a world's minPxcor and maxPxcor.
#'
#' @param world \code{NLworld} object.
#'
#' @param n     Integer. The number of random \code{pxcor} to generate.
#'
#' @return Integers.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#random-pcor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' pxcor_ <- randomPxcor(world = w1, n = 10)
#'
#'
#' @export
#' @docType methods
#' @rdname randomPxcor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "randomPxcor",
  function(world, n) {
    standardGeneric("randomPxcor")
  })

#' @export
#' @rdname randomPxcor
setMethod(
  "randomPxcor",
  signature = c("NLworlds", "numeric"),
  definition = function(world, n) {
    minPxcor <- minPxcor(world)
    maxPxcor <- maxPxcor(world)
    pxcor <- sample(minPxcor:maxPxcor, size = n, replace = TRUE)
    return(pxcor)
  }
)


################################################################################
#' Random \code{pycor}
#'
#' Report random \code{pycor} coordinates between minPycor and maxPycor.
#'
#' @param world \code{NLworlds} object.
#'
#' @param n     Integer. The number of random \code{pycor} to generate.
#'
#' @return Integers.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#random-pcor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' pycor_ <- randomPycor(world = w1, n = 10)
#'
#'
#' @export
#' @docType methods
#' @rdname randomPycor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "randomPycor",
  function(world, n) {
    standardGeneric("randomPycor")
  })

#' @export
#' @rdname randomPycor
setMethod(
  "randomPycor",
  signature = c("NLworlds", "numeric"),
  definition = function(world, n) {
    minPycor <- minPycor(world)
    maxPycor <- maxPycor(world)
    pycor <- sample(minPycor:maxPycor, size = n, replace = TRUE)
    return(pycor)
  }
)
