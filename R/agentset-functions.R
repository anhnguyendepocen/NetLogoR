################################################################################
#' All
#'
#' Reports \code{TRUE} if all the agents have their variable equal to a given value,
#' or \code{FALSE} otherwise.
#'
#' @param agents A matrix (ncol = 2) with the first column \code{pxcor} and the
#'               second column \code{pycor} representing the coordinates of the
#'               patches to evaluate.
#'               A SpatialPointsDataFrame created by \code{createTurtles()} or by
#'               \code{createOTurtles()} representing the turtles to evaluate.
#'
#' @param world   A \code{NLworlds} object. Only needed if \code{agents} are patches.
#'                Must not be provided if \code{agents} are turtles.
#'
#' @param varName Characters. The name of the variable to evaluate for the \code{agents}
#'                value. If \code{agents} are patches and the \code{world} is a
#'                \code{NLworld} object, \code{varName} must not be provided. If
#'                \code{agents} are patches and the \code{world} is a \code{NLworldStack}
#'                object, \code{varName} refers to the layer used for evaluating patches
#'                values. If \code{agents} are turtles, \code{varName} is one of
#'                the turtles' variable. \code{varName} can be equal to \code{"xcor"},
#'                \code{"ycor"}, any of the variables created when turtles were created,
#'                as well as any variable created using \code{turtlesOwn()}.
#'
#' @param val     Numeric or characters depending on the variable class. Can be of
#'                any length.
#'
#' @return Logical. \code{TRUE} if all the \code{agents} have their variable equal to
#'         \code{val}, return \code{FALSE} otherwise.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = 25, replace = TRUE)
#' NLall(agents = patches(world = w1), world = w1, val = 5)
#' w2 <- w1
#' w2[] <- 5
#' NLall(agents = patches(world = w2), world = w2, val = 5)
#'
#' # Turtles
#' t1 <- createTurtles(n = 5, coords = cbind(xcor = 1, ycor = 1), heading = c(1, 2, 2, 1, 2))
#' NLall(agents = t1, varName = "xcor", val = 1)
#' NLall(agents = t1, varName = "heading", val = 2)
#'
#'
#' @export
#' @docType methods
#' @rdname NLall
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "NLall",
  function(agents, world, varName, val) {
    standardGeneric("NLall")
  })

#' @export
#' @rdname NLall
setMethod(
  "NLall",
  signature = c("matrix", "NLworld", "missing", "ANY"),
  definition = function(agents, world, val) {
    withVal <- NLwith(agents = agents, world = world, val = val)
    allTrue <- ifelse(nrow(agents) == nrow(withVal), TRUE, FALSE)
    return(allTrue)
  }
)

#' @export
#' @rdname NLall
setMethod(
  "NLall",
  signature = c("matrix", "NLworldStack", "character", "ANY"),
  definition = function(agents, world, varName, val) {
    names_l <- names(world)
    l <- match(varName, names_l)
    world_l <- world[[l]]
    NLall(world = world_l, agents = agents, val = val)
  }
)

#' @export
#' @rdname NLall
setMethod(
  "NLall",
  signature = c("SpatialPointsDataFrame", "missing", "character", "ANY"),
  definition = function(agents, varName, val) {
    withVal <- NLwith(agents = agents, varName = varName, val = val)
    allTrue <- ifelse(length(agents) == length(withVal), TRUE, FALSE)
    return(allTrue)
  }
)


################################################################################
#' Any
#'
#' Reports \code{TRUE} if the given agentset \code{agents} is non empty, or
#' \code{FALSE} otherwise.
#'
#' @param agents A matrix (ncol = 2) with the first column \code{pxcor} and the
#'               second column \code{pycor} representing the coordinates of the
#'               patches to evaluate.
#'               A SpatialPointsDataFrame created by \code{createTurtles()} or by
#'               \code{createOTurtles()} representing the turtles to evaluate.
#'
#' @return Logical. \code{TRUE} if there is at least one patch or one turtle in the
#'         \code{agents}, return \code{FALSE} otherwise.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' p1 <- noPatches()
#' p2 <- patch(world = w1, xcor = 0, ycor = 0)
#' NLany(p1)
#' NLany(p2)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10))
#' t2 <- noTurtles()
#' NLany(t1)
#' NLany(t2)
#'
#'
#' @export
#' @docType methods
#' @rdname NLany
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "NLany",
  function(agents) {
    standardGeneric("NLany")
  })

#' @export
#' @rdname NLany
setMethod(
  "NLany",
  signature = c("matrix"),
  definition = function(agents) {
    anyAgents <- ifelse(nrow(agents) == 0, FALSE, TRUE)

    if(anyAgents == TRUE){
      nonNAs <- apply(agents, 2, function(x) length(which(!is.na(x))))
      if(sum(nonNAs) == 0){
        anyAgents <- FALSE
      }
    }

    return(anyAgents)
  }
)

#' @export
#' @rdname NLany
setMethod(
  "NLany",
  signature = c("SpatialPointsDataFrame"),
  definition = function(agents) {
    anyAgents <- ifelse(length(agents) == 0, FALSE, TRUE)
    return(anyAgents)
  }
)


################################################################################
#' Count
#'
#' Reports the number of agents.
#'
#' @param agents A matrix (ncol = 2) with the first column \code{pxcor} and the
#'               second column \code{pycor} representing the coordinates of the
#'               patches to count.
#'               A SpatialPointsDataFrame created by \code{createTurtles()} or by
#'               \code{createOTurtles()} representing the turtles to count.
#'
#' @return Integer.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4) # 25 patches
#' p1 <- patches(world = w1)
#' count(p1) # 25
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10))
#' count(t1) # 10
#'
#'
#' @export
#' @docType methods
#' @rdname count
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "count",
  function(agents) {
    standardGeneric("count")
  })

#' @export
#' @rdname count
setMethod(
  "count",
  signature = c("matrix"),
  definition = function(agents) {
    return(nrow(agents))
  }
)

#' @export
#' @rdname count
setMethod(
  "count",
  signature = c("SpatialPointsDataFrame"),
  definition = function(agents) {
    return(length(agents))
  }
)


################################################################################
#' Sort on
#'
#' Reports the coordinates \code{pxcor} and \code{pycor} of the patches sorted
#' according to their value.
#' Reports the who numbers of the turtles sorted according to their value.
#'
#' @param agents  A matrix (ncol = 2) with the first column \code{pxcor} and the
#'                second column \code{pycor} representing the coordinates of the
#'                patches to sort.
#'                A SpatialPointsDataFrame created by \code{createTurtles()} or by
#'                \code{createOTurtles()} representing the turtles to sort.
#'
#' @param world   A \code{NLworlds} object. Only needed if \code{agents} are patches.
#'                Must not be provided if \code{agents} are turtles.
#'
#' @param varName Characters. The name of the variable on which the sorting is based.
#'                If \code{agents} are patches and the \code{world} is a \code{NLworld}
#'                object, \code{varName} must not be provided. If \code{agents} are
#'                patches and the \code{world} is a \code{NLworldStack} object, \code{varName}
#'                refers to the layer used for sorting the patches. If \code{agents}
#'                are turtles, \code{varName} is one of the turtles' variable. \code{varName}
#'                can be equal to \code{"xcor"}, \code{"ycor"}, any of the variables created
#'                when turtles were created, as well as any variable created using
#'                \code{turtlesOwn()}.
#'
#' @return A matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the coordinates of the sorted patches if \code{agents}
#'         are patches.
#'         A vector of who numbers representing the sorted turtles if \code{agents} are
#'         turtles.
#'
#' @details The sorting of the agents based on their value is done in a increasing order.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = 25, replace = TRUE)
#' plot(w1)
#' p1 <- sortOn(agents = patches(world = w1), world = w1)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10))
#' sortdHeadingst1 <- sortOn(agents = t1, varName = "heading")
#'
#'
#' @export
#' @docType methods
#' @rdname sortOn
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "sortOn",
  function(agents, world, varName) {
    standardGeneric("sortOn")
  })

#' @export
#' @rdname sortOn
setMethod(
  "sortOn",
  signature = c("matrix", "NLworld", "missing"),
  definition = function(agents, world) {
    values <- values(world)
    pxcorW <- world@pxcor
    pycorW <- world@pycor
    agentsVal <- values[pxcorW == agents[,1] & pycorW == agents[,2]]
    pCoords <- agents[order(agentsVal),]
    return(pCoords)
  }
)

#' @export
#' @rdname sortOn
setMethod(
  "sortOn",
  signature = c("matrix", "NLworldStack", "character"),
  definition = function(agents, world, varName) {
    names_l <- names(world)
    l <- match(varName, names_l)
    world_l <- world[[l]]
    sortOn(world = world_l, agents = agents)
  }
)

#' @export
#' @rdname sortOn
setMethod(
  "sortOn",
  signature = c("SpatialPointsDataFrame", "missing", "character"),
  definition = function(agents, varName) {
    turtles <- cbind(agents@coords, agents@data)
    sortTurtles <- turtles[order(turtles[,varName]),]
    return(sortTurtles$who)
  }
)


################################################################################
#' NLwith
#'
#' Reports the coordinates \code{pxcor} and \code{pycor} of the patches with their
#' variable equals to a specific value.
#' Reports the turtles which have their variable equals to a specific value.
#'
#' @param agents  A matrix (ncol = 2) with the first column \code{pxcor} and the
#'                second column \code{pycor} representing the coordinates of the
#'                patches to evaluate.
#'                A SpatialPointsDataFrame created by \code{createTurtles()} or by
#'                \code{createOTurtles()} representing the turtles to evaluate.
#'
#' @param world   A \code{NLworlds} object. Only needed if \code{agents} are patches.
#'                Must not be provided if \code{agents} are turtles.
#'
#' @param varName Characters. The name of the variable to evaluate.
#'                If \code{agents} are patches and the \code{world} is a \code{NLworld}
#'                object, \code{varName} must not be provided. If \code{agents} are
#'                patches and the \code{world} is a \code{NLworldStack} object,
#'                \code{varName} refers to the layer used for evaluating patches
#'                values. If \code{agents} are turtles, \code{varName} is one of
#'                the turtles' variable. \code{varName} can be equal to \code{"xcor"},
#'                \code{"ycor"}, any of the variables created when turtles were created,
#'                as well as any variable created using \code{turtlesOwn()}.
#'
#' @param val     Numeric or characters depending on the variable class. Can be of
#'                any length. Any \code{agents} with their variable equals to any
#'                \code{val} is returned.
#'
#' @return A matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the coordinates of the patches with their value
#'         equal to \code{val}.
#'         A SpatialPointsDataFrame representing the turtles which have their
#'         \code{varName == val}.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = 25, replace = TRUE)
#' plot(w1)
#' p2 <- NLwith(agents = patches(world = w1), world = w1, val = 2)
#'
#' # Turtles
#' t1 <- createTurtles(n = 5, coords = randomXYcor(world = w1, n = 5), breed = c("sheep", "sheep", "wolf", "sheep", "sheperd"))
#' t2 <- NLwith(agents = t1, varName = "breed", val = "sheep")
#' t3 <- NLwith(agents = t1, varName = "breed", val = c("sheep", "wolf"))
#'
#'
#' @export
#' @docType methods
#' @rdname NLwith
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "NLwith",
  function(agents, world, varName, val) {
    standardGeneric("NLwith")
  })

#' @export
#' @rdname NLwith
setMethod(
  "NLwith",
  signature = c("matrix", "NLworld", "missing", "ANY"),
  definition = function(agents, world, val) {
    pxcor <- agents[,1]
    pycor <- agents[,2]
    values <- world[pxcor,pycor]
    pVal <- which(values %in% val)
    pxcorVal <- pxcor[pVal]
    pycorVal <- pycor[pVal]
    return(cbind(pxcor = pxcorVal, pycor = pycorVal))
  }
)

#' @export
#' @rdname NLwith
setMethod(
  "NLwith",
  signature = c("matrix", "NLworldStack", "character", "ANY"),
  definition = function(agents, world, varName, val) {
    names_l <- names(world)
    l <- match(varName, names_l)
    world_l <- world[[l]]
    NLwith(world = world_l, agents = agents, val = val)
  }
)

#' @export
#' @rdname NLwith
setMethod(
  "NLwith",
  signature = c("SpatialPointsDataFrame", "missing", "character", "ANY"),
  definition = function(agents, varName, val) {
    turtles <- cbind(agents@coords, agents@data)
    turtlesWith <- turtles[turtles[,varName] %in% val, ]
    if(nrow(turtlesWith) == 0){
      noTurtles()
    } else {
      newTurtles <- SpatialPointsDataFrame(coords = cbind(xcor = turtlesWith$xcor, ycor = turtlesWith$ycor),
                                           data = turtlesWith[,3:ncol(turtlesWith)])
      return(newTurtles)
    }
  }
)


################################################################################
#' With maximum
#'
#' Reports the coordinates \code{pxcor} and \code{pycor} of the patches which
#' have their variable equals to the maximum value.
#' Reports the turtles which have their variable equals to the maximum values.
#'
#' @param agents  A matrix (ncol = 2) with the first column \code{pxcor} and the
#'                second column \code{pycor} representing the coordinates for the
#'                patches to evaluate.
#'                A SpatialPointsDataFrame created by \code{createTurtles()} or by
#'                \code{createOTurtles()} representing the turtles to evaluate.
#'
#' @param world   A \code{NLworlds} object. Only needed if \code{agents} are patches.
#'                Must not be provided if \code{agents} are turtles.
#'
#' @param varName Characters. The name of the variable to evaluate for the maximum value.
#'                If \code{agents} are patches and the \code{world} is a \code{NLworld}
#'                object, \code{varName} must not be provided. If \code{agents} are
#'                patches and the \code{world} is a \code{NLworldStack} object,
#'                \code{varName} refers to the layer used for evaluating patches
#'                values. If \code{agents} are turtles, \code{varName} is one of
#'                the turtles' variable. \code{varName} can be equal to \code{"xcor"},
#'                \code{"ycor"}, any of the variables created when turtles were created,
#'                as well as any variable created using \code{turtlesOwn()}.
#'
#' @return A matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the coordinates for the patches with the maximum
#'         value among the \code{agents}.
#'         A SpatialPointsDataFrame representing the turtles which have their
#'         \code{varName} equals to the maximum values among the \code{agents}.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = 25, replace = TRUE)
#' plot(w1)
#' pMax <- withMax(agents = patches(world = w1), world = w1)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10), heading = sample(1:3, size = 10, replace= TRUE))
#' t2 <- withMax(agents = t1, varName = "heading")
#'
#'
#' @export
#' @docType methods
#' @rdname withMax
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "withMax",
  function(agents, world, varName) {
    standardGeneric("withMax")
  })

#' @export
#' @rdname withMax
setMethod(
  "withMax",
  signature = c("matrix", "NLworld", "missing"),
  definition = function(agents, world) {
    pxcor <- agents[,1]
    pycor <- agents[,2]
    val <- world[pxcor,pycor]

    if(length(val[is.na(val)]) == length(val)){
      stop("patches' values are all NAs")
    } else {
      maxVal <- max(val, na.rm = TRUE)
      NLwith(agents = agents, world = world, val = maxVal)
    }
  }
)

#' @export
#' @rdname withMax
setMethod(
  "withMax",
  signature = c("matrix", "NLworldStack", "character"),
  definition = function(agents, world, varName) {
    names_l <- names(world)
    l <- match(varName, names_l)
    world_l <- world[[l]]
    withMax(world = world_l, agents = agents)
  }
)

#' @export
#' @rdname withMax
setMethod(
  "withMax",
  signature = c("SpatialPointsDataFrame", "missing", "character"),
  definition = function(agents, varName) {
    val_varName <- of(turtles = agents, tVarName = varName)
    if(length(val_varName[is.na(val_varName)] == length(val_varName))){
      stop("varName equals to NA")
    } else {
      maxVal = max(val_varName, na.rm = TRUE)
      NLwith(agents = agents, varName = varName, val = maxVal)
    }
  }
)


################################################################################
#' With minimum
#'
#' Reports the coordinates \code{pxcor} and \code{pycor} of the patches which
#' have their variable equals to the minimum value.
#' Reports the turtles which have their variable equals to the minimum values.
#'
#' @param agents  A matrix (ncol = 2) with the first column \code{pxcor} and the
#'                second column \code{pycor} representing the coordinates for the
#'                patches to evaluate.
#'                A SpatialPointsDataFrame created by \code{createTurtles()} or by
#'                \code{createOTurtles()} representing the turtles to evaluate.
#'
#' @param world   A \code{NLworlds} object. Only needed if \code{agents} are patches.
#'                Must not be provided if \code{agents} are turtles.
#'
#' @param varName Characters. The name of the variable to evaluate for the minimum value.
#'                If \code{agents} are patches and the \code{world} is a \code{NLworld}
#'                object, \code{varName} must not be provided. If \code{agents} are
#'                patches and the \code{world} is a \code{NLworldStack} object,
#'                \code{varName} refers to the layer used for evaluating patches
#'                values. If \code{agents} are turtles, \code{varName} is one of
#'                the turtles' variable. \code{varName} can be equal to \code{"xcor"},
#'                \code{"ycor"}, any of the variables created when turtles were created,
#'                as well as any variable created using \code{turtlesOwn()}.
#'
#' @return A matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the coordinates for the patches with the minimum
#'         value among the \code{agents}.
#'         A SpatialPointsDataFrame representing the turtles which have their
#'         \code{varName} equals to the minimum values among the \code{agents}.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = 25, replace = TRUE)
#' plot(w1)
#' pMin <- withMin(agents = patches(world = w1), world = w1)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10), heading = sample(1:3, size = 10, replace= TRUE))
#' t2 <- withMin(agents = t1, varName = "heading")
#'
#'
#' @export
#' @docType methods
#' @rdname withMin
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "withMin",
  function(agents, world, varName) {
    standardGeneric("withMin")
  })

#' @export
#' @rdname withMin
setMethod(
  "withMin",
  signature = c("matrix", "NLworld", "missing"),
  definition = function(agents, world) {
    pxcor <- agents[,1]
    pycor <- agents[,2]
    val <- world[pxcor,pycor]

    if(length(val[is.na(val)]) == length(val)){
      stop("patches' values are all NAs")
    } else {
      minVal <- min(val, na.rm = TRUE)
      NLwith(agents = agents, world = world, val = minVal)
    }
  }
)

#' @export
#' @rdname withMin
setMethod(
  "withMin",
  signature = c("matrix", "NLworldStack", "character"),
  definition = function(agents, world, varName) {
    names_l <- names(world)
    l <- match(varName, names_l)
    world_l <- world[[l]]
    withMin(world = world_l, agents = agents)
  }
)

#' @export
#' @rdname withMin
setMethod(
  "withMin",
  signature = c("SpatialPointsDataFrame", "missing", "character"),
  definition = function(agents, varName) {
    val_varName <- of(turtles = agents, tVarName = varName)
    if(length(val_varName[is.na(val_varName)] == length(val_varName))){
      stop("varName equals to NA")
    } else {
      minVal = min(val_varName, na.rm = TRUE)
      NLwith(agents = agents, varName = varName, val = minVal)
    }
  }
)


################################################################################
#' One with maximum
#'
#' Reports one patch coordinates \code{pxcor} and \code{pycor} which has its
#' variable equals to the maximum value.
#' Reports one turtle which has its variable equals to the minimum values.
#'
#' @param agents  A matrix (ncol = 2) with the first column \code{pxcor} and the
#'                second column \code{pycor} representing the coordinates for the
#'                patches to evaluate.
#'                A SpatialPointsDataFrame created by \code{createTurtles()} or by
#'                \code{createOTurtles()} representing the turtles to evaluate.
#'
#' @param world   A \code{NLworlds} object. Only needed if \code{agents} are patches.
#'                Must not be provided if \code{agents} are turtles.
#'
#' @param varName Characters. The name of the variable to evaluate for the maximum value.
#'                If \code{agents} are patches and the \code{world} is a \code{NLworld}
#'                object, \code{varName} must not be provided. If \code{agents} are
#'                patches and the \code{world} is a \code{NLworldStack} object,
#'                \code{varName} refers to the layer used for evaluating patches
#'                values. If \code{agents} are turtles, \code{varName} is one of
#'                the turtles' variable. \code{varName} can be equal to \code{"xcor"},
#'                \code{"ycor"}, any of the variables created when turtles were created,
#'                as well as any variable created using \code{turtlesOwn()}.
#'
#' @return A matrix (ncol = 2, nrow = 1) with the first column \code{pxcor} and
#'         the second column \code{pycor} representing the coordinates for the patch
#'         (or one of the patches) with the maximum value among the \code{agents}.
#'         A SpatialPointsDataFrame of length 1 representing the turtle (or one of
#'         the turtles) which has its \code{varName} equals to the maximum value
#'         among the \code{agents}.
#'
#' @details If there are several \code{agents}. with the maximum value, one is chosen
#'          randomly. To access to all agents with their variable equal to the maximum
#'          value, use \code{withMax()}.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = 25, replace = TRUE)
#' plot(w1)
#' pMax <- maxOneOf(agents = patches(world = w1), world = w1)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10), heading = sample(1:3, size = 10, replace= TRUE))
#' t2 <- maxOneOf(agents = t1, varName = "heading")
#'
#'
#' @export
#' @docType methods
#' @rdname maxOneOf
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "maxOneOf",
  function(agents, world, varName) {
    standardGeneric("maxOneOf")
  })

#' @export
#' @rdname maxOneOf
setMethod(
  "maxOneOf",
  signature = c("matrix", "NLworld", "missing"),
  definition = function(agents, world) {
    maxAgents <- withMax(world = world, agents = agents)
    row <- sample(1:nrow(maxAgents), size = 1)
    return(maxAgents[row,])
  }
)

#' @export
#' @rdname maxOneOf
setMethod(
  "maxOneOf",
  signature = c("matrix", "NLworldStack", "character"),
  definition = function(agents, world, varName) {
    maxAgents <- withMax(world = world, agents = agents, varName = varName)
    row <- sample(1:nrow(maxAgents), size = 1)
    return(maxAgents[row,])
  }
)

#' @export
#' @rdname maxOneOf
setMethod(
  "maxOneOf",
  signature = c("SpatialPointsDataFrame", "missing", "character"),
  definition = function(agents, varName) {
    maxAgents <- withMax(agents = agents, varName = varName)
    row <- sample(1:nrow(maxAgents), size = 1)
    return(maxAgents[row,])
  }
)


################################################################################
#' One with minimum
#'
#' Reports one patch coordinates \code{pxcor} and \code{pycor} which has its
#' variable equals to the minimum value.
#' Reports one turtle which has its variable equals to the minimum values.
#'
#' @param agents  A matrix (ncol = 2) with the first column \code{pxcor} and the
#'                second column \code{pycor} representing the coordinates for the
#'                patches to evaluate.
#'                A SpatialPointsDataFrame created by \code{createTurtles()} or by
#'                \code{createOTurtles()} representing the turtles to evaluate.
#'
#' @param world   A \code{NLworlds} object. Only needed if \code{agents} are patches.
#'                Must not be provided if \code{agents} are turtles.
#'
#' @param varName Characters. The name of the variable to evaluate for the minimum value.
#'                If \code{agents} are patches and the \code{world} is a \code{NLworld}
#'                object, \code{varName} must not be provided. If \code{agents} are
#'                patches and the \code{world} is a \code{NLworldStack} object,
#'                \code{varName} refers to the layer used for evaluating patches
#'                values. If \code{agents} are turtles, \code{varName} is one of
#'                the turtles' variable. \code{varName} can be equal to \code{"xcor"},
#'                \code{"ycor"}, any of the variables created when turtles were created,
#'                as well as any variable created using \code{turtlesOwn()}.
#'
#' @return A matrix (ncol = 2, nrow = 1) with the first column \code{pxcor} and
#'         the second column \code{pycor} representing the coordinates for the patch
#'         (or one of the patches) with the minimum value among the \code{agents}.
#'         A SpatialPointsDataFrame of length 1 representing the turtle (or one of
#'         the turtles) which has its \code{varName} equals to the minimum value
#'         among the \code{agents}.
#'
#' @details If there are several \code{agents}. with the minimum value, one is chosen
#'          randomly. To access to all agents with their variable equal to the minimum
#'          value, use \code{withMin()}.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = 25, replace = TRUE)
#' plot(w1)
#' pMin <- minOneOf(agents = patches(world = w1), world = w1)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10), heading = sample(1:3, size = 10, replace= TRUE))
#' t2 <- minOneOf(agents = t1, varName = "heading")
#'
#'
#' @export
#' @docType methods
#' @rdname minOneOf
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "minOneOf",
  function(agents, world, varName) {
    standardGeneric("minOneOf")
  })

#' @export
#' @rdname minOneOf
setMethod(
  "minOneOf",
  signature = c("matrix", "NLworld", "missing"),
  definition = function(agents, world) {
    minAgents <- withMin(world = world, agents = agents)
    row <- sample(1:nrow(minAgents), size = 1)
    return(minAgents[row,])
  }
)

#' @export
#' @rdname minOneOf
setMethod(
  "minOneOf",
  signature = c("matrix", "NLworldStack", "character"),
  definition = function(agents, world, varName) {
    minAgents <- withMin(world = world, agents = agents, varName = varName)
    row <- sample(1:nrow(minAgents), size = 1)
    return(minAgents[row,])
  }
)

#' @export
#' @rdname minOneOf
setMethod(
  "minOneOf",
  signature = c("SpatialPointsDataFrame", "missing", "character"),
  definition = function(agents, varName) {
    minAgents <- withMin(agents = agents, varName = varName)
    row <- sample(1:nrow(minAgents), size = 1)
    return(minAgents[row,])
  }
)
