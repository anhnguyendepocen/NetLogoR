################################################################################
#' All agents?
#'
#' Report \code{TRUE} if all \code{agents} have their variable equal to a given value,
#' report \code{FALSE} otherwise.
#'
#' @inheritParams fargs
#'
#' @return Logical. \code{TRUE} if all the \code{agents} have their variable equal to
#'         \code{val}, \code{FALSE} otherwise.
#'
#' @details \code{world} must not be provided if \code{agents} are turtles.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#all}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = length(w1), replace = TRUE)
#' NLall(agents = patches(w1), world = w1, val = 5)
#' w2 <- w1
#' w2[] <- 5
#' NLall(agents = patches(w2), world = w2, val = 5)
#'
#' # Turtles
#' t1 <- createTurtles(n = 5, coords = cbind(xcor = 1, ycor = 1), heading = c(1, 2, 2, 1, 2))
#' NLall(agents = t1, var = "xcor", val = 1)
#' NLall(agents = t1, var = "heading", val = 2)
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
  function(agents, world, var, val) {
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
  definition = function(agents, world, var, val) {
    names_l <- names(world)
    l <- match(var, names_l)
    world_l <- world[[l]]
    NLall(world = world_l, agents = agents, val = val)
  }
)

#' @export
#' @rdname NLall
setMethod(
  "NLall",
  signature = c("SpatialPointsDataFrame", "missing", "character", "ANY"),
  definition = function(agents, var, val) {
    withVal <- NLwith(agents = agents, var = var, val = val)
    allTrue <- ifelse(length(agents) == length(withVal), TRUE, FALSE)
    return(allTrue)
  }
)


################################################################################
#' Any agents?
#'
#' Report \code{TRUE} if \code{agents} is non empty, report \code{FALSE} otherwise.
#'
#' @inheritParams fargs
#'
#' @return Logical. \code{TRUE} if there is at least one patch or one turtle in the
#'         \code{agents}, \code{FALSE} otherwise.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#any}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' p1 <- noPatches()
#' p2 <- patch(w1, 0, 0)
#' NLany(p1)
#' NLany(p2)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
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
#' Count agents
#'
#' Report the number of patches or turtles in the \code{agents}.
#'
#' @inheritParams fargs
#'
#' @return Integer.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#count}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4) # 25 patches
#' p1 <- patches(w1)
#' count(p1) # 25
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
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
#' Sort agents
#'
#' Report the \code{agents} sorted according to their value.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2) with the first column "pxcor" and the second column
#'         "pycor" representing the coordinates of the patches, sorted according to
#'         their values, if \code{agents}
#'         are patches, or
#'
#'         SpatialPointsDataFrame representing the turtles, sorted according
#'         to their \code{var} values, if \code{agents} are
#'         turtles.
#'
#' @details \code{world} must not be provided if \code{agents} are turtles.
#'
#'          The sorting of the \code{agents} is done in a increasing order.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#sort-on}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = length(w1), replace = TRUE)
#' plot(w1)
#' p1 <- sortOn(agents = patches(w1), world = w1)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' sortdHeadingst1 <- sortOn(agents = t1, var = "heading")
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
  function(agents, world, var) {
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
  definition = function(agents, world, var) {
    names_l <- names(world)
    l <- match(var, names_l)
    world_l <- world[[l]]
    sortOn(world = world_l, agents = agents)
  }
)

#' @export
#' @rdname sortOn
setMethod(
  "sortOn",
  signature = c("SpatialPointsDataFrame", "missing", "character"),
  definition = function(agents, var) {
    turtles <- cbind(agents@coords, agents@data)
    sortData <- turtles[order(turtles[,var]),]

    agents@coords <- cbind(xcor = sortData[,1], ycor = sortData[,2])
    agents@data = sortData[,3:ncol(sortData)]
    return(agents)
  }
)


################################################################################
#' Agents with
#'
#' Report the patches or the turtles among \code{agents} which have their variable
#' equals to a specific value.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2) with the first column "pxcor" and the second column
#'         "pycor" representing the coordinates of the patches among the \code{agents}
#'         which have their variable
#'         equals to any \code{val}, or
#'
#'         SpatialPointsDataFrame representing the turtles among the \code{agents}
#'         which have their variable
#'         \code{var} equals to any \code{val}.
#'
#' @details \code{world} must not be provided if \code{agents} are turtles.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#with}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = length(w1), replace = TRUE)
#' plot(w1)
#' p2 <- NLwith(agents = patches(w1), world = w1, val = 2)
#'
#' # Turtles
#' t1 <- createTurtles(n = 5, coords = randomXYcor(w1, n = 5), breed = c("sheep", "sheep", "wolf", "sheep", "sheperd"))
#' t2 <- NLwith(agents = t1, var = "breed", val = "sheep")
#' t3 <- NLwith(agents = t1, var = "breed", val = c("sheep", "wolf"))
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
  function(agents, world, var, val) {
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
  definition = function(agents, world, var, val) {
    names_l <- names(world)
    l <- match(var, names_l)
    world_l <- world[[l]]
    NLwith(world = world_l, agents = agents, val = val)
  }
)

#' @export
#' @rdname NLwith
setMethod(
  "NLwith",
  signature = c("SpatialPointsDataFrame", "missing", "character", "ANY"),
  definition = function(agents, var, val) {
    turtles <- cbind(agents@coords, agents@data)
    turtlesWith <- turtles[turtles[,var] %in% val, ]
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
#' Agents with maximum
#'
#' Report the patches or turtles among \code{agents} which have their variable
#' equals to the maximum value.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2) with the first column "pxcor" and the second column
#'         "pycor" representing the coordinates of the patches among the \code{agents}
#'         which have their variable
#'         equal to the maximum value among the \code{agents}, or
#'
#'         SpatialPointsDataFrame representing the turtles among the \code{agents}
#'         which have their variable
#'         \code{var} equal to the maximum value among the \code{agents}.
#'
#' @details \code{world} must not be provided if \code{agents} are turtles.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#with-max}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = length(w1), replace = TRUE)
#' plot(w1)
#' p1 <- withMax(agents = patches(w1), world = w1)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10), heading = sample(1:3, size = 10, replace= TRUE))
#' t2 <- withMax(agents = t1, var = "heading")
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
  function(agents, world, var) {
    standardGeneric("withMax")
  })

#' @export
#' @rdname withMax
setMethod(
  "withMax",
  signature = c("matrix", "NLworld", "missing"),
  definition = function(agents, world) {

    val <- of(world = world, agents = agents)

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
  definition = function(agents, world, var) {
    names_l <- names(world)
    l <- match(var, names_l)
    world_l <- world[[l]]
    withMax(world = world_l, agents = agents)
  }
)

#' @export
#' @rdname withMax
setMethod(
  "withMax",
  signature = c("SpatialPointsDataFrame", "missing", "character"),
  definition = function(agents, var) {

    val_var <- of(agents = agents, var = var)

    if(length(val_var[is.na(val_var)] == length(val_var))){
      stop("var equals to NA")
    } else {
      maxVal = max(val_var, na.rm = TRUE)
      NLwith(agents = agents, var = var, val = maxVal)
    }
  }
)


################################################################################
#' Agents with minimum
#'
#' Report the patches or turtles among \code{agents} which have their variable
#' equals to the minimum value.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2) with the first column "pxcor" and the second column
#'         "pycor" representing the coordinates of the patches among the \code{agents}
#'         which have their variable
#'         equal to the minimum value among the \code{agents}, or
#'
#'         SpatialPointsDataFrame representing the turtles among the \code{agents}
#'         which have their variable
#'         \code{var} equal to the minimum value among the \code{agents}.
#'
#' @details \code{world} must not be provided if \code{agents} are turtles.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#with-min}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = length(w1), replace = TRUE)
#' plot(w1)
#' p1 <- withMin(agents = patches(w1), world = w1)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10), heading = sample(1:3, size = 10, replace= TRUE))
#' t2 <- withMin(agents = t1, var = "heading")
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
  function(agents, world, var) {
    standardGeneric("withMin")
  })

#' @export
#' @rdname withMin
setMethod(
  "withMin",
  signature = c("matrix", "NLworld", "missing"),
  definition = function(agents, world) {

    val <- of(world = world, agents = agents)

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
  definition = function(agents, world, var) {
    names_l <- names(world)
    l <- match(var, names_l)
    world_l <- world[[l]]
    withMin(world = world_l, agents = agents)
  }
)

#' @export
#' @rdname withMin
setMethod(
  "withMin",
  signature = c("SpatialPointsDataFrame", "missing", "character"),
  definition = function(agents, var) {

    val_var <- of(agents = agents, var = var)
    if(length(val_var[is.na(val_var)] == length(val_var))){
      stop("var equals to NA")
    } else {
      minVal = min(val_var, na.rm = TRUE)
      NLwith(agents = agents, var = var, val = minVal)
    }
  }
)


################################################################################
#' One agent with maximum
#'
#' Report one patch or one turtle among \code{agents} which has its variable equals
#' to the maximum value.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2, nrow = 1) with the first column "pxcor" and
#'         the second column "pycor" representing the coordinates of the patch
#'         (or of one of the patches) among the \code{agents} which has its variable
#'         equals to the maximum value
#'         among the \code{agents}, or
#'
#'         SpatialPointsDataFrame of length 1 representing the turtle (or one of
#'         the turtles) among the \code{agents} which has its variable \code{var}
#'         equals to the maximum value
#'         among the \code{agents}.
#'
#' @details \code{world} must not be provided if \code{agents} are turtles.
#'
#'          If there are several patches or turtles among \code{agents} with their
#'          variable equal to the maximum
#'          value, one is chosen randomly. To access to all patches or turtles among
#'          \code{agents} which have their variable equal
#'          to the maximum value, use \code{withMax()}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#max-one-of}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = length(w1), replace = TRUE)
#' plot(w1)
#' p1 <- maxOneOf(agents = patches(w1), world = w1)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10), heading = sample(1:3, size = 10, replace= TRUE))
#' t2 <- maxOneOf(agents = t1, var = "heading")
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
  function(agents, world, var) {
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
    maxAgent <- maxAgents[row,]
    return(cbind(pxcor = maxAgent[1], pycor = maxAgent[2]))
  }
)

#' @export
#' @rdname maxOneOf
setMethod(
  "maxOneOf",
  signature = c("matrix", "NLworldStack", "character"),
  definition = function(agents, world, var) {
    pos_l <- which(names(world) == var, TRUE) # find the layer
    world_l <- world[[pos_l]]
    maxOneOf(agents = agents, world = world_l)
  }
)

#' @export
#' @rdname maxOneOf
setMethod(
  "maxOneOf",
  signature = c("SpatialPointsDataFrame", "missing", "character"),
  definition = function(agents, var) {
    maxAgents <- withMax(agents = agents, var = var)
    if(length(maxAgents) == 1){
      return(maxAgents)
    } else {
      whoSample <- sample(maxAgents@data$who, size = 1)
      turtle(turtles = maxAgents, who = whoSample)
    }
  }
)


################################################################################
#' One agent with minimum
#'
#' Report one patch or one turtle among \code{agents} which has its variable equals
#' to the minimum value.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2, nrow = 1) with the first column "pxcor" and
#'         the second column "pycor" representing the coordinates of the patch
#'         (or of one of the patches) among the \code{agents} which has its variable
#'         equals to the minimum value
#'         among the \code{agents}, or
#'
#'         SpatialPointsDataFrame of length 1 representing the turtle (or one of
#'         the turtles) among the \code{agents} which has its variable \code{var}
#'         equals to the minimum value
#'         among the \code{agents}.
#'
#' @details \code{world} must not be provided if \code{agents} are turtles.
#'
#'          If there are several patches or turtles among \code{agents} with their
#'          variable equal to the minimum
#'          value, one is chosen randomly. To access to all patches or turtles among
#'          \code{agents} which have their variable equal
#'          to the minimum value, use \code{withMin()}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#min-one-of}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = length(w1), replace = TRUE)
#' plot(w1)
#' p1 <- minOneOf(agents = patches(w1), world = w1)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10), heading = sample(1:3, size = 10, replace= TRUE))
#' t2 <- minOneOf(agents = t1, var = "heading")
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
  function(agents, world, var) {
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
    minAgent <- minAgents[row,]
    return(cbind(pxcor = minAgent[1], minAgent[2]))
  }
)

#' @export
#' @rdname minOneOf
setMethod(
  "minOneOf",
  signature = c("matrix", "NLworldStack", "character"),
  definition = function(agents, world, var) {
    pos_l <- which(names(world) == var, TRUE) # find the layer
    world_l <- world[[pos_l]]
    minOneOf(agents = agents, world = world_l)
  }
)

#' @export
#' @rdname minOneOf
setMethod(
  "minOneOf",
  signature = c("SpatialPointsDataFrame", "missing", "character"),
  definition = function(agents, var) {
    minAgents <- withMin(agents = agents, var = var)
    if(length(minAgents) == 1){
      return(minAgents)
    } else {
      whoSample <- sample(minAgents@data$who, size = 1)
      turtle(turtles = minAgents, who = whoSample)
    }
  }
)


################################################################################
#' Type of object
#'
#' Report \code{TRUE} if the \code{agents} is of the \code{class} tested,
#' report \code{FALSE} otherwise.
#'
#' @inheritParams fargs
#'
#' @param class  Character. Can take one of the following options to define
#'               the \code{class}: \code{"agent"}, \code{"agentset"},
#'               \code{"patch"}, \code{"patchset"}. \code{"turtle"} or \code{"turtleset"}.
#'
#' @return Logical. \code{TRUE} if \code{agents} is of the \code{class} tested.
#'
#' @details Careful! The \code{class} tested does not correspond to actual R classes.
#'
#'          \code{agents} is \code{"patch"} if it is a matrix (ncol = 2) with the
#'          first column "pxcor" and the second column "pycor" with only
#'          one row. \code{agents} is \code{"patcheset"} if the matrix has more than
#'          one row.
#'
#'          \code{agents} is \code{"turtle"} if it is a SpatialPointsDataFrame
#'          of length 1 with the variables created when using \code{createTurtles()}
#'          or \code{createOTurtles()}. \code{agents} is \code{"turtleset"} if the
#'          SpatialPointsDataFrame is of length larger than 1.
#'
#'          \code{agents} is \code{"agent"} if it is either \code{"patch"} or
#'          \code{"turtle"}. \code{agents} is \code{"agentset"} if it is either
#'          \code{"patcheset"} or \code{"turtleset"}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#is-of-type}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10), heading = sample(1:3, size = 10, replace= TRUE))
#' isNLclass(agents = patches(w1), class = "patch")
#' isNLclass(agents = patches(w1), class = "patcheset")
#' isNLclass(agents = t1, class = "agentset")
#' isNLclass(agents = t1, class = "turtleset")
#'
#'
#' @export
#' @docType methods
#' @rdname isNLclass
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "isNLclass",
  function(agents, class) {
    standardGeneric("isNLclass")
  })

#' @export
#' @rdname isNLclass
setMethod(
  "isNLclass",
  signature = c("matrix", "character"),
  definition = function(agents, class) {
    # If it is this signature, it is a matrix, therefore patch or patches
    if(class == "agent"){
      class <- "patch"
    }
    if(class == "agentset"){
      class <- "patchset"
    }

    if((colnames(agents) == c("pxcor", "pycor") && nrow(agents) != 0)){
      if(nrow(agents) == 1){
        agentsClass <- "patch"
      } else {
        agentsClass <- "patchset"
      }
    } else {
      agentsClass <- "nothing"
    }

    matchClass <- ifelse(class == agentsClass, TRUE, FALSE)
    return(matchClass)
  }
)

#' @export
#' @rdname isNLclass
setMethod(
  "isNLclass",
  signature = c("SpatialPointsDataFrame", "character"),
  definition = function(agents, class) {
    # If it is this signature, it is a SPDF, therefore turtle or turtles
    if(class == "agent"){
      class <- "turtle"
    }
    if(class == "agentset"){
      class <- "turtleset"
    }

    if((colnames(agents@data[1:6]) == c("who", "heading", "prevX", "prevY", "breed", "color") && length(agents) != 0)){
      if(length(agents) == 1){
        agentsClass <- "turtle"
      } else {
        agentsClass <- "turtleset"
      }
    } else {
      agentsClass <- "nothing"
    }

    matchClass <- ifelse(class == agentsClass, TRUE, FALSE)
    return(matchClass)
  }
)


################################################################################
#' N random agents
#'
#' Report \code{n} patches or turtles randomly selected among \code{agents}.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2, nrow = \code{n}) with the first column "pxcor"
#'         and the second  column "pycor" representing the coordinates of the
#'         selected patches from \code{agents}, or
#'
#'         SpatialPointsDataFrame of length \code{n} representing the turtles
#'         selected from \code{agents}.
#'
#' @details \code{n} must be less or equal the number of patches or turtles in \code{agents}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#n-of}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' pSelect <- nOf(agents = patches(w1), n = 5)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' tSelect <- nOf(agents = t1, n = 2)
#'
#'
#' @export
#' @docType methods
#' @rdname nOf
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "nOf",
  function(agents, n) {
    standardGeneric("nOf")
  })

#' @export
#' @rdname nOf
setMethod(
  "nOf",
  signature = c("matrix", "numeric"),
  definition = function(agents, n) {
    row <- sample(1:nrow(agents), size = n, replace = FALSE)
    row <- row[order(row)]
    patches <- agents[row,]

    if(length(row) == 1){ # to keep the class = matrix
      patches <- cbind(pxcor = patches[1], pycor = patches[2])
    }

    return(patches)
  }
)

#' @export
#' @rdname nOf
setMethod(
  "nOf",
  signature = c("SpatialPointsDataFrame", "numeric"),
  definition = function(agents, n) {
    row <- sample(1:length(agents), size = n, replace = FALSE)
    row <- row[order(row)]
    newCoords <- agents@coords[row,]
    newData <- agents@data[row,]

    if(length(row) == 1){ # to keep the class = matrix
      newCoords <- cbind(xcor = newCoords[1], ycor = newCoords[2])
    }

    newTurtles <- SpatialPointsDataFrame(coords = newCoords, data = newData)
    return(newTurtles)
  }
)


################################################################################
#' One random agent
#'
#' Report one patch or turtle randomly selected among \code{agents}.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2, nrow = 1) with the first column "pxcor"
#'         and the second  column "pycor" representing the coordinates of the
#'         selected patch from \code{agents}, or
#'
#'         SpatialPointsDataFrame of length 1 representing the turtle
#'         selected from \code{agents}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#one-of}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' pSelect <- oneOf(agents = patches(w1))
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' tSelect <- oneOf(agents = t1)
#'
#'
#' @export
#' @docType methods
#' @rdname oneOf
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "oneOf",
  function(agents) {
    standardGeneric("oneOf")
  })

#' @export
#' @rdname oneOf
setMethod(
  "oneOf",
  signature = c("matrix"),
  definition = function(agents) {
    nOf(agents = agents, n = 1)
  }
)

#' @export
#' @rdname oneOf
setMethod(
  "oneOf",
  signature = c("SpatialPointsDataFrame"),
  definition = function(agents) {
    nOf(agents = agents, n = 1)
  }
)


################################################################################
#' N agents with maximum
#'
#' Report the \code{n} patches or turtles among \code{agents} which have their variable
#' among the maximum values.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2, nrow = \code{n}) with the first column "pxcor" and
#'         the second column "pycor" representing the coordinates of the \code{n}
#'         patches among the \code{agents} which have their variable values among
#'         the maximum values among the
#'         \code{agents}, or
#'
#'         SpatialPointsDataFrame of length \code{n} representing the turtles among the
#'         \code{agents} which
#'         have their \code{var} values among the maximum values among the \code{agents}.
#'
#' @details \code{world} must not be provided if \code{agents} are turtles.
#'
#'          If there is a tie that would make the number of returned patches or turtles larger
#'          than \code{n}, it is broken randomly.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#max-n-of}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:10, size = length(w1), replace = TRUE)
#' plot(w1)
#' p1 <- maxNof(agents = patches(w1), n = 6, world = w1)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10), heading = sample(1:5, size = 10, replace= TRUE))
#' t2 <- maxNof(agents = t1, n = 5, var = "heading")
#'
#'
#' @export
#' @docType methods
#' @rdname maxNof
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "maxNof",
  function(agents, n, world, var) {
    standardGeneric("maxNof")
  })

#' @export
#' @rdname maxNof
setMethod(
  "maxNof",
  signature = c("matrix", "numeric", "NLworld", "missing"),
  definition = function(agents, n, world) {

    if(n == 1){
      maxOneOf(agents = agents, world = world)
    } else if(n == 0){
      noPatches()
    } else if(n == nrow(agents)){
      return(agents)
    } else {

      val <- of(world = world, agents = agents)
      agentsVal <- cbind(val, agents)
      agentsVal <- agentsVal[order(-agentsVal[,"val"]),] # decreasing order

      minVal <- min(agentsVal[1:n, "val"], na.rm = TRUE)
      maxAgents <- agentsVal[agentsVal[,"val"] >= minVal,]

      # To break ties randomly
      if(nrow(maxAgents) != n){
        nToRemove <- nrow(maxAgents) - n # how many ties to remove
        toKeep <- sample(1:nrow(maxAgents[maxAgents[,"val"] == minVal,]),
                         size = nrow(maxAgents[maxAgents[,"val"] == minVal,]) -nToRemove) # rows to keep
        maxAgents <- rbind(maxAgents[maxAgents[,"val"] > minVal,], maxAgents[maxAgents[,"val"] == minVal,][toKeep,])
      }

      return(cbind(pxcor = maxAgents[,"pxcor"], pycor = maxAgents[,"pycor"]))
    }
  }
)

#' @export
#' @rdname maxNof
setMethod(
  "maxNof",
  signature = c("matrix", "numeric", "NLworldStack", "character"),
  definition = function(agents, n, world, var) {
    pos_l <- which(names(world) == var, TRUE) # find the layer
    world_l <- world[[pos_l]]
    maxNof(agents = agents, n = n, world = world_l)
  }
)

#' @export
#' @rdname maxNof
setMethod(
  "maxNof",
  signature = c("SpatialPointsDataFrame", "numeric", "missing", "character"),
  definition = function(agents, n, var) {

    if(n == 1){
      maxOneOf(agents = agents, var = var)
    } else if(n == 0){
      noTurtles()
    } else if(n == length(agents)){
      return(agents)
    } else {

      tData <- agents@data
      rownames(tData) <- 1:nrow(tData)
      tData <- tData[order(-tData[,var]),] # decreasing order

      minVal <- min(tData[1:n, var], na.rm = TRUE)
      maxAgents <- tData[tData[,var] >= minVal,]

      # To break ties randomly
      if(nrow(maxAgents) != n){
        nToRemove <- nrow(maxAgents) - n # how many ties to remove
        toKeep <- sample(1:nrow(maxAgents[maxAgents[,var] == minVal,]),
                         size = nrow(maxAgents[maxAgents[,var] == minVal,]) -nToRemove) # rows to keep
        maxAgents <- rbind(maxAgents[maxAgents[,var] > minVal,], maxAgents[maxAgents[,var] == minVal,][toKeep,])
      }

      tSelect <- as.numeric(rownames(maxAgents))
      tSelect <- sort(tSelect)

      tSelectCoords <- cbind(xcor = agents@coords[tSelect,1], ycor = agents@coords[tSelect,2])
      tSelectData <- agents@data[tSelect,]
      maxTurtles <- SpatialPointsDataFrame(coords = tSelectCoords, data = tSelectData)
      return(maxTurtles)
    }
  }
)


################################################################################
#' N agents with minimum
#'
#' Report the \code{n} patches or turtles among \code{agents} which have their variable
#' among the minimum values.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2, nrow = \code{n}) with the first column "pxcor" and
#'         the second column "pycor" representing the coordinates of the \code{n}
#'         patches among the \code{agents} which have their variable values among
#'         the minimum values among the
#'         \code{agents}, or
#'
#'         SpatialPointsDataFrame of length \code{n} representing the turtles among the
#'         \code{agents} which
#'         have their \code{var} values among the minimum values among the \code{agents}.
#'
#' @details \code{world} must not be provided if \code{agents} are turtles.
#'
#'          If there is a tie that would make the number of returned patches or turtles larger
#'          than \code{n}, it is broken randomly.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#min-n-of}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:10, size = length(w1), replace = TRUE)
#' plot(w1)
#' p1 <- minNof(agents = patches(w1), n = 6, world = w1)
#'
#' # Turtles
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10), heading = sample(1:5, size = 10, replace= TRUE))
#' t2 <- minNof(agents = t1, n = 5, var = "heading")
#'
#'
#' @export
#' @docType methods
#' @rdname minNof
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "minNof",
  function(agents, n, world, var) {
    standardGeneric("minNof")
  })

#' @export
#' @rdname minNof
setMethod(
  "minNof",
  signature = c("matrix", "numeric", "NLworld", "missing"),
  definition = function(agents, n, world) {

    if(n == 1){
      minOneOf(agents = agents, world = world)
    } else if(n == 0){
      noPatches()
    } else if(n == nrow(agents)){
      return(agents)
    } else {

      maxPatches <- maxNof(agents = agents, n = nrow(agents) - n, world = world)
      other(agents = agents, except = maxPatches)
    }
  }
)

#' @export
#' @rdname minNof
setMethod(
  "minNof",
  signature = c("matrix", "numeric", "NLworldStack", "character"),
  definition = function(agents, n, world, var) {
    pos_l <- which(names(world) == var, TRUE) # find the layer
    world_l <- world[[pos_l]]
    minNof(agents = agents, n = n, world = world_l)
  }
)

#' @export
#' @rdname minNof
setMethod(
  "minNof",
  signature = c("SpatialPointsDataFrame", "numeric", "missing", "character"),
  definition = function(agents, n, var) {

    if(n == 1){
      minOneOf(agents = agents, var = var)
    } else if(n == 0){
      noTurtles()
    } else if(n == length(agents)){
      return(agents)
    } else {

      maxTurtles <- maxNof(agents = agents, n = length(agents) - n, var = var)
      other(agents = agents, except = maxTurtles)
    }
  }
)


################################################################################
#' Agents in radius
#'
#' Report the patches or turtles among \code{agents2} within given distances of
#' each of the \code{agents}.
#'
#' @inheritParams fargs
#'
#' @param radius  Numeric. Vector of distances from \code{agents} to locate
#'                \code{agents2}. Must be of length 1 or of length \code{agents}.
#'
#' @param agents2 Matrix (ncol = 2) with the first column "pxcor" and the second
#'               column "pycor" representing the patches coordinates, or
#'
#'               SpatialPointsDataFrame created by \code{createTurtles()} or
#'               by \code{createOTurtles()} representing the moving agents.
#'
#' @return List of length equal to \code{count(agents)}.
#'         List items are matrices (ncol = 2) with the first column "pxcor"
#'         and the second column "pycor" representing the coordinates of the
#'         patches among \code{agents2} within \code{radius} distances for each \code{agents}, if
#'         \code{agents2} are patches, or SpatialPointsDataFrame objects representing
#'         the turtles among \code{agents2} within \code{radius} distances for each \code{agents} if
#'         \code{agents2} are turtles.
#'
#' @details Distances from/to patches are calculated from/to their center.
#'
#'          If \code{torus = TRUE}, the \code{radius} distances are calculared
#'          around the sides of the \code{world} to select \code{agents2}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#in-radius}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#'
#' p1 <- inRadius(agents = patch(w1, 0, 0), radius = 2, agents2 = patches(w1), world = w1)
#' t2 <- inRadius(agents = patch(w1, 0, 0), radius = 2, agents2 = t1, world = w1)
#' p2 <- inRadius(agents = t1, radius = 2, agents2 = patches(w1), world = w1)
#' t3 <- inRadius(agents = turtle(t1, who = 0), radius = 2, agents2 = t1, world = w1)
#'
#'
#' @export
#' @importFrom rgeos gBuffer
#' @importFrom sp over
#' @importFrom SpaDES wrap
#' @docType methods
#' @rdname inRadius
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "inRadius",
  function(agents, radius, agents2, world, torus = FALSE) {
    standardGeneric("inRadius")
  })

#' @export
#' @rdname inRadius
setMethod(
  "inRadius",
  signature = c(agents = "matrix", radius = "numeric", agents2 = "matrix", world = "NLworlds"),
  definition = function(agents, radius, agents2, world, torus) {
    inRadius(agents = SpatialPointsDataFrame(coords = agents, data = data.frame(rep(NA, nrow(agents)))), radius = radius, agents2 = agents2, world = world, torus = torus)
  }
)

#' @export
#' @rdname inRadius
setMethod(
  "inRadius",
  signature = c(agents = "matrix", radius = "numeric", agents2 = "SpatialPointsDataFrame", world = "NLworlds"),
  definition = function(agents, radius, agents2, world, torus) {
    inRadius(agents = SpatialPointsDataFrame(coords = agents, data = data.frame(rep(NA, nrow(agents)))), radius = radius, agents2 = agents2, world = world, torus = torus)
  }
)

#' @export
#' @rdname inRadius
setMethod(
  "inRadius",
  signature = c(agents = "SpatialPointsDataFrame", radius = "numeric", agents2 = "matrix", world = "NLworlds"),
  definition = function(agents, radius, agents2, world, torus) {

    # Create buffers around the locations of agents
    pBuffer <- gBuffer(agents, byid = TRUE, id = 1:length(agents), width = radius, quadsegs = 50)

    if(torus == TRUE){
      worldWrap <- createNLworld(minPxcor = minPxcor(world) - radius, maxPxcor = maxPxcor(world) + radius,
                                 minPycor = minPycor(world) - radius, maxPycor = maxPycor(world) + radius)
      pAllWrap <- patches(worldWrap)
      # Extract the locations of agents2 under the buffers
      pOver <- over(pBuffer, SpatialPoints(coords = pAllWrap), returnList = TRUE)
      list_agentsXY <- lapply(pOver, function(z){
        wrap(cbind(x = pAllWrap[as.numeric(z), 1], y = pAllWrap[as.numeric(z), 2]), extent(world))
      })
      colnames(agents2) <- c("pxcor", "pycor")
      list_agents <- lapply(list_agentsXY, function(x){
        colnames(x) <- c("pxcor", "pycor")
        as.matrix(merge(x, agents2))
      })

    } else {
      # Extract the locations of agents2 under the buffers
      pOver <- over(pBuffer, SpatialPoints(coords = agents2), returnList = TRUE)
      list_agents <- lapply(pOver, function(x){
        cbind(pxcor = agents2[as.numeric(x), 1], pycor = agents2[as.numeric(x), 2])
      })
    }

    return(list_agents)
  }
)

#' @export
#' @rdname inRadius
setMethod(
  "inRadius",
  signature = c(agents = "SpatialPointsDataFrame", radius = "numeric", agents2 = "SpatialPointsDataFrame", world = "NLworlds"),
  definition = function(agents, radius, agents2, world, torus) {

    # Create buffers around the locations of agents
    pBuffer <- gBuffer(agents, byid = TRUE, id = 1:length(agents), width = radius, quadsegs = 50)

    if(torus == TRUE){
      agents2c <- agents2@coords
      agents2c1 <- cbind(agents2c[,1] - (world@extent@xmax - world@extent@xmin), agents2c[,2] + (world@extent@ymax - world@extent@ymin))
      agents2c2 <- cbind(agents2c[,1], agents2c[,2] + (world@extent@ymax - world@extent@ymin))
      agents2c3 <- cbind(agents2c[,1] + (world@extent@xmax - world@extent@xmin), agents2c[,2] + (world@extent@ymax - world@extent@ymin))
      agents2c4 <- cbind(agents2c[,1] - (world@extent@xmax - world@extent@xmin), agents2c[,2])
      agents2c5 <- cbind(agents2c[,1] + (world@extent@xmax - world@extent@xmin), agents2c[,2])
      agents2c6 <- cbind(agents2c[,1] - (world@extent@xmax - world@extent@xmin), agents2c[,2] - (world@extent@ymax - world@extent@ymin))
      agents2c7 <- cbind(agents2c[,1], agents2c[,2] - (world@extent@ymax - world@extent@ymin))
      agents2c8 <- cbind(agents2c[,1] + (world@extent@xmax - world@extent@xmin), agents2c[,2] - (world@extent@ymax - world@extent@ymin))
      agents2cAll <- rbind(agents2c, agents2c1, agents2c2, agents2c3, agents2c4, agents2c5, agents2c6, agents2c7, agents2c8)

      # Extract the locations of agents2 under the buffers
      pOver <- over(pBuffer, SpatialPoints(coords = agents2cAll), returnList = TRUE)
      list_agentsXY <- lapply(pOver, function(z){
        wrap(cbind(x = agents2cAll[as.numeric(z), 1], y = agents2cAll[as.numeric(z), 2]), extent(world))
      })
      list_agents <- lapply(list_agentsXY, function(x){
        tWho <- merge(x, cbind(agents2@coords, agents2@data), by.x = c("x", "y"), by.y = c("xcor", "ycor"))
        turtle(turtles = agents2, who = tWho[,"who"])
      })

    } else {
      # Extract the locations of agents2 under the buffers
      pOver <- over(pBuffer, agents2, returnList = TRUE)
      list_agents <- lapply(pOver, function(x){
        turtle(turtles = agents2, who = x[,"who"])
      })
    }

    return(list_agents)
  }
)


################################################################################
#' Agents in cone
#'
#' Report the \code{agents} within the "cone of vision" in front of each one of the
#' \code{turtles}.
#'
#' @inheritParams fargs
#'
#' @param radius  Numeric. Vector of distances from \code{turtles} to locate
#'                \code{agents}. Must be of length 1 or of length \code{turtles}.
#'
#' @param angle   Numeric. Vector of angles to define the size of the cone of vision
#'                for the \code{turtles}. The cone of vision is defined between the
#'                direction of their headings minus \code{angle / 2}
#'                to the direction of their headings plus \code{angle / 2}. Must be of length 1 or
#'                of length \code{turtles}.
#'
#' @return List of length equal to \code{length(turtles)}.
#'         List items are either matrices (ncol = 2) with the first column "pxcor"
#'         and the second column "pycor" representing the coordinates of the
#'         patches among \code{agents} within the cone of vision of each of  the
#'         \code{turtles}, if \code{agents} are patches,
#'         or SpatialPointsDataFrame objects representing the turtles among \code{agents}
#'         within
#'         the cone of vision of each of the \code{turtles} if \code{agents} are turtles.
#'
#' @details \code{agents} are reported if there are within \code{radius}
#'          distance of the turtle and their direction from the turtle is within
#'          \code{[-angle, + angle]} of the turtle's heading.
#'
#'          Distances to patches are calculated to their center.
#'
#'          If \code{torus = TRUE}, the \code{radius} distances are calculated
#'          around the sides of the \code{world} to select \code{agents}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#in-cone}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#'
#' p1 <- inCone(turtles = t1, radius = 2, agents = patches(w1), angle = 90, world = w1)
#' t2 <- inCone(turtles = turtle(t1, who = 0), radius = 2, angle = 90, agents = t1, world = w1)
#'
#'
#' @export
#' @docType methods
#' @rdname inCone
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "inCone",
  function(turtles, radius, angle, agents, world, torus = FALSE) {
    standardGeneric("inCone")
  })

#' @export
#' @rdname inCone
setMethod(
  "inCone",
  signature = c(turtles = "SpatialPointsDataFrame", radius = "numeric", angle = "numeric", agents = "matrix", world = "NLworlds"),
  definition = function(turtles, radius, angle, agents, world, torus) {

    # Find the patches within distances
    agentsInRadius <- inRadius(agents = turtles, radius = radius, agents2 = agents, world = world, torus = torus)
    emptyL <- lapply(agentsInRadius, function(x){nrow(x)})
    emptyElem <- as.numeric(do.call(rbind,emptyL)[,1])
    if(sum(emptyElem) == 0){ # No patches are within radius distances for any turtles
      return(agentsInRadius)
    } else {
      agentsNoEmpty <- agentsInRadius[lapply(agentsInRadius,nrow) > 0]

      # Calculate the direction from each turtle to each one of the patches
      tList <- lapply(turtles@data$who, function(x){turtle(turtles, who = x)})
      # Remove turtles which do not have patches within radius distance
      tList <- tList[lapply(agentsInRadius,nrow) > 0]

      # Direction from the turtle to each of their patches within radius distance
      tDir <- mapply(function(x, y){
        towards(world = world, agents = x, agents2 = y, torus = torus)
      }, tList, agentsNoEmpty, SIMPLIFY = FALSE)
      # Define the rotation angle between the turtle heading and the direction to each patches
      tCone <- mapply(function(x, y){subHeadings(angle1 = x, angle2 = y, range360 = FALSE)}, tDir, tList, SIMPLIFY = FALSE)

      angle <- angle / 2
      if(length(angle) == 1){
        angle <- rep(angle, length(turtles))
      }
      angleList <- split(angle, 1:length(angle))
      # Remove the angle for the turtles which do not have patches within radius distance
      angleList <- angleList[lapply(agentsInRadius,nrow) > 0]

      # Is the rotation to face the patches smaller than the maximum rotation allowed
      tConeTRUE <- mapply(function(x, y){abs(x) < y}, tCone, angleList, SIMPLIFY = FALSE)
      pWithin <- lapply(tConeTRUE, function(x){which(x)})

      list_agents <- mapply(function(x, y){
        if(length(x) == 0){
          noPatches()
        } else {
          cbind(pxcor = y[x, 1], pycor = y[x, 2])
        }
      }, pWithin, agentsNoEmpty, SIMPLIFY = FALSE)

      # Reassign the results with the empty patches
      agentsInCone <- vector("list", length(turtles))
      agentsInCone[which(emptyElem %in% 0)] <- list(noPatches())
      agentsInCone[which(!emptyElem %in% 0)] <- list_agents

      return(agentsInCone)
    }
  }
)

#' @export
#' @rdname inCone
setMethod(
  "inCone",
  signature = c(turtles = "SpatialPointsDataFrame", radius = "numeric", angle = "numeric", agents = "SpatialPointsDataFrame", world = "NLworlds"),
  definition = function(turtles, radius, angle, agents, world, torus) {
    pCoords <- inCone(turtles = turtles, radius = radius, angle = angle, agents = agents@coords, world = world, torus = torus)
    # Merge the coordinates within the cone to the turtles data
    tWho <- lapply(pCoords, function(x){merge(x, cbind(agents@coords, agents@data), by.x = c("pxcor", "pycor"), by.y = c("xcor", "ycor"))})
    list_agents <- lapply(tWho, function(x){turtle(turtles = agents, who = x$who)})
    return(list_agents)
  }
)


################################################################################
#' Set an agents variable
#'
#' Assign values to the \code{agents} for their selected variable.
#'
#' @inheritParams fargs
#'
#' @param val Numeric or character. Vector of length 1 or length \code{count(agents)}.
#'
#' @return NLworlds object with the values \code{val} assigned to the patches in
#'         \code{agents}, or
#'
#'         SpatialPointsDataFrame representing the \code{turtles} with
#'         the values \code{tVal} assigned to the variable \code{tVar} for the \code{agents}.
#'
#' @details If \code{agents} are patches, \code{world} must be provided and \code{turtles}
#'          must not be provided. If \code{agents} are turtles, \code{turtles} must be
#'          provided and \code{world} must not be provided.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#set}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#'
#'
#' @export
#' @importFrom prodlim row.match
#' @docType methods
#' @rdname set
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "set",
  function(world, turtles, agents, var, val) {
    standardGeneric("set")
  })

#' @export
#' @rdname set
setMethod(
  "set",
  signature = c(world = "NLworld", turtles = "missing", agents = "matrix", var = "missing", val = "ANY"),
  definition = function(world, agents, val) {

    valuesW <- values(world)
    cells <- cellFromPxcorPycor(world = world, pxcor = agents[,1], pycor = agents[,2])
    valuesW[cells] <- val
    world[] <- valuesW

    return(world)
})

#' @export
#' @rdname set
setMethod(
  "set",
  signature = c(world = "NLworldStack", turtles = "missing", agents = "matrix", var = "character", val = "ANY"),
  definition = function(world, agents, var, val) {

    valuesW <- values(world)
    cells <- cellFromPxcorPycor(world = world, pxcor = agents[,1], pycor = agents[,2])
    valuesW[cells, var] <- val
    colNum <- match(var, colnames(valuesW))
    world@layers[[colNum]][] <- valuesW[,var]

    return(world)
})

#' @export
#' @rdname set
setMethod(
  "set",
  signature = c(world = "missing", turtles = "SpatialPointsDataFrame", agents = "SpatialPointsDataFrame",
                var = "character", val = "ANY"),
  definition = function(turtles, agents, var, val) {

    iAgents <- row.match(agents@data, turtles@data)

    if(var == "xcor"){
      turtles@coords[iAgents, 1] <- val
    } else if(var == "ycor"){
      turtles@coords[iAgents, 2] <- val
    } else {
      turtles@data[iAgents, var] <- val
    }

    return(turtles)
})
