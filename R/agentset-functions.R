################################################################################
#' All
#'
#' Reports \code{TRUE} if all the agents have their variable equal to a given value,
#' or \code{FALSE} otherwise.
#'
#'!!! Only implemented for patches so far !!!
#'
#' @param world  A \code{NLworld*} object.
#'
#' @param agents A matrix (ncol = 2) with the first column \code{pxcor} and the
#'               second column \code{pycor} representing the coordinates of the
#'               patches to be evaluated.
#'
#' @param pVar   If the world is a \code{NLworldStack}, pVar is the name (characters)
#'               of the layer used for evaluating the conditional value.
#'
#' @param val    Numeric or character depending on the variable class.
#'
#' @return Logical. \code{TRUE} if all the agents have their variable equal to \code{val},
#'         return \code{FALSE} otherwise.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = 25, replace = TRUE)
#' NLall(world = w1, agents = patches(world = w1), val = 5)
#' w2 <- w1
#' w2[] <- 5
#' NLall(world = w2, agents = patches(world = w2), val = 5)
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
  function(world, agents, pVar, val) {
    standardGeneric("NLall")
  })

#' @export
#' @rdname NLall
setMethod(
  "NLall",
  signature = c("NLworld", "matrix", "missing", "numeric"),
  definition = function(world, agents, val) {
    values <- values(world)
    pxcorW <- world@pxcor
    pycorW <- world@pycor
    agentsVal <- values[pxcorW == agents[,1] & pycorW == agents[,2]]
    compare <- agentsVal == val
    allTrue <- ifelse(length(compare[compare == TRUE]) == length(compare), TRUE, FALSE)
    return(allTrue)
  }
)

#' @export
#' @rdname NLall
setMethod(
  "NLall",
  signature = c("NLworldStack", "matrix", "character", "numeric"),
  definition = function(world, agents, pVar, val) {
    names_l <- names(world)
    l <- match(pVar, names_l)
    world_l <- world[[l]]
    NLall(world = world_l, agents = agents, val = val)
  }
)


################################################################################
#' Any
#'
#' Reports \code{TRUE} if the given agentset \code{agents} is non empty, or \code{FALSE}
#' otherwise.
#'
#'!!! Only implemented for patches so far !!!
#'
#' @param agents A matrix (ncol = 2) with the first column \code{pxcor} and the
#'               second column \code{pycor} representing the coordinates of the
#'               patches to be evaluated.
#'
#' @return Logical. \code{TRUE} if there is at least one patch coordinates in the
#'         \code{agents}, return \code{FALSE} otherwise.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' p1 <- noPatches()
#' p2 <- patch(world = w1, xcor = 0, ycor = 0)
#' NLany(p1)
#' NLany(p2)
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


################################################################################
#' Count
#'
#' Reports the number of agents.
#'
#'!!! Only implemented for patches so far !!!
#'
#' @param agents A matrix (ncol = 2) with the first column \code{pxcor} and the
#'               second column \code{pycor} representing the coordinates of the
#'               patches to be counted
#'
#' @return Integer.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4) # 25 patches
#' p1 <- patches(world = w1)
#' count(p1) # 25
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


################################################################################
#' Sort on
#'
#' Reports the coordinates \code{pxcor} and \code{pycor} of the patches sorted
#' according to their value.
#'
#'!!! Only implemented for patches so far !!!
#'
#' @param world  A \code{NLworld*} object.
#'
#' @param agents A matrix (ncol = 2) with the first column \code{pxcor} and the
#'               second column \code{pycor} representing the coordinates of the
#'               patches to be sorted.
#'
#' @param pVar   If the world is a \code{NLworldStack}, pVar is the name (characters)
#'               of the layer used for sorting the patches.
#'
#' @return A matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the coordinates of the sorted patches.
#'
#' @details The sorting of the agents based on their value is done in a increasing order.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = 25, replace = TRUE)
#' plot(w1)
#' p1 <- sortOn(world = w1, agents = patches(world = w1))
#'
#' @export
#' @docType methods
#' @rdname sortOn
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "sortOn",
  function(world, agents, pVar) {
    standardGeneric("sortOn")
  })

#' @export
#' @rdname sortOn
setMethod(
  "sortOn",
  signature = c("NLworld", "matrix", "missing"),
  definition = function(world, agents) {
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
  signature = c("NLworldStack", "matrix", "character"),
  definition = function(world, agents, pVar) {
    names_l <- names(world)
    l <- match(pVar, names_l)
    world_l <- world[[l]]
    sortOn(world = world_l, agents = agents)
  }
)

################################################################################
#' NLwith
#'
#' Reports the coordinates \code{pxcor} and \code{pycor} of the patches with their
#' variable equals to a specific value.
#'
#'!!! Only implemented for patches so far !!!
#'
#' @param world  A \code{NLworld*} object.
#'
#' @param agents A matrix (ncol = 2) with the first column \code{pxcor} and the
#'               second column \code{pycor} representing the coordinates of the
#'               patches to be evaluated.
#'
#' @param pVar   If the world is a \code{NLworldStack}, pVar is the name (characters)
#'               of the layer used for evaluating the conditional value.
#'
#' @param val    Numeric or character depending on the variable class.
#'
#' @return A matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the coordinates of the patches with their value
#'         equal to \code{val}.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = 25, replace = TRUE)
#' p2 <- NLwith(world = w1, agents = patches(world = w1), val = 2)
#' plot(w1)
#'
#' @export
#' @docType methods
#' @rdname NLwith
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "NLwith",
  function(world, agents, pVar, val) {
    standardGeneric("NLwith")
  })

#' @export
#' @rdname NLwith
setMethod(
  "NLwith",
  signature = c("NLworld", "matrix", "missing", "numeric"),
  definition = function(world, agents, val) {
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
  signature = c("NLworldStack", "matrix", "character", "numeric"),
  definition = function(world, agents, pVar, val) {
    names_l <- names(world)
    l <- match(pVar, names_l)
    world_l <- world[[l]]
    NLwith(world = world_l, agents = agents, val = val)
  }
)


################################################################################
#' With maximum
#'
#' Reports the coordinates \code{pxcor} and \code{pycor} of the patches which
#' have their variable equals to the maximum value.
#'
#'!!! Only implemented for patches so far !!!
#'
#' @param world  A \code{NLworld*} object.
#'
#' @param agents A matrix (ncol = 2) with the first column \code{pxcor} and the
#'               second column \code{pycor} representing the coordinates for the
#'               patches to be evaluated.
#'
#' @param pVar   If the world is a \code{NLworldStack}, pVar is the name (characters)
#'               of the layer used for evaluating the maximum patch value.
#'
#' @return A matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the coordinates for the patches with the maximum
#'         value among the agents.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = 25, replace = TRUE)
#' pMax <- withMax(world = w1, agents = patches(world = w1))
#' plot(w1)
#'
#' @export
#' @docType methods
#' @rdname withMax
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "withMax",
  function(world, agents, pVar) {
    standardGeneric("withMax")
  })

#' @export
#' @rdname withMax
setMethod(
  "withMax",
  signature = c("NLworld", "matrix", "missing"),
  definition = function(world, agents) {
    pxcor <- agents[,1]
    pycor <- agents[,2]
    val <- world[pxcor,pycor]
    maxVal <- max(val, na.rm = TRUE)
    pMax <- which(val %in% maxVal)
    pxcorMax <- pxcor[pMax]
    pycorMax <- pycor[pMax]
    return(cbind(pxcor = pxcorMax, pycor = pycorMax))
  }
)

#' @export
#' @rdname withMax
setMethod(
  "withMax",
  signature = c("NLworldStack", "matrix", "character"),
  definition = function(world, agents, pVar) {
    names_l <- names(world)
    l <- match(pVar, names_l)
    world_l <- world[[l]]
    withMax(world = world_l, agents = agents)
  }
)


################################################################################
#' With minimum
#'
#' Reports the coordinates \code{pxcor} and \code{pycor} of the patches which
#' have their variable equals to the minimum value.
#'
#'!!! Only implemented for patches so far !!!
#'
#' @param world  A \code{NLworld*} object.
#'
#' @param agents A matrix (ncol = 2) with the first column \code{pxcor} and the
#'               second column \code{pycor} representing the coordinates for the
#'               patches to be evaluated.
#'
#' @param pVar   If the world is a \code{NLworldStack}, pVar is the name (characters)
#'               of the layer used for evaluating the minimum patch value.
#'
#' @return A matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the coordinates for the patches with the minimum
#'         value among the agents.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = 25, replace = TRUE)
#' pMin <- withMin(world = w1, agents = patches(world = w1))
#' plot(w1)
#'
#' @export
#' @docType methods
#' @rdname withMin
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "withMin",
  function(world, agents, pVar) {
    standardGeneric("withMin")
  })

#' @export
#' @rdname withMin
setMethod(
  "withMin",
  signature = c("NLworld", "matrix", "missing"),
  definition = function(world, agents) {
    pxcor <- agents[,1]
    pycor <- agents[,2]
    val <- world[pxcor,pycor]
    minVal <- min(val, na.rm = TRUE)
    pMin <- which(val %in% minVal)
    pxcorMin <- pxcor[pMin]
    pycorMin <- pycor[pMin]
    return(cbind(pxcor = pxcorMin, pycor = pycorMin))
  }
)

#' @export
#' @rdname withMin
setMethod(
  "withMin",
  signature = c("NLworldStack", "matrix", "character"),
  definition = function(world, agents, pVar) {
    names_l <- names(world)
    l <- match(pVar, names_l)
    world_l <- world[[l]]
    withMin(world = world_l, agents = agents)
  }
)


################################################################################
#' One with maximum
#'
#' Reports one patch coordinates \code{pxcor} and \code{pycor} which has its
#' variable equals to the maximum value.
#'
#'!!! Only implemented for patches so far !!!
#'
#' @param world  A \code{NLworld*} object.
#'
#' @param agents A matrix (ncol = 2) with the first column \code{pxcor} and the
#'               second column \code{pycor} representing the coordinates for the
#'               patches to be evaluated.
#'
#' @param pVar   If the world is a \code{NLworldStack}, pVar is the name (characters)
#'               of the layer used for evaluating the maximum patch value.
#'
#' @return A matrix (ncol = 2, nrow = 1) with the first column \code{pxcor} and
#'         the second column \code{pycor} representing the coordinates for the patch
#'         (or one of the patches) with the maximum value among the agents.
#'
#' @details If there are several agents with the maximum value, one is chosen randomly.
#'          To access to all agents with their variable equal to the maximum value,
#'          use \code{withMax()}.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = 25, replace = TRUE)
#' pMax <- maxOneOf(world = w1, agents = patches(world = w1))
#' plot(w1)
#'
#' @export
#' @docType methods
#' @rdname maxOneOf
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "maxOneOf",
  function(world, agents, pVar) {
    standardGeneric("maxOneOf")
  })

#' @export
#' @rdname maxOneOf
setMethod(
  "maxOneOf",
  signature = c("NLworld", "matrix", "missing"),
  definition = function(world, agents) {
    maxAgents <- withMax(world = world, agents = agents)
    row <- sample(1:nrow(maxAgents), size = 1)
    return(maxAgents[row,])
  }
)

#' @export
#' @rdname maxOneOf
setMethod(
  "maxOneOf",
  signature = c("NLworldStack", "matrix", "character"),
  definition = function(world, agents, pVar) {
    maxAgents <- withMax(world = world, agents = agents, pVar = pVar)
    row <- sample(1:nrow(maxAgents), size = 1)
    return(maxAgents[row,])
  }
)


################################################################################
#' One with minimum
#'
#' Reports one patch coordinates \code{pxcor} and \code{pycor} which has its
#' variable equals to the minimum value.
#'
#'!!! Only implemented for patches so far !!!
#'
#' @param world  A \code{NLworld*} object.
#'
#' @param agents A matrix (ncol = 2) with the first column \code{pxcor} and the
#'               second column \code{pycor} representing the coordinates for the
#'               patches to be evaluated.
#'
#' @param pVar   If the world is a \code{NLworldStack}, pVar is the name (characters)
#'               of the layer used for evaluating the minimum patch value.
#'
#' @return A matrix (ncol = 2, nrow = 1) with the first column \code{pxcor} and
#'         the second column \code{pycor} representing the coordinates for the patch
#'         (or one of the patches) with the minimum value among the agents.
#'
#' @details If there are several agents with the minimum value, one is chosen randomly.
#'          To access to all agents with their variable equal to the minimum value,
#'          use \code{withMin()}.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- sample(1:5, size = 25, replace = TRUE)
#' pMin <- minOneOf(world = w1, agents = patches(world = w1))
#' plot(w1)
#'
#' @export
#' @docType methods
#' @rdname minOneOf
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "minOneOf",
  function(world, agents, pVar) {
    standardGeneric("minOneOf")
  })

#' @export
#' @rdname minOneOf
setMethod(
  "minOneOf",
  signature = c("NLworld", "matrix", "missing"),
  definition = function(world, agents) {
    minAgents <- withMin(world = world, agents = agents)
    row <- sample(1:nrow(minAgents), size = 1)
    return(minAgents[row,])
  }
)

#' @export
#' @rdname minOneOf
setMethod(
  "minOneOf",
  signature = c("NLworldStack", "matrix", "character"),
  definition = function(world, agents, pVar) {
    minAgents <- withMin(world = world, agents = agents, pVar = pVar)
    row <- sample(1:nrow(minAgents), size = 1)
    return(minAgents[row,])
  }
)
