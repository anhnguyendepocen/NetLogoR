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
