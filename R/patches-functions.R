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
