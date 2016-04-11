################################################################################
#' Clear turtles
#'
#' Kill all the turtles.
#'
#' @inheritParams fargs
#'
#' @param envir   The R environment where \code{turtles} are located.
#'                Default value is \code{envir = parent.frame(n = 1)}.
#'
#' @details Removes the \code{turtles} object from the R environment. If some of
#'          the agents in \code{turtles} are present in other agentsets, it does
#'          not remove them from these other agentsets. This function just remove
#'          the given \code{turtles} agentset.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#clear-turtles}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld()
#' t1 <- createTurtles(n = 10, world = w1)
#' t1 # show the object containing the turtles
#' clearTurtles(t1)
#' t1 # 't1' not found
#'
#'
#' @export
#' @docType methods
#' @rdname clearTurtles
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "clearTurtles",
  function(turtles, envir = parent.frame(n = 1)) {
    standardGeneric("clearTurtles")
  }
)

#' @export
#' @rdname clearTurtles
setMethod(
  "clearTurtles",
  signature = c("SpatialPointsDataFrame"),
  definition = function(turtles, envir) {
    rm(list = deparse(substitute(turtles)), envir = envir)
  }
)


## Test
test_that("clearTurtles works", {
  t1 <- SpatialPointsDataFrame(coords = matrix(c(1,2), nrow = 1, ncol = 2), data = data.frame(NA))
  clearTurtles(turtles = t1)
  expect_false(exists("t1", inherits = FALSE)) # If you have the object in global it will find it, unless inherits FALSE
})


################################################################################
#' Resize a world
#'
#' Change the size of the \code{world} and clear the patches and given \code{turtles}.
#'
#' @inheritParams fargs
#'
#' @return NLworld object composed of \code{(maxPxcor - minPxcor + 1) * (maxPycor - minPycor + 1)}
#'         patches. Patch value are \code{NA}.
#'
#' @details The given \code{world} and \code{turtles} are deleted, as well as the
#'          patches values contained in the \code{world}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#resize-world}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld()
#' w1[] <- runif(length(w1))
#' t1 <- createTurtles(n = 10, world = w1)
#' w2 <- resizeWorld(world = w1, minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9, turtles = t1)
#' w2[] <- runif(length(w2))
#' plot(w1) # does not work, 'w1' does not exist anymore
#' plot(w2)
#'
#'
#' @export
#' @importFrom sp SpatialPointsDataFrame
#' @docType methods
#' @rdname resizeWorld
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "resizeWorld",
  function(world, minPxcor, maxPxcor, minPycor, maxPycor, turtles) {
    standardGeneric("resizeWorld")
  })

#' @export
#' @rdname resizeWorld
setMethod(
  "resizeWorld",
  signature = c("NLworlds", "numeric", "numeric", "numeric", "numeric", "SpatialPointsDataFrame"), # works
  definition = function(world, minPxcor, maxPxcor, minPycor, maxPycor, turtles) {
    rm(list = deparse(substitute(world)), envir = parent.frame(n = 1))
    rm(list = deparse(substitute(turtles)), envir = parent.frame(n = 1)) # I could not make clearTurles works here
    newWorld <- createNLworld(minPxcor = minPxcor, maxPxcor = maxPxcor,
                              minPycor = minPycor, maxPycor = maxPycor)
  }
)

#' @export
#' @rdname resizeWorld
setMethod(
  "resizeWorld",
  signature = c("NLworlds", "numeric", "numeric", "numeric", "numeric", "missing"), # does not work (old world still there)
  definition = function(world, minPxcor, maxPxcor, minPycor, maxPycor) {
    rm(list = deparse(substitute(world)), envir = parent.frame(n = 1)) # why this doesn't work here?
    newWorld <- createNLworld(minPxcor = minPxcor, maxPxcor = maxPxcor,
                              minPycor = minPycor, maxPycor = maxPycor)
  }
)
