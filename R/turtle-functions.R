################################################################################
#' Create turtles
#'
#' Create \code{n} new turtles with a set of defined variables.
#'
#' @param world   A \code{NLworld*} object, representing the world in which the turtles
#'                will evolve.
#'
#' @param n       Integer. The number of new turtles to create.
#'
#' @param coords  A matrix (ncol = 2, nrow = n) with the first column \code{xcor}
#'                and the second column \code{ycor} representing the coordinates
#'                for the turtles to be created. Given coordinates must be inside
#'                the world's extent. If missing, turtles are put in the center of
#'                the world.
#'
#' @param heading Numeric value(s) between 0 and 360. Either of length 1 representing
#'                the heading for all turtles or of length \code{n} if the different
#'                turtles have different headings. If missing, a random heading is
#'                assigned for each turtle.
#'
#' @param breed   String of characters. Either of length 1 representing the breed
#'                for all turtles or of length \code{n} if the different turtles have
#'                different breeds. If missing, \code{breed = "turtle"} for all turtles.
#'
#' @param color   String of characters of length \code{n} representing the color
#'                of each turtle when plotted. If missing, colors are assigned using
#'                the function \code{palette(rainbow(n))}.
#'
#' @return A SpatialPointsDataFrame object of length \code{n} with the columns for
#'         the dataframe being: "who", "heading", "prevX", "prevY", "breed", and "color".
#'
#' @details The identity of the turtles is defined by their "who" number. This
#'          numbering starts at 0 and increments by 1.
#'          The coordinates from the previous time step are stored in "prevX" and
#'          "prevY". The initial values are \code{NA}.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- runif(25)
#' t1 <- createTurtles(world = w1, n = 10, coords = cbind(xcor = runif(10, 0, 4), ycor = runif(10, 0, 4)))
#' plot(w1)
#' points(t1, pch = 16, col = t1@data$color)
#'
#'
#' @export
#' @docType methods
#' @rdname createTurtles
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "createTurtles",
  function(world, n, coords, heading, breed, color) {
    standardGeneric("createTurtles")
  })

#' @export
#' @rdname createTurtles
setMethod(
  "createTurtles",
  signature = c(world = "NLworld", n = "numeric"),
  definition = function(world, n, coords, heading, breed, color) {

    li <- lapply(names(match.call()[-1]), function(x) eval(parse(text=x)))
    names(li) <- names(match.call())[-1]

    if(missing(coords))
      li$coords <- cbind(xcor = rep((((world@extent@xmax - world@extent@xmin) / 2) + world@extent@xmin), n),
                         ycor = rep((((world@extent@ymax - world@extent@ymin) / 2) + world@extent@ymin), n))

    if(missing(breed))
      li$breed <- rep("turtle", n)

    if(length(li$breed) == 1){
      li$breed <- rep(li$breed, n)
    }

    if(missing(heading))
      li$heading <- runif(n = n, min = 0, max = 360)

    if(length(li$heading) == 1){
      li$heading <- rep(li$heading, n)
    }

    if(missing(color))
      li$color <- rainbow(n)

    turtles<-SpatialPointsDataFrame(coords = li$coords,
                                    data = data.frame(who = seq(from = 0, to = n - 1, by = 1),
                                                      heading = li$heading,
                                                      prevX = rep(NA, n),
                                                      prevY = rep(NA, n),
                                                      breed = li$breed,
                                                      color = li$color,
                                                      stringsAsFactors=FALSE))
    return(turtles)
  }
)


################################################################################
#' Create ordered turtles
#'
#' Create \code{n} new turtles at the center of the world with their headings evenly
#' distributed.
#'
#' @param world   A \code{NLworld*} object, representing the world in which the turtles
#'                will evolve.
#'
#' @param n       Integer. The number of new turtles to create.
#'
#' @param breed   String of characters. Either of length 1 representing the breed
#'                for all turtles or of length \code{n} if the different turtles have
#'                different breeds. If missing, \code{breed = "turtle"} for all turtles.
#'
#' @param color   String of characters of length \code{n} representing the color
#'                of each turtle when plotted. If missing, colors are assigned using
#'                the function \code{palette(rainbow(n))}.
#'
#' @return A SpatialPointsDataFrame object of length \code{n} with the columns for
#'         the dataframe being: "who", "heading", "prevX", "prevY", "breed", and "color".
#'
#' @details The identity of the turtles is defined by their "who" number. This
#'          numbering starts at 0 and increments by 1.
#'          The first turtle \code{who = 0} has its heading set to 0.
#'          The coordinates from the previous time step are stored in "prevX" and
#'          "prevY". The initial values are \code{NA}.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- runif(25)
#' t1 <- createOTurtles(world = w1, n = 10)
#' plot(w1)
#' points(t1, pch = 16, col = t1@data$color)
#' t1 <- fd(world = w1, turtles = t1, step = 1)
#' points(t1, pch = 16, col = t1@data$color)
#'
#'
#' @export
#' @docType methods
#' @rdname createOTurtles
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "createOTurtles",
  function(world, n, breed, color) {
    standardGeneric("createOTurtles")
  })

#' @export
#' @rdname createOTurtles
setMethod(
  "createOTurtles",
  signature = c(world = "NLworld", n = "numeric"),
  definition = function(world, n, breed, color) {

    heading <- 0
    for(i in 2:n){
      heading <- c(heading, heading[i - 1] + 360 / n)
    }

    li <- lapply(names(match.call()[-1]), function(x) eval(parse(text=x)))
    names(li) <- names(match.call())[-1]

    if(missing(breed))
      li$breed <- rep("turtle", n)

    if(length(li$breed) == 1){
      li$breed <- rep(li$breed, n)
    }

    if(missing(color))
      li$color <- rainbow(n)


    createTurtles(world = world, n = n, heading = heading, breed = li$breed, color = li$color)
  }
)


################################################################################
#' Forward
#'
#' Move the turtles forward of \code{step} distance(s).
#'
#' @param world   A \code{NLworld*} object, representing the world in which the
#'                turtles move onto.
#'
#' @param turtles A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the agents that will move
#'                forward.
#'
#' @param step    Numeric. Distance(s) by which the turtles will move forward. Must
#'                be of length 1 if all turtles move the same distance or of length
#'                turtles if each turtle moves a different distance.
#'
#' @param torus   Logical to determine if the \code{NLworld*} is wrapped.
#'                Default is \code{torus = FALSE}.
#'
#' @return A SpatialPointsDataFrame with updated coordinates.
#'
#' @details If the \code{NLworld*} is wrapped (\code{torus = TRUE}) and the distance
#'          to move lead the turtle outside of the world's extent, it is
#'          relocated on the other of the world, inside the world's extent. Otherwise,
#'          if \code{torus = FALSE}, the turtle moves past the world's extent.
#'          If a given \code{step} value is negative, then the turtle moves
#'          backward.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- runif(25)
#' t1 <- createOTurtles(world = w1, n = 10)
#' plot(w1)
#' points(t1, pch = 16, col = t1@data$color)
#' t1 <- fd(world = w1, turtles = t1, step = 1)
#' points(t1, pch = 16, col = t1@data$color)
#'
#'
#' @export
#' @importFrom CircStats rad
#' @importFrom SpaDES wrap
#' @docType methods
#' @rdname fd
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "fd",
  function(world, turtles, step, torus = FALSE) {
    standardGeneric("fd")
  })

#' @export
#' @rdname fd
setMethod(
  "fd",
  signature = c(world = "NLworld", turtles = "SpatialPointsDataFrame", step = "numeric"),
  definition = function(world, turtles, step, torus) {

    turtles@data$prevX <- turtles@coords[,1]
    turtles@data$prevY <- turtles@coords[,2]

    fdXcor <- turtles@coords[,1] + cos(rad(turtles@data$heading)) * step
    fdYcor <- turtles@coords[,2] + sin(rad(turtles@data$heading)) * step
    if(torus == TRUE){
      tCoords <- wrap(cbind(x = fdXcor, y = fdYcor), extent(world))
      fdXcor <- tCoords[,1]
      fdYcor <- tCoords[,2]
    }

    turtles <- SpatialPointsDataFrame(coords = cbind(xcor = fdXcor, ycor = fdYcor), data = turtles@data)
    return(turtles)
  }
)


