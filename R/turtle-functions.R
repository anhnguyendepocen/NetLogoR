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
  signature = c(world = "NLworlds", n = "numeric"),
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
  signature = c(world = "NLworlds", n = "numeric"),
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
#' Move the turtles forward of \code{step} distance(s) in the direction of the
#' turtles' heading.
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
  signature = c(world = "NLworlds", turtles = "SpatialPointsDataFrame", step = "numeric"),
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


################################################################################
#' Backward
#'
#' Move the turtles backward of \code{step} distance(s) regarding the direction
#' of the turtles' heading.
#'
#' @param world   A \code{NLworld*} object, representing the world in which the
#'                turtles move onto.
#'
#' @param turtles A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the agents that will move
#'                backward.
#'
#' @param step    Numeric. Distance(s) by which the turtles will move backward.
#'                Must be of length 1 if all turtles move the same distance or
#'                of length turtles if each turtle moves a different distance.
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
#'          forward.
#'          This function is equivalent to the forward function with the distance(s)
#'          to move equal to \code{step * -1}.
#'          The turtles' heading is not affected by the function (i.e., the turtles
#'          do not head backward).
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
#' t1 <- fd(world = w1, turtles = t1, step = 2)
#' points(t1, pch = 16, col = t1@data$color)
#' t1 <- bk(world = w1, turtles = t1, step = 1)
#' points(t1, pch = 16, col = t1@data$color)
#' t1 <- fd(world = w1, turtles = t1, step = 0.5)
#' points(t1, pch = 16, col = t1@data$color)
#'
#'
#' @export
#' @docType methods
#' @rdname fd
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "bk",
  function(world, turtles, step, torus = FALSE) {
    standardGeneric("bk")
  })

#' @export
#' @rdname bk
setMethod(
  "bk",
  signature = c(world = "NLworlds", turtles = "SpatialPointsDataFrame", step = "numeric"),
  definition = function(world, turtles, step, torus) {

    fd(world = world, turtles = turtles, step = (step * -1), torus = torus)

  }
)


################################################################################
#' Home
#'
#' Move the turtles back "home".
#'
#' @param world   A \code{NLworld*} object, representing the world in which the
#'                turtles move onto.
#'
#' @param turtles A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the agents to move.
#'
#' @param home    Character. Can take one of the following options to define where
#'                is home for the turtles.
#'                \code{home = "home0"} will place the turtles at the location
#'                \code{xcor = 0, ycor = 0}.
#'                \code{home = "center"} will place the turtles at the center of
#'                the world.
#'                \code{home = "pCorner"} will place the turtles at the center of
#'                the patch located in the left bottom corner of the world.
#'                \code{home = "corner"} will place the turtles at the left bottom
#'                corner of the world.
#'
#' @return A SpatialPointsDataFrame with updated coordinates.
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
#' t1 <- home(world = w1, turtles = t1, home = "pCorner")
#' points(t1, pch = 16, col = t1@data$color)
#'
#'
#' @export
#' @docType methods
#' @rdname home
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "home",
  function(world, turtles, home) {
    standardGeneric("home")
  })

#' @export
#' @rdname home
setMethod(
  "home",
  signature = c("NLworlds", "SpatialPointsDataFrame", "character"),
  definition = function(world, turtles, home) {

    if(home == "home0"){
      if(world@extent@xmin <= 0 & world@extent@xmax >= 0 & world@extent@ymin <= 0 & world@extent@ymax >= 0){
        turtles@coords <- cbind(xcor = rep(0, length(turtles)), ycor = rep(0, length(turtles)))
      } else {
        stop("The world provided does not contain the location [xcor = 0, ycor = 0]")
      }
    }

    if(home == "center"){
      turtles@coords <- cbind(xcor = rep((((world@extent@xmax - world@extent@xmin) / 2) + world@extent@xmin), length(turtles)),
                              ycor = rep((((world@extent@ymax - world@extent@ymin) / 2) + world@extent@ymin), length(turtles)))
    }

    if(home == "pCorner"){
      turtles@coords <- cbind(xcor = rep(minPxcor(world = world), length(turtles)),
                              ycor = rep(minPycor(world = world), length(turtles)))
    }

    if(home == "corner"){
      turtles@coords <- cbind(xcor = rep(world@extent@xmin, length(turtles)), ycor = rep(world@extent@ymin, length(turtles)))
    }

    return(turtles)
  }
)

