################################################################################
#' Create turtles
#'
#' Create \code{n} new turtles with a set of defined variables.
#'
#' @param n       Integer. The number of new turtles to create.
#'
#' @param coords  A matrix (ncol = 2, nrow = n) with the first column \code{xcor}
#'                and the second column \code{ycor} representing the coordinates
#'                for the turtles to be created. \code{nrow(coords)} must be either
#'                equal to 1 if all turtles have the same initial position or equal
#'                to \code{n} if different turtles have different initial position.
#'                Given coordinates must be inside the world's extent. If missing,
#'                turtles are put in the center of the \code{world}.
#'
#' @param world   A \code{NLworlds} object, representing the world in which the turtles
#'                will evolve. If \code{coords} are provided, \code{world} should
#'                not be provided.
#'
#' @param heading Numeric value(s) between 0 and 360. Either of length 1 representing
#'                the heading for all turtles or of length \code{n} if different
#'                turtles have different headings. If missing, a random heading is
#'                assigned to each turtle.
#'
#' @param breed   String of characters. Either of length 1 representing the breed
#'                for all turtles or of length \code{n} if the different turtles have
#'                different breeds. If missing, \code{breed = "turtle"} for all turtles.
#'
#' @param color   String of characters of length \code{n} representing the color
#'                of each turtle when plotted. If missing, colors are assigned using
#'                the function \code{rainbow(n)}.
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
#' t1 <- createTurtles(n = 10, coords = cbind(xcor = runif(10, 0, 4), ycor = runif(10, 0, 4)))
#'
#' library(SpaDES)
#' clearPlot()
#' Plot(w1)
#' Plot(t1, addTo ="w1") # automatically uses color column in SpatialPointsDataFrame
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
  function(n, coords, world, heading, breed, color) {
    standardGeneric("createTurtles")
  })

#' @export
#' @rdname createTurtles
setMethod(
  "createTurtles",
  signature = c("numeric", "matrix", "missing", "ANY", "ANY", "ANY"),
  definition = function(n, coords, world, heading, breed, color) {

    li <- lapply(names(match.call()[-1]), function(x) eval(parse(text=x)))
    names(li) <- names(match.call())[-1]

    if(nrow(li$coords) == 1){
      li$coords <- cbind(xcor = as.numeric(rep(li$coords[,1], n)), ycor = as.numeric(rep(li$coords[,2], n)))
    }

    if(missing(heading))
      li$heading <- runif(n = n, min = 0, max = 360)

    if(length(li$heading) == 1){
      li$heading <- rep(li$heading, n)
    }

    if(missing(breed))
      li$breed <- rep("turtle", n)

    if(length(li$breed) == 1){
      li$breed <- rep(li$breed, n)
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

#' @export
#' @rdname createTurtles
setMethod(
  "createTurtles",
  signature = c("numeric", "missing", "NLworlds", "ANY", "ANY", "ANY"),
  definition = function(n, coords, world, heading, breed, color) {

    li <- lapply(names(match.call()[-1]), function(x) eval(parse(text=x)))
    names(li) <- names(match.call())[-1]

    if(missing(heading))
      li$heading <- runif(n = n, min = 0, max = 360)

    if(length(li$heading) == 1){
      li$heading <- rep(li$heading, n)
    }

    if(missing(breed))
      li$breed <- rep("turtle", n)

    if(length(li$breed) == 1){
      li$breed <- rep(li$breed, n)
    }

    if(missing(color))
      li$color <- rainbow(n)

    coords <- cbind(xcor = rep((((world@extent@xmax - world@extent@xmin) / 2) + world@extent@xmin), n),
                    ycor = rep((((world@extent@ymax - world@extent@ymin) / 2) + world@extent@ymin), n))

    createTurtles(n = n, coords = coords, heading = li$heading, breed = li$breed, color = li$color)
  }
)


################################################################################
#' Create ordered turtles
#'
#' Create \code{n} new turtles at the center of the world with their headings evenly
#' distributed.
#'
#' @param n       Integer. The number of new turtles to create.
#'
#' @param world   A \code{NLworlds} object, representing the world in which the turtles
#'                will evolve.
#'
#' @param breed   String of characters. Either of length 1 representing the breed
#'                for all turtles or of length \code{n} if the different turtles have
#'                different breeds. If missing, \code{breed = "turtle"} for all turtles.
#'
#' @param color   String of characters of length \code{n} representing the color
#'                of each turtle when plotted. If missing, colors are assigned using
#'                the function \code{rainbow(n)}.
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
#' t1 <- createOTurtles(n = 10, world = w1)
#' plot(w1)
#' points(t1, pch = 16, col = t1@data$color)
#' t1 <- fd(world = w1, turtles = t1, step = 1)
#' points(t1, pch = 16, col = t1@data$color)
#'
#' \dontrun{
#' # Can be used with Plot in package SpaDES for modular plotting that is faster with large datasets
#'   library(SpaDES)
#'   clearPlot()
#'   Plot(w1)
#'   Plot(t1, addTo ="w1") # automatically uses color column in SpatialPointsDataFrame
#' }
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
  function(n, world, breed, color) {
    standardGeneric("createOTurtles")
  })

#' @export
#' @rdname createOTurtles
setMethod(
  "createOTurtles",
  signature = c(n = "numeric", world = "NLworlds"),
  definition = function(n, world, breed, color) {

    heading <- numeric(n)
    heading[1] <- 0
    if(n > 1) {
      heading[2:n] <- heading[1:(n-1)] + (360 / n) * (1:(n - 1))
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

    createTurtles(n = n, world = world, heading = heading, breed = li$breed, color = li$color)
  }
)


################################################################################
#' Forward
#'
#' Move the turtles forward of \code{step} distance(s) with the turtles' heading
#' direction.
#'
#' @param world   A \code{NLworlds} object, representing the world which the
#'                turtles move onto.
#'
#' @param turtles A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the moving turtles.
#'
#' @param step    Numeric. Distance(s) by which the turtles will move forward. Must
#'                be of length 1 if all turtles move the same distance or of length
#'                \code{turtles} if each turtle moves a different distance.
#'
#' @param torus   Logical to determine if the \code{NLworlds} object is wrapped.
#'                Default is \code{torus = FALSE}.
#'
#' @param out     Logical. Determine if turtles should move at all when
#'                \code{torus = FALSE} and their ending position will be outside of
#'                the world's extent. Default is \code{out = TRUE}.
#'
#' @return A SpatialPointsDataFrame representing the moving turtles with updated
#'         coordinates and updated data for their previous coordinates (i.e.,
#'         \code{turtles@data$prevX} and \code{turtles@data$prevY}).
#'
#' @details If the \code{NLworlds} object is wrapped (\code{torus = TRUE}) and the
#'          distance to move lead a turtle outside of the world's extent, it is
#'          relocated on the other side of the world, inside the world's extent. If
#'          \code{torus = FALSE} and \code{out = TRUE}, the turtle moves past the
#'          world's extent. If \code{torus = FALSE} and \code{out = FALSE}, the
#'          turtle does not move at all. In the event that a turtle does not move,
#'          its previous coordinates are still updated with its position before
#'          running \code{fd()} (i.e., its current position).
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
#'
#' library(SpaDES)
#' clearPlot()
#' Plot(w1)
#' Plot(t1, addTo ="w1")
#'
#' t1 <- fd(world = w1, turtles = t1, step = 1)
#' Plot(t1, addTo ="w1")
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
  function(world, turtles, step, torus = FALSE, out = TRUE) {
    standardGeneric("fd")
  })

#' @export
#' @rdname fd
setMethod(
  "fd",
  signature = c(world = "NLworlds", turtles = "SpatialPointsDataFrame", step = "numeric"),
  definition = function(world, turtles, step, torus, out) {

    turtles@data$prevX <- turtles@coords[,1]
    turtles@data$prevY <- turtles@coords[,2]

    fdXcor <- round(turtles@coords[,1] + sin(rad(turtles@data$heading)) * step, digits = 5)
    fdYcor <- round(turtles@coords[,2] + cos(rad(turtles@data$heading)) * step, digits = 5)

    if(torus == TRUE){
      tCoords <- wrap(cbind(x = fdXcor, y = fdYcor), extent(world))
      fdXcor <- round(tCoords[,1], digits = 5)
      fdYcor <- round(tCoords[,2], digits = 5)
    }

    if(torus == FALSE & out == FALSE){
      outX <- fdXcor < world@extent@xmin | fdXcor > world@extent@xmax
      outY <- fdYcor < world@extent@ymin | fdYcor > world@extent@ymax
      outXY <- which(outX | outY) # position of turtles out of the world's extent
      fdXcor[outXY] <- turtles@coords[,1][outXY]
      fdYcor[outXY] <- turtles@coords[,2][outXY]
    }

    turtles <- SpatialPointsDataFrame(coords = cbind(xcor = fdXcor, ycor = fdYcor), data = turtles@data)
    return(turtles)
  }
)


################################################################################
#' Backward
#'
#' Move the turtles backward of \code{step} distance(s) with the turtles' heading
#' direction.
#'
#' @param world   A \code{NLworlds} object, representing the world which the
#'                turtles move onto.
#'
#' @param turtles A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the moving turtles.
#'
#' @param step    Numeric. Distance(s) by which the turtles will move backward.
#'                Must be of length 1 if all turtles move the same distance or
#'                of length \code{turtles} if each turtle moves a different distance.
#'
#' @param torus   Logical to determine if the \code{NLworlds} object is wrapped.
#'                Default is \code{torus = FALSE}.
#'
#' @param out     Logical. Determine if turtles should move at all when
#'                \code{torus = FALSE} and their ending position will be outside of
#'                the world's extent. Default is \code{out = TRUE}.
#'
#' @return A SpatialPointsDataFrame representing the moving turtles with updated
#'         coordinates and updated data for their previous coordinates (i.e.,
#'         \code{turtles@data$prevX} and \code{turtles@data$prevY}).
#'
#' @details If the \code{NLworlds} object is wrapped (\code{torus = TRUE}) and the
#'          distance to move lead a turtle outside of the world's extent, it is
#'          relocated on the other side of the world, inside the world's extent. If
#'          \code{torus = FALSE} and \code{out = TRUE}, the turtle moves past the
#'          world's extent. If \code{torus = FALSE} and \code{out = FALSE}, the
#'          turtle does not move at all. In the event that a turtle does not move,
#'          its previous coordinates are still updated with its position before
#'          running \code{bk()} (i.e., its current position).
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
#'
#' library(SpaDES)
#' clearPlot()
#' Plot(w1)
#' Plot(t1, addTo ="w1")
#'
#' t1 <- fd(world = w1, turtles = t1, step = 2)
#' Plot(t1, addTo ="w1")
#' t1 <- bk(world = w1, turtles = t1, step = 1)
#' Plot(t1, addTo ="w1")
#' t1 <- fd(world = w1, turtles = t1, step = 0.5)
#' Plot(t1, addTo ="w1")
#'
#'
#' @export
#' @docType methods
#' @rdname bk
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "bk",
  function(world, turtles, step, torus = FALSE, out = TRUE) {
    standardGeneric("bk")
  })

#' @export
#' @rdname bk
setMethod(
  "bk",
  signature = c(world = "NLworlds", turtles = "SpatialPointsDataFrame", step = "numeric"),
  definition = function(world, turtles, step, torus, out) {

    fd(world = world, turtles = turtles, step = -step, torus = torus, out = out)

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
#' t1 <- createTurtles(n = 10, coords = cbind(xcor = runif(10, 0, 4), ycor = runif(10, 0, 4)))
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


################################################################################
#' dx
#'
#' Reports the x-increment (i.e., the amount by which the turtles' xcor would change)
#' if the turtles were to move forward of \code{step} distance(s) with their current
#' heading.
#'
#' @param turtles A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the agents that were to
#'                move forward.
#'
#' @param step    Numeric. The distance(s) the turtles would have to move forward to
#'                compute the x-increment value. Must be of length 1 if all the
#'                turtles would have to move the same distance or of the same length
#'                as the turtles object. The default value is \code{step = 1}.
#'
#' @return A vector of the length of the turtles object.
#'
#' @details Reports the sine of the turtles' heading multiplied by the \code{step}
#'          value(s). Heading 0 is north and angles are calculated in degrees in a
#'          clockwise manner.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createOTurtles(world = w1, n = 10)
#' dx(turtles = t1)
#'
#'
#' @export
#' @importFrom CircStats rad
#' @docType methods
#' @rdname dx
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "dx",
  function(turtles, step = 1) {
    standardGeneric("dx")
  })

#' @export
#' @rdname dx
setMethod(
  "dx",
  signature = c("SpatialPointsDataFrame", "numeric"),
  definition = function(turtles, step) {
    xIncr <- round(sin(rad(turtles@data$heading)) * step, digits = 5)
    return(xIncr)
  }
)

#' @export
#' @rdname dx
setMethod(
  "dx",
  signature = c("SpatialPointsDataFrame", "missing"),
  definition = function(turtles) {
    dx(turtles = turtles, step = 1)
  }
)


################################################################################
#' dy
#'
#' Reports the y-increment (i.e., the amount by which the turtles' ycor would change)
#' if the turtles were to move forward of \code{step} distance(s) with their current
#' heading.
#'
#' @param turtles A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the agents that were to
#'                move forward.
#'
#' @param step    Numeric. The distance(s) the turtles would have to move forward to
#'                compute the y-increment value. Must be of length 1 if all the
#'                turtles would have to move the same distance or of the same length
#'                as the turtles object. The default value is \code{step = 1}.
#'
#' @return A vector of the length of the turtles object.
#'
#' @details Reports the cosine of the turtles' heading multiplied by the \code{step}
#'          value(s). Heading 0 is north and angles are calculated in degrees in a
#'          clockwise manner.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createOTurtles(world = w1, n = 10)
#' dy(turtles = t1)
#'
#'
#' @export
#' @importFrom CircStats rad
#' @docType methods
#' @rdname dy
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "dy",
  function(turtles, step = 1) {
    standardGeneric("dy")
  })

#' @export
#' @rdname dy
setMethod(
  "dy",
  signature = c("SpatialPointsDataFrame", "numeric"),
  definition = function(turtles, step) {
    yIncr <- round(cos(rad(turtles@data$heading)) * step, digits = 5)
    return(yIncr)
  }
)

#' @export
#' @rdname dy
setMethod(
  "dy",
  signature = c("SpatialPointsDataFrame", "missing"),
  definition = function(turtles) {
    dy(turtles = turtles, step = 1)
  }
)


################################################################################
#' Die
#'
#' Kill the turtle(s).
#'
#' @param turtles A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()}.
#'
#' @param who     Numeric. The who number(s) of the turtle(s) that die(s).
#'
#' @return A SpatialPointsDataFrame of length equal to \code{length(turtles) - length(who)}.
#'
#' @details The who numbers of the other turtles are maintained.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createTurtles(n = 10, world = w1)
#' length(t1)
#' t1 <- die(turtles = t1, who = c(2, 3, 4))
#' length(t1)
#'
#'
#' @export
#' @docType methods
#' @rdname die
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "die",
  function(turtles, who) {
    standardGeneric("die")
  })

#' @export
#' @rdname die
setMethod(
  "die",
  signature = c("SpatialPointsDataFrame", "numeric"),
  definition = function(turtles, who) {
    iTurtles <- match(who, turtles@data$who)
    newCoords <- cbind(xcor = turtles@coords[-iTurtles,1], ycor = turtles@coords[-iTurtles,2])
    newData <- turtles@data[-iTurtles,]
    newTurtles <- SpatialPointsDataFrame(coords = newCoords, data = newData)
    return(newTurtles)
  }
)


################################################################################
#' Hatch
#'
#' Create new turtle(s) from a parent turtle.
#'
#' @param turtles A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} containing the parent turtle.
#'
#' @param who     Numeric. The who number of the parent turtle.
#'
#' @param n       Numeric. The number of new turtle(s) to create.
#'
#' @param breed   Character. The breed for the turtle(s) to be created. If missing,
#'                the created turtle(s) is/are of the same breed as the parent turtle.
#'
#' @return A SpatialPointsDataFrame of length equal to \code{length(turtles) + n}.
#'
#' @details The created turtle(s) inherit(s) of all the data from the parent turtle,
#'          except for the breed if specified otherwise, and for the who number.
#'          The who numbers of the turtles created take on following the highest
#'          who number among the turtles.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createTurtles(n = 10, world = w1)
#' length(t1)
#' t1 <- hatch(turtles = t1, who = 0, n = 2)
#' length(t1)
#'
#'
#' @export
#' @docType methods
#' @rdname hatch
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "hatch",
  function(turtles, who, n, breed) {
    standardGeneric("hatch")
  })

#' @export
#' @rdname hatch
setMethod(
  "hatch",
  signature = c("SpatialPointsDataFrame", "numeric", "numeric", "character"),
  definition = function(turtles, who, n, breed) {

    iTurtle <- match(who, turtles@data$who)
    parentCoords <- turtles@coords[iTurtle,]
    parentData <- turtles@data[iTurtle,]
    newCoords <- rbind(turtles@coords, cbind(xcor = rep(as.numeric(parentCoords[1]), n), ycor = rep(as.numeric(parentCoords[2]), n)))
    newData <- rbind(turtles@data, parentData[rep(seq_len(nrow(parentData)), each = n),])
    rownames(newData) <- seq_len(nrow(newData))

    # Update the who numbers and breed
    newData[(nrow(turtles) + 1):nrow(newData), "who"] <- (max(turtles@data$who) + 1):(max(turtles@data$who) + n)
    newData[(nrow(turtles) + 1):nrow(newData), "breed"] <- rep(breed, n)

    newTurtles <- SpatialPointsDataFrame(coords = newCoords, data = newData)
    return(newTurtles)
  }
)

#' @export
#' @rdname hatch
setMethod(
  "hatch",
  signature = c("SpatialPointsDataFrame", "numeric", "numeric", "missing"),
  definition = function(turtles, who, n) {
    breed <- turtles@data[turtles@data$who == who, "breed"]
    hatch(turtles = turtles, who = who, n = n, breed = breed)
  }
)


################################################################################
#' Can move
#'
#' Reports \code{TRUE} or \code{FALSE} if the turtle(s) can move the given distance(s)
#' without leaving the world's extent.
#'
#' @param world   A \code{NLworlds} object, representing the world in which the
#'                turtles would move onto.
#'
#' @param turtles A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the turtle(s) to be
#'                evaluated.
#'
#' @param step    Numeric. The distance(s) the turtles would move. Must be of
#'                length 1 if all the turtles would move the same distance or
#'                of the same length as the turtles object.
#'
#' @return A vector of logical of length equal to \code{length(turtles)}.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createTurtles(n = 10, world = w1)
#' canMove(world = w1, turtles = t1, step = 1:10)
#'
#'
#' @export
#' @docType methods
#' @rdname canMove
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "canMove",
  function(world, turtles, step) {
    standardGeneric("canMove")
  })

#' @export
#' @rdname canMove
setMethod(
  "canMove",
  signature = c("NLworlds", "SpatialPointsDataFrame", "numeric"),
  definition = function(world, turtles, step) {
    wrapFalse <- fd(world = world, turtles = turtles, step = step, torus = FALSE)
    wrapTrue <- fd(world = world, turtles = turtles, step = step, torus = TRUE)
    test <- wrapFalse@coords == wrapTrue@coords
    return(test[,1] & test[,2])
  }
)


################################################################################
#' Random xcor
#'
#' Reports random xcor coordinate(s) inside the world's extent.
#'
#' @param world A \code{NLworlds} object.
#'
#' @param n     Numeric. The number of xcor coordinates to generate.
#'
#' @return A vector of xcor coordinate values of length \code{n}.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createTurtles(n = 10,coords = cbind(xcor = randomXcor(world = w1, n = 10),
#'                                           ycor = randomYcor(world = w1, n = 10)))
#' w1[] <- runif(25)
#' plot(w1)
#' points(t1, pch = 16, col = t1@data$color)
#'
#'
#' @export
#' @docType methods
#' @rdname randomXcor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "randomXcor",
  function(world, n) {
    standardGeneric("randomXcor")
  })

#' @export
#' @rdname randomXcor
setMethod(
  "randomXcor",
  signature = c("NLworlds", "numeric"),
  definition = function(world, n) {
    xmin <- world@extent@xmin
    xmax <- world@extent@xmax
    xcor <- round(runif(n = n, min = xmin, max = xmax), digits = 5)
    return(xcor)
  }
)


################################################################################
#' Random ycor
#'
#' Reports random ycor coordinate(s) inside the world's extent.
#'
#' @param world A \code{NLworlds} object.
#'
#' @param n     Numeric. The number of ycor coordinates to generate.
#'
#' @return A vector of ycor coordinate values of length \code{n}.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createTurtles(n = 10, coords = cbind(xcor = randomXcor(world = w1, n = 10),
#'                                            ycor = randomYcor(world = w1, n = 10)))
#' w1[] <- runif(25)
#' plot(w1)
#' points(t1, pch = 16, col = t1@data$color)
#'
#'
#' @export
#' @docType methods
#' @rdname randomYcor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "randomYcor",
  function(world, n) {
    standardGeneric("randomYcor")
  })

#' @export
#' @rdname randomXcor
setMethod(
  "randomYcor",
  signature = c("NLworlds", "numeric"),
  definition = function(world, n) {
    ymin <- world@extent@ymin
    ymax <- world@extent@ymax
    ycor <- round(runif(n = n, min = ymin, max = ymax), digits = 5)
    return(ycor)
  }
)


################################################################################
#' Towards
#'
#' Reports the heading from agent(s) towards other agent(s) or location(s) [x,y].
#'
#' @param world A \code{NLworlds} object.
#'
#' @param from  A matrix (ncol = 2) with the first column \code{pxcor} and the
#'              second column \code{pycor} representing the coordinates of the
#'              patch(es) from which the heading will be computed.
#'              A SpatialPointsDataFrame created by \code{createTurtles()} or by
#'              \code{createOTurtles()} representing the turtle(s) from which the
#'              heading will be computed.
#'
#' @param to    A matrix (ncol = 2) with the first column \code{pxcor} and the
#'              second column \code{pycor} representing the coordinates of the
#'              patch(es) to which the heading will be computed.
#'              A SpatialPointsDataFrame created by \code{createTurtles()} or by
#'              \code{createOTurtles()} representing the turtle(s) to which the
#'              heading will be computed.
#'              A matrix (ncol = 2) with the first column \code{x} and the second
#'              column \code{y} representing the coordinates of the location(s) to
#'              which the heading will be computed.
#'              \code{to} must be of length 1 or of the same length as \code{from}.
#'
#' @param torus  Logical to determine if the \code{NLworlds} object is wrapped.
#'               Default is \code{torus = FALSE}.
#'
#' @return A vector of angles in degrees of the length of \code{from}.
#'
#' @details If \code{torus = TRUE} and the distance from one agent \code{from} to
#'          its corresponding agent or location \code{to} is smaller around the
#'          sides of the world than across it, then the heading to the agent or location
#'          going around the sides of the world is reported.
#'          The heading from an agent to itself or its own location will return 0.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' towards(world = w1, from = patches(world = w1), to = cbind(x = 0, y = 0))
#' t1 <- createTurtles(n = 10, world = w1)
#' towards(world = w1, from = t1, to = cbind(x = 0, y = 0))
#'
#'
#' @export
#' @importFrom CircStats deg
#' @docType methods
#' @rdname towards
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "towards",
  function(world, from, to, torus = FALSE) {
    standardGeneric("towards")
  })

#' @export
#' @rdname towards
setMethod(
  "towards",
  signature = c(world = "NLworlds", from = "matrix", to = "matrix"),
  definition = function(world, from, to, torus) {

    heading <- deg(atan2(to[,1] - from[,1], to[,2] - from[,2])) # angles between -180 and 180
    heading[heading < 0] <- heading[heading < 0] + 360

    if(torus == TRUE){
      # Need to create coordinates for "to" in a wrapped world
      # For all the 8 possibilities of wrapping (to the left, right, top, bottom and 4 corners)
      # Find the smallest distances across or around the world

      if(nrow(to) == 1 & nrow(from) != 1){
        to <- cbind(x = rep(to[,1], nrow(from)), y = rep(to[,2], nrow(from)))
      }

      toShortest <- to

      for(i in 1:nrow(from)){
        to1 <- cbind(to[i,1] - (world@extent@xmax - world@extent@xmin), to[i,2] + (world@extent@ymax - world@extent@ymin))
        to2 <- cbind(to[i,1], to[i,2] + (world@extent@ymax - world@extent@ymin))
        to3 <- cbind(to[i,1] + (world@extent@xmax - world@extent@xmin), to[i,2] + (world@extent@ymax - world@extent@ymin))
        to4 <- cbind(to[i,1] - (world@extent@xmax - world@extent@xmin), to[i,2])
        to5 <- cbind(to[i,1] + (world@extent@xmax - world@extent@xmin), to[i,2])
        to6 <- cbind(to[i,1] - (world@extent@xmax - world@extent@xmin), to[i,2] - (world@extent@ymax - world@extent@ymin))
        to7 <- cbind(to[i,1], to[i,2] - (world@extent@ymax - world@extent@ymin))
        to8 <- cbind(to[i,1] + (world@extent@xmax - world@extent@xmin), to[i,2] - (world@extent@ymax - world@extent@ymin))


        dist <- pointDistance(p1 = from[i,], p2 = to[i,], lonlat = FALSE, allpairs = FALSE)
        dist1 <- pointDistance(p1 = from[i,], p2 = to1, lonlat = FALSE, allpairs = FALSE)
        dist2 <- pointDistance(p1 = from[i,], p2 = to2, lonlat = FALSE, allpairs = FALSE)
        dist3 <- pointDistance(p1 = from[i,], p2 = to3, lonlat = FALSE, allpairs = FALSE)
        dist4 <- pointDistance(p1 = from[i,], p2 = to4, lonlat = FALSE, allpairs = FALSE)
        dist5 <- pointDistance(p1 = from[i,], p2 = to5, lonlat = FALSE, allpairs = FALSE)
        dist6 <- pointDistance(p1 = from[i,], p2 = to6, lonlat = FALSE, allpairs = FALSE)
        dist7 <- pointDistance(p1 = from[i,], p2 = to7, lonlat = FALSE, allpairs = FALSE)
        dist8 <- pointDistance(p1 = from[i,], p2 = to8, lonlat = FALSE, allpairs = FALSE)

        allDist <- c(dist, dist1, dist2, dist3, dist4, dist5, dist6, dist7, dist8)
        distMin <- min(allDist)
        allToCoords <- rbind(to[i,], to1, to2, to3, to4, to5, to6, to7, to8)
        toShortest[i,] <- allToCoords[match(distMin, allDist),]
      }

      heading <- deg(atan2(toShortest[,1] - from[,1], toShortest[,2] - from[,2])) # angles between -180 and 180
      heading[heading < 0] <- heading[heading < 0] + 360
    }
    return(heading)
  }
)

#' @export
#' @rdname towards
setMethod(
  "towards",
  signature = c(world = "NLworlds", from = "SpatialPointsDataFrame", to = "matrix"),
  definition = function(world, from, to, torus) {
    towards(world = world, from = from@coords, to = to, torus = torus)
  }
)

#' @export
#' @rdname towards
setMethod(
  "towards",
  signature = c(world = "NLworlds", from = "matrix", to = "SpatialPointsDataFrame"),
  definition = function(world, from, to, torus) {
    towards(world = world, from = from, to = to@coords, torus = torus)
  }
)

#' @export
#' @rdname towards
setMethod(
  "towards",
  signature = c(world = "NLworlds", from = "SpatialPointsDataFrame", to = "SpatialPointsDataFrame"),
  definition = function(world, from, to, torus) {
    towards(world = world, from = from@coords, to = to@coords, torus = torus)
  }
)


################################################################################
#' Face
#'
#' Set the turtles' heading towards some agent(s) or location(s) [x,y].
#'
#' @param world   A \code{NLworlds} object.
#'
#' @param turtles A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the turtle(s) from which
#'                the heading will be modified.
#'
#' @param to      A matrix (ncol = 2) with the first column \code{pxcor} and the
#'                second column \code{pycor} representing the coordinates of the
#'                patch(es) towards which the turtles' heading will be set
#'                A SpatialPointsDataFrame created by \code{createTurtles()} or by
#'                \code{createOTurtles()} representing the turtle(s) towards which
#'                the turtles' heading will be set
#'                A matrix (ncol = 2) with the first column \code{x} and the second
#'                column \code{y} representing the coordinates of the location(s)
#'                towards which the heading will be set
#'                \code{to} must be of length 1 or of the same length as \code{from}.
#'
#' @param torus   Logical to determine if the \code{NLworlds} object is wrapped.
#'                Default is \code{torus = FALSE}.
#'
#' @return A SpatialPointsDataFrame representing the turtles with updated headings.
#'
#' @details If \code{torus = TRUE} and the distance from one turtle \code{from} to
#'          its corresponding agent or location \code{to} is smaller around the
#'          sides of the world than across it, then the heading to the agent or location
#'          going around the sides of the world is given to the turtle.
#'          There is no change in heading for a turtle towards itself or its own location.
#'          This function is similar to setting the agents' heading to the results of
#'          the \code{towards()} function.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- runif(25)
#' t1 <- createTurtles(n = 10, coords = cbind(pxcor = randomXcor(world = w1, n = 10),
#'                                            pycor = randomYcor(world = w1, n = 10)))
#' plot(w1)
#' points(t1, pch = 16, col = t1@data$color)
#' t1 <- face(world = w1, turtles = t1, to = cbind(x = 0, y = 0))
#' t1 <- fd(world = w1, turtles = t1, step = 0.5)
#' points(t1, pch = 16, col = t1@data$color)
#'
#'
#' @export
#' @docType methods
#' @rdname face
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "face",
  function(world, turtles, to, torus = FALSE) {
    standardGeneric("face")
  })

#' @export
#' @rdname face
setMethod(
  "face",
  signature = c(world = "NLworlds", turtles = "SpatialPointsDataFrame", to = "matrix"),
  definition = function(world, turtles, to, torus) {

    newHeading <- towards(world = world, from = turtles, to = to, torus = torus)

    if(nrow(to) == 1 & nrow(turtles) != 1){
      to <- cbind(x = rep(to[,1], nrow(turtles)), y = rep(to[,2], nrow(turtles)))
    }
    # Do not change the heading if the turtles is facing its position
    for(i in 1:nrow(turtles@coords)){
      if(turtles@coords[i,1] == to[i,1] & turtles@coords[i,2] == to[i,2]){
        newHeading[i] <- turtles@data$heading[i]
      }
    }
    newData <- turtles@data
    newData[, "heading"] <- newHeading
    newTurtles <- SpatialPointsDataFrame(coords = turtles@coords, data = newData)
    return(newTurtles)
  }
)

#' @export
#' @rdname face
setMethod(
  "face",
  signature = c(world = "NLworlds", turtles = "SpatialPointsDataFrame", to = "SpatialPointsDataFrame"),
  definition = function(world, turtles, to, torus) {
    face(world = world, turtles = turtles, to = to@coords, torus = torus)
  }
)


################################################################################
#' Left
#'
#' Rotate the turtles's heading to the left.
#'
#' @param turtles  A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                 by \code{createOTurtles()} representing the turtle(s) to rotate.
#'
#' @param nDegrees Numeric. The number of degrees by which to rotate the turtles'
#'                 heading to the left. Must be of length 1 or of the same length
#'                 as the \code{turtles}.
#'
#' @return A SpatialPointsDataFrame representing the turtles with updated headings.
#'
#' @details If \code{nDegrees} is negative, the turtle rotate to the right.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' t1 <- createTurtles(n = 10, world = w1)
#' t1@data$heading
#' t1 <- left(turtles = t1, nDegrees = 180)
#' t1@data$heading
#'
#'
#' @export
#' @docType methods
#' @rdname left
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "left",
  function(turtles, nDegrees) {
    standardGeneric("left")
  })

#' @export
#' @rdname left
setMethod(
  "left",
  signature = c("SpatialPointsDataFrame", "numeric"),
  definition = function(turtles, nDegrees) {
    newHeading <- turtles@data$heading - nDegrees
    newHeading[newHeading < 0] <- newHeading[newHeading < 0] + 360
    newHeading[newHeading >= 360] <- newHeading[newHeading >= 360] - 360

    newData <- turtles@data
    newData[, "heading"] <- newHeading
    newTurtles <- SpatialPointsDataFrame(coords = turtles@coords, data = newData)
    return(newTurtles)
  }
)


################################################################################
#' Right
#'
#' Rotate the turtles's heading to the right.
#'
#' @param turtles  A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                 by \code{createOTurtles()} representing the turtle(s) to rotate.
#'
#' @param nDegrees Numeric. The number of degrees by which to rotate the turtles'
#'                 heading to the right Must be of length 1 or of the same length
#'                 as the \code{turtles}.
#'
#' @return A SpatialPointsDataFrame representing the turtles with updated headings.
#'
#' @details If \code{nDegrees} is negative, the turtle rotate to the left.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' t1 <- createTurtles(n = 10, world = w1)
#' t1@data$heading
#' t1 <- right(turtles = t1, nDegrees = 180)
#' t1@data$heading
#'
#'
#' @export
#' @docType methods
#' @rdname right
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "right",
  function(turtles, nDegrees) {
    standardGeneric("right")
  })

#' @export
#' @rdname right
setMethod(
  "right",
  signature = c("SpatialPointsDataFrame", "numeric"),
  definition = function(turtles, nDegrees) {
    left(turtles = turtles, nDegrees = -nDegrees)
  }
)


################################################################################
#' Downhill
#'
#' Move the turtles to their neighboring patch with the lowest value for the pacthes'
#' variable.
#'
#' @param world      A \code{NLworlds} object, representing the world in which the
#'                   turtles move onto.
#'
#' @param pVar       If the world is a \code{NLworldStack}, pVar is the name
#'                   (characters) of the layer used to define the patches's variable
#'                   used to move downihll.
#'
#' @param turtles    A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                   by \code{createOTurtles()} representing the moving turtles.
#'
#' @param nNeighbors 4 or 8 for the number of neighbor patches considered to move
#'                   downhill.
#'
#' @param torus      Logical to determine if the \code{NLworlds} object is wrapped.
#'                   Default is \code{torus = FALSE}.
#'
#' @return A SpatialPointsDataFrame representing the turtles with updated locations
#'         and headings.
#'
#' @details The turtles face the chosen patches and then move to their center. Both
#'          headings and locations are updated with \code{downhill}.
#'          If no neighboring patch has a smaller value than the patch where the
#'          turtle is currently located, the turtle stays on this patch. It still
#'          moves to the patch center if it was not already on it.
#'          If there are multiple neighboring patches with the same lowest value,
#'          the turtle chooses one patch at random.
#'          If \code{torus = FALSE}, turtles cannot move on the other side of the world.
#'          If a turtle is located on a patch on the edge of the world, it has fewer
#'          neighborhing patches for option to move than \code{nNeighbors}. If
#'          \code{torus = TRUE}, turtles can move on the other side of the world to
#'          go downhill and their choice of neighborhing patches is always among
#'          \code{nNeighbors} patches.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 1, maxPxcor = 10, minPycor = 1, maxPycor = 10)
#' w1[] <- runif(100)
#' t1 <- createTurtles(n = 10, coords = cbind(xcor = randomXcor(world = w1, n = 10),
#'                                            ycor = randomYcor(world = w1, n = 10)))
#' plot(w1)
#' points(t1, pch = 16, col = t1@data$color)
#' t1 <- downhill(world = w1, turtles = t1, nNeighbors = 8)
#' points(t1, pch = 16, col = t1@data$color)
#'
#'
#' @export
#' @docType methods
#' @rdname downhill
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "downhill",
  function(world, pVar, turtles, nNeighbors, torus = FALSE) {
    standardGeneric("downhill")
  })

#' @export
#' @rdname downhill
setMethod(
  "downhill",
  signature = c(world = "NLworld", pVar = "missing",turtles = "SpatialPointsDataFrame", nNeighbors = "numeric"),
  definition = function(world, turtles, nNeighbors, torus) {
    pNeighbors <- neighbors(world = world, agents = turtles, nNeighbors = nNeighbors, torus = torus)
    pValues <- values(world) # ordered by cellNumbers

    pMinNeighbors <- list()
    for(i in 1:length(pNeighbors)){
      pNeighbors[[i]] <- rbind(pNeighbors[[i]], patch(world = world, xcor = turtles@coords[i,1], ycor = turtles@coords[i,2])) # add the patch the turtle is located on
      pNeighbors[[i]] <- cbind(pNeighbors[[i]], cellNum = cellFromPxcorPycor(world = world, pxcor = pNeighbors[[i]][,"pxcor"], pycor = pNeighbors[[i]][,"pycor"]))
      pNeighbors[[i]] <- cbind(pNeighbors[[i]], pVal = pValues[pNeighbors[[i]][,"cellNum"]])
      pMinNeighbors[[i]] <- pNeighbors[[i]][pNeighbors[[i]][,"pVal"] == min(pNeighbors[[i]][,"pVal"]), c("pxcor", "pycor")]
      # If there are more than 1 patch with a minimum value, select a random one
      if(class(pMinNeighbors[[i]]) == "matrix"){ # otherwise it is "numeric"
        pMinNeighbors[[i]] <- pMinNeighbors[[i]][sample(nrow(pMinNeighbors[[i]]), 1), ]
      }
    }

    pMinNeighbors <- do.call(rbind, pMinNeighbors)
    newTurtles <- face(world = world, turtles = turtles, to = pMinNeighbors, torus = torus)
    # MoveTo function?
    newTurtles@coords <- cbind(xcor = pMinNeighbors[,1], ycor = pMinNeighbors[,2]) # pxcor and pycor otherwise for the names
    newTurtles@data$prevX <- turtles@coords[,1]
    newTurtles@data$prevY <- turtles@coords[,2]
    return(newTurtles)
  }
)

#' @export
#' @rdname downhill
setMethod(
  "downhill",
  signature = c(world = "NLworldStack", pVar = "character",turtles = "SpatialPointsDataFrame", nNeighbors = "numeric"),
  definition = function(world, pVar, turtles, nNeighbors, torus) {
    pos_l <- which(names(world) == pVar, TRUE) # find the layer
    world_l <- world[[pos_l]]
    downhill(world = world_l, turtles = turtles, nNeighbors = nNeighbors, torus = torus)
  }
)


################################################################################
#' Uphill
#'
#' Move the turtles to their neighboring patch with the highest value for the pacthes'
#' variable.
#'
#' @param world      A \code{NLworlds} object, representing the world in which the
#'                   turtles move onto.
#'
#' @param pVar       If the world is a \code{NLworldStack}, pVar is the name
#'                   (characters) of the layer used to define the patches's variable
#'                   used to move uphill.
#'
#' @param turtles    A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                   by \code{createOTurtles()} representing the moving turtles.
#'
#' @param nNeighbors 4 or 8 for the number of neighbor patches considered to move
#'                   uphill.
#'
#' @param torus      Logical to determine if the \code{NLworlds} object is wrapped.
#'                   Default is \code{torus = FALSE}.
#'
#' @return A SpatialPointsDataFrame representing the turtles with updated locations
#'         and headings.
#'
#' @details The turtles face the chosen patches and then move to their center. Both
#'          headings and locations are updated with \code{uphill}.
#'          If no neighboring patch has a larger value than the patch where the
#'          turtle is currently located, the turtle stays on this patch. It still
#'          moves to the patch center if it was not already on it.
#'          If there are multiple neighboring patches with the same highest value,
#'          the turtle chooses one patch at random.
#'          If \code{torus = FALSE}, turtles cannot move on the other side of the world.
#'          If a turtle is located on a patch on the edge of the world, it has fewer
#'          neighborhing patches for option to move than \code{nNeighbors}. If
#'          \code{torus = TRUE}, turtles can move on the other side of the world to
#'          go uphill and their choice of neighborhing patches is always among
#'          \code{nNeighbors} patches.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 1, maxPxcor = 10, minPycor = 1, maxPycor = 10)
#' w1[] <- runif(100)
#' t1 <- createTurtles(n = 10, coords = cbind(xcor = randomXcor(world = w1, n = 10),
#'                                            ycor = randomYcor(world = w1, n = 10)))
#' plot(w1)
#' points(t1, pch = 16, col = t1@data$color)
#' t1 <- uphill(world = w1, turtles = t1, nNeighbors = 8)
#' points(t1, pch = 16, col = t1@data$color)
#'
#'
#' @export
#' @docType methods
#' @rdname uphill
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "uphill",
  function(world, pVar, turtles, nNeighbors, torus = FALSE) {
    standardGeneric("uphill")
  })

#' @export
#' @rdname uphill
setMethod(
  "uphill",
  signature = c(world = "NLworld", pVar = "missing",turtles = "SpatialPointsDataFrame", nNeighbors = "numeric"),
  definition = function(world, turtles, nNeighbors, torus) {
    # Uphill is the inverse of downhill
    worldInv <- world
    worldInv[] <- 1 / values(world)
    downhill(world = worldInv, turtles = turtles, nNeighbors = nNeighbors, torus = torus)
  }
)

#' @export
#' @rdname uphill
setMethod(
  "uphill",
  signature = c(world = "NLworldStack", pVar = "character",turtles = "SpatialPointsDataFrame", nNeighbors = "numeric"),
  definition = function(world, pVar, turtles, nNeighbors, torus) {
    pos_l <- which(names(world) == pVar, TRUE) # find the layer
    world_l <- world[[pos_l]]
    uphill(world = world_l, turtles = turtles, nNeighbors = nNeighbors, torus = torus)
  }
)


################################################################################
#' Patch ahead
#'
#' Reports the patch(es) coordinates \code{pxcor} and \code{pycor} at the given
#' distance ahead of the turtle(s).
#'
#' @param world   A \code{NLworlds} object where the turtles evolve onto.
#'
#' @param turtles A SpatialPointsDataFrame created by \code{createTurtles()} or by
#'                \code{createOTurtles()} representing the moving turtles.
#'
#' @param dist    Numeric. Distance(s) from the turtle(s) to identify the patch.
#'                Must be of length 1 if the same distance is applied to all turtles
#'                or of length \code{turtles} if each turtle has a different distance.
#'
#' @param torus   Logical to determine if the \code{NLworlds} object is wrapped.
#'                Default is \code{torus = FALSE}.
#'
#' @return A matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the patches coordinates at \code{dist} of the
#'         \code{turtles}. The order of the patches follows the order of the \code{turtles}.
#'
#' @details If \code{torus = FALSE} and the patch at distance \code{dist} of the turtle
#'          is outside the world, \code{NA} is returned for the patch coordinates. Otherwise,
#'          if \code{torus = TRUE}, the patch coordinates from the wrapped world are
#'          returned.
#'          The distances from the turtles are computed with their current heading.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createTurtles(n = 10, coords = cbind(xcor = randomXcor(world = w1, n = 10),
#'                                            ycor = randomYcor(world = w1, n = 10)))
#' patchAhead(world = w1, turtles = t1, dist = 1)
#'
#'
#' @export
#' @importFrom CircStats rad
#' @docType methods
#' @rdname patchAhead
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patchAhead",
  function(world, turtles, dist, torus = FALSE) {
    standardGeneric("patchAhead")
  })

#' @export
#' @rdname patchAhead
setMethod(
  "patchAhead",
  signature = c(world = "NLworlds", turtles = "SpatialPointsDataFrame", dist = "numeric"),
  definition = function(world, turtles, dist, torus) {

    xcor <- round(turtles@coords[,1] + sin(rad(turtles@data$heading)) * dist, digits = 5)
    ycor <- round(turtles@coords[,2] + cos(rad(turtles@data$heading)) * dist, digits = 5)
    pAhead <- patch(world = world, xcor = xcor, ycor = ycor, duplicate = TRUE, torus = torus, out = TRUE)
    return(pAhead)

  }
)


################################################################################
#' Patch here
#'
#' Reports the patch(es) coordinates under the turtle(s).
#'
#' @param world   A \code{NLworlds} object where the turtles evolve onto.
#'
#' @param turtles A SpatialPointsDataFrame created by \code{createTurtles()} or by
#'                \code{createOTurtles()} representing the moving turtles.
#'
#' @return A matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the patches coordinates at the \code{turtles}
#'         location. The order of the patches follows the order of the \code{turtles}.
#'
#' @details If a turtle is located outside of the world's extent, \code{NA} is returned
#'          for its patch coordinates.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createTurtles(n = 10, coords = cbind(xcor = randomXcor(world = w1, n = 10),
#'                                            ycor = randomYcor(world = w1, n = 10)))
#' patchHere(world = w1, turtles = t1)
#'
#'
#' @export
#' @docType methods
#' @rdname patchHere
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patchHere",
  function(world, turtles) {
    standardGeneric("patchHere")
  })

#' @export
#' @rdname patchHere
setMethod(
  "patchHere",
  signature = c("NLworlds", "SpatialPointsDataFrame"),
  definition = function(world, turtles) {

    pTurtles <- patch(world = world, xcor = turtles@coords[,1], ycor = turtles@coords[,2], duplicate = TRUE, out = TRUE)
    return(pTurtles)

  }
)


################################################################################
#' Patch left
#'
#' Reports the patch(es) coordinates at a given distance to the left of the turtle(s).
#'
#' @param world    A \code{NLworlds} object where the turtles evolve onto.
#'
#' @param turtles  A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                 by \code{createOTurtles()} representing the moving turtles.
#'
#' @param dist     Numeric. Distance from the turtles. \code{dist} must be a single
#'                 value or of the length of \code{turtles}.
#'
#' @param nDegrees Numeric. The number of degrees the turtle's heading should rotate
#'                 to the left to locate the patch. Must be of length 1 or of the
#'                 same length as the \code{turtles}.
#'
#' @param torus    Logical to determine if the \code{NLworlds} object is wrapped.
#'                 Default is \code{torus = FALSE}.
#'
#' @return A matrix (ncol = 2) with the first column \code{pxcor} and the second
#'         column \code{pycor} representing the patches coordinates at \code{dist}
#'         of the turtles location and \code{nDegrees} to the left of their heading.
#'         The order of the patches follows the order of the \code{turtles}.
#'
#' @details If \code{nDegrees} is negative, the turtle would rotate to the right.
#'          If \code{dist} is negative, the turtle would move backward.
#'          If a turtle is located outside of the world's extent, \code{NA} is
#'          returned for its patch coordinates.
#'          If \code{torus = FALSE} and the patch at distance \code{dist} and
#'          heading \code{nDregrees} to the left of the turtles is outside the
#'          world's extent, \code{NA} is returned. If \code{torus = TRUE}, the
#'          patch coordinates from the wrapped world are returned.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createTurtles(n = 1, coords = cbind(xcor = 2, ycor = 2), heading = 90)
#' patchLeft(world = w1, turtles = t1, dist = 2, nDegrees = 90)
#'
#'
#' @export
#' @docType methods
#' @rdname patchLeft
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patchLeft",
  function(world, turtles, dist, nDegrees, torus = FALSE) {
    standardGeneric("patchLeft")
  })

#' @export
#' @rdname patchLeft
setMethod(
  "patchLeft",
  signature = c(world = "NLworlds", turtles = "SpatialPointsDataFrame", dist = "numeric", nDegrees = "numeric"),
  definition = function(world, turtles, dist, nDegrees, torus) {

    tLeft <- left(turtles = turtles, nDegrees = nDegrees)
    tFd <- fd(world = world, turtles = tLeft, step = dist, torus = torus)
    pLeftFd <- patchHere(world = world, turtles = tFd)

    return(pLeftFd)
  }
)


################################################################################
#' Patch right
#'
#' Reports the patch(es) coordinates at a given distance to the right of the turtle(s).
#'
#' @param world    A \code{NLworlds} object where the turtles evolve onto.
#'
#' @param turtles  A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                 by \code{createOTurtles()} representing the moving turtles.
#'
#' @param dist     Numeric. Distance from the turtles. \code{dist} must be a single
#'                 value or of the length of \code{turtles}.
#'
#' @param nDegrees Numeric. The number of degrees the turtle's heading should rotate
#'                 to the right to locate the patch. Must be of length 1 or of the
#'                 same length as the \code{turtles}.
#'
#' @param torus    Logical to determine if the \code{NLworlds} object is wrapped.
#'                 Default is \code{torus = FALSE}.
#'
#' @return A matrix (ncol = 2) with the first column \code{pxcor} and the second
#'         column \code{pycor} representing the patches coordinates at \code{dist}
#'         of the turtles location and \code{nDegrees} to the right of their heading.
#'         The order of the patches follows the order of the \code{turtles}.
#'
#' @details If \code{nDegrees} is negative, the turtle would rotate to the left.
#'          If \code{dist} is negative, the turtle would move backward.
#'          If a turtle is located outside of the world's extent, \code{NA} is
#'          returned for its patch coordinates.
#'          If \code{torus = FALSE} and the patch at distance \code{dist} and
#'          heading \code{nDregrees} to the right of the turtles is outside the
#'          world's extent, \code{NA} is returned. If \code{torus = TRUE}, the
#'          patch coordinates from the wrapped world are returned.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createTurtles(n = 1, coords = cbind(xcor = 2, ycor = 2), heading = 90)
#' patchRight(world = w1, turtles = t1, dist = 2, nDegrees = 90)
#'
#'
#' @export
#' @docType methods
#' @rdname patchRight
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "patchRight",
  function(world, turtles, dist, nDegrees, torus = FALSE) {
    standardGeneric("patchRight")
  })

#' @export
#' @rdname patchRight
setMethod(
  "patchRight",
  signature = c(world = "NLworlds", turtles = "SpatialPointsDataFrame", dist = "numeric", nDegrees = "numeric"),
  definition = function(world, turtles, dist, nDegrees, torus) {
    patchLeft(world = world, turtles = turtles, dist = dist, nDegrees = -nDegrees, torus = torus)
  }
)


################################################################################
#' Set xcor and ycor
#'
#' Set the turtles \code{xcor} and \code{ycor} coordinates.
#'
#' @param world   A \code{NLworlds} object where the turtles evolve onto.
#'
#' @param turtles A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the moving turtles.
#'
#' @param xcor    Numeric. X coordinates for the turles. Must be of length 1 or
#'                of the same length as the \code{turtles}.
#'
#' @param ycor    Numeric. X coordinates for the turles. Must be of length 1 or
#'                of the same length as the \code{turtles}
#'
#' @param torus   Logical to determine if the \code{NLworlds} object is wrapped.
#'                Default is \code{torus = FALSE}.
#'
#' @return A SpatialPointsDataFrame representing the moving turtles with updated
#'         coordinates and updated data for their previous coordinates (i.e.,
#'         \code{turtles@data$prevX} and \code{turtles@data$prevY}).
#'
#' @details If \code{torus = TRUE} and the given coordinates \code{xcor} or \code{ycor}
#'          are located outside of the world's extent, then the coordinates assigned
#'          are the ones from a wrapped word. If \code{torus = FALSE}, the turtle
#'          is then located outside of the world's extent with the given coordinates.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' w1[] <- runif(100)
#' t1 <- createTurtles(n = 5, coords = cbind(xcor = randomXcor(world = w1, n = 5),
#'                                           ycor = randomYcor(world = w1, n = 5)))
#' library(SpaDES)
#' clearPlot()
#' Plot(w1)
#' Plot(t1, addTo ="w1")
#'
#' t1 <- setXY(world = w1, turtles = t1, xcor = 1:5, ycor = 1:5)
#' Plot(t1, addTo ="w1")
#'
#'
#' @export
#' @importFrom SpaDES wrap
#' @docType methods
#' @rdname setXY
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "setXY",
  function(world, turtles, xcor, ycor, torus = FALSE) {
    standardGeneric("setXY")
  })

#' @export
#' @rdname setXY
setMethod(
  "setXY",
  signature = c(world = "NLworlds", turtles = "SpatialPointsDataFrame", xcor = "numeric", ycor = "numeric"),
  definition = function(world, turtles, xcor, ycor, torus) {

    if(length(xcor) == 1 & length(turtles) != 1){
      xcor <- rep(xcor, length(turtles))
    }
    if(length(ycor) == 1 & length(turtles) != 1){
      ycor <- rep(ycor, length(turtles))
    }

    tCoords <- cbind(xcor, ycor)
    if(torus == TRUE){
      tCoords <- wrap(cbind(x = xcor, y = ycor), extent(world))
      colnames(tCoords) <- c("xcor", "ycor")
    }

    turtles@data$prevX <- turtles@coords[,1]
    turtles@data$prevY <- turtles@coords[,2]

    newTurtles <- SpatialPointsDataFrame(coords = tCoords, data = turtles@data)
    return(newTurtles)
  }
)


################################################################################
#' Sprout
#'
#' Create new turtles on specific patch(es).
#'
#' @param n       Integer. Number of new turtle(s) to create.
#'
#' @param patches A matrix (ncol = 2) with the first column \code{pxcor} and the
#'                second column \code{pycor} representing the coordinates of the
#'                patch(es) on which the new turtles are created. \code{nrow(patches)}
#'                must be equal to 1 or to \code{n}.
#'
#' @param ...     Additional arguments (see details).
#'
#' @param turtles A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing existing turtles.
#'
#' @return A SpatialPointsDataFrame with the new created turtles.
#'
#' @details The additional arguments to be passed on are some of the function
#'          \code{createTurtles()} which are: heading, breed and color. If not
#'          provided, these arguments take the default value as defined in
#'          \code{createTurtles()}.
#'          If \code{turtles} is provided, the new created turtles are added to
#'          the \code{turtles} when returned. The who number of the created turtles
#'          therefore follow the ones from the \code{turtles}. If no \code{turtles}
#'          is provided, a new SpatialPointsDataFrame is created and the who number
#'          starts at 0.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' t1 <- sprout(patches = cbind(pxcor = 2, pycor = 2), n = 3)
#' t2 <- sprout(patches = cbind(pxcor = 3, pycor = 3), n = 3, turtles = t1)
#'
#'
#' @export
#' @docType methods
#' @rdname sprout
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "sprout",
  function(n, patches, turtles, breed, heading, color) {

    standardGeneric("sprout")
  })

#' @export
#' @rdname sprout
setMethod(
  "sprout",
  signature = c(n = "numeric", patches = "matrix"),
  definition = function(n, patches, turtles, breed, heading, color) {

    li <- lapply(names(match.call()[-1]), function(x) eval(parse(text=x)))
    names(li) <- names(match.call())[-1]

    if(nrow(li$patches) == 1 & n != 1){
      li$patches <- cbind(as.numeric(rep(li$patches[,1], n)), as.numeric(rep(li$patches[,2], n)))
    }
    colnames(li$patches) <- c("xcor", "ycor")

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

    newTurtles <- createTurtles(n = n, coords = li$patches, heading = li$heading, breed = li$breed, color = li$color)

    if(missing(turtles)){
      return(newTurtles)
    } else {
      newTurtles@data$who <- (max(turtles@data$who) + 1):(n + max(turtles@data$who)) # unique who number
      newColor <- rainbow(n + length(turtles))
      newTurtles@data$color <- sample(newColor[! newColor %in% turtles@data$color], n) # unique color
      bothTurtles <- SpatialPointsDataFrame(coords = rbind(turtles@coords, newTurtles@coords),
                                            data = rbind(turtles@data, newTurtles@data))
      return(bothTurtles)
    }
  }
)


################################################################################
#' Inspect
#'
#' Display the variables value for the requested turtle(s).
#'
#' @param turtles A SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the existing turtles
#'                in the world.
#'
#' @param who     Integer. Id (who number(s)) for the turte(s) to inspect.
#'
#' @return A dataframe with the variables for the inspected turtle(s).
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createOTurtles(world = w1, n = 10)
#' inspect(turtles = t1, who = c(2, 3))
#'
#'
#' @export
#' @docType methods
#' @rdname inspect
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "inspect",
  function(turtles, who) {
    standardGeneric("inspect")
  })

#' @export
#' @rdname inspect
setMethod(
  "inspect",
  signature = c("SpatialPointsDataFrame", "numeric"),
  definition = function(turtles, who) {
    tData <- cbind(turtles[turtles$who %in% who,]@data, turtles[turtles$who %in% who,]@coords)
    return(tData)
  }
)
