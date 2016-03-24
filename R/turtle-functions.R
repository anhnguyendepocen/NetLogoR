################################################################################
#' Create turtles
#'
#' Create \code{n} new turtles with a set of defined variables.
#'
#' @param n       Integer. The number of new turtles to create.
#'
#' @param coords  Matrix (ncol = 2) with the first column \code{xcor} and the second
#'                column \code{ycor} representing the turtles inital location.
#'                \code{nrow(coords)} must be equal to 1 or to \code{n}.
#'                Given coordinates must be inside the world's extent. If missing,
#'                turtles are put in the center of the \code{world}.
#'
#' @param world   \code{NLworlds} object to place the turtles created at the center.
#'                If \code{coords} is provided, \code{world} should not be provided.
#'
#' @param heading Numeric. Vector or values between 0 and 360. Must be of length 1 or
#'                \code{n}. If missing, a random heading is assigned to each turtle.
#'
#' @param breed   Characters. Either of length 1 or \code{n}. If missing,
#'                \code{breed = "turtle"} for all turtles.
#'
#' @param color   Characters. Vector of length \code{n}. If missing, colors are
#'                assigned using the function \code{rainbow(n)}.
#'
#' @return SpatialPointsDataFrame of length \code{n} with the columns for the
#'         dataframe being: "who", "heading", "prevX", "prevY", "breed", and "color".
#'
#' @details The identity of the turtles is defined by their "who" number. This
#'          numbering starts at 0 and increments by 1.
#'
#'          The coordinates from the previous time step are stored in "prevX" and
#'          "prevY". The initial values are \code{NA}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#create-turtles}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- runif(length(w1))
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
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
#' @param world   \code{NLworlds} object.
#'
#' @param breed   Characters. Either of length 1 or \code{n}. If missing,
#'                \code{breed = "turtle"} for all turtles.
#'
#' @param color   Characters. Vector of length \code{n}. If missing, colors are
#'                assigned using the function \code{rainbow(n)}.
#'
#' @return SpatialPointsDataFrame of length \code{n} with the columns for the
#'         dataframe being: "who", "heading", "prevX", "prevY", "breed", and "color".
#'
#' @details The first turtle \code{who = 0} has its heading set to 0.
#'
#'          The identity of the turtles is defined by their "who" number. This
#'          numbering starts at 0 and increments by 1.
#'
#'          The coordinates from the previous time step are stored in "prevX" and
#'          "prevY". The initial values are \code{NA}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#create-ordered-turtles}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- runif(length(w1))
#' t1 <- createOTurtles(n = 10, world = w1)
#'
#' library(SpaDES)
#' clearPlot()
#' Plot(w1)
#' Plot(t1, addTo ="w1") # automatically uses color column in SpatialPointsDataFrame
#'
#' t1 <- fd(world = w1, turtles = t1, step = 1)
#' Plot(t1, addTo ="w1")
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
#' Move forward
#'
#' Move the turtles forward with the turtles' heading direction.
#'
#' @param world   \code{NLworlds} object, representing the world which the
#'                turtles move onto.
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the moving turtles.
#'
#' @param step    Numeric. Distances by which the turtles will move forward. Must
#'                be of length 1 or of length \code{turtles}.
#'
#' @param torus   Logical to determine if the \code{NLworlds} object is wrapped.
#'                Default is \code{torus = FALSE}.
#'
#' @param out     Logical. Determine if turtles should move at all when
#'                \code{torus = FALSE} and their ending position will be outside of
#'                the world's extent. Default is \code{out = TRUE}.
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with updated
#'         coordinates and updated data for their previous coordinates "prevX"
#'         and "prevY".
#'
#' @details If the \code{NLworlds} object is wrapped (\code{torus = TRUE}) and the
#'          distance to move leads a turtle outside of the world's extent, it is
#'          relocated on the other side of the world, inside the world's extent. If
#'          \code{torus = FALSE} and \code{out = TRUE}, the turtle moves past the
#'          world's extent. If \code{torus = FALSE} and \code{out = FALSE}, the
#'          turtle does not move at all. In the event that a turtle does not move,
#'          its previous coordinates are still updated with its position before
#'          running \code{fd()} (i.e., its current position).
#'
#'          If a given \code{step} value is negative, then the turtle moves
#'          backward.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#forward}
#'
#'          \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#jump}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- runif(length(w1))
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
#' Move backward
#'
#' Move the turtles backward of the turtles' heading direction.
#'
#' @param world   \code{NLworlds} object, representing the world which the
#'                turtles move onto.
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the moving turtles.
#'
#' @param step    Numeric. Distances by which the turtles will move backward.
#'                Must be of length 1 or of length \code{turtles}.
#'
#' @param torus   Logical to determine if the \code{NLworlds} object is wrapped.
#'                Default is \code{torus = FALSE}.
#'
#' @param out     Logical. Determine if turtles should move at all when
#'                \code{torus = FALSE} and their ending position will be outside of
#'                the world's extent. Default is \code{out = TRUE}.
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with updated
#'         coordinates and updated data for their previous coordinates "prevX" and
#'         "prevY".
#'
#' @details If the \code{NLworlds} object is wrapped (\code{torus = TRUE}) and the
#'          distance to move leads a turtle outside of the world's extent, it is
#'          relocated on the other side of the world, inside the world's extent. If
#'          \code{torus = FALSE} and \code{out = TRUE}, the turtle moves past the
#'          world's extent. If \code{torus = FALSE} and \code{out = FALSE}, the
#'          turtle does not move at all. In the event that a turtle does not move,
#'          its previous coordinates are still updated with its position before
#'          running \code{bk()} (i.e., its current position).
#'
#'          If a given \code{step} value is negative, then the turtle moves
#'          forward.
#'
#'          This function is equivalent to the forward function with the distances
#'          to move equal to \code{step * -1}.
#'
#'          The turtles' heading are not affected by the function (i.e., the turtles
#'          do not head backward).
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#back}
#'
#'          \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#jump}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- runif(length(w1))
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
#' Return home
#'
#' Move the turtles back "home".
#'
#' @param world   \code{NLworlds} object, representing the world in which the
#'                turtles move onto.
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the moving turtles.
#'
#' @param home    Characters. Can take one of the following options to define where
#'                to relocate the \code{turtles}:
#'
#'                \code{home = "home0"} will place the turtles at the location
#'                \code{xcor = 0, ycor = 0}.
#'
#'                \code{home = "center"} will place the turtles at the center of
#'                the world.
#'
#'                \code{home = "pCorner"} will place the turtles at the center of
#'                the patch located in the left bottom corner of the world.
#'
#'                \code{home = "corner"} will place the turtles at the left bottom
#'                corner of the world.
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with updated
#'         coordinates and updated data for their previous coordinates "prevX" and
#'         "prevY".
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#home}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- runif(length(w1))
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#'
#' library(SpaDES)
#' clearPlot()
#' Plot(w1)
#' Plot(t1, addTo ="w1")
#'
#' t1 <- home(world = w1, turtles = t1, home = "pCorner")
#' Plot(t1, addTo ="w1")
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
#' x-increment
#'
#' Report the amount by which the turtles' xcor would change if the turtles were
#' to move forward of \code{step} distances with their current heading.
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the moving turtles.
#'
#' @param step    Numeric. The distances the turtles would have to move forward to
#'                compute the x-increment value. Must be of length 1 or length
#'                \code{turtles}. The default value is \code{step = 1}.
#'
#' @return Numeric. Vector of length \code{turtles}.
#'
#' @details Report the sine of the turtles' heading multiplied by the \code{step}
#'          values. Heading 0 is north and angles are calculated in degrees in a
#'          clockwise manner.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#dxy}
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
#' y-increment
#'
#' Reports the amount by which the turtles' ycor would change if the turtles were
#' to move forward of \code{step} distance(s) with their current heading.
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the moving turtles.
#'
#' @param step    Numeric. The distances the turtles would have to move forward to
#'                compute the y-increment value. Must be of length 1 or length
#'                \code{turtles}. The default value is \code{step = 1}.
#'
#' @return Numeric. Vector of length \code{turtles}.
#'
#' @details Report the cosine of the turtles' heading multiplied by the \code{step}
#'          values. Heading 0 is north and angles are calculated in degrees in a
#'          clockwise manner.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#dxy}
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
#' Kill turtles
#'
#' Kill specific turtles.
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()}.
#'
#' @param who     Integers. The who numbers of the turtles to kill.
#'
#' @return SpatialPointsDataFrame of length equal to \code{length(turtles) - length(who)}.
#'
#' @details The who numbers of the other turtles are maintained.
#'
#'          To remove all the turtles, you can use \code{clearTurtles()}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#die}
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
#' Hatch new turtles
#'
#' Create new turtles from a parent turtle.
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} containing the parent turtle.
#'
#' @param who     Integer. The who number of the parent turtle.
#'
#' @param n       Integer. The number of new turtles to create.
#'
#' @param breed   Character. The breed for the turtles to create. If missing,
#'                the created turtles are of the same breed as the parent turtle.
#'
#' @return SpatialPointsDataFrame of length equal to \code{length(turtles) + n}.
#'
#' @details The created turtles inherit of all the data from the parent turtle,
#'          except for the breed, if specified otherwise, and for the who number.
#'          The who numbers of the turtles created take on following the highest
#'          who number among the \code{turtles}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#hatch}
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
#' Can the turtle move?
#'
#' Report \code{TRUE} or \code{FALSE} if the turtles can move the given distances
#' without leaving the world's extent.
#'
#' @param world   \code{NLworlds} object, representing the world which the
#'                turtles move onto.
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the turtles to
#'                evaluate.
#'
#' @param step    Numeric. The distances the turtles would move. Must be of
#'                length 1 or of length \code{turtles}.
#'
#' @return Logicals. Vector of length \code{turtles}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#can-move}
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
#' Generate random xcor
#'
#' Report random xcor coordinates inside the world's extent.
#'
#' @param world \code{NLworlds} object.
#'
#' @param n     Integer. The number of xcor coordinates to generate.
#'
#' @return Numeric. Vector of xcor coordinate values of length \code{n}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#random-cor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- runif(length(w1))
#' t1 <- createTurtles(n = 10,coords = cbind(xcor = randomXcor(world = w1, n = 10),
#'                                           ycor = randomYcor(world = w1, n = 10)))
#' library(SpaDES)
#' clearPlot()
#' Plot(w1)
#' Plot(t1, addTo = "w1")
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
#' Generate random ycor
#'
#' Report random ycor coordinates inside the world's extent.
#'
#' @param world \code{NLworlds} object.
#'
#' @param n     Integer. The number of ycor coordinates to generate.
#'
#' @return Numeric. Vector of ycor coordinate values of length \code{n}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#random-cor}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- runif(length(w1))
#' t1 <- createTurtles(n = 10, coords = cbind(xcor = randomXcor(world = w1, n = 10),
#'                                            ycor = randomYcor(world = w1, n = 10)))
#' library(SpaDES)
#' clearPlot()
#' Plot(w1)
#' Plot(t1, addTo = "w1")
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
#' @rdname randomYcor
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
#' Direction towards
#'
#' Report the headings of agents \code{from} towards agents or locations \code{to}.
#'
#' @param world \code{NLworlds} object.
#'
#' @param from  Matrix (ncol = 2) with the first column \code{pxcor} and the
#'              second column \code{pycor} representing the coordinates of the
#'              patches from which the headings are computed.
#'
#'              SpatialPointsDataFrame created by \code{createTurtles()} or by
#'              \code{createOTurtles()} representing the turtles from which the
#'              headings are computed.
#'
#' @param to    Matrix (ncol = 2) with the first column \code{pxcor} and the
#'              second column \code{pycor} representing the coordinates of the
#'              patches towards which the heading are computed.
#'
#'              SpatialPointsDataFrame created by \code{createTurtles()} or by
#'              \code{createOTurtles()} representing the turtles towards which the
#'              headings are be computed.
#'
#'              Matrix (ncol = 2) with the first column \code{x} and the second
#'              column \code{y} representing the coordinates of the locations
#'              towards which the headings are computed.
#'
#' @param torus  Logical to determine if the \code{NLworlds} object is wrapped.
#'               Default is \code{torus = FALSE}.
#'
#' @return Numeric. Vector of angles in degrees of length equal to the largest
#'         number of agents/locations between the ones contained in\code{from} or
#'         in \code{to}.
#'
#' @details \code{from} and \code{to} must be of the same length or if different, one
#'          of the two has be of length 1.
#'
#'          If \code{torus = TRUE} and the distance from one agent \code{from} to
#'          its corresponding agent/location \code{to} is smaller around the
#'          sides of the world than across it, then the heading to the agent/location
#'          going around the sides of the world is reported.
#'
#'          The heading from a patch to its location returns 0, the heading from
#'          a turtle to its location returns the turtle's heading.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#towards}
#'
#'          \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#towardsxy}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' towards(world = w1, from = patches(w1), to = cbind(x = 0, y = 0))
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
      if(nrow(from) == 1 & nrow(to) != 1){
        from <- cbind(x = rep(from[,1], nrow(to)), y = rep(from[,2], nrow(to)))
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
    heading <- towards(world = world, from = from@coords, to = to, torus = torus)
    # The direction to a turtle's location return the turtle's heading
    heading <- ifelse(from@coords[,1] == to[,1] & from@coords[,2] == to[,2], from@data$heading, heading)
    return(heading)
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
    heading <- towards(world = world, from = from@coords, to = to@coords, torus = torus)
    # The direction to a turtle's location return the turtle's heading
    heading <- ifelse(from@coords[,1] == to@coords[,1] & from@coords[,2] == to@coords[,2], from@data$heading, heading)
    return(heading)
  }
)


################################################################################
#' Face directions
#'
#' Set the turtles' heading towards some agents or locations.
#'
#' @param world   \code{NLworlds} object.
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the turtles to modify
#'                their heading.
#'
#' @param to      Matrix (ncol = 2) with the first column \code{pxcor} and the
#'                second column \code{pycor} representing the coordinates of the
#'                patches towards which the turtles' heading is set
#'
#'                SpatialPointsDataFrame created by \code{createTurtles()} or by
#'                \code{createOTurtles()} representing the turtles towards which
#'                the turtles' heading is set.
#'
#'                Matrix (ncol = 2) with the first column \code{x} and the second
#'                column \code{y} representing the coordinates of the locations
#'                towards which the heading is set.
#'
#' @param torus   Logical to determine if the \code{NLworlds} object is wrapped.
#'                Default is \code{torus = FALSE}.
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with updated headings.
#'
#' @details \code{to} must be of length 1 or of length \code{turtles}.
#'
#'          If \code{torus = TRUE} and the distance from one turtle to
#'          its corresponding agent/location \code{to} is smaller around the
#'          sides of the world than across it, then the heading to the agent/location
#'          going around the sides of the world is given to the turtle.
#'
#'          There is no change in the heading when the turtle faces itself or its own location.
#'
#'          This function is similar to setting the agents' heading to the results of
#'          \code{towards()}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#face}
#'
#'          \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#facexy}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- runif(length(w1))
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#'
#' library(SpaDES)
#' clearplot()
#' Plot(w1)
#' Plot(t1, addTo = "w1")
#'
#' t1 <- face(world = w1, turtles = t1, to = cbind(x = 0, y = 0))
#' t1 <- fd(world = w1, turtles = t1, step = 0.5)
#' Plot(t1, addTo = "w1")
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
#' Rotate to the left
#'
#' Rotate the turtles's heading to the left.
#'
#' @param turtles  SpatialPointsDataFrame created by \code{createTurtles()} or
#'                 by \code{createOTurtles()} representing the turtles to rotate.
#'
#' @param nDegrees Numeric. The number of degrees by which to rotate the turtles'
#'                 heading to the left. Must be of length 1 or of length \code{turtles}.
#'
#' @return SpatialPointsDataFrame representing the turtles with updated headings.
#'
#' @details If \code{nDegrees} is negative, the turtle rotate to the right.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#left}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' t1 <- createTurtles(n = 10, world = w1)
#' of(t1, tVarName = "heading")
#' t1 <- left(turtles = t1, nDegrees = 180)
#' of(t1, tVarName = "heading")
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
#' Rotate to the right
#'
#' Rotate the turtles's heading to the right.
#'
#' @param turtles  SpatialPointsDataFrame created by \code{createTurtles()} or
#'                 by \code{createOTurtles()} representing the turtles to rotate.
#'
#' @param nDegrees Numeric. The number of degrees by which to rotate the turtles'
#'                 heading to the right Must be of length 1 or of length \code{turtles}.
#'
#' @return SpatialPointsDataFrame representing the turtles with updated headings.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#right}
#'
#' @details If \code{nDegrees} is negative, the turtle rotate to the left.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' t1 <- createTurtles(n = 10, world = w1)
#' of(t1, tVarName = "heading")
#' t1 <- right(turtles = t1, nDegrees = 180)
#' of(t1, tVarName = "heading")
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
#' Move downhill
#'
#' Move the turtles to their neighboring patch with the lowest value for the patches'
#' variable.
#'
#' @param world      \code{NLworlds} object, representing the world which the
#'                   turtles move onto.
#'
#' @param pVar       Characters. If the world is a \code{NLworldStack}, \code{pVar}
#'                   is the name of the layer used to define the patches's variable
#'                   used to move downihll. Should not be provided if the world is
#'                   \code{NLworld}.
#'
#' @param turtles    SpatialPointsDataFrame created by \code{createTurtles()} or
#'                   by \code{createOTurtles()} representing the moving turtles.
#'
#' @param nNeighbors Integer: 4 or 8. The number of neighbor patches considered to move
#'                   downhill.
#'
#' @param torus      Logical to determine if the \code{NLworlds} object is wrapped.
#'                   Default is \code{torus = FALSE}.
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with updated
#'         coordinates and updated data for their headings and previous coordinates
#'         "prevX" and "prevY".
#'
#' @details If no neighboring patch has a smaller value than the patch where the
#'          turtle is currently located on, the turtle stays on this patch. It still
#'          moves to the patch center if it was not already on it.
#'
#'          If there are multiple neighboring patches with the same lowest value,
#'          the turtle chooses one patch at random.
#'
#'          If \code{torus = FALSE}, turtles cannot move on the other side of the world.
#'          If a turtle is located on a patch on the edge of the world, it has fewer
#'          neighborhing patches for option to move than \code{nNeighbors}. If
#'          \code{torus = TRUE}, turtles can move on the other side of the world to
#'          go downhill and their choice of neighborhing patches is always among
#'          \code{nNeighbors} patches.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#downhill}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 1, maxPxcor = 10, minPycor = 1, maxPycor = 10)
#' w1[] <- runif(length(w1))
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#'
#' library(SpaDES)
#' clearPlot()
#' Plot(w1)
#' Plot(t1, addTo = "w1")
#'
#' t1 <- downhill(world = w1, turtles = t1, nNeighbors = 8)
#' Plot(t1, addTo = "w1")
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
#' Move uphill
#'
#' Move the turtles to their neighboring patch with the highest value for the pacthes'
#' variable.
#'
#' @param world      \code{NLworlds} object, representing the world which the
#'                   turtles move onto.
#'
#' @param pVar       Characters. If the world is a \code{NLworldStack}, \code{pVar}
#'                   is the name of the layer used to define the patches's variable
#'                   used to move uphill. Should not be provided if the world is
#'                   \code{NLworld}
#'
#' @param turtles    SpatialPointsDataFrame created by \code{createTurtles()} or
#'                   by \code{createOTurtles()} representing the moving turtles.
#'
#' @param nNeighbors Integer: 4 or 8. The number of neighbor patches considered to move
#'                   uphill.
#'
#' @param torus      Logical to determine if the \code{NLworlds} object is wrapped.
#'                   Default is \code{torus = FALSE}.
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with updated
#'         coordinates and updated data for their headings and previous coordinates
#'         "prevX" and "prevY".
#'
#' @details If no neighboring patch has a larger value than the patch where the
#'          turtle is currently located on, the turtle stays on this patch. It still
#'          moves to the patch center if it was not already on it.
#'
#'          If there are multiple neighboring patches with the same highest value,
#'          the turtle chooses one patch at random.
#'
#'          If \code{torus = FALSE}, turtles cannot move on the other side of the world.
#'          If a turtle is located on a patch on the edge of the world, it has fewer
#'          neighborhing patches for option to move than \code{nNeighbors}. If
#'          \code{torus = TRUE}, turtles can move on the other side of the world to
#'          go uphill and their choice of neighborhing patches is always among
#'          \code{nNeighbors} patches.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#uphill}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 1, maxPxcor = 10, minPycor = 1, maxPycor = 10)
#' w1[] <- runif(length(w1))
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#'
#' library(SpaDES)
#' clearPlot()
#' Plot(w1)
#' Plot(t1, addTo = "w1")
#'
#' t1 <- uphill(world = w1, turtles = t1, nNeighbors = 8)
#' Plot(t1, addTo = "w1")
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
#' Report the patches coordinates \code{pxcor} and \code{pycor} at the given
#' distance in the direction of the turtles' headings.
#'
#' @param world   \code{NLworlds} object, representing the world which the
#'                turtles move onto.
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or by
#'                \code{createOTurtles()} representing the moving turtles.
#'
#' @param dist    Numeric. Distances from the turtles to identify the patches ahead.
#'                Must be of length 1 or of length \code{turtles}.
#'
#' @param torus   Logical to determine if the \code{NLworlds} object is wrapped.
#'                Default is \code{torus = FALSE}.
#'
#' @return Matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the patches coordinates at \code{dist} of the
#'         \code{turtles}. The order of the patches follows the order of the \code{turtles}.
#'
#' @details If \code{torus = FALSE} and the patch at distance \code{dist} of the turtle
#'          is outside the world, \code{NA} are returned for the patch coordinates. Otherwise,
#'          if \code{torus = TRUE}, the patch coordinates from the wrapped world are
#'          returned.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch-ahead}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
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
#' Report the patches coordinates \code{pxcor} and \code{pycor} under the turtles
#' locations.
#'
#' @param world   \code{NLworlds} object, representing the world which the turtles
#'                move onto.
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or by
#'                \code{createOTurtles()} representing the moving turtles.
#'
#' @return Matrix (ncol = 2) with the first column \code{pxcor} and the second column
#'         \code{pycor} representing the patches coordinates at the \code{turtles}
#'         location. The order of the patches follows the order of the \code{turtles}.
#'
#' @details If a turtle is located outside of the world's extent, \code{NA} are returned
#'          for the patch coordinates.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch-here}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
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
#' Patch on the left
#'
#' Report the patches coordinates \code{pxcor} and \code{pycor} at given distances
#' to the left of the turtles.
#'
#' @param world    \code{NLworlds} object, representing the world which the turtles
#'                 move onto.
#'
#' @param turtles  SpatialPointsDataFrame created by \code{createTurtles()} or
#'                 by \code{createOTurtles()} representing the moving turtles.
#'
#' @param dist     Numeric. Distances from the \code{turtles} locations. Must be of
#'                 length 1 or of length \code{turtles}.
#'
#' @param nDegrees Numeric. The number of degrees the turtle's heading should rotate
#'                 to the left to locate the patches. Must be of length 1 or of
#'                 length \code{turtles}.
#'
#' @param torus    Logical to determine if the \code{NLworlds} object is wrapped.
#'                 Default is \code{torus = FALSE}.
#'
#' @return Matrix (ncol = 2) with the first column \code{pxcor} and the second
#'         column \code{pycor} representing the patches coordinates at \code{dist}
#'         of the turtles locations and \code{nDegrees} to the left of their headings.
#'         The order of the patches follows the order of the \code{turtles}.
#'
#' @details If \code{nDegrees} is negative, the turtle would rotate to the right.
#'          If \code{dist} is negative, the turtle would move backward.
#'
#'          If \code{torus = FALSE} and the patch at distance \code{dist} and
#'          heading \code{nDregrees} to the left of the turtle is outside the
#'          world's extent, \code{NA} are returned for the patch coordinates.
#'          If \code{torus = TRUE}, the patch coordinates from the wrapped world
#'          are returned.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch-lr-and-ahead}
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
#' Patch on the right
#'
#' Report the patches coordinates \code{pxcor} and \code{pycor} at a given distances
#' to the right of the turtles.
#'
#' @param world    \code{NLworlds} object, representing the world which the turtles
#'                 move onto.
#'
#' @param turtles  SpatialPointsDataFrame created by \code{createTurtles()} or
#'                 by \code{createOTurtles()} representing the moving turtles.
#'
#' @param dist     Numeric. Distances from the \code{turtles} locations. Must be of
#'                 length 1 or of length \code{turtles}.
#'
#' @param nDegrees Numeric. The number of degrees the turtle's heading should rotate
#'                 to the right to locate the patches. Must be of length 1 or of
#'                 length \code{turtles}.
#'
#' @param torus    Logical to determine if the \code{NLworlds} object is wrapped.
#'                 Default is \code{torus = FALSE}.
#'
#' @return Matrix (ncol = 2) with the first column \code{pxcor} and the second
#'         column \code{pycor} representing the patches coordinates at \code{dist}
#'         of the turtles locations and \code{nDegrees} to the right of their headings.
#'         The order of the patches follows the order of the \code{turtles}.
#'
#' @details If \code{nDegrees} is negative, the turtle would rotate to the left.
#'          If \code{dist} is negative, the turtle would move backward.
#'
#'          If \code{torus = FALSE} and the patch at distance \code{dist} and
#'          heading \code{nDregrees} to the right of the turtles is outside the
#'          world's extent, \code{NA} are returned for the patch coordinate.
#'          If \code{torus = TRUE}, the patch coordinates from the wrapped world
#'          are returned.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#patch-lr-and-ahead}
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
#' Set turtles locations
#'
#' Set the turtles \code{xcor} and \code{ycor} coordinates.
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the moving turtles.
#'
#' @param xcor    Numeric. X coordinates for the turles. Must be of length 1 or
#'                of length \code{turtles}.
#'
#' @param ycor    Numeric. X coordinates for the turles. Must be of length 1 or
#'                of length \code{turtles}.
#'
#' @param world   \code{NLworlds} object, representing the world which the
#'                turtles move onto. Should be provided only if \code{torus = TRUE}.
#'
#' @param torus   Logical to determine if the \code{NLworlds} object is wrapped.
#'                Default is \code{torus = FALSE}.
#'
#' @return SpatialPointsDataFrame representing the moving turtles with updated
#'         coordinates and updated data for their previous coordinates "prevX"
#'         and "prevY".
#'
#' @details If \code{torus = TRUE} and the given coordinates \code{xcor} or \code{ycor}
#'          are located outside of the world's extent, then the coordinates assigned
#'          are the ones from a wrapped word. If \code{torus = FALSE}, the turtle
#'          is then located outside of the world's extent with the given coordinates.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#setxy}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' w1[] <- runif(length(w1))
#' t1 <- createTurtles(n = 5, coords = randomXYcor(w1, n = 5))
#'
#' library(SpaDES)
#' clearPlot()
#' Plot(w1)
#' Plot(t1, addTo ="w1")
#'
#' t1 <- setXY(turtles = t1, xcor = 1:5, ycor = 1:5)
#' Plot(t1, addTo ="w1")
#'
#'
#' @export
#' @docType methods
#' @rdname setXY
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "setXY",
  function(turtles, xcor, ycor, world, torus = FALSE) {
    standardGeneric("setXY")
  })

#' @export
#' @rdname setXY
setMethod(
  "setXY",
  signature = c("SpatialPointsDataFrame", "numeric", "numeric", "missing", "ANY"),
  definition = function(turtles, xcor, ycor, torus) {

    if(length(xcor) == 1 & length(turtles) != 1){
      xcor <- as.numeric(rep(xcor, length(turtles)))
    }
    if(length(ycor) == 1 & length(turtles) != 1){
      ycor <- as.numeric(rep(ycor, length(turtles)))
    }
    tCoords <- cbind(xcor, ycor)

    turtles@data$prevX <- turtles@coords[,1]
    turtles@data$prevY <- turtles@coords[,2]

    newTurtles <- SpatialPointsDataFrame(coords = tCoords, data = turtles@data)
    return(newTurtles)
  }
)

#' @export
#' @importFrom SpaDES wrap
#' @rdname setXY
setMethod(
  "setXY",
  signature = c("SpatialPointsDataFrame", "numeric", "numeric", "NLworlds", "logical"),
  definition = function(turtles, xcor, ycor, world, torus) {

    wrapCoords <- wrap(cbind(x = xcor, y = ycor), extent(world))
    setXY(turtles = turtles, xcor = wrapCoords[,1], ycor = wrapCoords[,2], torus = FALSE)

  }
)


################################################################################
#' Sprout new turtles
#'
#' Create new turtles on specific patches.
#'
#' @param n       Integer. The number of new turtles to create.
#'
#' @param patches Matrix (ncol = 2) with the first column \code{pxcor} and the
#'                second column \code{pycor} representing the coordinates of the
#'                patches on which the new turtles are created. \code{nrow(patches)}
#'                must be equal to 1 or to \code{n}.
#'
#' @param ...     Additional arguments (see details).
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing existing turtles.
#'
#' @return SpatialPointsDataFrame with the new created turtles.
#'
#' @details The additional arguments to be passed on are some of the function
#'          \code{createTurtles()}: heading, breed and color. If not
#'          provided, these arguments take the default value as defined in
#'          \code{createTurtles()}.
#'
#'          If \code{turtles} is provided, the new created turtles are added to
#'          the \code{turtles} when returned. The who number of the created turtles
#'          therefore follow the ones from the \code{turtles}. If no \code{turtles}
#'          is provided, a new SpatialPointsDataFrame is created and the who number
#'          starts at 0.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#sprout}
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
#' Inspect turtles
#'
#' Display the variables values for the requested turtles.
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the moving turtles.
#'
#' @param who     Integers. Identity (i.e., "who" numbers) for the turtes to inspect.
#'
#' @return Dataframe (nrow = \code{length(who)}) with the variables for the inspected turtles.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#inspect}
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


################################################################################
#' Move to
#'
#' Move the turtles to the same location as the agents \code{to}.
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the moving turtles.
#'
#' @param to      Matrix (ncol = 2) with the first column \code{pxcor} and the
#'                second column \code{pycor} representing the coordinates of the
#'                patches where the \code{turtles} move to.
#'
#'                SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the turtles whose
#'                locations the \code{turtles} move to.
#'
#' @return SpatialPointsDataFrame representing the moving turtles with updated
#'         coordinates and updated data for their previous coordinates "prevX"
#'         and "prevY".
#'
#' @details The number of agents \code{to} (if matrix) must be equal to 1 or of
#'          length \code{turtles}.
#'
#'          The turtle's headings are not affected with this function.
#'
#'          If a turtle is moving to a patch location, it will be located at
#'          the patch center.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#move-to}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' w1[] <- runif(length(w1))
#' t1 <- createTurtles(n = 5, coords = randomXYcor(w1, n = 5))
#'
#' library(SpaDES)
#' clearPlot()
#' Plot(w1)
#' Plot(t1, addTo ="w1")
#'
#' t1 <- moveTo(turtles = t1, to = turtle(t1, who = 0))
#' Plot(t1, addTo ="w1")
#' t1 <- moveTo(turtles = t1, to = patch(w1, 9, 9))
#' Plot(t1, addTo ="w1")
#'
#'
#' @export
#' @docType methods
#' @rdname moveTo
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "moveTo",
  function(turtles, to) {
    standardGeneric("moveTo")
  })

#' @export
#' @rdname moveTo
setMethod(
  "moveTo",
  signature = c("SpatialPointsDataFrame", "matrix"),
  definition = function(turtles, to) {
    setXY(turtles = turtles, xcor = as.numeric(to[,1]), ycor = as.numeric(to[,2]), torus = FALSE)
  }
)

#' @export
#' @rdname moveTo
setMethod(
  "moveTo",
  signature = c("SpatialPointsDataFrame", "SpatialPointsDataFrame"),
  definition = function(turtles, to) {
    moveTo(turtles = turtles, to = to@coords)
  }
)


################################################################################
#' Generante random turtles coordinates
#'
#' Report random xcor and ycor coordinates inside the world's extent.
#'
#' @param world \code{NLworlds} object.
#'
#' @param n     Integer. The number of set of coordinates to generate.
#'
#' @return Matrix (ncol = 2, nrow = n) with the first column \code{xcor} and the second
#'         column \code{ycor}.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' w1[] <- runif(length(w1))
#' t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10))
#'
#' library(SpaDES)
#' clearPlot()
#' Plot(w1)
#' Plot(t1, addTo = "w1")
#'
#'
#' @export
#' @docType methods
#' @rdname randomXYcor
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "randomXYcor",
  function(world, n) {
    standardGeneric("randomXYcor")
  })

#' @export
#' @rdname randomXYcor
setMethod(
  "randomXYcor",
  signature = c("NLworlds", "numeric"),
  definition = function(world, n) {
    xycor <- cbind(xcor = randomXcor(world = world, n = n), ycor = randomYcor(world = world, n = n))
    return(xycor)
  }
)


################################################################################
#' Do the turtles exist?
#'
#' Report \code{TRUE} if the turtles exist inside the turtle agenset, report
#' \code{FALSE} otherwise.
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the turtle agentset.
#'
#' @param who     Integers. The who numbers of the turtles to check.
#'
#' @param breed   Characters. To specify the breed of turtles to check
#'                for existence. Must be of length 1 or of length \code{n}. If
#'                \code{breed} is of length \code{n} then the order of breeds
#'                should follow the order of \code{who}. If missing, there is
#'                no distinction based upon breed to check existence.
#'
#' @return Logicals. Vector of \code{TRUE} or \code{FALSE} if the turtles with
#'         the given who numbers and potentially given \code{breed} exists or not
#'         in the given \code{turtles}.
#'
#' @details If \code{breed} is provided, the turtle with the given \code{who} number
#'          AND given \code{breed} must exists inside \code{turtles} for \code{TRUE}
#'          to be returned.
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10), breed = c(rep("sheep", 5), rep("wolf", 5)))
#' tExists(turtles = t1, who = 3, breed = "sheep")
#' tExists(turtles = t1, who = 9, breed = "sheep")
#' tExists(turtles = t1, who = c(3, 9))
#'
#'
#' @export
#' @docType methods
#' @rdname tExists
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "tExists",
  function(turtles, who, breed) {
    standardGeneric("tExists")
  })

#' @export
#' @rdname tExists
setMethod(
  "tExists",
  signature = c("SpatialPointsDataFrame", "numeric", "missing"),
  definition = function(turtles, who) {

    tExist <- who %in% turtles@data$who
    return(tExist)

  }
)

#' @export
#' @rdname tExists
setMethod(
  "tExists",
  signature = c("SpatialPointsDataFrame", "numeric", "character"),
  definition = function(turtles, who, breed) {

    whoExist <- tExists(turtles = turtles, who = who)

    if(length(breed) == 1 & length(who) != 1){
      breed <- rep(breed, length(who))
    }
    whoTurtles <- turtles@data[turtles@data$who %in% who,] # select the who turtles
    whoTurtles <- whoTurtles[match(who, whoTurtles$who),] # order them by the order of given who
    breedExist <- whoTurtles$breed == breed

    return(whoExist & breedExist)

  }
)


################################################################################
#' Turtle
#'
#' Report the turtles according to their who numbers and breeds.
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or
#'                by \code{createOTurtles()} representing the turtles.
#'
#' @param who     Integers. The who numbers of the turtles to report.
#'
#' @param breed   Characters. To specify the breed of turtles to report.
#'                Must be of length 1 or of length \code{n}. If \code{breed} is
#'                of length \code{n} then the order of breeds should follow the
#'                order of \code{who}. If missing, there is no distinction based
#'                upon breed to report the turtles.
#'
#' @return SpatialPointsDataFrame of the selected turtles.
#'
#' @details If no turtle matches the given who numbers with potentially the given
#'          \code{breed}, then an empty SpatialPointsDataFrame is returned.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#turtle}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' t2 <- turtle(t1, who = 2)
#'
#'
#' @export
#' @docType methods
#' @rdname turtle
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "turtle",
  function(turtles, who, breed) {
    standardGeneric("turtle")
  })

#' @export
#' @rdname turtle
setMethod(
  "turtle",
  signature = c("SpatialPointsDataFrame", "numeric", "missing"),
  definition = function(turtles, who) {
    return(turtles[turtles$who %in% who, ])
  }
)

#' @export
#' @rdname turtle
setMethod(
  "turtle",
  signature = c("SpatialPointsDataFrame", "numeric", "character"),
  definition = function(turtles, who, breed) {

    whoTurtles <- turtle(turtles = turtles, who = who)

    if(length(breed) == 1 & length(who) != 1){
      breed <- rep(breed, length(who))
    }

    tSelect <- whoTurtles@data
    tSelect <- tSelect[match(who, tSelect$who),] # order them by the order of given who
    whoBreed <- tSelect[tSelect$breed == breed, "who"]

    turtle(turtles = turtles, who = whoBreed)
  }
)


################################################################################
#' Turtles on
#'
#' Report the turtles at some patches locations or on the same patches as some turtles.
#'
#' @param world   \code{NLworlds} object, representing the world which the turtles
#'                move onto.
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or by
#'                \code{createOTurtles()} representing the turtles among which the
#'                ones on the same patches as \code{agents} are going to
#'                be returned.
#'
#' @param agents  Matrix (ncol = 2) with the first column \code{pxcor} and the
#'                second column \code{pycor} representing the coordinates of the
#'                patches where to look for turtles.
#'
#'                SpatialPointsDataFrame created by \code{createTurtles()} or by
#'                \code{createOTurtles()} representing turtles whose patches locations
#'                are the patches where to look for turtles.
#'
#' @param breed   Characters. To specify the breeds of turtles to report.
#'                If missing, there is no distinction based upon breed to report turtles.
#'
#' @return SpatialPointsDataFrame representing the turtles among \code{turtles}
#'         which are located on the \code{agent} patches and of the given \code{breed}
#'         if specified.
#'
#' @details Patches or turtles given as \code{agents} must be located inside the
#'          world's extent.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#turtles-on}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' w1[] <- runif(length(w1))
#' t1 <- createTurtles(n = 500, coords = randomXYcor(w1, n = 500))
#'
#' library(SpaDES)
#' clearPlot()
#' Plot(w1)
#' Plot(t1, addTo = "w1")
#'
#' t2 <- turtlesOn(world = w1, turtles = t1, agents = patch(w1, 2, 2))
#'
#'
#' @export
#' @docType methods
#' @rdname turtlesOn
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "turtlesOn",
  function(world, turtles, agents, breed) {
    standardGeneric("turtlesOn")
  })

#' @export
#' @rdname turtlesOn
setMethod(
  "turtlesOn",
  signature = c("NLworlds", "SpatialPointsDataFrame", "matrix", "missing"),
  definition = function(world, turtles, agents) {
    pTurtles <- patchHere(world = world, turtles = turtles) # patches where the turtles are
    pTurtles <- cbind(pTurtles, turtles@data$who)

    pOn <- merge(agents, pTurtles) # patches where the turtles are among the agents patches

    if(nrow(pOn) == 0){
      return(noTurtles())
    } else {
      turtle(turtles = turtles, who = pOn[,3])
    }
  }
)

#' @export
#' @rdname turtlesOn
setMethod(
  "turtlesOn",
  signature = c("NLworlds", "SpatialPointsDataFrame", "matrix", "character"),
  definition = function(world, turtles, agents, breed) {
    tBreed <- turtles[turtles$breed %in% breed,]
    turtlesOn(world = world, turtles = tBreed, agents = agents)
  }
)

#' @export
#' @rdname turtlesOn
setMethod(
  "turtlesOn",
  signature = c("NLworlds", "SpatialPointsDataFrame", "SpatialPointsDataFrame", "missing"),
  definition = function(world, turtles, agents) {
    turtlesOn(world = world, turtles = turtles, agents = patchHere(world = world, turtles = agents))
  }
)

#' @export
#' @rdname turtlesOn
setMethod(
  "turtlesOn",
  signature = c("NLworlds", "SpatialPointsDataFrame", "SpatialPointsDataFrame", "character"),
  definition = function(world, turtles, agents, breed) {
    turtlesOn(world = world, turtles = turtles, agents = patchHere(world = world, turtles = agents), breed = breed)
  }
)


################################################################################
#' No turtles
#'
#' Report an empty turtle agentset.
#'
#' @return SpatialPointsDataFrame with the turtle variables defined
#'         as when using \code{createTurtles()} or \code{createOTurtles()} but
#'         of length equals to 0.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#no-turtles}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' t1 <- noTurtles()
#' length(t1)
#'
#'
#' @export
#' @docType methods
#' @rdname noTurtles
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "noTurtles",
  function(x) {
    standardGeneric("noTurtles")
  })

#' @export
#' @rdname noTurtles
setMethod(
  "noTurtles",
  signature = "missing",
  definition = function() {
    t0 <- createTurtles(n = 1, coords = cbind(xcor = 0, ycor = 0))
    return(t0[t0$who == 1,])
  }
)


################################################################################
#' Turtles at
#'
#' Report the turtles on the patches at \code{(dx, dy)} distances of the
#' \code{agents}.
#'
#' @param world   \code{NLworlds} object, representing the world which the turtles
#'                move onto.
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or by
#'                \code{createOTurtles()} representing the turtles among which the
#'                ones at \code{(dx, dy)} distance of the \code{agents} are
#'                returned.
#'
#' @param agents  Matrix (ncol = 2) with the first column \code{pxcor} and the
#'                second column \code{pycor} representing the coordinates of the
#'                patches from which \code{(dx, dy)} are computed.
#'
#'                SpatialPointsDataFrame created by \code{createTurtles()} or by
#'                \code{createOTurtles()} representing the turtles from which
#'                \code{(dx, dy)} are computed.
#'
#' @inheritParams patchAt
#'
#' @param breed   Characters. To specify the breed of turtles to report.
#'                If missing, there is no distinction based upon breed to report turtles.
#'
#' @param torus   Logical to determine if the \code{NLworlds} object is wrapped.
#'                Default is \code{torus = FALSE}.
#'
#' @return SpatialPointsDataFrame representing the turtles among \code{turtles}
#'         which are located on the patches at \code{(dx, dy)} distance of the
#'         \code{agents}.
#'
#' @details If \code{torus = FALSE} and the patch at distance \code{(dx, dy)}
#'          of the agent is outside of the world's extent, no turtle is returned.
#'          If \code{torus = TRUE}, the turtle located on the patch whose coordinates
#'          are defined from the wrapped world is returned.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#turtles-at}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createTurtles(n = 10, coords = cbind(xcor = 0:9, ycor = 0:9), breed = c(rep("sheep", 5), rep("wolf", 5)))
#' t2 <- turtlesAt(world = w1, turtles = t1, agents = turtle(t1, who = 0), dx = 1, dy = 1)
#' t3 <- turtlesAt(world = w1, turtles = t1, agents = patch(w1, c(3,4,5), c(3,4,5)), dx = 1, dy = 1, breed = "sheep")
#'
#'
#' @export
#' @docType methods
#' @rdname turtlesAt
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "turtlesAt",
  function(world, turtles, agents, dx, dy, breed, torus = FALSE) {
    standardGeneric("turtlesAt")
  })

#' @export
#' @rdname turtlesAt
setMethod(
  "turtlesAt",
  signature = c("NLworlds", "SpatialPointsDataFrame", "matrix", "numeric", "numeric", "missing", "ANY"),
  definition = function(world, turtles, agents, dx, dy, torus) {
    pAt <- patchAt(world = world, agents = agents, dx = dx, dy = dy)
    turtlesOn(world = world, turtles = turtles, agents = pAt)
  }
)

#' @export
#' @rdname turtlesAt
setMethod(
  "turtlesAt",
  signature = c("NLworlds", "SpatialPointsDataFrame", "SpatialPointsDataFrame", "numeric", "numeric", "missing", "ANY"),
  definition = function(world, turtles, agents, dx, dy, torus) {
    pAt <- patchAt(world = world, agents = agents, dx = dx, dy = dy)
    turtlesOn(world = world, turtles = turtles, agents = pAt)
  }
)

#' @export
#' @rdname turtlesAt
setMethod(
  "turtlesAt",
  signature = c("NLworlds", "SpatialPointsDataFrame", "matrix", "numeric", "numeric", "character", "ANY"),
  definition = function(world, turtles, agents, dx, dy, breed, torus) {
    pAt <- patchAt(world = world, agents = agents, dx = dx, dy = dy)
    turtlesOn(world = world, turtles = turtles, agents = pAt, breed = breed)
  }
)

#' @export
#' @rdname turtlesAt
setMethod(
  "turtlesAt",
  signature = c("NLworlds", "SpatialPointsDataFrame", "SpatialPointsDataFrame", "numeric", "numeric", "character", "ANY"),
  definition = function(world, turtles, agents, dx, dy, breed, torus) {
    pAt <- patchAt(world = world, agents = agents, dx = dx, dy = dy)
    turtlesOn(world = world, turtles = turtles, agents = pAt, breed = breed)
  }
)


################################################################################
#' Create a turtle agenset
#'
#' Report a turtle agentset containing all turtles provided in the inputs.
#'
#' @param ... SpatialPointsDataFrame objects created by \code{createTurtles()} or
#'            by \code{createOTurtles()} representing turtles.
#'
#' @return SpatialPointsDataFrame with all the turtles provided in the inputs.
#'
#' @details This functions does not affect turtles coordinates and variables.
#'          Therefore there may be multiple turtles with the same variable (e.g.,
#'          who number, color, ...) . This must be taken care of prior or later
#'          using this function.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#turtle-set}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10), breed = "sheep")
#' t2 <- createTurtles(n = 2, coords = randomXYcor(w1, n = 2), breed = "wolf")
#' t3 <- createTurtles(n = 1, coords = randomXYcor(w1, n = 1), breed = "sheperd")
#' t4 <- turtleSet(t1, t2, t3)
#'
#'
#' @export
#' @docType methods
#' @rdname turtleSet
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "turtleSet",
  function(...) {
    standardGeneric("turtleSet")
  })

#' @export
#' @rdname turtleSet
setMethod(
  "turtleSet",
  signature = "SpatialPointsDataFrame",
  definition = function(...) {

    dots <-list(...)

    allCoords <- dots[[1]]@coords
    allData <- dots[[1]]@data
    for(i in 2:length(dots)){
      allCoords <- rbind(allCoords, dots[[i]]@coords)
      allData <- rbind(allData, dots[[i]]@data)
    }

    allTurtles <- SpatialPointsDataFrame(coords = allCoords, data = allData)
    return(allTurtles)
  }
)


################################################################################
#' Turtles variable
#'
#' Create a new variable for the turtles provided.
#'
#' @param turtles  SpatialPointsDataFrame created by \code{createTurtles()} or by
#'                 \code{createOTurtles()} representing turtles.
#'
#' @param tVarName Characters. Name of the new variable to create.
#'
#' @param tVar     Any type of vector representing the value of \code{tVarName}.
#'                 Must be of length 1 or of length \code{turtles}.
#'                 If missing, \code{NA} is given.
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with the new
#'         variable \code{tVarName} added.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#turtles-own}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' t1 <- createTurtles(n = 5, coords = cbind(xcor = 0, ycor = 0))
#' t1 <- turtlesOwn(turtles = t1, tVarName = "sex", tVar = c("F", "F", "F", "M", "M"))
#'
#'
#' @export
#' @docType methods
#' @rdname turtlesOwn
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "turtlesOwn",
  function(turtles, tVarName, tVar) {
    standardGeneric("turtlesOwn")
  })

#' @export
#' @rdname turtlesOwn
setMethod(
  "turtlesOwn",
  signature = c("SpatialPointsDataFrame", "character", "missing"),
  definition = function(turtles, tVarName) {
    newData <- cbind(turtles@data, rep(NA, length(turtles)))
    colnames(newData)[ncol(turtles@data) + 1] <- tVarName
    turtles@data <- newData
    return(turtles)
  }
)

#' @export
#' @rdname turtlesOwn
setMethod(
  "turtlesOwn",
  signature = c("SpatialPointsDataFrame", "character", "ANY"),
  definition = function(turtles, tVarName, tVar) {
    newTurtles <- turtlesOwn(turtles = turtles, tVarName = tVarName)
    turtles@data[,tVarName] <- tVar
    return(turtles)
  }
)


################################################################################
#' Substract headings
#'
#' Compute the difference between headings.
#'
#' @param heading1 SpatialPointsDataFrame created by \code{createTurtles()} or by
#'                 \code{createOTurtles()} representing the turtles with their
#'                 headings.
#'
#'                 Numeric.
#'
#' @param heading2 SpatialPointsDataFrame created by \code{createTurtles()} or by
#'                 \code{createOTurtles()} representing turtles with their
#'                 headings.
#'
#'                 Numeric.
#'
#'                 \code{heading2} must be of length 1 or of length \code{heading1}.
#'
#' @param range360 Logical. Determine if the returned values are between 0° and
#'                 360° (\code{range360 = TRUE}) or between -180° and 180°
#'                 (\code{range360 = FALSE}). Default is \code{range360 = FALSE}.
#'
#' @return Numeric. Vector of values representing the smallest angle in degrees
#'         by which \code{heading1} could be rotated to produce \code{heading2}
#'         (i.e., the target heading). Note: this is the opposite as in NetLogo where
#'         heading1 is the target.
#'
#' @details Positive values mean clockwise rotation, negative value mean counterclockwise.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#subtract-headings}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createOTurtles(n = 10, world = w1)
#' subHeadings(heading1 = t1, heading2 = 0)
#'
#'
#' @export
#' @importFrom CircStats rad
#' @importFrom CircStats deg
#' @docType methods
#' @rdname subHeadings
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "subHeadings",
  function(heading1, heading2, range360 = FALSE) {
    standardGeneric("subHeadings")
  })

#' @export
#' @rdname subHeadings
setMethod(
  "subHeadings",
  signature = c(heading1 = "numeric", heading2 = "numeric"),
  definition = function(heading1, heading2, range360) {

    if(length(heading2) != length(heading1)){
      if(length(heading2) == 1){
        heading2 <- rep(heading2, length(heading1))
      } else {
        stop("heading2 must be of length 1 or length(heading1)")
      }
    }

    angles <- deg(atan2(sin(rad(heading2) - rad(heading1)), cos(rad(heading2) - rad(heading1))))

    if(range360 == TRUE){
      angles[angles < 0] <- angles[angles < 0] + 360
    }

    return(angles)
  }
)

#' @export
#' @rdname subHeadings
setMethod(
  "subHeadings",
  signature = c(heading1 = "SpatialPointsDataFrame", heading2 = "numeric"),
  definition = function(heading1, heading2, range360) {
    subHeadings(heading1 = heading1@data$heading, heading2 = heading2, range360 = range360)
  }
)
#' @export
#' @rdname subHeadings
setMethod(
  "subHeadings",
  signature = c(heading1 = "numeric", heading2 = "SpatialPointsDataFrame"),
  definition = function(heading1, heading2, range360) {
    subHeadings(heading1 = heading1, heading2 = heading2@data$heading, range360 = range360)
  }
)
#' @export
#' @rdname subHeadings
setMethod(
  "subHeadings",
  signature = c(heading1 = "SpatialPointsDataFrame", heading2 = "SpatialPointsDataFrame"),
  definition = function(heading1, heading2, range360) {
    subHeadings(heading1 = heading1@data$heading, heading2 = heading2@data$heading, range360 = range360)
  }
)


################################################################################
#' Other
#'
#' Report an agentset of all \code{agents} except specific ones.
#'
#' @param agents Matrix (ncol = 2) with the first column \code{pxcor} and the
#'               second column \code{pycor} representing the coordinates of the
#'               patches to evaluate.
#'
#'               SpatialPointsDataFrame created by \code{createTurtles()} or by
#'               \code{createOTurtles()} representing the turtles to evaluate.
#'
#' @param except Matrix (ncol = 2) with the first column \code{pxcor} and the
#'               second column \code{pycor} representing the coordinates of the
#'               patches to remove from the \code{agents}.
#'
#'               SpatialPointsDataFrame created by \code{createTurtles()} or by
#'               \code{createOTurtles()} representing the turtles to remove from
#'               the \code{agents}.
#'
#' @return Matrix (ncol = 2) with the first column \code{pxcor} and the second
#'         column \code{pycor} representing the patches in \code{agents} with
#'         the patches in \code{except} removed.
#'
#'         SpatialPointsDataFrame representing the turtles in \code{agents} with
#'         the turtles in \code{except} removed.
#'
#' @details Both \code{agents} and \code{except} must be of the same class (e.g., both
#'          patches or both turtles).
#'
#'          Carefull: this function removes turtles only based on similar who numbers
#'          and breeds.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#other}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' # Patches
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9) # 100 patches
#' p1 <- other(agents = patches(w1), except = patch(w1, 0, 0))
#' nrow(p1) # 99 patches
#'
#'# Turtles
#' t1 <- createTurtles(n = 10, coords = cbind(xcor = 0, ycor = 0)) # 10 turtles
#' t2 <- other(agents = t1, except = turtle(t1, who = 0))
#' length(t2) # 9 turtles
#'
#'
#' @export
#' @docType methods
#' @rdname other
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "other",
  function(agents, except) {
    standardGeneric("other")
  })

#' @export
#' @rdname other
setMethod(
  "other",
  signature = c("matrix", "matrix"),
  definition = function(agents, except) {

    pCoords <- agents[!duplicated(rbind(except, agents))[-(1:nrow(except))],]
    return(pCoords)
  }
)

#' @export
#' @rdname other
setMethod(
  "other",
  signature = c("SpatialPointsDataFrame", "SpatialPointsDataFrame"),
  definition = function(agents, except) {
    t1Data <- agents@data[,c("who", "breed")]
    t2Data <- except@data[,c("who", "breed")]
    sameTurtles <- merge(t1Data, t2Data)

    if(nrow(sameTurtles) == 0){
      # If agents does not contain except
      return(agents)
    } else {

      tRemove <- match(sameTurtles$who, t1Data$who)
      newCoords <- agents@coords[-tRemove,]
      newData <- agents@data[-tRemove,]

      if(nrow(newCoords) == 0){
        # If agents and except are the same
        noTurtles()
      } else {

        newTurtles <- SpatialPointsDataFrame(coords = newCoords, data = newData)
        return(newTurtles)
      }
    }
  }
)


################################################################################
#' Layout turtles on a circle
#'
#' Arrange the turtles in a circle centered on the world with the given radius.
#'
#' @param world   \code{NLworlds} object, representing the world which the
#'                turtles move onto.
#'
#' @param turtles SpatialPointsDataFrame created by \code{createTurtles()} or by
#'                \code{createOTurtles()} representing the turtles.
#'
#' @param radius  Numeric. Radius of the cercle.
#'
#' @param torus   Logical to determine if the \code{NLworlds} object is wrapped.
#'                Default is \code{torus = FALSE}.
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with updated coordinates
#'         and updated data for their previous coordinates "prevX" and "prevY".
#'
#' @details The turtles point outwards.
#'
#'          If the \code{NLworlds} object is wrapped (\code{torus = TRUE}) and the
#'          \code{radius} values lead a turtle outside of the world's extent, it is
#'          relocated on the other side of the world, inside the world's extent. If
#'          \code{torus = FALSE} and \code{out = TRUE}, the turtle are located past
#'          the world's extent.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#layout-circle}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' w1[] <- runif(length(w1))
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#'
#' library(SpaDES)
#' clearPlot()
#' Plot(w1)
#' Plot(t1, addTo = "w1")
#'
#' t2 <- layoutCircle(world = w1, turtles = t1, radius = 3)
#' Plot(t2, addTo = "w1")
#'
#'
#' @export
#' @docType methods
#' @rdname layoutCircle
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "layoutCircle",
  function(world, turtles, radius, torus = FALSE) {
    standardGeneric("layoutCircle")
  })

#' @export
#' @rdname layoutCircle
setMethod(
  "layoutCircle",
  signature = c(world = "NLworld", turtles = "SpatialPointsDataFrame", radius = "numeric"),
  definition = function(world, turtles, radius, torus) {
    tSurrogates <- createOTurtles(n = length(turtles), world = world)
    turtles@coords <- tSurrogates@coords
    turtles@data$heading <- tSurrogates@data$heading
    fd(world = world, turtles = turtles, step = radius, torus = torus, out = TRUE)
  }
)


################################################################################
#' Values of turtles variables
#'
#' Report the value of the turtles variables.
#'
#' @param turtles  SpatialPointsDataFrame created by \code{createTurtles()} or
#'                 by \code{createOTurtles()} representing the turtles.
#'
#' @param tVarName Characters. The name of the turtle's variable whose values to
#'                 report. \code{tVarName} can be equal to \code{"coords"}, \code{"xcor"},
#'                 \code{"ycor"}, \code{"who"}, \code{"heading"}, \code{"prevCoords"},
#'                 \code{"prevX"}, \code{"prevY"}, \code{"breed"}, \code{"color"} or
#'                 any of the variables created with \code{turtlesOwn()}.
#'
#' @return \code{tVarName} values for the \code{turtles}. The class depends
#'         on the class of the variable. The order of the values follows the order
#'         of the \code{turtles}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#of}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' of(turtles = t1, tVarName = "heading")
#'
#'
#' @export
#' @docType methods
#' @rdname of
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "of",
  function(turtles, tVarName) {
    standardGeneric("of")
  })

#' @export
#' @rdname of
setMethod(
  "of",
  signature = c("SpatialPointsDataFrame", "character"),
  definition = function(turtles, tVarName) {
    if(tVarName == "coords"){
      return(turtles@coords)
    } else if(tVarName == "xcor"){
      return(turtles@coords[,1])
    } else if(tVarName == "ycor"){
      return(turtles@coords[,2])
    } else if(tVarName == "prevCoords"){
      return(cbind(turtles@data$prevX, turtles@data$prevY))
    } else {
      return(turtles@data[,tVarName])
    }
  }
)
