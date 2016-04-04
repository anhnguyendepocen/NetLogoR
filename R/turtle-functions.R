################################################################################
#' Create turtles
#'
#' Create \code{n} turtles with a set of defined variables.
#'
#' @inheritParams fargs
#'
#' @param coords  Matrix (ncol = 2) with the first column "xcor" and the second
#'                column "ycor" representing the turtles inital locations.
#'                \code{nrow(coords)} must be equal to 1 or to \code{n}.
#'                Given coordinates must be inside the \code{world}'s extent. If missing,
#'                turtles are put in the center of the \code{world}.
#'
#' @param heading Numeric. Vector of values between 0 and 360. Must be of length 1 or
#'                of length \code{n}. If missing, a random heading is assigned to
#'                each turtle.
#'
#' @param breed   Character. Vector of "breed" names. Must be of length 1 or of length
#'                \code{n}. If missing, \code{breed = "turtle"} for all turtles.
#'
#' @return SpatialPointsDataFrame of length \code{n} with the columns for the
#'         dataframe being: "who", "heading", "prevX", "prevY", "breed", and "color".
#'
#' @details If \code{coords} is provided, \code{world} must not be provided.
#'
#'          The identity of the turtles is defined by their "who" number. This
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
#' Create \code{n} turtles at the center of the \code{world} with their headings evenly
#' distributed.
#'
#' @inheritParams fargs
#'
#' @inheritParams createTurtles
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
#' t1 <- fd(world = w1, turtles = t1, dist = 1)
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
#' Move the \code{turtles} forward with their headings as directions.
#'
#' @inheritParams fargs
#'
#' @param dist    Numeric. Vector of distances to move. Must
#'                be of length 1 or of length \code{turtles}.
#'
#' @param out     Logical. Determine if a turtle should move when
#'                \code{torus = FALSE} and its ending position will be outside of
#'                the \code{world}'s extent. Default is \code{out = TRUE}.
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with updated
#'         coordinates and updated data for their previous coordinates "prevX"
#'         and "prevY".
#'
#' @details If a distance to move leads a turtle outside of the \code{world}'s extent
#'          and \code{torus = TRUE}, the turtle is
#'          relocated on the other side of the \code{world}, inside its extent; if
#'          \code{torus = FALSE} and \code{out = TRUE}, the turtle moves past the
#'          \code{world}'s extent; if \code{torus = FALSE} and \code{out = FALSE}, the
#'          turtle does not move at all. In the event that a turtle does not move,
#'          its previous coordinates are still updated with its position before
#'          running \code{fd()} (i.e., its current position).
#'
#'          If a given \code{dist} value is negative, then the turtle moves
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
#' t1 <- fd(world = w1, turtles = t1, dist = 1)
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
  function(world, turtles, dist, torus = FALSE, out = TRUE) {
    standardGeneric("fd")
  })

#' @export
#' @rdname fd
setMethod(
  "fd",
  signature = c(world = "NLworlds", turtles = "SpatialPointsDataFrame", dist = "numeric"),
  definition = function(world, turtles, dist, torus, out) {

    turtles@data$prevX <- turtles@coords[,1]
    turtles@data$prevY <- turtles@coords[,2]

    fdXcor <- round(turtles@coords[,1] + sin(rad(turtles@data$heading)) * dist, digits = 5)
    fdYcor <- round(turtles@coords[,2] + cos(rad(turtles@data$heading)) * dist, digits = 5)

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
#' Move the \code{turtles} backward of their headings' directions.
#'
#' @inheritParams fargs
#'
#' @inheritParams fd
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with updated
#'         coordinates and updated data for their previous coordinates "prevX"
#'         and "prevY".
#'
#' @details If a distance to move leads a turtle outside of the \code{world}'s extent
#'          and \code{torus = TRUE}, the turtle is
#'          relocated on the other side of the \code{world}, inside its extent; if
#'          \code{torus = FALSE} and \code{out = TRUE}, the turtle moves past the
#'          \code{world}'s extent; if \code{torus = FALSE} and \code{out = FALSE}, the
#'          turtle does not move at all. In the event that a turtle does not move,
#'          its previous coordinates are still updated with its position before
#'          running \code{bk()} (i.e., its current position).
#'
#'          If a given \code{dist} value is negative, then the turtle moves
#'          forward.
#'
#'          The \code{turtles}' headings are not affected by the function (i.e., the
#'          \code{turtles} do not face backward).
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
#' t1 <- fd(world = w1, turtles = t1, dist = 2)
#' Plot(t1, addTo ="w1")
#' t1 <- bk(world = w1, turtles = t1, dist = 1)
#' Plot(t1, addTo ="w1")
#' t1 <- fd(world = w1, turtles = t1, dist = 0.5)
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
  function(world, turtles, dist, torus = FALSE, out = TRUE) {
    standardGeneric("bk")
  })

#' @export
#' @rdname bk
setMethod(
  "bk",
  signature = c(world = "NLworlds", turtles = "SpatialPointsDataFrame", dist = "numeric"),
  definition = function(world, turtles, dist, torus, out) {

    fd(world = world, turtles = turtles, dist = -dist, torus = torus, out = out)

  }
)


################################################################################
#' Return home
#'
#' Move the \code{turtles} back \code{home}.
#'
#' @inheritParams fargs
#'
#' @param home    Character. Can take one of the following options to define where
#'                to relocate the \code{turtles}:
#'
#'                \code{home = "home0"} will place the \code{turtles} at the location
#'                \code{x = 0, y = 0}.
#'
#'                \code{home = "center"} will place the \code{turtles} at the center of
#'                the \code{world}.
#'
#'                \code{home = "pCorner"} will place the \code{turtles} at the center of
#'                the patch located in the left bottom corner of the \code{world}.
#'
#'                \code{home = "corner"} will place the \code{turtles} at the left bottom
#'                corner of the \code{world}.
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with updated
#'         coordinates and updated data for their previous coordinates "prevX"
#'         and "prevY".
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
        newTurtles<- setXY(turtles = turtles, xcor = 0, ycor = 0, world = world, torus = FALSE)
      } else {
        stop("The world provided does not contain the location [x = 0, y = 0]")
      }
    }

    if(home == "center"){
      newTurtles<- setXY(turtles = turtles, xcor = (((world@extent@xmax - world@extent@xmin) / 2) + world@extent@xmin),
                         ycor = (((world@extent@ymax - world@extent@ymin) / 2) + world@extent@ymin),
                         world = world, torus = FALSE)
    }

    if(home == "pCorner"){
      newTurtles<- setXY(turtles = turtles, xcor = minPxcor(world), ycor = minPycor(world), world = world, torus = FALSE)
    }

    if(home == "corner"){
      newTurtles<- setXY(turtles = turtles, xcor = world@extent@xmin, ycor = world@extent@ymin, world = world, torus = FALSE)
    }

    return(newTurtles)
  }
)


################################################################################
#' x-increment
#'
#' Report the amount by which the \code{turtles}' coordinates xcor would change
#' if the \code{turtles} were
#' to move forward the given distances with their current headings.
#'
#' @inheritParams fargs
#'
#' @param dist    Numeric. Vector of distances the \code{turtles} would have to
#'                move forward to
#'                compute the increment values. Must be of length 1 or of length
#'                \code{turtles}. The default value is \code{dist = 1}.
#'
#' @return Numeric. Vector of length \code{turtles}.
#'
#' @details Report the sine of the \code{turtles}' heading multiplied by the \code{dist}
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
  function(turtles, dist = 1) {
    standardGeneric("dx")
  })

#' @export
#' @rdname dx
setMethod(
  "dx",
  signature = c("SpatialPointsDataFrame", "numeric"),
  definition = function(turtles, dist) {
    xIncr <- round(sin(rad(turtles@data$heading)) * dist, digits = 5)
    return(xIncr)
  }
)

#' @export
#' @rdname dx
setMethod(
  "dx",
  signature = c("SpatialPointsDataFrame", "missing"),
  definition = function(turtles) {
    dx(turtles = turtles, dist = 1)
  }
)


################################################################################
#' y-increment
#'
#' Report the amount by which the \code{turtles}' coordinates ycor would change
#' if the \code{turtles} were
#' to move forward the given distances with their current headings.
#'
#' @inheritParams fargs
#'
#' @inheritParams dx
#'
#' @return Numeric. Vector of length \code{turtles}.
#'
#' @details Report the cosine of the \code{turtles}' heading multiplied by the \code{dist}
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
  function(turtles, dist = 1) {
    standardGeneric("dy")
  })

#' @export
#' @rdname dy
setMethod(
  "dy",
  signature = c("SpatialPointsDataFrame", "numeric"),
  definition = function(turtles, dist) {
    yIncr <- round(cos(rad(turtles@data$heading)) * dist, digits = 5)
    return(yIncr)
  }
)

#' @export
#' @rdname dy
setMethod(
  "dy",
  signature = c("SpatialPointsDataFrame", "missing"),
  definition = function(turtles) {
    dy(turtles = turtles, dist = 1)
  }
)


################################################################################
#' Kill turtles
#'
#' Kill selected turtles.
#'
#' @inheritParams fargs
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with the selected
#'         ones removed.
#'
#' @details The "who" numbers of the remaining \code{turtles} are unchanged.
#'
#'          To remove all the \code{turtles}, you can use \code{clearTurtles()}.
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
#' Create \code{n} new turtles from a parent turtle.
#'
#' @inheritParams fargs
#'
#' @param who     Integer. The "who" number of the parent turtle.
#'
#' @param breed   Character. Vector of "breed" names. Must be of length 1 or of length
#'                \code{n}. If missing,
#'                the created turtles are of the same "breed" as the parent turtle.
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with the new
#'         hatched ones.
#'
#' @details The parent turtle must be contained in the \code{turtles}.
#'
#'          The created turtles inherit of all the data from the parent turtle,
#'          except for the "breed", if specified otherwise, and for the "who" numbers.
#'          The "who" numbers of the turtles created take on following the highest
#'          "who" number among the \code{turtles}.
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
    if(length(breed) == 1){
      breed <- rep(breed, n)
    }
    newData[(nrow(turtles) + 1):nrow(newData), "breed"] <- breed

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
#' Can the turtles move?
#'
#' Report \code{TRUE} if a turtle can move the given distance without leaving
#' the \code{world}'s extent, report \code{FALSE} otherwise.
#'
#' @inheritParams fargs
#'
#' @inheritParams fd
#'
#' @return Logical. Vector of length \code{turtles}.
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
#' canMove(world = w1, turtles = t1, dist = 1:10)
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
  function(world, turtles, dist) {
    standardGeneric("canMove")
  })

#' @export
#' @rdname canMove
setMethod(
  "canMove",
  signature = c("NLworlds", "SpatialPointsDataFrame", "numeric"),
  definition = function(world, turtles, dist) {
    wrapFalse <- fd(world = world, turtles = turtles, dist = dist, torus = FALSE)
    wrapTrue <- fd(world = world, turtles = turtles, dist = dist, torus = TRUE)
    test <- wrapFalse@coords == wrapTrue@coords
    return(test[,1] & test[,2])
  }
)


################################################################################
#' Random xcor
#'
#' Report \code{n} random xcor coordinates within the \code{world}'s extent.
#'
#' @inheritParams fargs
#'
#' @return Numeric. Vector of length \code{n} of xcor coordinates.
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
#' Random ycor
#'
#' Report \code{n} random ycor coordinates within the \code{world}'s extent.
#'
#' @inheritParams fargs
#'
#' @return Numeric. Vector of length \code{n} of ycor coordinates.
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
#' Directions towards
#'
#' Report the directions of each \code{agents} towards each corresponding \code{agents2}.
#'
#' @inheritParams fargs
#'
#' @return Numeric. Vector of angles in degrees of length equal to the largest
#'         number of agents/locations between \code{agents} and \code{agents2}.
#'
#' @details \code{agents} and \code{agents2} must have the same number of agents/locations
#'          or if different, one of them must have only one agent/location. If
#'          \code{agents} and \code{agents2} have the same number of agents/locations,
#'          the directions are calculated for each pair \code{agents[i]} and \code{agents2[i]}
#'          and not for each \code{agents} towards all the \code{agents2}.
#'
#'          If \code{torus = TRUE} and the distance from one \code{agents} to
#'          its corresponding \code{agents2} is smaller around the
#'          sides of the \code{world} than across it, then the direction to \code{agents2}
#'          going around the sides of the \code{world} is returned.
#'
#'          The direction from a patch to its location returns 0; the direction from
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
#' towards(world = w1, agents = patches(w1), agents2 = cbind(x = 0, y = 0))
#' t1 <- createTurtles(n = 10, world = w1)
#' towards(world = w1, agents = t1, agents2 = cbind(x = 0, y = 0))
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
  function(world, agents, agents2, torus = FALSE) {
    standardGeneric("towards")
  })

#' @export
#' @rdname towards
setMethod(
  "towards",
  signature = c(world = "NLworlds", agents = "matrix", agents2 = "matrix"),
  definition = function(world, agents, agents2, torus) {

    if(torus == FALSE){

      heading <- deg(atan2(agents2[,1] - agents[,1], agents2[,2] - agents[,2])) # angles between -180 and 180
      heading[heading < 0] <- heading[heading < 0] + 360

    } else {

      if(nrow(agents2) == 1 & nrow(agents) != 1){
        agents2 <- cbind(x = rep(agents2[,1], nrow(agents)), y = rep(agents2[,2], nrow(agents)))
      }
      if(nrow(agents) == 1 & nrow(agents2) != 1){
        agents <- cbind(x = rep(agents[,1], nrow(agents2)), y = rep(agents[,2], nrow(agents2)))
      }

      # Need to create coordinates for "agents2" in a wrapped world
      # For all the 8 possibilities of wrapping (to the left, right, top, bottom and 4 corners)
      # Find the smallest distances across or around the world

      to1 <- cbind(agents2[,1] - (world@extent@xmax - world@extent@xmin), agents2[,2] + (world@extent@ymax - world@extent@ymin))
      to2 <- cbind(agents2[,1], agents2[,2] + (world@extent@ymax - world@extent@ymin))
      to3 <- cbind(agents2[,1] + (world@extent@xmax - world@extent@xmin), agents2[,2] + (world@extent@ymax - world@extent@ymin))
      to4 <- cbind(agents2[,1] - (world@extent@xmax - world@extent@xmin), agents2[,2])
      to5 <- cbind(agents2[,1] + (world@extent@xmax - world@extent@xmin), agents2[,2])
      to6 <- cbind(agents2[,1] - (world@extent@xmax - world@extent@xmin), agents2[,2] - (world@extent@ymax - world@extent@ymin))
      to7 <- cbind(agents2[,1], agents2[,2] - (world@extent@ymax - world@extent@ymin))
      to8 <- cbind(agents2[,1] + (world@extent@xmax - world@extent@xmin), agents2[,2] - (world@extent@ymax - world@extent@ymin))

      # All distances in a wrapped world
      dist_agents2 <- pointDistance(p1 = agents, p2 = agents2, lonlat = FALSE, allpairs = FALSE)
      dist_to1 <- pointDistance(p1 = agents, p2 = to1, lonlat = FALSE, allpairs = FALSE)
      dist_to2 <- pointDistance(p1 = agents, p2 = to2, lonlat = FALSE, allpairs = FALSE)
      dist_to3 <- pointDistance(p1 = agents, p2 = to3, lonlat = FALSE, allpairs = FALSE)
      dist_to4 <- pointDistance(p1 = agents, p2 = to4, lonlat = FALSE, allpairs = FALSE)
      dist_to5 <- pointDistance(p1 = agents, p2 = to5, lonlat = FALSE, allpairs = FALSE)
      dist_to6 <- pointDistance(p1 = agents, p2 = to6, lonlat = FALSE, allpairs = FALSE)
      dist_to7 <- pointDistance(p1 = agents, p2 = to7, lonlat = FALSE, allpairs = FALSE)
      dist_to8 <- pointDistance(p1 = agents, p2 = to8, lonlat = FALSE, allpairs = FALSE)

      # Which distance is the minimum
      allDist <- cbind(dist_agents2, dist_to1, dist_to2, dist_to3, dist_to4, dist_to5, dist_to6, dist_to7, dist_to8)
      distMin <- apply(allDist, 1, min)

      toShortest <- agents2
      for(i in 1:nrow(agents)){
        # All the possibilities for each agents (i.e., agents2 and the wrapped agents2)
        allToCoords <- rbind(agents2[i,], to1[i,], to2[i,], to3[i,], to4[i,], to5[i,], to6[i,], to7[i,], to8[i,])
        toShortest[i,] <- allToCoords[match(distMin[i], allDist[i,]),] # if ties, take the first match (good because favor the non wrapped distances)
      }

      heading <- deg(atan2(toShortest[,1] - agents[,1], toShortest[,2] - agents[,2])) # angles between -180 and 180
      heading[heading < 0] <- heading[heading < 0] + 360
    }

    return(heading)
  }
)

#' @export
#' @rdname towards
setMethod(
  "towards",
  signature = c(world = "NLworlds", agents = "SpatialPointsDataFrame", agents2 = "matrix"),
  definition = function(world, agents, agents2, torus) {
    heading <- towards(world = world, agents = agents@coords, agents2 = agents2, torus = torus)
    # The direction to a turtle's location return the turtle's heading
    heading <- ifelse(agents@coords[,1] == agents2[,1] & agents@coords[,2] == agents2[,2], agents@data$heading, heading)
    return(heading)
  }
)

#' @export
#' @rdname towards
setMethod(
  "towards",
  signature = c(world = "NLworlds", agents = "matrix", agents2 = "SpatialPointsDataFrame"),
  definition = function(world, agents, agents2, torus) {
    towards(world = world, agents = agents, agents2 = agents2@coords, torus = torus)
  }
)

#' @export
#' @rdname towards
setMethod(
  "towards",
  signature = c(world = "NLworlds", agents = "SpatialPointsDataFrame", agents2 = "SpatialPointsDataFrame"),
  definition = function(world, agents, agents2, torus) {
    heading <- towards(world = world, agents = agents@coords, agents2 = agents2@coords, torus = torus)
    # The direction to a turtle's location return the turtle's heading
    heading <- ifelse(agents@coords[,1] == agents2@coords[,1] & agents@coords[,2] == agents2@coords[,2], agents@data$heading, heading)
    return(heading)
  }
)


################################################################################
#' Face something
#'
#' Set the \code{turtles}' heading towards \code{agents2}.
#'
#' @inheritParams fargs
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with updated headings.
#'
#' @details The number of agents/locations in \code{agents2} must be equal to 1 or
#'          to the length of \code{turtles}.
#'
#'          If \code{torus = TRUE} and the distance from one \code{turtles} to
#'          its corresponding agent/location \code{agents2} is smaller around the
#'          sides of the \code{world} than across it, then the direction to the agent/location
#'          \code{agents2} going around the sides of the \code{world} is given to the turtle.
#'
#'          If a turtle is facing its own location, its heading does not change.
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
#' t1 <- face(world = w1, turtles = t1, agents2 = cbind(x = 0, y = 0))
#' t1 <- fd(world = w1, turtles = t1, dist = 0.5)
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
  function(world, turtles, agents2, torus = FALSE) {
    standardGeneric("face")
  })

#' @export
#' @rdname face
setMethod(
  "face",
  signature = c(world = "NLworlds", turtles = "SpatialPointsDataFrame", agents2 = "matrix"),
  definition = function(world, turtles, agents2, torus) {

    newHeading <- towards(world = world, agents = turtles, agents2 = agents2, torus = torus)
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
  signature = c(world = "NLworlds", turtles = "SpatialPointsDataFrame", agents2 = "SpatialPointsDataFrame"),
  definition = function(world, turtles, agents2, torus) {
    face(world = world, turtles = turtles, agents2 = agents2@coords, torus = torus)
  }
)


################################################################################
#' Rotate to the left
#'
#' Rotate the \code{turtles}'s headings to the left of \code{angle} degrees.
#'
#' @inheritParams fargs
#'
#' @param angle   Numeric. Vector of angles in degrees by which to rotate the \code{turtles}'
#'                headings. Must be of length 1 or of length \code{turtles}.
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with updated "heading" values.
#'
#' @details If a given \code{angle} value is negative, then the turtle rotates to the right.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#left}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' t1 <- createTurtles(n = 10, world = w1)
#' of(t1, tVar = "heading")
#' t1 <- left(turtles = t1, angle = 180)
#' of(t1, tVar = "heading")
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
  function(turtles, angle) {
    standardGeneric("left")
  })

#' @export
#' @rdname left
setMethod(
  "left",
  signature = c("SpatialPointsDataFrame", "numeric"),
  definition = function(turtles, angle) {
    newHeading <- turtles@data$heading - angle
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
#' Rotate the \code{turtles}'s headings to the right of \code{angle} degrees.
#'
#' @inheritParams fargs
#'
#' @inheritParams left
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with updated "heading" values.
#'
#' @details If a given \code{angle} value is negative, then the turtle rotates to the left.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#right}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' t1 <- createTurtles(n = 10, world = w1)
#' of(t1, tVar = "heading")
#' t1 <- right(turtles = t1, angle = 180)
#' of(t1, tVar = "heading")
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
  function(turtles, angle) {
    standardGeneric("right")
  })

#' @export
#' @rdname right
setMethod(
  "right",
  signature = c("SpatialPointsDataFrame", "numeric"),
  definition = function(turtles, angle) {
    left(turtles = turtles, angle = -angle)
  }
)


################################################################################
#' Move downhill
#'
#' Move the \code{turtles} to their neighboring patch with the lowest value.
#'
#' @inheritParams fargs
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with updated
#'         coordinates and updated data for their "heading" values and
#'         previous coordinates "prevX"
#'         and "prevY".
#'
#' @details If no neighboring patch has a smaller value than the patch where the
#'          turtle is currently located on, the turtle stays on this patch. It still
#'          moves to the patch center if it was not already on it.
#'
#'          If there are multiple neighboring patches with the same lowest value,
#'          the turtle chooses one patch randomly.
#'
#'          If a turtle is located on a patch on the edge
#'          of the \code{world} and \code{torus = FALSE}, it has fewer
#'          neighborhing patches as options to move than \code{nNeighbors}; if
#'          \code{torus = TRUE}, the turtle can move on the other side of the \code{world} to
#'          move downhill and its choice of neighborhing patches is always equals to
#'          \code{nNeighbors}.
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
#' @importFrom data.table rbindlist
#' @importFrom car some
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

    pListDF <- lapply(pNeighbors, as.data.frame)
    pDF <- as.data.frame(rbindlist(pListDF)) # faster than do.call(rbind, ...)
    pDF$id <- rep(1:length(turtles), unlist(lapply(pNeighbors, nrow)))
    tDF <- data.frame(patchHere(world, turtles), id = 1:length(turtles))
    allPatches <- rbind(pDF, tDF) # neighbors patches + patches under the turtles

    allPatches$cellNum <- cellFromPxcorPycor(world = world, pxcor = allPatches$pxcor, pycor = allPatches$pycor)
    allPatches$pVal <- pValues[allPatches$cellNum]

    pMin <- aggregate(pVal ~ id, allPatches, function(x) min(x)) # minimum patch value per id
    pMinCoords <- merge(pMin, allPatches)
    pMinCoords1 <- pMinCoords[tapply(1:nrow(pMinCoords), pMinCoords$id, some, 1),] # select randomly one row per id
    pMinCoords1 <- pMinCoords1[order(pMinCoords1$id),] # order by turtles
    pMinCoords2 <- cbind(pxcor = pMinCoords1[,3], pycor = pMinCoords1[,4])

    newTurtles <- face(world = world, turtles = turtles, agents2 = pMinCoords2, torus = torus)
    newTurtles <- moveTo(turtles = newTurtles, agents = pMinCoords2)
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
#' Move the \code{turtles} to their neighboring patch with the highest value.
#'
#' @inheritParams fargs
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with updated
#'         coordinates and updated data for their "heading" values and
#'         previous coordinates "prevX"
#'         and "prevY".
#'
#' @details If no neighboring patch has a larger value than the patch where the
#'          turtle is currently located on, the turtle stays on this patch. It still
#'          moves to the patch center if it was not already on it.
#'
#'          If there are multiple neighboring patches with the same highest value,
#'          the turtle chooses one patch randomly.
#'
#'          If a turtle is located on a patch on the edge
#'          of the \code{world} and \code{torus = FALSE}, it has fewer
#'          neighborhing patches as options to move than \code{nNeighbors}; if
#'          \code{torus = TRUE}, the turtle can move on the other side of the \code{world} to
#'          move uphill and its choice of neighborhing patches is always equals to
#'          \code{nNeighbors}.
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
#' Patches ahead
#'
#' Report the coordinates of the patches at the given
#' distances of the \code{turtles} in the direction of their headings.
#'
#' @inheritParams fargs
#'
#' @param dist   Numeric. Vector of distances from the \code{turtles}. \code{dist} must be
#'               of length 1 or of length \code{turtles}.
#'
#' @return Matrix (ncol = 2) with the first column "pxcor" and the second column
#'         "pycor" representing the coordinates of the patches at the distances \code{dist}
#'         and \code{turtles}'s headings directions
#'         of \code{turtles}. The order of the patches follows the order of the \code{turtles}.
#'
#' @details If \code{torus = FALSE} and the patch at distance \code{dist} of a turtle
#'          is outside the \code{world}'s extent, \code{NA}
#'          are returned for the patch coordinates. If \code{torus = TRUE}, the patch
#'          coordinates from a wrapped \code{world} are returned.
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
    pAhead <- patch(world = world, x = xcor, y = ycor, duplicate = TRUE, torus = torus, out = TRUE)
    return(pAhead)

  }
)


################################################################################
#' Patches here
#'
#' Report the coordinates of the patches under the \code{turtles}
#' locations.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2) with the first column "pxcor" and the second column
#'         "pycor" representing the coordinates of the patches at the \code{turtles}
#'         location. The order of the patches follows the order of the \code{turtles}.
#'
#' @details If a turtle is located outside of the \code{world}'s extent, \code{NA} are returned
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

    pTurtles <- patch(world = world, x = turtles@coords[,1], y = turtles@coords[,2], duplicate = TRUE, out = TRUE)
    return(pTurtles)

  }
)


################################################################################
#' Patches on the left
#'
#' Report the coordinates of the patches at the given distances of the \code{turtles}
#' on the left of their headings.
#'
#' @inheritParams fargs
#'
#' @inheritParams patchAhead
#'
#' @param angle   Numeric. Vector of angles in degrees by which the \code{turtle}'s
#'                headings should rotate to locate the patches. Must be of length 1 or of
#'                length \code{turtles}.
#'
#' @return Matrix (ncol = 2) with the first column "pxcor" and the second
#'         column "pycor" representing the coordinates of the patches at \code{dist}
#'         distances of the \code{turtles} and \code{angle} to the left of their headings.
#'         The order of the patches follows the order of the \code{turtles}.
#'
#' @details If a given \code{dist} value is negative, then the turtle would move backward.
#'          If a given \code{angle} value is negative, then the turtle would rotate to the right.
#'
#'          If \code{torus = FALSE} and the patch at distance \code{dist} of a turtle
#'          and \code{angle} degrees to the left of its heading is outside the \code{world}'s extent, \code{NA}
#'          are returned for the patch coordinates. If \code{torus = TRUE}, the patch
#'          coordinates from a wrapped \code{world} are returned.
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
#' patchLeft(world = w1, turtles = t1, dist = 2, angle = 90)
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
  function(world, turtles, dist, angle, torus = FALSE) {
    standardGeneric("patchLeft")
  })

#' @export
#' @rdname patchLeft
setMethod(
  "patchLeft",
  signature = c(world = "NLworlds", turtles = "SpatialPointsDataFrame", dist = "numeric", angle = "numeric"),
  definition = function(world, turtles, dist, angle, torus) {

    tLeft <- left(turtles = turtles, angle = angle)
    tFd <- fd(world = world, turtles = tLeft, dist = dist, torus = torus)
    pLeftFd <- patchHere(world = world, turtles = tFd)

    return(pLeftFd)
  }
)


################################################################################
#' Patches on the right
#'
#' Report the coordinates of the patches at the given distances of the \code{turtles}
#' on the right of their headings.
#'
#' @inheritParams fargs
#'
#' @inheritParams patchLeft
#'
#' @return Matrix (ncol = 2) with the first column "pxcor" and the second
#'         column "pycor" representing the coordinates of the patches at \code{dist}
#'         distances of the \code{turtles} and \code{angle} to the right of their headings.
#'         The order of the patches follows the order of the \code{turtles}.
#'
#' @details If a given \code{dist} value is negative, then the turtle would move backward.
#'          If a given \code{angle} value is negative, then the turtle would rotate to the left.
#'
#'          If \code{torus = FALSE} and the patch at distance \code{dist} of a turtle
#'          and \code{angle} degrees to the right of its heading is outside the \code{world}'s extent, \code{NA}
#'          are returned for the patch coordinates. If \code{torus = TRUE}, the patch
#'          coordinates from a wrapped \code{world} are returned.
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
#' patchRight(world = w1, turtles = t1, dist = 2, angle = 90)
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
  function(world, turtles, dist, angle, torus = FALSE) {
    standardGeneric("patchRight")
  })

#' @export
#' @rdname patchRight
setMethod(
  "patchRight",
  signature = c(world = "NLworlds", turtles = "SpatialPointsDataFrame", dist = "numeric", angle = "numeric"),
  definition = function(world, turtles, dist, angle, torus) {
    patchLeft(world = world, turtles = turtles, dist = dist, angle = -angle, torus = torus)
  }
)


################################################################################
#' Set turtles' locations
#'
#' Set the turtles \code{xcor} and \code{ycor} coordinates.
#'
#' @inheritParams fargs
#'
#' @param xcor    Numeric. Vector of x coordinates. Must be of length 1 or
#'                of length \code{turtles}.
#'
#' @param ycor    Numeric. Vector of y coordinates. Must be of length 1 or
#'                of length \code{turtles}.
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with updated coordinates
#'         and updated data for their previous coordinates "prevX" and "prevY"
#'
#' @details \code{world} must be provided if only if \code{torus = TRUE}.
#'
#'          If the given coordinates \code{[xcor, ycor]}
#'          are located outside of the \code{world}'s extent and \code{torus = TRUE},
#'          then the coordinates assigned to the turtle
#'          are the ones from a wrapped \code{word}; if \code{torus = FALSE}, the turtle
#'          is located outside of the \code{world}'s extent with the given coordinates.
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
#' Create \code{n} new turtles on specific \code{patches}.
#'
#' @inheritParams fargs
#'
#' @inheritParams createTurtles
#'
#' @return SpatialPointsDataFrame including the new
#'         sprouted turtles.
#'
#' @details \code{nrow(patches)} must be equal to 1 or to \code{n}.
#'
#'          If \code{turtles} is provided, the new turtles are added to
#'          the \code{turtles} when returned. The who numbers of the sprouted turtles
#'          therefore follow the ones from the \code{turtles}. If no \code{turtles}
#'          is provided, a new SpatialPointsDataFrame is created and the who numbers
#'          start at 0.
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
  function(n, patches, breed, heading, color, turtles) {
    standardGeneric("sprout")
  })

#' @export
#' @rdname sprout
setMethod(
  "sprout",
  signature = c(n = "numeric", patches = "matrix"),
  definition = function(n, patches, breed, heading, color, turtles) {

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
#' Display the variables values for the selected individuals among the \code{turtles}.
#'
#' @inheritParams fargs
#'
#' @return Dataframe (nrow = \code{length(who)}) with the variables for the selected
#'         individuals among the \code{turtles}.
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
#' Move the \code{turtles} to the \code{agents}' locations.
#'
#' @inheritParams fargs
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with updated coordinates
#'         and updated data for their previous coordinates "prevX" and "prevY".
#'
#' @details The number of \code{agents} must be equal to 1 or to
#'          length \code{turtles}.
#'
#'          The \code{turtle}'s headings are not affected with this function.
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
#' t1 <- moveTo(turtles = t1, agents = turtle(t1, who = 0))
#' Plot(t1, addTo ="w1")
#' t1 <- moveTo(turtles = t1, agents = patch(w1, 9, 9))
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
  function(turtles, agents) {
    standardGeneric("moveTo")
  })

#' @export
#' @rdname moveTo
setMethod(
  "moveTo",
  signature = c("SpatialPointsDataFrame", "matrix"),
  definition = function(turtles, agents) {
    setXY(turtles = turtles, xcor = as.numeric(agents[,1]), ycor = as.numeric(agents[,2]), torus = FALSE)
  }
)

#' @export
#' @rdname moveTo
setMethod(
  "moveTo",
  signature = c("SpatialPointsDataFrame", "SpatialPointsDataFrame"),
  definition = function(turtles, agents) {
    moveTo(turtles = turtles, agents = agents@coords)
  }
)


################################################################################
#' Random turtles coordinates
#'
#' Report \code{n} random xcor and ycor coordinates within the \code{world}'s extent.
#'
#' @inheritParams fargs
#'
#' @return Matrix (ncol = 2, nrow = \code{n}) with the first column "xcor" and the second
#'         column "ycor".
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
#' Report \code{TRUE} if a turtle exists inside the \code{turtles} agentset, report
#' \code{FALSE} otherwise.
#'
#' @inheritParams fargs
#'
#' @param who     Integer. Vector of the "who" numbers of the turtles to check for existence.
#'
#' @param breed   Characters. Vector of "breed" names for the turtles to check
#'                for existence. Must be of length 1 or of length \code{n}.
#'                If missing, there is
#'                no distinction based upon "breed".
#'
#' @return Logical. Vector of \code{TRUE} or \code{FALSE} if the turtles with
#'         the given \code{who} numbers and potentially given \code{breed} exist or not
#'         in the given \code{turtles} agentset.
#'
#' @details If \code{breed} is provided, the turtle with the given \code{who} number
#'          AND given \code{breed} must exists inside \code{turtles} for \code{TRUE}
#'          to be returned.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#is-of-type}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10), breed = c(rep("sheep", 5), rep("wolf", 5)))
#' tExist(turtles = t1, who = 3, breed = "sheep")
#' tExist(turtles = t1, who = 9, breed = "sheep")
#' tExist(turtles = t1, who = c(3, 9))
#'
#'
#' @export
#' @docType methods
#' @rdname tExist
#'
#' @author Sarah Bauduin
#'
setGeneric(
  "tExist",
  function(turtles, who, breed) {
    standardGeneric("tExist")
  })

#' @export
#' @rdname tExist
setMethod(
  "tExist",
  signature = c("SpatialPointsDataFrame", "numeric", "missing"),
  definition = function(turtles, who) {

    tExist <- who %in% turtles@data$who
    return(tExist)

  }
)

#' @export
#' @rdname tExist
setMethod(
  "tExist",
  signature = c("SpatialPointsDataFrame", "numeric", "character"),
  definition = function(turtles, who, breed) {

    whoExist <- tExist(turtles = turtles, who = who)

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
#' Select turtles
#'
#' Report the individuals among \code{turtles} based on their \code{who} numbers
#' and \code{breed}.
#'
#' @inheritParams fargs
#'
#' @param breed   Characters. Vector of "breed" names to select the \code{turtles}.
#'                Must be of length 1 or of length \code{turtles}.
#'                If missing, there is
#'                no distinction based upon "breed".
#'
#' @return SpatialPointsDataFrame of the selected turtles.
#'
#' @details If no turtle matches the given \code{who} numbers, with potentially the given
#'          \code{breed}, inside \code{turtles}, then an empty SpatialPointsDataFrame is returned.
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
#' Report the individuals among \code{turtles} that are at the same locations as the \code{agents}.
#'
#' @inheritParams fargs
#'
#' @inheritParams turtle
#'
#' @return SpatialPointsDataFrame representing any individuals from \code{turtles} of the given
#'        \code{breed}, if speficied,
#'         located at the same locations as any \code{agents}.
#'
#' @details The \code{agents} must be located inside the
#'          \code{world}'s extent.
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
#' Report the indviduals among \code{turtles} that are located on the patches at
#' \code{(dx, dy)} distances of the
#' \code{agents}.
#'
#' @inheritParams fargs
#'
#' @inheritParams turtle
#'
#' @return SpatialPointsDataFrame representing the individuals among \code{turtles}
#'         of the given \code{breed}, if specified,
#'         which are located on the patches at \code{(dx, dy)} distances of the
#'         \code{agents}.
#'
#' @details If the patch at distance \code{(dx, dy)}
#'          of an agent is outside of the \code{world}'s extent and \code{torus = FALSE}, no turtle is returned;
#'          if \code{torus = TRUE}, the turtle located on the patch whose coordinates
#'          are defined from the wrapped \code{world} is returned.
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
#'            by \code{createOTurtles()} representing the moving agents.
#'
#' @return SpatialPointsDataFrame with all the turtles provided in the inputs.
#'
#' @details Duplicated turtles are removed. Duplicates are identified based on the
#'          turtles' coordinates and all their variables values.
#'
#'          This functions does not affect the turtles coordinates and variables.
#'          Therefore there may be multiple turtles with the same variable value (e.g.,
#'          "who" number, and color). This must be taken care of prior or later
#'          using this function to avoid further confusions.
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
#' @importFrom data.table rbindlist
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

    allList <- lapply(dots, function(x){cbind(x@coords, x@data)})
    allDf <- rbindlist(allList)
    allDf <- as.data.frame(unique(allDf))

    allTurtles <- SpatialPointsDataFrame(coords = cbind(xcor = allDf[,1], ycor = allDf[,2]), data = allDf[,3:ncol(allDf)])
    return(allTurtles)
  }
)


################################################################################
#' New turtles variable
#'
#' Create a new variable for the turtles.
#'
#' @inheritParams fargs
#'
#' @param tVar    Character. the name of the \code{turtles} variable to create.
#'
#' @param tVal    Vector representing the values of \code{tVar}.
#'                Must be of length 1 or of length \code{turtles}.
#'                If missing, \code{NA} is given.
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with the new
#'         variable \code{tVar} added.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#turtles-own}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' t1 <- createTurtles(n = 5, coords = cbind(xcor = 0, ycor = 0))
#' t1 <- turtlesOwn(turtles = t1, tVar = "sex", tVal = c("F", "F", "F", "M", "M"))
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
  function(turtles, tVar, tVal) {
    standardGeneric("turtlesOwn")
  })

#' @export
#' @rdname turtlesOwn
setMethod(
  "turtlesOwn",
  signature = c("SpatialPointsDataFrame", "character", "missing"),
  definition = function(turtles, tVar) {
    newData <- cbind(turtles@data, rep(NA, length(turtles)))
    colnames(newData)[ncol(turtles@data) + 1] <- tVar
    turtles@data <- newData
    return(turtles)
  }
)

#' @export
#' @rdname turtlesOwn
setMethod(
  "turtlesOwn",
  signature = c("SpatialPointsDataFrame", "character", "ANY"),
  definition = function(turtles, tVar, tVal) {
    newTurtles <- turtlesOwn(turtles = turtles, tVar = tVar)
    turtles@data[,tVar] <- tVal
    return(turtles)
  }
)


################################################################################
#' Substract headings
#'
#' Compute the difference between headings.
#'
#' @param angle1 SpatialPointsDataFrame created by \code{createTurtles()} or
#'               by \code{createOTurtles()} representing the moving agents, or
#'
#'               Numeric. Vector of angles.
#'
#' @param angle2 SpatialPointsDataFrame created by \code{createTurtles()} or
#'               by \code{createOTurtles()} representing the moving agents, or
#'
#'               Numeric. Vector of angles.
#'
#' @param range360 Logical. If \code{range360 = TRUE}, returned values are between 0° and
#'                 360°; if \code{range360 = FALSE}, returned values are between -180° and 180°.
#'                 Default is \code{range360 = FALSE}.
#'
#' @return Numeric. Vector of the smallest angles in degrees
#'         by which \code{angle1} could be rotated to produce \code{angle2}
#'         (i.e., the target heading).
#'
#' @details This function does the opposite as the one in NetLogo where
#'          \code{angle1} is the target heading.
#'
#'         \code{angle1} and \code{angle2} must be of the same length or if different,
#'         one of them must be of length 1.
#'
#'          Positive values mean clockwise rotations, negative value mean counterclockwise rotations.
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
#' subHeadings(angle1 = t1, angle2 = 0)
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
  function(angle1, angle2, range360 = FALSE) {
    standardGeneric("subHeadings")
  })

#' @export
#' @rdname subHeadings
setMethod(
  "subHeadings",
  signature = c(angle1 = "numeric", angle2 = "numeric"),
  definition = function(angle1, angle2, range360) {

    if(length(angle2) != length(angle1)){
      if(length(angle2) == 1){
        angle2 <- rep(angle2, length(angle1))
      } else if(length(angle1) == 1){
        angle1 <- rep(angle1, length(angle2))
      } else {
        stop("angle1 and angle2 must be of the same length or one must be of length 1")
      }
    }

    angles <- deg(atan2(sin(rad(angle2) - rad(angle1)), cos(rad(angle2) - rad(angle1))))

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
  signature = c(angle1 = "SpatialPointsDataFrame", angle2 = "numeric"),
  definition = function(angle1, angle2, range360) {
    subHeadings(angle1 = angle1@data$heading, angle2 = angle2, range360 = range360)
  }
)
#' @export
#' @rdname subHeadings
setMethod(
  "subHeadings",
  signature = c(angle1 = "numeric", angle2 = "SpatialPointsDataFrame"),
  definition = function(angle1, angle2, range360) {
    subHeadings(angle1 = angle1, angle2 = angle2@data$heading, range360 = range360)
  }
)
#' @export
#' @rdname subHeadings
setMethod(
  "subHeadings",
  signature = c(angle1 = "SpatialPointsDataFrame", angle2 = "SpatialPointsDataFrame"),
  definition = function(angle1, angle2, range360) {
    subHeadings(angle1 = angle1@data$heading, angle2 = angle2@data$heading, range360 = range360)
  }
)


################################################################################
#' Others
#'
#' Report an agentset of all \code{agents} except specific ones.
#'
#' @inheritParams fargs
#'
#' @param except Matrix (ncol = 2) with the first column "pxcor" and the second
#'               column "pycor" representing the patches coordinates, or
#'
#'               SpatialPointsDataFrame created by \code{createTurtles()} or
#'               by \code{createOTurtles()} representing the moving agents.
#'
#' @return Matrix (ncol = 2) with the first column "pxcor" and the second
#'         column "pycor" representing the patches in \code{agents} without
#'         the ones in \code{except}, or
#'
#'         SpatialPointsDataFrame representing the turtles in \code{agents} without
#'         the ones in \code{except}.
#'
#' @details Both \code{agents} and \code{except} must be of the same class (e.g., both
#'          patches or both turtles).
#'
#'          Carefull: this function removes turtles only based on similar "who" numbers
#'          and "breed" names.
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
#' Relocate the \code{turtles} on a circle centered on the \code{world}.
#'
#' @inheritParams fargs
#'
#' @param radius  Numeric. Radius of the circle.
#'
#' @return SpatialPointsDataFrame representing the \code{turtles} with updated
#'         coordinates and updated data for their "heading" values and
#'         previous coordinates "prevX"
#'         and "prevY".
#'
#' @details The \code{turtles} point outwards.
#'
#'          If the
#'          \code{radius} value leads turtles outside of the \code{world}'s extent
#'          and \code{torus = TRUE}, they are
#'          relocated on the other sides of the \code{world}, inside its extent; if
#'          \code{torus = FALSE}, the turtles are located past
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
    fd(world = world, turtles = turtles, dist = radius, torus = torus, out = TRUE)
  }
)


################################################################################
#' Values of a turtles variable
#'
#' Report the \code{turtles} values for the requested variable.
#'
#' @inheritParams fargs
#'
#' @param tVar    Character. The name of one turtles' variable to report. Can be equal to
#'                \code{"coords"}, \code{"xcor"},
#'                \code{"ycor"}, \code{"who"}, \code{"heading"}, \code{"prevCoords"},
#'                \code{"prevX"}, \code{"prevY"}, \code{"breed"}, \code{"color"} or
#'                any of the variables created with \code{turtlesOwn()}.
#'
#' @return Vector of \code{tVar} values for the \code{turtles}. The class depends
#'         of the variable class. The order of the vector follows the order
#'         of the \code{turtles}.
#'
#' @seealso \url{https://ccl.northwestern.edu/netlogo/docs/dictionary.html#of}
#'
#' @references Wilensky, U. 1999. NetLogo. http://ccl.northwestern.edu/netlogo/.
#'             Center for Connected Learning and Computer-Based Modeling,
#'             Northwestern University. Evanston, IL.
#'
#' @examples
#' w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
#' t1 <- createTurtles(n = 10, coords = randomXYcor(w1, n = 10))
#' of(turtles = t1, tVar = "heading")
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
  function(turtles, tVar) {
    standardGeneric("of")
  })

#' @export
#' @rdname of
setMethod(
  "of",
  signature = c("SpatialPointsDataFrame", "character"),
  definition = function(turtles, tVar) {
    if(tVar == "coords"){
      return(turtles@coords)
    } else if(tVar == "xcor"){
      return(turtles@coords[,1])
    } else if(tVar == "ycor"){
      return(turtles@coords[,2])
    } else if(tVar == "prevCoords"){
      return(cbind(turtles@data$prevX, turtles@data$prevY))
    } else {
      return(turtles@data[,tVar])
    }
  }
)
