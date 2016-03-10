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

    fdXcor <- round(turtles@coords[,1] + sin(rad(turtles@data$heading)) * step, digits = 5)
    fdYcor <- round(turtles@coords[,2] + cos(rad(turtles@data$heading)) * step, digits = 5)
    if(torus == TRUE){
      tCoords <- wrap(cbind(x = fdXcor, y = fdYcor), extent(world))
      fdXcor <- round(tCoords[,1], digits = 5)
      fdYcor <- round(tCoords[,2], digits = 5)
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
#' t1 <- createTurtles(world = w1, n = 10)
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
#' t1 <- createTurtles(world = w1, n = 10)
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
#' t1 <- createTurtles(world = w1, n = 10)
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
#' t1 <- createTurtles(world = w1, n = 10,
#'                     coords = cbind(xcor = randomXcor(world = w1, n = 10), ycor = randomYcor(world = w1, n = 10)))
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
#' t1 <- createTurtles(world = w1, n = 10,
#'                     coords = cbind(xcor = randomXcor(world = w1, n = 10), ycor = randomYcor(world = w1, n = 10)))
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
