test_that("createTurtles works with different missing inputs",{
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)

  t1 <- createTurtles(world = w1, n = 10)
  expect_identical(cbind(xcor = rep(2, 10), ycor = rep(2, 10)), t1@coords)
  t2 <- createTurtles(world = w1, n = 10, coords = cbind(xcor = rep(0, 10), ycor = rep(0, 10)))
  expect_identical(cbind(xcor = rep(0, 10), ycor = rep(0, 10)), t2@coords)

  t3 <- createTurtles(world = w1, n = 10, heading = 0)
  expect_identical(rep(0, 10), t3@data$heading)
  t4 <- createTurtles(world = w1, n = 10, heading = 1:10)
  expect_identical(1:10, t4@data$heading)
  t1head <- t1@data$heading >= 0 & t1@data$heading <= 360
  expect_identical(t1head, rep(TRUE, 10))

  t5 <- createTurtles(world = w1, n = 10, breed = "caribou")
  expect_identical(rep("caribou", 10), t5@data$breed)
  t6 <- createTurtles(world = w1, n = 10, breed = c(rep("caribou", 5), rep("moose", 5)))
  expect_identical(c(rep("caribou", 5), rep("moose", 5)), t6@data$breed)
  t1breed <- t1@data$breed == rep("turtle", 10)
  expect_identical(t1breed, rep(TRUE, 10))

  expect_equivalent(length(unique(t1@data$color)), 10)
  expect_equivalent(t1@data$who, 0:9)
  expect_equivalent(t1@data$prevX, rep(NA, 10))
  expect_equivalent(t1@data$prevY, rep(NA, 10))
  expect_equivalent(length(t1), 10)

  # Same tests for the NLworldStack
  w2 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  w1[] <- runif(25)
  w2[] <- runif(25)
  ws <-NLstack(w1, w2)

  t1 <- createTurtles(world = ws, n = 10)
  expect_identical(cbind(xcor = rep(2, 10), ycor = rep(2, 10)), t1@coords)
  t2 <- createTurtles(world = ws, n = 10, coords = cbind(xcor = rep(0, 10), ycor = rep(0, 10)))
  expect_identical(cbind(xcor = rep(0, 10), ycor = rep(0, 10)), t2@coords)

  t3 <- createTurtles(world = ws, n = 10, heading = 0)
  expect_identical(rep(0, 10), t3@data$heading)
  t4 <- createTurtles(world = ws, n = 10, heading = 1:10)
  expect_identical(1:10, t4@data$heading)
  t1head <- t1@data$heading >= 0 & t1@data$heading <= 360
  expect_identical(t1head, rep(TRUE, 10))

  t5 <- createTurtles(world = ws, n = 10, breed = "caribou")
  expect_identical(rep("caribou", 10), t5@data$breed)
  t6 <- createTurtles(world = ws, n = 10, breed = c(rep("caribou", 5), rep("moose", 5)))
  expect_identical(c(rep("caribou", 5), rep("moose", 5)), t6@data$breed)
  t1breed <- t1@data$breed == rep("turtle", 10)
  expect_identical(t1breed, rep(TRUE, 10))

  expect_equivalent(length(unique(t1@data$color)), 10)
  expect_equivalent(t1@data$who, 0:9)
  expect_equivalent(t1@data$prevX, rep(NA, 10))
  expect_equivalent(t1@data$prevY, rep(NA, 10))
  expect_equivalent(length(t1), 10)
})

test_that("createOTurtles works",{
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)

  t1 <- createOTurtles(world = w1, n = 10)
  expect_identical(cbind(xcor = rep(2, 10), ycor = rep(2, 10)), t1@coords)
  expect_identical(seq(0, 360 - (360 / 10), by = 360 /10), t1@data$heading)

  t5 <- createOTurtles(world = w1, n = 10, breed = "caribou")
  expect_identical(rep("caribou", 10), t5@data$breed)
  t6 <- createOTurtles(world = w1, n = 10, breed = c(rep("caribou", 5), rep("moose", 5)))
  expect_identical(c(rep("caribou", 5), rep("moose", 5)), t6@data$breed)
  t1breed <- t1@data$breed == rep("turtle", 10)
  expect_identical(t1breed, rep(TRUE, 10))

  expect_equivalent(length(unique(t1@data$color)), 10)
  expect_equivalent(t1@data$who, 0:9)
  expect_equivalent(t1@data$prevX, rep(NA, 10))
  expect_equivalent(t1@data$prevY, rep(NA, 10))
  expect_equivalent(length(t1), 10)

  # Same tests for the NLworldStack
  w2 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  w1[] <- runif(25)
  w2[] <- runif(25)
  ws <-NLstack(w1, w2)

  t1 <- createOTurtles(world = ws, n = 10)
  expect_identical(cbind(xcor = rep(2, 10), ycor = rep(2, 10)), t1@coords)
  expect_identical(seq(0, 360 - (360 / 10), by = 360 /10), t1@data$heading)

  t5 <- createOTurtles(world = ws, n = 10, breed = "caribou")
  expect_identical(rep("caribou", 10), t5@data$breed)
  t6 <- createOTurtles(world = ws, n = 10, breed = c(rep("caribou", 5), rep("moose", 5)))
  expect_identical(c(rep("caribou", 5), rep("moose", 5)), t6@data$breed)
  t1breed <- t1@data$breed == rep("turtle", 10)
  expect_identical(t1breed, rep(TRUE, 10))

  expect_equivalent(length(unique(t1@data$color)), 10)
  expect_equivalent(t1@data$who, 0:9)
  expect_equivalent(t1@data$prevX, rep(NA, 10))
  expect_equivalent(t1@data$prevY, rep(NA, 10))
  expect_equivalent(length(t1), 10)
})

test_that("fd works",{
 w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  t1 <- createTurtles(world = w1, n = 10, coords = cbind(xcor = rep(0, 10), ycor = rep(0, 10)), heading = 90)
  t2 <- fd(world = w1, turtles = t1, step = 1)
  expect_identical(t1@coords, cbind(xcor = t2@data$prevX, ycor = t2@data$prevY))
  expect_identical(cbind(xcor = t1@coords[,1] + 1, ycor = t1@coords[,2]), t2@coords)
  t3 <- fd(world = w1, turtles = t1, step = 5, torus = FALSE)
  t4 <- fd(world = w1, turtles = t1, step = 5, torus = TRUE)
  expect_identical(cbind(xcor = t1@coords[,1] + 5, ycor = t1@coords[,2]), t3@coords)
  expect_identical(t1@coords, t4@coords)
  t5 <- fd(world = w1, turtles = t1, step = -1, torus = TRUE)
  expect_identical(cbind(xcor = rep(4, 10), ycor = t1@coords[,2]), t5@coords)

  # Same tests for the NLworldStack
  w2 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  w1[] <- runif(25)
  w2[] <- runif(25)
  ws <-NLstack(w1, w2)

  t1 <- createTurtles(world = ws, n = 10, coords = cbind(xcor = rep(0, 10), ycor = rep(0, 10)), heading = 90)
  t2 <- fd(world = ws, turtles = t1, step = 1)
  expect_identical(t1@coords, cbind(xcor = t2@data$prevX, ycor = t2@data$prevY))
  expect_identical(cbind(xcor = t1@coords[,1] + 1, ycor = t1@coords[,2]), t2@coords)
  t3 <- fd(world = ws, turtles = t1, step = 5, torus = FALSE)
  t4 <- fd(world = ws, turtles = t1, step = 5, torus = TRUE)
  expect_identical(cbind(xcor = t1@coords[,1] + 5, ycor = t1@coords[,2]), t3@coords)
  expect_identical(t1@coords, t4@coords)
  t5 <- fd(world = ws, turtles = t1, step = -1, torus = TRUE)
  expect_identical(cbind(xcor = rep(4, 10), ycor = t1@coords[,2]), t5@coords)
})

test_that("bk works",{
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  t1 <- createTurtles(world = w1, n = 10, coords = cbind(xcor = rep(0, 10), ycor = rep(0, 10)), heading = 90)
  t2 <- bk(world = w1, turtles = t1, step = -1)
  expect_identical(t1@coords, cbind(xcor = t2@data$prevX, ycor = t2@data$prevY))
  expect_identical(cbind(xcor = t1@coords[,1] + 1, ycor = t1@coords[,2]), t2@coords)
  t3 <- bk(world = w1, turtles = t1, step = -5, torus = FALSE)
  t4 <- bk(world = w1, turtles = t1, step = -5, torus = TRUE)
  expect_identical(cbind(xcor = t1@coords[,1] + 5, ycor = t1@coords[,2]), t3@coords)
  expect_identical(t1@coords, t4@coords)
  t5 <- bk(world = w1, turtles = t1, step = 1, torus = TRUE)
  expect_identical(cbind(xcor = rep(4, 10), ycor = t1@coords[,2]), t5@coords)

  # Same tests for the NLworldStack
  w2 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  w1[] <- runif(25)
  w2[] <- runif(25)
  ws <-NLstack(w1, w2)

  t1 <- createTurtles(world = ws, n = 10, coords = cbind(xcor = rep(0, 10), ycor = rep(0, 10)), heading = 90)
  t2 <- bk(world = ws, turtles = t1, step = -1)
  expect_identical(t1@coords, cbind(xcor = t2@data$prevX, ycor = t2@data$prevY))
  expect_identical(cbind(xcor = t1@coords[,1] + 1, ycor = t1@coords[,2]), t2@coords)
  t3 <- bk(world = ws, turtles = t1, step = -5, torus = FALSE)
  t4 <- bk(world = ws, turtles = t1, step = -5, torus = TRUE)
  expect_identical(cbind(xcor = t1@coords[,1] + 5, ycor = t1@coords[,2]), t3@coords)
  expect_identical(t1@coords, t4@coords)
  t5 <- bk(world = ws, turtles = t1, step = 1, torus = TRUE)
  expect_identical(cbind(xcor = rep(4, 10), ycor = t1@coords[,2]), t5@coords)

})

test_that("home works",{
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  t1 <- createTurtles(world = w1, n = 10, coords = cbind(xcor = runif(10, 0, 4), ycor = runif(10, 0, 4)))
  t2 <- home(world = w1, turtles = t1, home = "home0")
  expect_identical(cbind(xcor = rep(0, 10), ycor = rep(0, 10)), t2@coords)
  t3 <- home(world = w1, turtles = t1, home = "center")
  expect_identical(cbind(xcor = rep(2, 10), ycor = rep(2, 10)), t3@coords)
  t4 <- home(world = w1, turtles = t1, home = "pCorner")
  expect_identical(cbind(xcor = rep(0, 10), ycor = rep(0, 10)), t4@coords)
  t5 <- home(world = w1, turtles = t1, home = "corner")
  expect_identical(cbind(xcor = rep(-0.5, 10), ycor = rep(-0.5, 10)), t5@coords)

  # Same tests for the NLworldStack
  w2 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  w1[] <- runif(25)
  w2[] <- runif(25)
  ws <-NLstack(w1, w2)

  t1 <- createTurtles(world = ws, n = 10, coords = cbind(xcor = runif(10, 0, 4), ycor = runif(10, 0, 4)))
  t2 <- home(world = ws, turtles = t1, home = "home0")
  expect_identical(cbind(xcor = rep(0, 10), ycor = rep(0, 10)), t2@coords)
  t3 <- home(world = ws, turtles = t1, home = "center")
  expect_identical(cbind(xcor = rep(2, 10), ycor = rep(2, 10)), t3@coords)
  t4 <- home(world = ws, turtles = t1, home = "pCorner")
  expect_identical(cbind(xcor = rep(0, 10), ycor = rep(0, 10)), t4@coords)
  t5 <- home(world = ws, turtles = t1, home = "corner")
  expect_identical(cbind(xcor = rep(-0.5, 10), ycor = rep(-0.5, 10)), t5@coords)

  w3 <- createNLworld(minPxcor = -5, maxPxcor = -1, minPycor = -10, maxPycor = -5)
  expect_error(home(world = w3, turtles = t1, home = "home0"))
})

test_that("dx and dy works",{
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  t1 <- createTurtles(world = w1, n = 1, coords = cbind(xcor = 0, ycor = 0), heading = 90)
  expect_equivalent(dx(turtles = t1), 1)
  expect_equivalent(dx(turtles = t1, step = 2), 2)
  expect_equivalent(dy(turtles = t1), 0)
  expect_equivalent(dy(turtles = t1, step = 2), 0)
  t2 <- createTurtles(world = w1, n = 1, coords = cbind(xcor = 0, ycor = 0), heading = 0)
  expect_equivalent(dx(turtles = t2), 0)
  expect_equivalent(dx(turtles = t2, step = 2), 0)
  expect_equivalent(dy(turtles = t2), 1)
  expect_equivalent(dy(turtles = t2, step = 2), 2)
  t3 <- createTurtles(world = w1, n = 1, coords = cbind(xcor = 0, ycor = 0), heading = 225)
  expect_equivalent(dx(turtles = t3, step = sqrt(2)), -1)
  expect_equivalent(dy(turtles = t3, step = sqrt(2)), -1)
})

test_that("die works",{
  w1 <- createNLworld(minPxcor = 1, maxPxcor = 10, minPycor = 1, maxPycor = 10)
  t1 <- createTurtles(world = w1, n = 10, coords = cbind(xcor = 1:10, ycor = 10:1))
  t2 <- die(turtles = t1, who = 1:9)
  expect_equivalent(length(t2), 1)
  expect_equivalent(t2@coords, cbind(xcor = 1, ycor = 10))
  expect_equivalent(t2@data$who, 0)
  t3 <- die(turtles = t1, who = 0)
  expect_equivalent(length(t3), 9)
  expect_equivalent(t3@coords, cbind(xcor = 2:10, ycor = 9:1))
  expect_equivalent(t3@data$who, 1:9)
})

test_that("hatch works",{
  w1 <- createNLworld(minPxcor = 1, maxPxcor = 10, minPycor = 1, maxPycor = 10)
  t1 <- createTurtles(world = w1, n = 10, coords = cbind(xcor = 1:10, ycor = 10:1))
  t2 <- hatch(turtles = t1, who = 1, n = 1)
  expect_equivalent(length(t2), length(t1) + 1)
  expect_equivalent(t2@coords[11,], c(2, 9))
  expect_equivalent(t2@data$heading[11], t1@data$heading[2])
  expect_identical(t2@data$breed[11], "turtle")
  t3 <- hatch(turtles = t1, who = 4, n = 2, breed = "moose")
  expect_equivalent(length(t3), length(t1) + 2)
  expect_equivalent(t3@coords[11:12,], cbind(xcor = c(5, 5), ycor = c(6, 6)))
  expect_equivalent(t3@data$heading[12], t1@data$heading[5])
  expect_identical(t3@data$breed[11], "moose")
})

test_that("canMove works",{
  w1 <- createNLworld(minPxcor = 1, maxPxcor = 10, minPycor = 1, maxPycor = 10)
  t1 <- createOTurtles(world = w1, n = 4)
  expect_identical(canMove(world = w1, turtles = t1, step = 1), rep(TRUE, 4))
  expect_identical(canMove(world = w1, turtles = t1, step = 4), rep(TRUE, 4))
  expect_identical(canMove(world = w1, turtles = t1, step = 6), rep(FALSE, 4))
  expect_identical(canMove(world = w1, turtles = t1, step = c(1,4,6,4)), c(TRUE, TRUE, FALSE, TRUE))

  w2 <- w1
  w1[] <- runif(100)
  w2[] <- runif(100)
  ws <-NLstack(w1, w2)
  expect_identical(canMove(world = ws, turtles = t1, step = 1), rep(TRUE, 4))
  expect_identical(canMove(world = ws, turtles = t1, step = 4), rep(TRUE, 4))
  expect_identical(canMove(world = ws, turtles = t1, step = 6), rep(FALSE, 4))
  expect_identical(canMove(world = ws, turtles = t1, step = c(1,4,6,4)), c(TRUE, TRUE, FALSE, TRUE))
})

test_that("randomXcor and randomYcor works",{
  w1 <- createNLworld(minPxcor = 1, maxPxcor = 100, minPycor = -100, maxPycor = -1)
  t1 <- createTurtles(world = w1, n = 10000,
                     coords = cbind(xcor = randomXcor(world = w1, n = 10000), ycor = randomYcor(world = w1, n = 10000)))
  expect_identical(canMove(world = w1, turtles = t1, step = 0), rep(TRUE, length(t1)))

  w2 <- w1
  w1[] <- runif(10000)
  w2[] <- runif(10000)
  ws <-NLstack(w1, w2)
  t2 <- createTurtles(world = ws, n = 10000,
                      coords = cbind(xcor = randomXcor(world = ws, n = 10000), ycor = randomYcor(world = ws, n = 10000)))
  expect_identical(canMove(world = ws, turtles = t2, step = 0), rep(TRUE, length(t2)))
})

test_that("towards works",{
  w1 <- createNLworld(minPxcor = 1, maxPxcor = 10, minPycor = 1, maxPycor = 10)

  # Patches to patch
  pTOp <- towards(world = w1, from = patches(world = w1), to = patches(world = w1))
  expect_equivalent(pTOp, rep(0, 100))
  pTOp <- towards(world = w1, from = patches(world = w1), to = patch(world = w1, xcor = 1, ycor = 1), torus = FALSE)
  expect_equivalent(pTOp[1], 180)
  pTOp <- towards(world = w1, from = patches(world = w1), to = patch(world = w1, xcor = 1, ycor = 1), torus = TRUE)
  expect_equivalent(pTOp[1], 0)

  # Patches to location
  pTOl <- towards(world = w1, from = patches(world = w1), to = cbind(x = 4, y = 8), torus = FALSE)
  expect_equivalent(pTOl[21], 90)
  pTOl <- towards(world = w1, from = patches(world = w1), to = cbind(x = 4, y = 8), torus = FALSE)
  expect_equivalent(pTOl[30], 270)
  pTOl <- towards(world = w1, from = patches(world = w1), to = cbind(x = 4, y = 8), torus = TRUE)
  expect_equivalent(pTOl[30], 90)

  # Patches to turtle
  t1 <- createTurtles(world = w1, n = 1)
  pTOt <- towards(world = w1, from = patches(world = w1), to = t1, torus = FALSE)
  expect_equivalent(pTOt[100], 315)
  t2 <- createTurtles(world = w1, n = 1, coord = cbind(xcor = 5.5, ycor = 10.5))
  pTOt <- towards(world = w1, from = patches(world = w1), to = t2, torus = TRUE)
  expect_equivalent(pTOt[95], 135)

  # Turtles to patch
  t3 <- createTurtles(world = w1, n = 4, coords = cbind(xcor = c(2,5,6,7), ycor = c(4,6,2,9)))
  tTOp <- towards(world = w1, from = t3, to = patch(world = w1, xcor = 5, ycor = 4), torus = FALSE)
  expect_equivalent(tTOp[1], 90)
  tTOp <- towards(world = w1, from = t3, to = patch(world = w1, xcor = 7, ycor = 2), torus = TRUE)
  expect_equivalent(tTOp[4], 0)

  # Turtles to location
  tTOl <- towards(world = w1, from = t3, to = cbind(x = 8, y = 4), torus = FALSE)
  expect_equivalent(tTOl[1], 90)
  tTOl <- towards(world = w1, from = t3, to = cbind(x = 8, y = 4), torus = FALSE)
  expect_equivalent(tTOl[3], 45)
  tTOl <- towards(world = w1, from = t3, to = cbind(x = 8, y = 4), torus = TRUE)
  expect_equivalent(tTOl[1], 270)

  # Turtles to turtle
  tTOt <- towards(world = w1, from = t3, to = t3, torus = FALSE)
  expect_equivalent(tTOt, rep(0, 4))
  tTOt <- towards(world = w1, from = t3, to = t2, torus = FALSE)
  expect_equivalent(tTOt[4], 315)

  w2 <- w1
  w1[] <- runif(100)
  w2[] <- runif(100)
  ws <-NLstack(w1, w2)
  # Patches to patch
  pTOp <- towards(world = ws, from = patches(world = ws), to = patch(world = ws, xcor = 1, ycor = 1), torus = TRUE)
  expect_equivalent(pTOp[1], 0)
  # Patches to location
  pTOl <- towards(world = ws, from = patches(world = ws), to = cbind(x = 4, y = 8), torus = TRUE)
  expect_equivalent(pTOl[30], 90)
  # Patches to turtle
  pTOt <- towards(world = ws, from = patches(world = ws), to = t2, torus = TRUE)
  expect_equivalent(pTOt[95], 135)
  # Turtles to patch
  tTOp <- towards(world = ws, from = t3, to = patch(world = ws, xcor = 7, ycor = 2), torus = TRUE)
  expect_equivalent(tTOp[4], 0)
  # Turtles to location
  tTOl <- towards(world = ws, from = t3, to = cbind(x = 8, y = 4), torus = TRUE)
  expect_equivalent(tTOl[1], 270)
  # Turtles to turtle
  tTOt <- towards(world = ws, from = t3, to = t2, torus = FALSE)
  expect_equivalent(tTOt[4], 315)
})

test_that("face works",{
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  t1 <- createTurtles(world = w1, n = 5)
  t2 <- face(world = w1, turtles = t1, to = cbind(x = 2, y = 0))
  expect_identical(t2@data$heading, rep(180, 5))
  t3 <- face(world = w1, turtles = t1, to = patch(world = w1, xcor = 0, ycor = 2))
  expect_identical(t3@data$heading, rep(270, 5))
  t4 <-createTurtles(world = w1, n = 1, coords = cbind(xcor = 1, ycor = 3))
  t5 <- face(world = w1, turtles = t1, to = t4)
  expect_identical(t5@data$heading, rep(315, 5))
  t6 <- face(world = w1, turtles = t4, to = cbind(x = 1, y = 0), torus = FALSE)
  t7 <- face(world = w1, turtles = t4, to = cbind(x = 1, y = 0), torus = TRUE)
  expect_identical(t6@data$heading, 180)
  expect_identical(t7@data$heading, 0)
  t8 <- face(world = w1, turtles = t1, to = t1)
  expect_identical(t8@data$heading, t1@data$heading)
})

test_that("left and right work",{
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  t1 <- createOTurtles(world = w1, n = 4)
  t2 <- left(turtles = t1, nDegrees = 45)
  expect_identical(t2@data$heading, c(315, 45, 135, 225))
  t3 <- right(turtles = t2, nDegrees = 45)
  expect_identical(t3@data$heading, t1@data$heading)
})
