test_that("diffuse works for NLworld with 4 neighbors", {
  w1 <- createNLworld(0, 2, 0, 2)
  w1[] <- c(1,3,6,2,8,10,3,8,2)
  w2 <- diffuse(world = w1, share = 0.6, nNeighbors = 4)

  expect_identical(sum(values(w1)), sum(values(w2)))

  val02 <- 1 - (2 * (1 * 0.6 / 4)) + (3 * 0.6 / 4) + (2 * 0.6 / 4)
  # w1[0,2] - given to w1[1,2] and w1[0,1] + received from w1[1,2] and w1[0,1]
  expect_identical(as.numeric(w2[0,2]),val02) # test a few patches, done by hand

  val12 <- 3 - (3 * (3 * 0.6 / 4)) + (1 * 0.6 / 4) + (6 * 0.6 / 4) + (8 * 0.6 / 4)
  expect_identical(as.numeric(w2[1,2]),val12)

  val11 <- 8 - (4 * (8 * 0.6 / 4)) + (3 * 0.6 / 4) + (2 * 0.6 / 4) + (10 * 0.6 / 4) + (8 * 0.6 / 4)
  expect_identical(as.numeric(w2[1,1]),val11)
})

test_that("diffuse works for NLworldStack with 4 neighbors", {
  w1 <- createNLworld(0, 2, 0, 2)
  w1[] <- c(1,3,6,2,8,10,3,8,2)
  w2 <- createNLworld(0, 2, 0, 2)
  w2[] <- runif(9)
  ws <- NLstack(w1, w2)
  ws2 <- diffuse(world = ws,pVar = "w1", share = 0.6, nNeighbors = 4)

  expect_identical(sum(values(ws$w1)), sum(values(ws2$w1)))

  val02 <- 1 - (2 * (1 * 0.6 / 4)) + (3 * 0.6 / 4) + (2 * 0.6 / 4)
  # w1[0,2] - given to w1[1,2] and w1[0,1] + received from w1[1,2] and w1[0,1]
  expect_identical(as.numeric(ws2$w1[0,2]),val02)

  val12 <- 3 - (3 * (3 * 0.6 / 4)) + (1 * 0.6 / 4) + (6 * 0.6 / 4) + (8 * 0.6 / 4)
  expect_identical(as.numeric(ws2$w1[1,2]),val12)

  val11 <- 8 - (4 * (8 * 0.6 / 4)) + (3 * 0.6 / 4) + (2 * 0.6 / 4) + (10 * 0.6 / 4) + (8 * 0.6 / 4)
  expect_identical(as.numeric(ws2$w1[1,1]),val11)
})

test_that("diffuse works for NLworld with 8 neighbors", {
  w1 <- createNLworld(0, 2, 0, 2)
  w1[] <- c(1,3,6,2,8,10,3,8,2)
  w2 <- diffuse(world = w1, share = 0.6, nNeighbors = 8)

  expect_identical(sum(values(w1)), sum(values(w2)))

  val02 <- 1 - (3 * (1 * 0.6 / 8)) + (3 * 0.6 / 8) + (2 * 0.6 / 8) + (8 * 0.6 / 8)
  # w1[0,2] - given to w1[1,2], w1[0,1] and w1[1,1] + received from w1[1,2], w1[0,1] and w1[1,1]
  expect_identical(as.numeric(w2[0,2]),val02)

  val12 <- 3 - (5 * (3 * 0.6 / 8)) + (1 * 0.6 / 8) + (6 * 0.6 / 8) + (8 * 0.6 / 8) + (2 * 0.6 / 8) + (10 * 0.6 / 8)
  expect_identical(as.numeric(w2[1,2]),val12)

  val11 <- 8 - (8 * (8 * 0.6 / 8)) + (3 * 0.6 / 8) + (2 * 0.6 / 8) + (10 * 0.6 / 8) + (8 * 0.6 / 8) + (1 * 0.6 / 8) + (6 * 0.6 / 8) + (3 * 0.6 / 8) + (2 * 0.6 / 8)
  expect_identical(as.numeric(w2[1,1]),val11)
})

test_that("diffuse works for NLworldStack with 8 neighbors", {
  w1 <- createNLworld(0, 2, 0, 2)
  w1[] <- c(1,3,6,2,8,10,3,8,2)
  w2 <- createNLworld(0, 2, 0, 2)
  w2[] <- runif(9)
  ws <- NLstack(w1, w2)
  ws2 <- diffuse(world = ws,pVar = "w1", share = 0.6, nNeighbors = 8)

  expect_identical(sum(values(ws$w1)), sum(values(ws2$w1)))

  val02 <- 1 - (3 * (1 * 0.6 / 8)) + (3 * 0.6 / 8) + (2 * 0.6 / 8) + (8 * 0.6 / 8)
  # w1[0,2] - given to w1[1,2], w1[0,1] and w1[1,1] + received from w1[1,2], w1[0,1] and w1[1,1]
  expect_identical(as.numeric(ws2$w1[0,2]),val02)

  val12 <- 3 - (5 * (3 * 0.6 / 8)) + (1 * 0.6 / 8) + (6 * 0.6 / 8) + (8 * 0.6 / 8) + (2 * 0.6 / 8) + (10 * 0.6 / 8)
  expect_identical(as.numeric(ws2$w1[1,2]),val12)

  val11 <- 8 - (8 * (8 * 0.6 / 8)) + (3 * 0.6 / 8) + (2 * 0.6 / 8) + (10 * 0.6 / 8) + (8 * 0.6 / 8) + (1 * 0.6 / 8) + (6 * 0.6 / 8) + (3 * 0.6 / 8) + (2 * 0.6 / 8)
  expect_identical(as.numeric(ws2$w1[1,1]),val11)
})

test_that("isPatch works", {
  w1 <- createNLworld(0, 2, 0, 2)
  expect_false(isPatch(w1, 1, 3))
  expect_true(isPatch(w1, 1, 1))

  w1[] <- c(1,3,6,2,8,10,3,8,2)
  expect_identical(isPatch(w1, c(0, 1), c(3, 1)), c(FALSE, TRUE))

  w2 <- createNLworld(0, 2, 0, 2)
  w2[] <- runif(9)
  ws <- NLstack(w1, w2)

  # Same as for w1
  expect_false(isPatch(ws, 1, 3))
  expect_true(isPatch(ws, 1, 1))
  expect_identical(isPatch(ws, c(0, 1), c(3, 1)), c(FALSE, TRUE))
})

test_that("neighbors works", {
  w1 <- createNLworld(0, 9, 0, 9)
  n4 <- neighbors(world = w1, agent = cbind(pxcor = c(0, 9, 0, 9), pycor = c(9, 9, 0, 0)), nNeighbors = 4)
  n41 <- cbind(pxcor = c(1, 0), pycor = c(9, 8))
  n43 <- cbind(pxcor = c(1, 0), pycor = c(0, 1))
  expect_identical(n4[[1]], n41)
  expect_identical(n4[[3]], n43)

  n8 <- neighbors(world = w1, agent = cbind(pxcor = c(0, 9, 0, 9), pycor = c(9, 9, 0, 0)), nNeighbors = 8)
  n82 <- cbind(pxcor = c(8, 8, 9), pycor = c(9, 8, 8))
  expect_identical(n8[[2]], n82)

  w1[] <- runif(100)
  w2 <- w1
  w2[] <- runif(100)
  ws <- NLstack(w1, w2)

  # Same as for w1
  n4 <- neighbors(world = ws, agent = cbind(pxcor = c(0, 9, 0, 9), pycor = c(9, 9, 0, 0)), nNeighbors = 4)
  expect_identical(n4[[1]], n41)
  expect_identical(n4[[3]], n43)
  n8 <- neighbors(world = ws, agent = cbind(pxcor = c(0, 9, 0, 9), pycor = c(9, 9, 0, 0)), nNeighbors = 8)
  expect_identical(n8[[2]], n82)
})

test_that("patch works", {
  w1 <- createNLworld(0, 9, 0, 9)
  w1[] <- 1:100
  expect_identical(patch(world = w1, xcor = 0.1, ycor = -0.4), cbind(pxcor = 0, pycor = 0))
  expect_identical(patch(world = w1, xcor = c(1, 0), ycor = c(0, 0)), cbind(pxcor = c(1, 0), pycor = c(0, 0)))
  expect_identical(patch(world = w1, xcor = -1, ycor = 0, torus = TRUE), cbind(pxcor = 9, pycor = 0))
  expect_identical(patch(world = w1, xcor = -1, ycor = 0, torus = FALSE), cbind(pxcor = NA, pycor = 0))

  w2 <- w1
  w2[] <- 100:1
  ws <- NLstack(w1, w2)
  expect_identical(patch(world = ws, xcor = 0.1, ycor = -0.4), cbind(pxcor = 0, pycor = 0))
  expect_identical(patch(world = ws, xcor = c(1, 0), ycor = c(0, 0)), cbind(pxcor = c(1, 0), pycor = c(0, 0)))
  expect_identical(patch(world = ws, xcor = -1, ycor = 0, torus = TRUE), cbind(pxcor = 9, pycor = 0))
  expect_identical(patch(world = ws, xcor = -1, ycor = 0, torus = FALSE), cbind(pxcor = NA, pycor = 0))
})

test_that("other works", {
  w1 <- createNLworld(0, 9, 0, 9)
  w1[] <- 1:100
  otherPatches <- other(world = w1, agent = cbind(pxcor = 0, pycor = 0))
  expect_identical(as.numeric(nrow(otherPatches)), 99)
  otherPatches <- other(world = w1, agent = cbind(pxcor = c(0,1,2,2), pycor = c(0,1,2,2)))
  expect_identical(as.numeric(nrow(otherPatches)), 97)
  otherPatches <- other(world = w1, agent = cbind(pxcor = 0, pycor = -1), torus = FALSE)
  expect_identical(as.numeric(nrow(otherPatches)), 100)
  otherPatches <- other(world = w1, agent = cbind(pxcor = 0, pycor = -1), torus = TRUE)
  expect_identical(as.numeric(nrow(otherPatches)), 99)

  w2 <- w1
  w2[] <- 100:1
  ws <- NLstack(w1, w2)
  otherPatches <- other(world = ws, agent = cbind(pxcor = 0, pycor = 0))
  expect_identical(as.numeric(nrow(otherPatches)), 99)
  otherPatches <- other(world = ws, agent = cbind(pxcor = c(0,1,2,2), pycor = c(0,1,2,2)))
  expect_identical(as.numeric(nrow(otherPatches)), 97)
  otherPatches <- other(world = ws, agent = cbind(pxcor = 0, pycor = -1), torus = FALSE)
  expect_identical(as.numeric(nrow(otherPatches)), 100)
  otherPatches <- other(world = ws, agent = cbind(pxcor = 0, pycor = -1), torus = TRUE)
  expect_identical(as.numeric(nrow(otherPatches)), 99)
})
