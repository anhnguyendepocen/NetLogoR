test_that("diffuse works for NLworld with 4 neighbors", {
  w1 <- createNLworld(0, 2, 0, 2)
  w1[] <- c(1,3,6,2,8,10,3,8,2)
  w2 <- diffuse(world = w1, share = 0.6, nNeighbors = 4)

  expect_identical(sum(values(w1)), sum(values(w2)))

  val11 <- 1 - (2 * (1 * 0.6 / 4)) + (3 * 0.6 / 4) + (2 * 0.6 / 4)
  # w1[1,1] - given to w1[1,2] and w1[2,1] + received from w1[1,2] and w1[2,1]
  expect_identical(as.numeric(w2[1,1]),val11) # test a few patches, done by hand (!if the patches number [,] changes, it will break)

  val12 <- 3 - (3 * (3 * 0.6 / 4)) + (1 * 0.6 / 4) + (6 * 0.6 / 4) + (8 * 0.6 / 4)
  expect_identical(as.numeric(w2[1,2]),val12)

  val22 <- 8 - (4 * (8 * 0.6 / 4)) + (3 * 0.6 / 4) + (2 * 0.6 / 4) + (10 * 0.6 / 4) + (8 * 0.6 / 4)
  expect_identical(as.numeric(w2[2,2]),val22)
})

test_that("diffuse works for NLworldStack with 4 neighbors", {
  w1 <- createNLworld(0, 2, 0, 2)
  w1[] <- c(1,3,6,2,8,10,3,8,2)
  w2 <- createNLworld(0, 2, 0, 2)
  w2[] <- runif(9)
  ws <- NLstack(w1, w2)
  ws2 <- diffuse(world = ws,pVar = "w1", share = 0.6, nNeighbors = 4)

  expect_identical(sum(values(ws$w1)), sum(values(ws2$w1)))

  val11 <- 1 - (2 * (1 * 0.6 / 4)) + (3 * 0.6 / 4) + (2 * 0.6 / 4)
  # w1[1,1] - given to w1[1,2] and w1[2,1] + received from w1[1,2] and w1[2,1]
  expect_identical(as.numeric(ws2$w1[1,1]),val11) # test a few patches, done by hand (!if the patches number [,] changes, it will break)

  val12 <- 3 - (3 * (3 * 0.6 / 4)) + (1 * 0.6 / 4) + (6 * 0.6 / 4) + (8 * 0.6 / 4)
  expect_identical(as.numeric(ws2$w1[1,2]),val12)

  val22 <- 8 - (4 * (8 * 0.6 / 4)) + (3 * 0.6 / 4) + (2 * 0.6 / 4) + (10 * 0.6 / 4) + (8 * 0.6 / 4)
  expect_identical(as.numeric(ws2$w1[2,2]),val22)
})

test_that("diffuse works for NLworld with 8 neighbors", {
  w1 <- createNLworld(0, 2, 0, 2)
  w1[] <- c(1,3,6,2,8,10,3,8,2)
  w2 <- diffuse(world = w1, share = 0.6, nNeighbors = 8)

  expect_identical(sum(values(w1)), sum(values(w2)))

  val11 <- 1 - (3 * (1 * 0.6 / 8)) + (3 * 0.6 / 8) + (2 * 0.6 / 8) + (8 * 0.6 / 8)
  # w1[1,1] - given to w1[1,2], w1[2,1] and w1[2,2] + received from w1[1,2], w1[2,1] and w1[2,2]
  expect_identical(as.numeric(w2[1,1]),val11) # test a few patches, done by hand (!if the patches number [,] changes, it will break)

  val12 <- 3 - (5 * (3 * 0.6 / 8)) + (1 * 0.6 / 8) + (6 * 0.6 / 8) + (8 * 0.6 / 8) + (2 * 0.6 / 8) + (10 * 0.6 / 8)
  expect_identical(as.numeric(w2[1,2]),val12)

  val22 <- 8 - (8 * (8 * 0.6 / 8)) + (3 * 0.6 / 8) + (2 * 0.6 / 8) + (10 * 0.6 / 8) + (8 * 0.6 / 8) + (1 * 0.6 / 8) + (6 * 0.6 / 8) + (3 * 0.6 / 8) + (2 * 0.6 / 8)
  expect_identical(as.numeric(w2[2,2]),val22)
})

test_that("diffuse works for NLworldStack with 8 neighbors", {
  w1 <- createNLworld(0, 2, 0, 2)
  w1[] <- c(1,3,6,2,8,10,3,8,2)
  w2 <- createNLworld(0, 2, 0, 2)
  w2[] <- runif(9)
  ws <- NLstack(w1, w2)
  ws2 <- diffuse(world = ws,pVar = "w1", share = 0.6, nNeighbors = 8)

  expect_identical(sum(values(ws$w1)), sum(values(ws2$w1)))

  val11 <- 1 - (3 * (1 * 0.6 / 8)) + (3 * 0.6 / 8) + (2 * 0.6 / 8) + (8 * 0.6 / 8)
  # w1[1,1] - given to w1[1,2], w1[2,1] and w1[2,2] + received from w1[1,2], w1[2,1] and w1[2,2]
  expect_identical(as.numeric(ws2$w1[1,1]),val11) # test a few patches, done by hand (!if the patches number [,] changes, it will break)

  val12 <- 3 - (5 * (3 * 0.6 / 8)) + (1 * 0.6 / 8) + (6 * 0.6 / 8) + (8 * 0.6 / 8) + (2 * 0.6 / 8) + (10 * 0.6 / 8)
  expect_identical(as.numeric(ws2$w1[1,2]),val12)

  val22 <- 8 - (8 * (8 * 0.6 / 8)) + (3 * 0.6 / 8) + (2 * 0.6 / 8) + (10 * 0.6 / 8) + (8 * 0.6 / 8) + (1 * 0.6 / 8) + (6 * 0.6 / 8) + (3 * 0.6 / 8) + (2 * 0.6 / 8)
  expect_identical(as.numeric(ws2$w1[2,2]),val22)
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
