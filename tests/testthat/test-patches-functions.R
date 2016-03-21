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

test_that("distance works for patches", {
  w1 <- createNLworld(0, 9, 0, 9)
  dist <- NLdist(world = w1, from = cbind(pxcor = 0, pycor = 0), to = cbind(pxcor = 1, pycor = 1))
  expect_identical(as.numeric(dist), sqrt(1^2+1^2))
  dist <- NLdist(world = w1, from = cbind(pxcor = 0, pycor = 0), to = cbind(pxcor = c(0,0), pycor = c(1,9)))
  expect_identical(dist, c(1, 9))
  dist <- NLdist(world = w1, from = cbind(pxcor = 0, pycor = 0), to = cbind(pxcor = c(0,0), pycor = c(1,9)), torus = TRUE)
  expect_identical(dist, c(1, 1))
  dist <- NLdist(world = w1, from = cbind(pxcor = c(0,0), pycor = c(0,1)), to = cbind(pxcor = c(0,0), pycor = c(2,9)))
  expect_identical(dist, c(2, 8))
  dist <- NLdist(world = w1, from = cbind(pxcor = c(0,0), pycor = c(0,1)), to = cbind(pxcor = c(0,0), pycor = c(2,9)), allPairs = TRUE)
  expect_identical(dist[,1], c(2, 1))
  expect_identical(dist[,2], c(9, 8))

  w1[] <- runif(100)
  w2 <- w1
  w2[] <- runif(100)
  ws <- NLstack(w1, w2)
  dist <- NLdist(world = ws, from = cbind(pxcor = 0, pycor = 0), to = cbind(pxcor = 1, pycor = 1))
  expect_identical(as.numeric(dist), sqrt(1^2+1^2))
  dist <- NLdist(world = ws, from = cbind(pxcor = 0, pycor = 0), to = cbind(pxcor = c(0,0), pycor = c(1,9)))
  expect_identical(dist, c(1, 9))
  dist <- NLdist(world = ws, from = cbind(pxcor = 0, pycor = 0), to = cbind(pxcor = c(0,0), pycor = c(1,9)), torus = TRUE)
  expect_identical(dist, c(1, 1))
  dist <- NLdist(world = ws, from = cbind(pxcor = c(0,0), pycor = c(0,1)), to = cbind(pxcor = c(0,0), pycor = c(2,9)))
  expect_identical(dist, c(2, 8))
  dist <- NLdist(world = ws, from = cbind(pxcor = c(0,0), pycor = c(0,1)), to = cbind(pxcor = c(0,0), pycor = c(2,9)), allPairs = TRUE)
  expect_identical(dist[,1], c(2, 1))
  expect_identical(dist[,2], c(9, 8))

  w3 <- createNLworld(-5, 5, -10, -2)
  dist <- NLdist(world = w3, from = cbind(pxcor = -2, pycor = -5), to = cbind(pxcor = c(-1,5), pycor = c(-5, -5)), torus = TRUE)
  expect_identical(dist, c(1, 4))
})

test_that("distance works with turtles", {
  w1 <- createNLworld(0, 9, 0, 9)
  t1 <- createTurtles(n = 4, coords = cbind(xcor = c(1,2,3,4), ycor = c(1,2,3,4)))
  # Patches to turtles
  distPT <- NLdist(world = w1, from = cbind(pxcor = 2, pycor = 3), to = t1)
  expect_identical(distPT, c(sqrt(1^2+2^2), 1, 1, sqrt(1^2+2^2)))
  distPT <- NLdist(world = w1, from = cbind(pxcor = 8, pycor = 1), to = t1)
  expect_identical(distPT[1], 7)
  distPT <- NLdist(world = w1, from = cbind(pxcor = 8, pycor = 1), to = t1, torus = TRUE)
  expect_identical(distPT[1], 3)

  # Turtles to patches
  distTP <- NLdist(world = w1, from = t1, to = cbind(pxcor = 2, pycor = 3))
  expect_identical(distTP, c(sqrt(1^2+2^2), 1, 1, sqrt(1^2+2^2)))

  # Turtles to turtles
  distTT <- NLdist(world = w1, from = t1, to = t1)
  expect_equivalent(distTT, rep(0, 4))
  distTT <- NLdist(world = w1, from = t1, to = t1, allPairs = TRUE)
  expect_equivalent(distTT[1,2], sqrt(1^1+1^1))

  w1[] <- runif(100)
  w2 <- w1
  w2[] <- runif(100)
  ws <- NLstack(w1, w2)
  distPT <- NLdist(world = ws, from = cbind(pxcor = 8, pycor = 1), to = t1, torus = TRUE)
  expect_identical(distPT[1], 3)
  distTP <- NLdist(world = ws, from = t1, to = cbind(pxcor = 2, pycor = 3))
  expect_identical(distTP, c(sqrt(1^2+2^2), 1, 1, sqrt(1^2+2^2)))
  distTT <- NLdist(world = ws, from = t1, to = t1, allPairs = TRUE)
  expect_equivalent(distTT[1,2], sqrt(1^1+1^1))
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

test_that("neighbors works with patches", {
  w1 <- createNLworld(0, 9, 0, 9)
  n4 <- neighbors(world = w1, agents = cbind(pxcor = c(0, 9, 0, 9), pycor = c(9, 9, 0, 0)), nNeighbors = 4)
  n41 <- cbind(pxcor = c(1, 0), pycor = c(9, 8))
  n43 <- cbind(pxcor = c(0, 1), pycor = c(1, 0))
  expect_identical(n4[[1]], n41)
  expect_identical(n4[[3]], n43)

  n8 <- neighbors(world = w1, agents = cbind(pxcor = c(0, 9, 0, 9), pycor = c(9, 9, 0, 0)), nNeighbors = 8)
  n82 <- cbind(pxcor = c(8, 8, 9), pycor = c(9, 8, 8))
  expect_identical(n8[[2]], n82)

  w1[] <- runif(100)
  w2 <- w1
  w2[] <- runif(100)
  ws <- NLstack(w1, w2)

  # Same as for w1
  n4 <- neighbors(world = ws, agents = cbind(pxcor = c(0, 9, 0, 9), pycor = c(9, 9, 0, 0)), nNeighbors = 4)
  expect_identical(n4[[1]], n41)
  expect_identical(n4[[3]], n43)
  n8 <- neighbors(world = ws, agents = cbind(pxcor = c(0, 9, 0, 9), pycor = c(9, 9, 0, 0)), nNeighbors = 8)
  expect_identical(n8[[2]], n82)

  # With torus = TRUE
  nCorner <- neighbors(world = w1, agents = cbind(pxcor = 9, pycor = 9), nNeighbors = 4, torus = FALSE)
  expect_equivalent(nrow(nCorner[[1]]), 2)
  nCorner <- neighbors(world = w1, agents = cbind(pxcor = 9, pycor = 9), nNeighbors = 4, torus = TRUE)
  expect_equivalent(nrow(nCorner[[1]]), 4)
  expect_identical(nCorner[[1]], cbind(pxcor = c(9, 8, 0, 9), pycor = c(0, 9, 9, 8)))
  nCorner <- neighbors(world = ws, agents = cbind(pxcor = 9, pycor = 9), nNeighbors = 4, torus = TRUE)
  expect_equivalent(nrow(nCorner[[1]]), 4)
  expect_identical(nCorner[[1]], cbind(pxcor = c(9, 8, 0, 9), pycor = c(0, 9, 9, 8)))
})

test_that("neighbors works with turtles", {
  w1 <- createNLworld(0, 9, 0, 9)
  t1 <- createTurtles(n = 4, coords = cbind(xcor = c(0, 9, 0, 9), ycor = c(9, 9, 0, 0)))
  n4 <- neighbors(world = w1, agents = t1, nNeighbors = 4)
  n41 <- cbind(pxcor = c(1, 0), pycor = c(9, 8))
  n43 <- cbind(pxcor = c(0, 1), pycor = c(1, 0))
  expect_identical(n4[[1]], n41)
  expect_identical(n4[[3]], n43)

  n8 <- neighbors(world = w1, agents = t1, nNeighbors = 8)
  n82 <- cbind(pxcor = c(8, 8, 9), pycor = c(9, 8, 8))
  expect_identical(n8[[2]], n82)

  w1[] <- runif(100)
  w2 <- w1
  w2[] <- runif(100)
  ws <- NLstack(w1, w2)

  # Same as for w1
  n4 <- neighbors(world = ws, agents = t1, nNeighbors = 4)
  expect_identical(n4[[1]], n41)
  expect_identical(n4[[3]], n43)
  n8 <- neighbors(world = ws, agents = t1, nNeighbors = 8)
  expect_identical(n8[[2]], n82)

  # With torus = TRUE
  t1 <- createTurtles(n = 1, coords = cbind(xcor = 0.2, ycor = 0.3))
  nCorner <- neighbors(world = w1, agents = t1, nNeighbors = 8, torus = TRUE)
  expect_equivalent(nrow(nCorner[[1]]), 8)
  expect_equivalent(sum(nCorner[[1]][,"pxcor"]), 3*9+3*1+2*0)
  expect_equivalent(sum(nCorner[[1]][,"pycor"]), 3*9+3*1+2*0)
})

test_that("patch works", {
  w1 <- createNLworld(0, 9, 0, 9)
  w1[] <- 1:100
  expect_identical(patch(world = w1, xcor = 0.1, ycor = -0.4), cbind(pxcor = 0, pycor = 0))
  expect_identical(patch(world = w1, xcor = c(1, 0), ycor = c(0, 0)), cbind(pxcor = c(1, 0), pycor = c(0, 0)))
  expect_identical(patch(world = w1, xcor = c(0,-1), ycor = c(0,0), out = FALSE), cbind(pxcor = 0, pycor = 0))
  expect_identical(patch(world = w1, xcor = c(0,-1), ycor = c(0,0), out = TRUE), cbind(pxcor = c(0,NA), pycor = c(0,NA)))
  expect_identical(patch(world = w1, xcor = c(0,-1), ycor = c(0,0), torus = TRUE), cbind(pxcor = c(0, 9), pycor = c(0, 0)))
  expect_identical(patch(world = w1, xcor = c(0,-1), ycor = c(0,0), torus = TRUE, out = TRUE), cbind(pxcor = c(0, 9), pycor = c(0, 0)))
  expect_identical(patch(world = w1, xcor = c(0, 0.1, 0.4), ycor = c(-0.4, 0, 0.2)), cbind(pxcor = 0, pycor = 0))
  expect_identical(patch(world = w1, xcor = c(0, 0.1, 0.4), ycor = c(-0.4, 0, 0.2), duplicate = TRUE), cbind(pxcor = c(0,0,0), pycor = c(0,0,0)))

  w2 <- w1
  w2[] <- 100:1
  ws <- NLstack(w1, w2)
  expect_identical(patch(world = ws, xcor = 0.1, ycor = -0.4), cbind(pxcor = 0, pycor = 0))
  expect_identical(patch(world = ws, xcor = c(1, 0), ycor = c(0, 0)), cbind(pxcor = c(1, 0), pycor = c(0, 0)))
  expect_identical(patch(world = ws, xcor = c(0,-1), ycor = c(0,0), out = FALSE), cbind(pxcor = 0, pycor = 0))
  expect_identical(patch(world = ws, xcor = c(0,-1), ycor = c(0,0), out = TRUE), cbind(pxcor = c(0,NA), pycor = c(0,NA)))
  expect_identical(patch(world = ws, xcor = c(0,-1), ycor = c(0,0), torus = TRUE), cbind(pxcor = c(0, 9), pycor = c(0, 0)))
  expect_identical(patch(world = ws, xcor = c(0,-1), ycor = c(0,0), torus = TRUE, out = TRUE), cbind(pxcor = c(0, 9), pycor = c(0, 0)))
  expect_identical(patch(world = ws, xcor = c(0, 0.1, 0.4), ycor = c(-0.4, 0, 0.2)), cbind(pxcor = 0, pycor = 0))
  expect_identical(patch(world = ws, xcor = c(0, 0.1, 0.4), ycor = c(-0.4, 0, 0.2), duplicate = TRUE), cbind(pxcor = c(0,0,0), pycor = c(0,0,0)))
})

test_that("noPatches works", {
  p1 <- noPatches()
  expect_equivalent(nrow(p1), 0)
  expect_equivalent(ncol(p1), 2)
})

test_that("patchAt works with patches", {
  w1 <- createNLworld(0, 9, 0, 9)
  p1 <- patchAt(world = w1, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dx = 1, dy = 2)
  expect_identical(p1, patch(w1, c(0+1, 1+1, 3+1), c(0+2, 1+2, 5+2)))
  p1 <- patchAt(world = w1, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dx = c(1,3,5), dy = c(2, 4, 6))
  expect_identical(p1, patch(w1, c(1, 4, 8), c(2, 5, 11), out = TRUE))
  p1 <- patchAt(world = w1, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dx = c(1,3,5), dy = c(2, 4, 6), torus = TRUE)
  expect_identical(p1, patch(w1, c(1, 4, 8), c(2, 5, 1)))

  w1[] <- runif(100)
  w2 <- w1
  w2[] <- runif(100)
  ws <- NLstack(w1, w2)
  p1 <- patchAt(world = ws, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dx = 1, dy = 2)
  expect_identical(p1, patch(ws, c(0+1, 1+1, 3+1), c(0+2, 1+2, 5+2)))
  p1 <- patchAt(world = ws, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dx = c(1,3,5), dy = c(2, 4, 6))
  expect_identical(p1, patch(ws, c(1, 4, 8), c(2, 5, 11), out = TRUE))
  p1 <- patchAt(world = ws, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dx = c(1,3,5), dy = c(2, 4, 6), torus = TRUE)
  expect_identical(p1, patch(ws, c(1, 4, 8), c(2, 5, 1)))

  w1 <- createNLworld(-5, 5, -5, 5)
  p1 <- patchAt(world = w1, agents = cbind(pxcor = c(0, -2, 3), pycor = c(0, 1, 5)), dx = -4, dy = 1)
  expect_identical(p1, patch(w1, c(-4, -6, -1), c(1, 2, 6), out = TRUE, duplicate = TRUE))
})

test_that("patchAt works with turtles", {
  w1 <- createNLworld(0, 9, 0, 9)
  t1 <- createTurtles(n = 3, coords = cbind(xcor = c(0.2, 0.9, 3.1), ycor = c(-0.4, 1, 5.4)))
  p1 <- patchAt(world = w1, agents = t1, dx = 1, dy = 2)
  expect_identical(p1, patch(w1, c(0+1, 1+1, 3+1), c(0+2, 1+2, 5+2)))
  p1 <- patchAt(world = w1, agents = t1, dx = c(1,3,5), dy = c(2, 4, 6))
  expect_identical(p1, patch(w1, c(1, 4, 8), c(2, 5, 11), out = TRUE))
  p1 <- patchAt(world = w1, agents = t1, dx = c(1,3,5), dy = c(2, 4, 6), torus = TRUE)
  expect_identical(p1, patch(w1, c(1, 4, 8), c(2, 5, 1)))

  w1[] <- runif(100)
  w2 <- w1
  w2[] <- runif(100)
  ws <- NLstack(w1, w2)
  p1 <- patchAt(world = ws, agents = t1, dx = 1, dy = 2)
  expect_identical(p1, patch(ws, c(0+1, 1+1, 3+1), c(0+2, 1+2, 5+2)))
  p1 <- patchAt(world = ws, agents = t1, dx = c(1,3,5), dy = c(2, 4, 6))
  expect_identical(p1, patch(ws, c(1, 4, 8), c(2, 5, 11), out = TRUE))
  p1 <- patchAt(world = ws, agents = t1, dx = c(1,3,5), dy = c(2, 4, 6), torus = TRUE)
  expect_identical(p1, patch(ws, c(1, 4, 8), c(2, 5, 1)))

  w1 <- createNLworld(-5, 5, -5, 5)
  t2 <- createTurtles(n = 3, coords = cbind(pxcor = c(0, -2, 3), pycor = c(0, 1, 5)))
  p1 <- patchAt(world = w1, agents = t2, dx = -4, dy = 1)
  expect_identical(p1, patch(w1, c(-4, -6, -1), c(1, 2, 6), out = TRUE, duplicate = TRUE))
})

test_that("patchDistHead works with patches", {
  w1 <- createNLworld(0, 9, 0, 9)
  p1 <- patchDistHead(world = w1, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dist = 3, head = 45)
  expect_identical(p1, patch(w1, c(2, 3, 5), c(2, 3, 7)))
  p1 <- patchDistHead(world = w1, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dist = 3, head = -45, torus = TRUE)
  p2 <- patchDistHead(world = w1, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dist = 3, head = 315, torus = TRUE)
  expect_identical(p1, patch(w1, c(8, 9, 1), c(2, 3, 7)))
  expect_identical(p1, p2)
  p1 <- patchDistHead(world = w1, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dist = 3, head = -45)
  expect_identical(p1, patch(w1, c(NA, NA, 1), c(NA, NA, 7), out = TRUE, duplicate = TRUE))

  w1[] <- runif(100)
  w2 <- w1
  w2[] <- runif(100)
  ws <- NLstack(w1, w2)
  p1 <- patchDistHead(world = ws, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dist = 3, head = 45)
  expect_identical(p1, patch(ws, c(2, 3, 5), c(2, 3, 7)))
  p1 <- patchDistHead(world = ws, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dist = 3, head = -45, torus = TRUE)
  p2 <- patchDistHead(world = ws, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dist = 3, head = 315, torus = TRUE)
  expect_identical(p1, patch(ws, c(8, 9, 1), c(2, 3, 7)))
  expect_identical(p1, p2)
  p1 <- patchDistHead(world = ws, agents = cbind(pxcor = c(0, 1, 3), pycor = c(0, 1, 5)), dist = 3, head = -45)
  expect_identical(p1, patch(ws, c(-2, -1, 1), c(-2, -1, 7), duplicate = TRUE, out = TRUE))

  w1 <- createNLworld(-5, 5, -5, 5)
  p1 <- patchDistHead(world = w1, agents = cbind(pxcor = c(0, -2, 3), pycor = c(0, 1, 5)), dist = 4, head = 270, torus = TRUE)
  expect_identical(p1, patch(w1, c(-4, 5, -1), c(0, 1, 5)))
  p1 <- patchDistHead(world = w1, agents = cbind(pxcor = c(0, -2, 3), pycor = c(0, 1, 5)), dist = -4, head = 270, torus = FALSE)
  expect_identical(p1, patch(w1, c(4, 2, 7), c(0, 1, 5), duplicate = TRUE, out = TRUE))
})

test_that("patchDistHead works with turtles", {
  w1 <- createNLworld(0, 9, 0, 9)
  t1 <- createTurtles(n = 3, coords = cbind(xcor = c(0.1, 0.9, 3), ycor = c(-0.4, 1, 5.2)))
  p1 <- patchDistHead(world = w1, agents = t1, dist = 3, head = 45)
  expect_identical(p1, patch(w1, c(2, 3, 5), c(2, 3, 7)))
  p1 <- patchDistHead(world = w1, agents = t1, dist = 3, head = -45, torus = TRUE)
  p2 <- patchDistHead(world = w1, agents = t1, dist = 3, head = 315, torus = TRUE)
  expect_identical(p1, patch(w1, c(8, 9, 1), c(2, 3, 7)))
  expect_identical(p1, p2)
  p1 <- patchDistHead(world = w1, agents = t1, dist = 3, head = -45)
  expect_identical(p1, patch(w1, c(NA, NA, 1), c(NA, NA, 7), out = TRUE, duplicate = TRUE))

  w1[] <- runif(100)
  w2 <- w1
  w2[] <- runif(100)
  ws <- NLstack(w1, w2)
  p1 <- patchDistHead(world = ws, agents = t1, dist = 3, head = 45)
  expect_identical(p1, patch(ws, c(2, 3, 5), c(2, 3, 7)))
  p1 <- patchDistHead(world = ws, agents = t1, dist = 3, head = -45, torus = TRUE)
  p2 <- patchDistHead(world = ws, agents = t1, dist = 3, head = 315, torus = TRUE)
  expect_identical(p1, patch(ws, c(8, 9, 1), c(2, 3, 7)))
  expect_identical(p1, p2)
  p1 <- patchDistHead(world = ws, agents = t1, dist = 3, head = -45)
  expect_identical(p1, patch(ws, c(-2, -1, 1), c(-2, -1, 7), duplicate = TRUE, out = TRUE))

  w1 <- createNLworld(-5, 5, -5, 5)
  t2 <- createTurtles(n = 3, coords = cbind(pxcor = c(-0.1, -2.2, 3.4), pycor = c(0.2, 0.8, 5.4)))
  p1 <- patchDistHead(world = w1, agents = t2, dist = 4, head = 270, torus = TRUE)
  expect_identical(p1, patch(w1, c(-4, 5, -1), c(0, 1, 5)))
  p1 <- patchDistHead(world = w1, agents = t2, dist = -4, head = 270, torus = FALSE)
  expect_identical(p1, patch(w1, c(4, 2, 7), c(0, 1, 5), duplicate = TRUE, out = TRUE))
})

test_that("randPxcor and randPycor works", {
  w1 <- createNLworld(0, 9, -10, -5)
  pxcor100 <- randPxcor(world = w1, n = 100)
  expect_equivalent(min(pxcor100), minPxcor(w1))
  expect_equivalent(max(pxcor100), maxPxcor(w1))

  w1[] <- runif(60)
  w2 <- w1
  w2[] <- runif(60)
  ws <- NLstack(w1, w2)
  pxcor100 <- randPxcor(world = ws, n = 100)
  expect_equivalent(min(pxcor100), minPxcor(ws))
  expect_equivalent(max(pxcor100), maxPxcor(ws))

  pycor100 <- randPycor(world = w1, n = 100)
  expect_equivalent(min(pycor100), minPycor(w1))
  expect_equivalent(max(pycor100), maxPycor(w1))
  pycor100 <- randPycor(world = ws, n = 100)
  expect_equivalent(min(pycor100), minPycor(ws))
  expect_equivalent(max(pycor100), maxPycor(ws))
})
