test_that("NLall works with patches",{
  w1 <- createNLworld(0, 4, 0, 4)
  w1[] <- sample(1:5, size = 25, replace = TRUE)
  expect_identical(NLall(world = w1, agents = patches(world = w1), val = 5), FALSE)
  w2 <- w1
  w2[] <- 5
  expect_identical(NLall(world = w2, agents = patches(world = w2), val = 5), TRUE)
  w3 <- w2
  w3[0,0] <- 4
  expect_identical(NLall(world = w2, agents = patch(world = w2, xcor = c(0,1,2,3,4), ycor = c(4,4,4,4,4)), val = 5), TRUE)

  ws <- NLstack(w1, w2, w3)
  expect_identical(NLall(world = ws, agents = patches(world = ws), varName = "w1", val = 5), FALSE)
  expect_identical(NLall(world = ws, agents = patches(world = ws), varName = "w2", val = 5), TRUE)
  expect_identical(NLall(world = ws, agents = patch(world = ws, xcor = c(0,1,2,3,4), ycor = c(4,4,4,4,4)), varName = "w3", val = 5), TRUE)
})

test_that("NLall works with turtles",{
  t1 <- createTurtles(n = 5, coords = cbind(xcor = 1, ycor = 1), heading = c(1, 2, 2, 1, 2))
  expect_identical(NLall(agents = t1, varName = "xcor", val = 1), TRUE)
  expect_identical(NLall(agents = t1, varName = "heading", val = 2), FALSE)
  expect_identical(NLall(agents = turtle(t1, who = c(1, 2, 4)), varName = "heading", val = 2), TRUE)
  expect_identical(NLall(agents = t1, varName = "xcor", val = 2), FALSE)
})

test_that("NLany works with patches",{
  w1 <- createNLworld(0, 4, 0, 4)
  p1 <- noPatches()
  p2 <- patch(world = w1, xcor = 0, ycor = 0)
  p3 <- patch(world = w1, xcor = -1, ycor = -1)
  p4 <- patches(world = w1)
  p5 <- patch(world = w1, xcor = c(-1, 0), ycor = c(-1, 0))
  expect_identical(NLany(p1), FALSE)
  expect_identical(NLany(p2), TRUE)
  expect_identical(NLany(p3), FALSE)
  expect_identical(NLany(p4), TRUE)
  expect_identical(NLany(p5), TRUE)
})

test_that("NLany works with turtles",{
  w1 <- createNLworld(0, 4, 0, 4)
  t1 <- createTurtles(n = 10, coords = randomXYcor(world= w1, n = 10))
  t2 <- noTurtles()
  t3 <- NLwith(agents = t1, varName = "xcor", val = 10)
  t4 <- turtle(t1, who = 0)
  expect_identical(NLany(t1), TRUE)
  expect_identical(NLany(t2), FALSE)
  expect_identical(NLany(t3), FALSE)
  expect_identical(NLany(t4), TRUE)
})

test_that("count works with patches",{
  w1 <- createNLworld(0, 4, 0, 4)
  p1 <- noPatches()
  p2 <- patch(world = w1, xcor = 0, ycor = 0)
  p3 <- patch(world = w1, xcor = -1, ycor = -1)
  p4 <- patches(world = w1)
  p5 <- patch(world = w1, xcor = c(-1, 0), ycor = c(-1, 0))
  p6 <- patch(world = w1, xcor = c(-1, 0), ycor = c(0, 0))
  expect_equivalent(count(p1), 0)
  expect_equivalent(count(p2), 1)
  expect_equivalent(count(p3), 0)
  expect_equivalent(count(p4), 25)
  expect_equivalent(count(p5), 1)
  expect_equivalent(count(p6), 1)
})

test_that("count works with turtles",{
  w1 <- createNLworld(0, 4, 0, 4)
  t1 <- noTurtles()
  t2 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10))
  t3 <- turtle(turtles = t2, who = c(1, 2, 3))
  expect_equivalent(count(t1), 0)
  expect_equivalent(count(t2), 10)
  expect_equivalent(count(t3), 3)
})

test_that("sortOn works with patches",{
  w1 <- createNLworld(0, 4, 0, 4)
  w1[] <- 25:1
  p1 <- sortOn(world = w1, agents = patches(world = w1))
  expect_equivalent(cbind(p1[1,1], p1[1,2]), patch(w1, xcor = 4, ycor = 0))
  expect_equivalent(cbind(p1[25,1],p1[25,2]), patch(w1, xcor = 0, ycor = 4))

  w2 <- w1
  w2[] <- 1:25
  ws <- NLstack(w1, w2)
  p1 <- sortOn(world = ws, agents = patch(world = ws, xcor = c(0,1,2,3,4), ycor = c(4,4,4,4,4)), varName = "w1")
  p2 <- sortOn(world = ws, agents = patch(world = ws, xcor = c(0,1,2,3,4), ycor = c(4,4,4,4,4)), varName = "w2")
  expect_equivalent(p1, cbind(pxcor = c(4,3,2,1,0), pycor = c(4,4,4,4,4)))
  expect_equivalent(p2, cbind(pxcor = c(0,1,2,3,4), pycor = c(4,4,4,4,4)))
})

test_that("sortOn works with turtles",{
  t1 <- createTurtles(n = 5, coords = cbind(xcor = 1:5, ycor = 5:1), heading = c(4,5,1,3,2))
  t2 <- sortOn(agents = t1, varName = "xcor")
  t3 <- sortOn(agents = t1, varName = "ycor")
  t4 <- sortOn(agents = t1, varName = "heading")
  expect_equivalent(t2, c(0,1,2,3,4))
  expect_equivalent(t3, c(4,3,2,1,0))
  expect_equivalent(t4, c(2, 4, 3, 0, 1))

  t5 <- sortOn(agents = turtle(turtles = t1, who = 0), varName = "heading")
  expect_equivalent(t5, 0)
  t6 <- createTurtles(n = 5, coords = cbind(xcor = 1, ycor = 1), heading = 1)
  t7 <- sortOn(agents = t6, varName = "xcor")
})

test_that("NLwith works with patches",{
  w1 <- createNLworld(0, 4, 0, 4)
  w1[] <- 0
  w1[1,c(1,4)] <- c(1,1)
  p1 <- NLwith(world = w1, agents = patches(world = w1), val = 1)
  expect_identical(p1, patch(w1, xcor = c(1,1), ycor = c(4,1)))
  p2 <- NLwith(agents = patches(world = w1), world = w1, val = 0)
  expect_identical(p2, other(agents = patches(w1), except = p1))
  p3 <- NLwith(agents = patches(world = w1), world = w1, val = c(0, 1))
  expect_identical(patches(world = w1), p3)
  p4 <- NLwith(agents = patches(world = w1), world = w1, val = 10)
  expect_equivalent(p4, noPatches())

  w2 <- w1
  w2[] <- 0
  w2[2,c(2,4)] <- c(1,1)
  ws <- NLstack(w1, w2)
  p1w1 <- NLwith(world = ws, agents = patches(world = ws), varName = "w1", val = 1)
  p1w2 <- NLwith(world = ws, agents = patches(world = ws), varName = "w2", val = 1)
  expect_identical(p1w1, patch(ws, xcor = c(1,1), ycor = c(4,1)))
  expect_identical(p1w2, patch(ws, xcor = c(2,2), ycor = c(4,2)))
  p5 <- NLwith(agents = patches(world = ws), world = ws, varName = "w1", val = 10)
  expect_equivalent(p5, noPatches())
})

test_that("NLwith works with turtles",{
  t1 <- createTurtles(n = 5, coords = cbind(xcor = c(1,1,1,2,3), ycor = c(2,3,4,4,5)), heading = 0,
                      breed = c("sheep", "sheep", "wolf", "sheep", "sheperd"))
  t2 <- NLwith(agents = t1, varName = "xcor", val = 1)
  expect_identical(t2, turtle(turtles = t1, who = c(0, 1, 2)))
  t3 <- NLwith(agents = t1, varName = "ycor", val = c(2, 3))
  expect_identical(t3, turtle(turtles = t1, who = c(0, 1)))
  t4 <- NLwith(agents = t1, varName = "heading", val = 0)
  expect_identical(t4, t1)
  t5 <- NLwith(agents = t1, varName = "breed", val = "sheep")
  expect_identical(t5, turtle(turtles = t1, who = c(0, 1, 3)))
  t6 <- NLwith(agents = t1, varName = "breed", val = c("sheep", "wolf"))
  expect_identical(t6, turtle(turtles = t1, who = c(0, 1, 2, 3)))
  t7 <- NLwith(agents = t1, varName = "breed", val = "moose")
  expect_identical(t7, noTurtles())
})

test_that("withMax works with patches",{
  w1 <- createNLworld(0, 4, 0, 4)
  expect_error(withMax(agents = patches(world = w1), world = w1))
  w1[] <- 1
  expect_identical(withMax(agents = patches(w1), world = w1), patches(w1))
  w1[] <- runif(25)
  w1[1,c(1,4)] <- c(2,2)
  pMax <- withMax(world = w1, agents = patches(world = w1))
  expect_identical(pMax, patch(w1, xcor = c(1,1), ycor = c(4,1)))

  w2 <- w1
  w2[] <- runif(25)
  w2[2,c(2,4)] <- c(2,2)
  ws <- NLstack(w1, w2)
  pMaxw1 <- withMax(world = ws, agents = patches(world = ws), varName = "w1")
  pMaxw2 <- withMax(world = ws, agents = patches(world = ws), varName = "w2")
  expect_identical(pMaxw1, patch(ws, xcor = c(1,1), ycor = c(4,1)))
  expect_identical(pMaxw2, patch(ws, xcor = c(2,2), ycor = c(4,2)))

  w1[1,1] <- 0
  pMax <- withMax(world = w1, agents = patches(world = w1))
  expect_identical(pMax, patch(w1, xcor = 1, ycor = 4))
})

test_that("withMax works with turtles",{
  t1 <- createTurtles(n = 10, coords = cbind(xcor = 1:10, ycor = 10:1), heading = c(1,2,3,4,4,2,3,4,4,3))
  maxXcor <- withMax(agents = t1, varName = "xcor")
  expect_identical(maxXcor, turtle(t1, who = 9))
  maxHeading <- withMax(agents = t1, varName = "heading")
  expect_identical(maxHeading, turtle(t1, who = c(3, 4, 7, 8)))
  expect_error(withMax(agents = t1, varName = "prevX"))
})

test_that("withMin works with patches",{
  w1 <- createNLworld(0, 4, 0, 4)
  expect_error(withMin(agents = patches(world = w1), world = w1))
  w1[] <- 1
  expect_identical(withMin(agents = patches(w1), world = w1), patches(w1))
  w1[] <- runif(25)
  w1[1,c(1,4)] <- c(-1,-1)
  pMin <- withMin(world = w1, agents = patches(world = w1))
  expect_identical(pMin, patch(w1, xcor = c(1,1), ycor = c(4,1)))

  w2 <- w1
  w2[] <- runif(25)
  w2[2,c(2,4)] <- c(-1,-1)
  ws <- NLstack(w1, w2)
  pMinw1 <- withMin(world = ws, agents = patches(world = ws), varName = "w1")
  pMinw2 <- withMin(world = ws, agents = patches(world = ws), varName = "w2")
  expect_identical(pMinw1, patch(ws, xcor = c(1,1), ycor = c(4,1)))
  expect_identical(pMinw2, patch(ws, xcor = c(2,2), ycor = c(4,2)))

  w1[1,1] <- 0
  pMin <- withMin(world = w1, agents = patches(world = w1))
  expect_identical(pMin, patch(w1, xcor = 1, ycor = 4))
})

test_that("withMin works with turtles",{
  t1 <- createTurtles(n = 10, coords = cbind(xcor = 10:1, ycor = 10:1), heading = c(1,2,3,0,0,2,3,0,0,3))
  maxXcor <- withMin(agents = t1, varName = "xcor")
  expect_identical(maxXcor, turtle(t1, who = 9))
  maxHeading <- withMin(agents = t1, varName = "heading")
  expect_identical(maxHeading, turtle(t1, who = c(3, 4, 7, 8)))
  expect_error(withMin(agents = t1, varName = "prevX"))
})

test_that("maxOneOf works with patches",{
  w1 <- createNLworld(0, 4, 0, 4)
  w1[] <- sample(1:5, size = 25, replace = TRUE)
  allpMax <- withMax(world = w1, agents = patches(world = w1))
  onepMax <- maxOneOf(world = w1, agents = patches(world = w1))
  compare <- cbind(a = as.numeric(allpMax[,1])==as.numeric(onepMax[1]),b = as.numeric(allpMax[,2])==as.numeric(onepMax[2]))
  rowTRUE <- compare[compare[,1] == TRUE & compare[,2] == TRUE,,drop = FALSE]
  expect_equivalent(nrow(rowTRUE),1)

  w2 <- w1
  w2[] <- 1 / values(w1)
  ws <- NLstack(w1, w2)
  onepMax1 <- maxOneOf(world = ws, agents = patches(world = w1), varName = "w1")
  onepMax2 <- maxOneOf(world = ws, agents = patches(world = w1), varName = "w2")
  compare1 <- cbind(a = as.numeric(allpMax[,1])==as.numeric(onepMax1[1]),b = as.numeric(allpMax[,2])==as.numeric(onepMax1[2]))
  rowTRUE <- compare1[compare1[,1] == TRUE & compare1[,2] == TRUE,,drop = FALSE]
  expect_equivalent(nrow(rowTRUE),1)
  compare2 <- onepMax2 == onepMax1
  expect_less_than(length(compare2[compare2 == TRUE]), 2)
})

test_that("maxOneOf works with turtles",{
  t1 <- createTurtles(n = 10, coords = cbind(xcor = 1:10, ycor = 10:1), heading = c(1,2,3,4,4,2,3,4,4,3))
  maxXcor1 <- withMax(agents = t1, varName = "xcor")
  maxXcor2 <- maxOneOf(agents = t1, varName = "xcor")
  expect_identical(maxXcor1, maxXcor2)
  maxHeading1 <- withMax(agents = t1, varName = "heading")
  maxHeading2 <- maxOneOf(agents = t1, varName = "heading")
  expect_equivalent(length(maxHeading2), 1)
  maxH12 <- merge(maxHeading1@data, maxHeading2@data)
  expect_equivalent(nrow(maxH12), 1)
  expect_error(maxOneOf(agents = t1, varName = "prevX"))
})

test_that("minOneOf works with patches",{
  w1 <- createNLworld(0, 4, 0, 4)
  w1[] <- sample(1:5, size = 25, replace = TRUE)
  allpMin <- withMin(world = w1, agents = patches(world = w1))
  onepMin <- minOneOf(world = w1, agents = patches(world = w1))
  compare <- cbind(a = as.numeric(allpMin[,1])==as.numeric(onepMin[1]),b = as.numeric(allpMin[,2])==as.numeric(onepMin[2]))
  rowTRUE <- compare[compare[,1] == TRUE & compare[,2] == TRUE,,drop = FALSE]
  expect_equivalent(nrow(rowTRUE),1)

  w2 <- w1
  w2[] <- 1 / values(w1)
  ws <- NLstack(w1, w2)
  onepMin1 <- minOneOf(world = ws, agents = patches(world = w1), varName = "w1")
  onepMin2 <- minOneOf(world = ws, agents = patches(world = w1), varName = "w2")
  compare1 <- cbind(a = as.numeric(allpMin[,1])==as.numeric(onepMin1[1]),b = as.numeric(allpMin[,2])==as.numeric(onepMin1[2]))
  rowTRUE <- compare1[compare1[,1] == TRUE & compare1[,2] == TRUE,,drop = FALSE]
  expect_equivalent(nrow(rowTRUE),1)
  compare2 <- onepMin2 == onepMin1
  expect_less_than(length(compare2[compare2 == TRUE]), 2)
})

test_that("minOneOf works with turtles",{
  t1 <- createTurtles(n = 10, coords = cbind(xcor = 1:10, ycor = 10:1), heading = c(1,2,3,4,4,2,3,4,4,3))
  minXcor1 <- withMin(agents = t1, varName = "xcor")
  minXcor2 <- minOneOf(agents = t1, varName = "xcor")
  expect_identical(minXcor1, minXcor2)
  minHeading1 <- withMin(agents = t1, varName = "heading")
  minHeading2 <- minOneOf(agents = t1, varName = "heading")
  expect_equivalent(length(minHeading2), 1)
  minH12 <- merge(minHeading1@data, minHeading2@data)
  expect_equivalent(nrow(minH12), 1)
  expect_error(minOneOf(agents = t1, varName = "prevX"))
})

test_that("isNLclass works",{
  w1 <- createNLworld(0, 4, 0, 4)
  t1 <- createTurtles(n = 10, randomXYcor(w1, n = 10))
  expect_identical(isNLclass(agents = patch(w1, xcor = 0, ycor = 0), class = "patch"), TRUE)
  expect_identical(isNLclass(agents = patches(w1), class = "patchset"), TRUE)
  expect_identical(isNLclass(agents = turtle(t1, who = 0), class = "turtle"), TRUE)
  expect_identical(isNLclass(agents = t1, class = "turtleset"), TRUE)
  expect_identical(isNLclass(agents = patch(w1, xcor = 0, ycor = 0), class = "agent"), TRUE)
  expect_identical(isNLclass(agents = turtle(t1, who = 0), class = "agent"), TRUE)
  expect_identical(isNLclass(agents = patches(w1), class = "agentset"), TRUE)
  expect_identical(isNLclass(agents = t1, class = "agentset"), TRUE)

  expect_identical(isNLclass(agents = patch(w1, xcor = c(0,2), ycor = c(1,0)), class = "patch"), FALSE)
  expect_identical(isNLclass(agents = noPatches(), class = "patchset"), FALSE)
  expect_identical(isNLclass(agents = turtle(t1, who = c(0,2)), class = "turtle"), FALSE)
  expect_identical(isNLclass(agents = noTurtles(), class = "turtleset"), FALSE)
  expect_identical(isNLclass(agents = cbind(xcor = 2, ycor = 3), class = "agent"), FALSE)
  expect_identical(isNLclass(agents = patches(w1), class = "agent"), FALSE)
  expect_identical(isNLclass(agents = patch(w1, xcor = 0, ycor = 0), class = "agentset"), FALSE)
  expect_identical(isNLclass(agents = turtle(t1, who = 0), class = "agentset"), FALSE)

  t2 <- turtlesOwn(turtles = t1, tVarName = "age")
  expect_identical(isNLclass(agents = t2, class = "agentset"), TRUE)
  expect_identical(isNLclass(agents = t2, class = "turtleset"), TRUE)
})

test_that("nOf works",{
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  p1 <- nOf(agents = patches(world = w1), n = 1)
  expect_equivalent(nrow(p1), 1)
  p11 <- as.matrix(merge(p1, patches(world = w1)))
  expect_equivalent(p1, p11)
  p2 <- nOf(agents = patches(w1), n = nrow(patches(w1)))
  expect_equivalent(nrow(p2), nrow(patches(w1)))
  expect_equivalent(p2, patches(w1))
  p3 <- nOf(agents = patches(w1), n = 10)
  expect_identical(nrow(p3), nrow(unique(p3)))

  t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10))
  t2 <- nOf(agents = t1, n = 2)
  expect_equivalent(length(t2), 2)
  t2coords <- merge(t2@coords, t1@coords)
  expect_equivalent(nrow(t2coords), 2)
  expect_equivalent(nrow(unique(t2coords)), 2)
  t2data <- merge(t2@data, t1@data)
  expect_equivalent(nrow(t2data), 2)
  expect_equivalent(nrow(unique(t2data)), 2)
  t3 <- nOf(agents = t1, n = 10)
  expect_identical(t3, t1)
})

test_that("oneOf works",{
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  p1 <- oneOf(agents = patches(world = w1))
  expect_equivalent(nrow(p1), 1)
  p11 <- as.matrix(merge(p1, patches(world = w1)))
  expect_equivalent(p1, p11)
  p2 <- oneOf(agents = patch(w1, xcor = 0, ycor = 0))
  p3 <- nOf(agents = patch(w1, xcor = 0, ycor = 0), n = 1)
  expect_identical(p2, p3)

  t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10))
  t2 <- oneOf(agents = t1)
  expect_equivalent(length(t2), 1)
  t2coords <- merge(t2@coords, t1@coords)
  expect_equivalent(nrow(t2coords), 1)
  t2data <- merge(t2@data, t1@data)
  expect_equivalent(nrow(t2data), 1)
  t3 <- nOf(agents = turtle(t1, who = 0), n = 1)
  t4 <- oneOf(agents = turtle(t1, who = 0))
  expect_identical(t3, t4)
})

test_that("maxNof works",{
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  w1[] <- 25:1
  p1 <- maxNof(agents = patches(world = w1), n = 5, world = w1)
  expect_identical(p1, PxcorPycorFromCell(world = w1, 1:5))
  p2 <- maxNof(agents = patches(world = w1), n = 1, world = w1)
  expect_equivalent(p2, PxcorPycorFromCell(world = w1, 1))
  p3 <- maxNof(agents = patches(world = w1), n = length(w1), world = w1)
  expect_equivalent(nrow(p3), 25)
  expect_identical(p3, patches(w1))
  w1[] <- 25
  p4 <- maxNof(agents = patches(world = w1), n = 5, world = w1)
  expect_equivalent(nrow(p4), 5)
  p5 <- maxNof(agents = patches(world = w1), n = 0, world = w1)
  expect_equivalent(nrow(p5), 0)
  expect_identical(p5, noPatches())

  t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10), heading = 1:10)
  t2 <- maxNof(agents = t1, n = 5, varName = "heading")
  expect_identical(t2, turtle(t1, who = c(5,6,7,8,9)))
  t3 <- maxNof(agents = t1, n = 1, varName = "heading")
  expect_identical(t3, turtle(t1, who = 9))
  t4 <- maxNof(agents = t1, n = 10, varName = "heading")
  expect_identical(t4, t1)
  t5 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10), heading = 10)
  t6 <- maxNof(agents = t5, n = 5, varName = "heading")
  expect_equivalent(length(t6), 5)
  t7 <- minNof(agents = t1, n = 0, varName = "heading")
  expect_identical(t7, noTurtles())
  expect_equivalent(length(t7), 0)
})

test_that("minNof works",{
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  w1[] <- 1:25
  p1 <- minNof(agents = patches(world = w1), n = 5, world = w1)
  expect_identical(p1, PxcorPycorFromCell(world = w1, 1:5))
  p2 <- minNof(agents = patches(world = w1), n = 1, world = w1)
  expect_equivalent(p2, PxcorPycorFromCell(world = w1, 1))
  p3 <- minNof(agents = patches(world = w1), n = length(w1), world = w1)
  expect_equivalent(nrow(p3), 25)
  expect_identical(p3, patches(w1))
  w1[] <- 25
  p4 <- minNof(agents = patches(world = w1), n = 5, world = w1)
  expect_equivalent(nrow(p4), 5)
  p5 <- minNof(agents = patches(world = w1), n = 0, world = w1)
  expect_equivalent(nrow(p5), 0)
  expect_identical(p5, noPatches())

  t1 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10), heading = 10:1)
  t2 <- minNof(agents = t1, n = 5, varName = "heading")
  expect_identical(t2, turtle(t1, who = c(5,6,7,8,9)))
  t3 <- minNof(agents = t1, n = 1, varName = "heading")
  expect_identical(t3, turtle(t1, who = 9))
  t4 <- minNof(agents = t1, n = 10, varName = "heading")
  expect_identical(t4, t1)
  t5 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10), heading = 10)
  t6 <- minNof(agents = t5, n = 5, varName = "heading")
  expect_equivalent(length(t6), 5)
  t7 <- minNof(agents = t1, n = 0, varName = "heading")
  expect_identical(t7, noTurtles())
  expect_equivalent(length(t7), 0)
})

test_that("inRadius works",{
  # Patches to patches
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  p1 <- inRadius(agents1 = patch(w1, 0, 0), radius = 2, agents2 = patches(w1), world = w1)
  expect_identical(p1[[1]], cbind(pxcor = c(0, 0, 1, 0, 1, 2), pycor = c(2, 1, 1, 0, 0, 0)))
  p2 <- inRadius(agents1 = patches(w1), radius = 2, agents2 = patch(w1, 0, 0), world = w1)
  expect_equivalent(p2[[11]], cbind(pxcor = 0, pycor = 0))
  expect_equivalent(p2[[1]], noPatches())
  expect_identical(p2[[1]], p2[[2]])
  p3 <- inRadius(agents1 = patch(w1, 0, 0), radius = 2, agents2 = patch(w1, 0, 0), world = w1)
  expect_equivalent(p3[[1]], patch(w1, 0, 0))
  expect_equivalent(length(p3), 1)
  p4 <- inRadius(agents1 = patches(w1), radius = 10, agents2 = patches(w1), world = w1)
  expect_identical(p4[[1]], patches(w1))
  expect_equivalent(length(p4), nrow(patches(w1)))
  expect_identical(p4[[1]], p4[[2]])

  # Patches to turtles
  t1 <- createTurtles(n = 5, coords = cbind(xcor = 0:4, ycor = 0:4))
  t2 <- inRadius(agents1 = patch(w1, xcor = 0, ycor = 0), radius = 1, agents2 = t1, world = w1)
  expect_identical(t2[[1]], turtle(t1, 0))
  t3 <- inRadius(agents1 = patch(w1, xcor = 0, ycor = 0), radius = 2, agents2 = t1, world = w1)
  expect_identical(t3[[1]], turtle(t1, c(0, 1)))
  t4 <- inRadius(agents1 = patch(w1, xcor = 0, ycor = 0), radius = 2, agents2 = t1, world = w1, torus = TRUE)
  expect_identical(t4[[1]], turtle(t1, c(0, 1, 4)))
  t5 <- inRadius(agents1 = patches(w1), radius = 1, agents2 = t1, world = w1)
  expect_equivalent(length(t5), nrow(patches(w1)))
  expect_identical(t5[[5]], turtle(t1, 4))
  expect_identical(t5[[9]], turtle(t1, 3))
  t6 <- inRadius(agents1 = patches(w1), radius = 10, agents2 = t1, world = w1)
  expect_identical(t6[[1]], t1)
  expect_identical(t6[[25]], t1)
  t7 <- inRadius(agents1 = patches(w1), radius = 10, agents2 = t1, world = w1, torus = TRUE)
  expect_identical(t7[[1]], t1)
  expect_identical(t7[[25]], t1)

  # Turtles to patches
  p5 <- inRadius(agents1 = turtle(t1, 0), radius = 2, agents2 = patches(w1), world = w1)
  expect_identical(p5, p1)
  p6 <- inRadius(agents1 = t1, radius = 0.5, agents2 = patches(w1), world = w1)
  expect_equivalent(length(p6), length(t1))
  expect_equivalent(p6[[1]], turtle(t1, 0)@coords)
  p7 <- inRadius(agents1 = turtle(t1, 0), radius = 1, agents2 = patches(w1), world = w1, torus = TRUE)
  expect_equivalent(nrow(merge(p7[[1]], cbind(pxcor = c(0, 0, 0, 1, 4), pycor = c(4, 1, 0, 0, 0)))), nrow(p7[[1]]))
  p8 <- inRadius(agents1 = turtle(t1, c(0, 4)), radius = 1, agents2 = patches(w1), world = w1, torus = TRUE)
  expect_equivalent(nrow(merge(p8[[1]], cbind(pxcor = c(0, 0, 0, 1, 4), pycor = c(4, 1, 0, 0, 0)))), nrow(p8[[1]]))
  expect_equivalent(nrow(merge(p8[[2]], cbind(pxcor = c(0, 3, 4, 4, 4), pycor = c(4, 4, 4, 3, 0)))), nrow(p8[[2]]))
  p9 <- inRadius(agents1 = turtle(t1, 0), radius = 1, agents2 = patch(w1, 4, 4), world = w1)
  expect_equivalent(p9[[1]], noPatches())

  # Turtles to turtles
  t8 <- inRadius(agents1 = turtle(t1, 0), radius = 1, agents = t1, world = w1)
  expect_identical(t8[[1]], turtle(t1, 0))
  t9 <- inRadius(agents1 = turtle(t1, 0), radius = 2, agents = t1, world = w1)
  expect_identical(t9[[1]], turtle(t1, c(0, 1)))
  t10 <- inRadius(agents1 = turtle(t1, 0), radius = 2, agents = t1, world = w1, torus = TRUE)
  expect_identical(t10[[1]], turtle(t1, c(0, 1, 4)))
  t11 <- inRadius(agents1 = t1, radius = 10, agents2 = t1, world = w1)
  expect_identical(length(t11), length(t1))
  expect_identical(t11[[1]], t1)
})

