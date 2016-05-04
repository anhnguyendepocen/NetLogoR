test_that("NLall works with patches",{
  w1 <- createNLworld(0, 4, 0, 4)
  w1[] <- sample(1:5, size = 25, replace = TRUE)
  expect_identical(NLall(world = w1, agents = patches(world = w1), val = 5), FALSE)
  w2 <- w1
  w2[] <- 5
  expect_identical(NLall(world = w2, agents = patches(world = w2), val = 5), TRUE)
  w3 <- w2
  w3[0,0] <- 4
  expect_identical(NLall(world = w2, agents = patch(world = w2, x = c(0,1,2,3,4), y = c(4,4,4,4,4)), val = 5), TRUE)

  ws <- NLstack(w1, w2, w3)
  expect_identical(NLall(world = ws, agents = patches(world = ws), var = "w1", val = 5), FALSE)
  expect_identical(NLall(world = ws, agents = patches(world = ws), var = "w2", val = 5), TRUE)
  expect_identical(NLall(world = ws, agents = patch(world = ws, x = c(0,1,2,3,4), y = c(4,4,4,4,4)), var = "w3", val = 5), TRUE)
})

test_that("NLall works with turtles",{
  t1 <- createTurtles(n = 5, coords = cbind(xcor = 1, ycor = 1), heading = c(1, 2, 2, 1, 2))
  expect_identical(NLall(agents = t1, var = "xcor", val = 1), TRUE)
  expect_identical(NLall(agents = t1, var = "heading", val = 2), FALSE)
  expect_identical(NLall(agents = turtle(t1, who = c(1, 2, 4)), var = "heading", val = 2), TRUE)
  expect_identical(NLall(agents = t1, var = "xcor", val = 2), FALSE)
})

test_that("NLall works with NLworldMs and agentMatrix",{
  w1 <- createNLworldMatrix(0, 4, 0, 4, data = sample(1:5, size = 25, replace = TRUE))
  expect_identical(NLall(world = w1, agents = patches(world = w1), val = 5), FALSE)
  w2 <- w1
  w2[] <- 5
  expect_identical(NLall(world = w2, agents = patches(world = w2), val = 5), TRUE)
  w3 <- w2
  w3[0,0] <- 4
  expect_identical(NLall(world = w2, agents = patch(world = w2, x = c(0,1,2,3,4), y = c(4,4,4,4,4)), val = 5), TRUE)

  ws <- NLworldArray(w1, w2, w3)
  expect_identical(NLall(world = ws, agents = patches(world = ws), var = "w1", val = 5), FALSE)
  expect_identical(NLall(world = ws, agents = patches(world = ws), var = "w2", val = 5), TRUE)
  expect_identical(NLall(world = ws, agents = patch(world = ws, x = c(0,1,2,3,4), y = c(4,4,4,4,4)), var = "w3", val = 5), TRUE)

  # Turtles
  t1 <- createTurtlesAM(n = 5, coords = cbind(xcor = 1, ycor = 1), heading = c(1, 2, 2, 1, 2))
  expect_identical(NLall(agents = t1, var = "xcor", val = 1), TRUE)
  expect_identical(NLall(agents = t1, var = "heading", val = 2), FALSE)
  expect_identical(NLall(agents = turtle(t1, who = c(1, 2, 4)), var = "heading", val = 2), TRUE)
  expect_identical(NLall(agents = t1, var = "xcor", val = 2), FALSE)
})

test_that("NLany works with patches",{
  w1 <- createNLworld(0, 4, 0, 4)
  p1 <- noPatches()
  p2 <- patch(world = w1, x = 0, y = 0)
  p3 <- patch(world = w1, x = -1, y = -1)
  p4 <- patches(world = w1)
  p5 <- patch(world = w1, x = c(-1, 0), y = c(-1, 0))
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
  t3 <- NLwith(agents = t1, var = "xcor", val = 10)
  t4 <- turtle(t1, who = 0)
  expect_identical(NLany(t1), TRUE)
  expect_identical(NLany(t2), FALSE)
  expect_identical(NLany(t3), FALSE)
  expect_identical(NLany(t4), TRUE)
})

test_that("NLany works with NLworldMs and agentMatrix",{
  w1 <- createNLworldMatrix(0, 4, 0, 4)
  p1 <- noPatches()
  p2 <- patch(world = w1, x = 0, y = 0)
  p3 <- patch(world = w1, x = -1, y = -1)
  p4 <- patches(world = w1)
  p5 <- patch(world = w1, x = c(-1, 0), y = c(-1, 0))
  expect_identical(NLany(p1), FALSE)
  expect_identical(NLany(p2), TRUE)
  expect_identical(NLany(p3), FALSE)
  expect_identical(NLany(p4), TRUE)
  expect_identical(NLany(p5), TRUE)

  # Turtles
  t1 <- createTurtlesAM(n = 10, coords = randomXYcor(world= w1, n = 10))
  t2 <- noTurtlesAM()
  t3 <- NLwith(agents = t1, var = "xcor", val = 10)
  t4 <- turtle(t1, who = 0)
  expect_identical(NLany(t1), TRUE)
  expect_identical(NLany(t2), FALSE)
  expect_identical(NLany(t3), FALSE)
  expect_identical(NLany(t4), TRUE)
})

test_that("count works with patches",{
  w1 <- createNLworld(0, 4, 0, 4)
  p1 <- noPatches()
  p2 <- patch(world = w1, x = 0, y = 0)
  p3 <- patch(world = w1, x = -1, y = -1)
  p4 <- patches(world = w1)
  p5 <- patch(world = w1, x = c(-1, 0), y = c(-1, 0))
  p6 <- patch(world = w1, x = c(-1, 0), y = c(0, 0))
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

test_that("count works with agentMatrix",{
  w1 <- createNLworldMatrix(0, 4, 0, 4)
  t1 <- noTurtlesAM()
  t2 <- createTurtlesAM(n = 10, coords = randomXYcor(world = w1, n = 10))
  t3 <- turtle(turtles = t2, who = c(1, 2, 3))
  expect_equivalent(count(t1), 0)
  expect_equivalent(count(t2), 10)
  expect_equivalent(count(t3), 3)
})


test_that("sortOn works with patches",{
  w1 <- createNLworld(0, 4, 0, 4)
  w1[] <- 25:1
  p1 <- sortOn(world = w1, agents = patches(world = w1))
  expect_equivalent(cbind(p1[1,1], p1[1,2]), patch(w1, x = 4, y = 0))
  expect_equivalent(cbind(p1[25,1],p1[25,2]), patch(w1, x = 0, y = 4))

  w2 <- w1
  w2[] <- 1:25
  ws <- NLstack(w1, w2)
  p1 <- sortOn(world = ws, agents = patch(world = ws, x = c(0,1,2,3,4), y = c(4,4,4,4,4)), var = "w1")
  p2 <- sortOn(world = ws, agents = patch(world = ws, x = c(0,1,2,3,4), y = c(4,4,4,4,4)), var = "w2")
  expect_equivalent(p1, cbind(pxcor = c(4,3,2,1,0), pycor = c(4,4,4,4,4)))
  expect_equivalent(p2, cbind(pxcor = c(0,1,2,3,4), pycor = c(4,4,4,4,4)))
})

test_that("sortOn works with turtles",{
  t1 <- createTurtles(n = 5, coords = cbind(xcor = 1:5, ycor = 5:1), heading = c(4,5,1,3,2))
  t2 <- sortOn(agents = t1, var = "xcor")
  t3 <- sortOn(agents = t1, var = "ycor")
  t4 <- sortOn(agents = t1, var = "heading")
  expect_equivalent(t2@data$who, c(0,1,2,3,4))
  expect_equivalent(t3@data$who, c(4,3,2,1,0))
  expect_equivalent(t4@data$who, c(2, 4, 3, 0, 1))

  t5 <- sortOn(agents = turtle(turtles = t1, who = 0), var = "heading")
  expect_equivalent(t5@data$who, 0)
  t6 <- createTurtles(n = 5, coords = cbind(xcor = 1, ycor = 1), heading = 1)
  t7 <- sortOn(agents = t6, var = "xcor")
  expect_equivalent(t6, t7)
})

test_that("sortOn works with NLworldMs and agentMatrix",{
  w1 <- createNLworldMatrix(0, 4, 0, 4, data = 25:1)
  p1 <- sortOn(world = w1, agents = patches(world = w1))
  expect_equivalent(cbind(p1[1,1], p1[1,2]), patch(w1, x = 4, y = 0))
  expect_equivalent(cbind(p1[25,1],p1[25,2]), patch(w1, x = 0, y = 4))

  w2 <- w1
  w2[] <- 1:25
  ws <- NLworldArray(w1, w2)
  p1 <- sortOn(world = ws, agents = patch(world = ws, x = c(0,1,2,3,4), y = c(4,4,4,4,4)), var = "w1")
  p2 <- sortOn(world = ws, agents = patch(world = ws, x = c(0,1,2,3,4), y = c(4,4,4,4,4)), var = "w2")
  expect_equivalent(p1, cbind(pxcor = c(4,3,2,1,0), pycor = c(4,4,4,4,4)))
  expect_equivalent(p2, cbind(pxcor = c(0,1,2,3,4), pycor = c(4,4,4,4,4)))

  #Turtles
  t1 <- createTurtlesAM(n = 5, coords = cbind(xcor = 1:5, ycor = 5:1), heading = c(4,5,1,3,2))
  t2 <- sortOn(agents = t1, var = "xcor")
  t3 <- sortOn(agents = t1, var = "ycor")
  t4 <- sortOn(agents = t1, var = "heading")
  expect_equivalent(t2@.Data[,"who"], c(0,1,2,3,4))
  expect_equivalent(t3@.Data[,"who"], c(4,3,2,1,0))
  expect_equivalent(t4@.Data[,"who"], c(2, 4, 3, 0, 1))

  t5 <- sortOn(agents = turtle(turtles = t1, who = 0), var = "heading")
  expect_equivalent(t5@.Data[,"who"], 0)
  t6 <- createTurtlesAM(n = 5, coords = cbind(xcor = 1, ycor = 1), heading = 1)
  t7 <- sortOn(agents = t6, var = "xcor")
  expect_equivalent(t6, t7)
})

test_that("NLwith works with patches",{
  w1 <- createNLworld(0, 4, 0, 4)
  w1[] <- 0
  w1[1,c(1,4)] <- c(1,1)
  p1 <- NLwith(world = w1, agents = patches(world = w1), val = 1)
  expect_identical(p1, patch(w1, x = c(1,1), y = c(4,1)))
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
  p1w1 <- NLwith(world = ws, agents = patches(world = ws), var = "w1", val = 1)
  p1w2 <- NLwith(world = ws, agents = patches(world = ws), var = "w2", val = 1)
  expect_identical(p1w1, patch(ws, x = c(1,1), y = c(4,1)))
  expect_identical(p1w2, patch(ws, x = c(2,2), y = c(4,2)))
  p5 <- NLwith(agents = patches(world = ws), world = ws, var = "w1", val = 10)
  expect_equivalent(p5, noPatches())

  # With NLworldMatrix
  valw3 <- rep(0, 25)
  valw3[c(2, 17)] <- 1
  w3 <- createNLworldMatrix(data = valw3, minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  p3 <- NLwith(world = w3, agents = patches(world = w3), val = 1)
  expect_equivalent(p1, p3)
  p4 <- NLwith(agents = patches(world = w3), world = w3, val = 0)
  expect_equivalent(p2, p4)
  p5 <- NLwith(agents = patches(world = w3), world = w3, val = c(0, 1))
  expect_equivalent(patches(world = w1), p5)
  p6 <- NLwith(agents = patches(world = w3), world = w3, val = 10)
  expect_equivalent(p6, noPatches())

  # With NLworldArray
  valw4 <- rep(0, 25)
  valw4[c(3, 13)] <- 1
  w4 <- createNLworldMatrix(data = valw4, minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  w5 <- NLworldArray(w3, w4)
  p7 <- NLwith(world = w5, agents = patches(world = w5), var = "w3", val = 1)
  p8 <- NLwith(world = w5, agents = patches(world = w5), var = "w4", val = 1)
  expect_equivalent(p1w1, p7)
  expect_equivalent(p1w2, p8)
  p9 <- NLwith(agents = patches(world = w5), world = w5, var = "w3", val = 10)
  expect_equivalent(p9, noPatches())
})

test_that("NLwith works with turtles",{
  t1 <- createTurtles(n = 5, coords = cbind(xcor = c(1,1,1,2,3), ycor = c(2,3,4,4,5)), heading = 0,
                      breed = c("sheep", "sheep", "wolf", "sheep", "sheperd"))
  t2 <- NLwith(agents = t1, var = "xcor", val = 1)
  expect_identical(t2, turtle(turtles = t1, who = c(0, 1, 2)))
  t3 <- NLwith(agents = t1, var = "ycor", val = c(2, 3))
  expect_identical(t3, turtle(turtles = t1, who = c(0, 1)))
  t4 <- NLwith(agents = t1, var = "heading", val = 0)
  expect_identical(t4, t1)
  t5 <- NLwith(agents = t1, var = "breed", val = "sheep")
  expect_identical(t5, turtle(turtles = t1, who = c(0, 1, 3)))
  t6 <- NLwith(agents = t1, var = "breed", val = c("sheep", "wolf"))
  expect_identical(t6, turtle(turtles = t1, who = c(0, 1, 2, 3)))
  t7 <- NLwith(agents = t1, var = "breed", val = "moose")
  expect_equivalent(t7, noTurtles())
})

test_that("NLwith works with agentMatrix",{
  t1 <- createTurtlesAM(n = 5, coords = cbind(xcor = c(1,1,1,2,3), ycor = c(2,3,4,4,5)), heading = 0,
                      breed = c("sheep", "sheep", "wolf", "sheep", "sheperd"))
  t2 <- NLwith(agents = t1, var = "xcor", val = 1)
  expect_identical(t2, turtle(turtles = t1, who = c(0, 1, 2)))
  t3 <- NLwith(agents = t1, var = "ycor", val = c(2, 3))
  expect_identical(t3, turtle(turtles = t1, who = c(0, 1)))
  t4 <- NLwith(agents = t1, var = "heading", val = 0)
  expect_identical(t4, t1)
  t5 <- NLwith(agents = t1, var = "breed", val = "sheep")
  expect_identical(t5, turtle(turtles = t1, who = c(0, 1, 3)))
  t6 <- NLwith(agents = t1, var = "breed", val = c("sheep", "wolf"))
  expect_identical(t6, turtle(turtles = t1, who = c(0, 1, 2, 3)))
  t7 <- NLwith(agents = t1, var = "breed", val = "moose")
  expect_equivalent(t7, noTurtlesAM())
})

test_that("withMax works with patches",{
  w1 <- createNLworld(0, 4, 0, 4)
  expect_error(withMax(agents = patches(world = w1), world = w1))
  w1[] <- 1
  expect_identical(withMax(agents = patches(w1), world = w1), patches(w1))
  w1[] <- runif(25)
  w1[1,c(1,4)] <- c(2,2)
  pMax <- withMax(world = w1, agents = patches(world = w1))
  expect_identical(pMax, patch(w1, x = c(1,1), y = c(4,1)))

  w2 <- w1
  w2[] <- runif(25)
  w2[2,c(2,4)] <- c(2,2)
  ws <- NLstack(w1, w2)
  pMaxw1 <- withMax(world = ws, agents = patches(world = ws), var = "w1")
  pMaxw2 <- withMax(world = ws, agents = patches(world = ws), var = "w2")
  expect_identical(pMaxw1, patch(ws, x = c(1,1), y = c(4,1)))
  expect_identical(pMaxw2, patch(ws, x = c(2,2), y = c(4,2)))

  w1[1,1] <- 0
  pMax <- withMax(world = w1, agents = patches(world = w1))
  expect_identical(pMax, patch(w1, x = 1, y = 4))
})

test_that("withMax works with turtles",{
  t1 <- createTurtles(n = 10, coords = cbind(xcor = 1:10, ycor = 10:1), heading = c(1,2,3,4,4,2,3,4,4,3))
  maxXcor <- withMax(agents = t1, var = "xcor")
  expect_equivalent(maxXcor, turtle(t1, who = 9))
  maxHeading <- withMax(agents = t1, var = "heading")
  expect_equivalent(maxHeading, turtle(t1, who = c(3, 4, 7, 8)))
  expect_error(withMax(agents = t1, var = "prevX"))
})

test_that("withMin works with patches",{
  w1 <- createNLworld(0, 4, 0, 4)
  expect_error(withMin(agents = patches(world = w1), world = w1))
  w1[] <- 1
  expect_identical(withMin(agents = patches(w1), world = w1), patches(w1))
  w1[] <- runif(25)
  w1[1,c(1,4)] <- c(-1,-1)
  pMin <- withMin(world = w1, agents = patches(world = w1))
  expect_identical(pMin, patch(w1, x = c(1,1), y = c(4,1)))

  w2 <- w1
  w2[] <- runif(25)
  w2[2,c(2,4)] <- c(-1,-1)
  ws <- NLstack(w1, w2)
  pMinw1 <- withMin(world = ws, agents = patches(world = ws), var = "w1")
  pMinw2 <- withMin(world = ws, agents = patches(world = ws), var = "w2")
  expect_identical(pMinw1, patch(ws, x = c(1,1), y = c(4,1)))
  expect_identical(pMinw2, patch(ws, x = c(2,2), y = c(4,2)))

  w1[1,1] <- 0
  pMin <- withMin(world = w1, agents = patches(world = w1))
  expect_identical(pMin, patch(w1, x = 1, y = 4))
})

test_that("withMin works with turtles",{
  t1 <- createTurtles(n = 10, coords = cbind(xcor = 10:1, ycor = 10:1), heading = c(1,2,3,0,0,2,3,0,0,3))
  maxXcor <- withMin(agents = t1, var = "xcor")
  expect_equivalent(maxXcor, turtle(t1, who = 9))
  maxHeading <- withMin(agents = t1, var = "heading")
  expect_equivalent(maxHeading, turtle(t1, who = c(3, 4, 7, 8)))
  expect_error(withMin(agents = t1, var = "prevX"))
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
  onepMax1 <- maxOneOf(world = ws, agents = patches(world = w1), var = "w1")
  onepMax2 <- maxOneOf(world = ws, agents = patches(world = w1), var = "w2")
  compare1 <- cbind(a = as.numeric(allpMax[,1])==as.numeric(onepMax1[1]),b = as.numeric(allpMax[,2])==as.numeric(onepMax1[2]))
  rowTRUE <- compare1[compare1[,1] == TRUE & compare1[,2] == TRUE,,drop = FALSE]
  expect_equivalent(nrow(rowTRUE),1)
  compare2 <- onepMax2 == onepMax1
  expect_less_than(length(compare2[compare2 == TRUE]), 2)
})

test_that("maxOneOf works with turtles",{
  t1 <- createTurtles(n = 10, coords = cbind(xcor = 1:10, ycor = 10:1), heading = c(1,2,3,4,4,2,3,4,4,3))
  maxXcor1 <- withMax(agents = t1, var = "xcor")
  maxXcor2 <- maxOneOf(agents = t1, var = "xcor")
  expect_equivalent(maxXcor1, maxXcor2)
  maxHeading1 <- withMax(agents = t1, var = "heading")
  maxHeading2 <- maxOneOf(agents = t1, var = "heading")
  expect_equivalent(length(maxHeading2), 1)
  maxH12 <- merge(maxHeading1@data, maxHeading2@data)
  expect_equivalent(nrow(maxH12), 1)
  expect_error(maxOneOf(agents = t1, var = "prevX"))
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
  onepMin1 <- minOneOf(world = ws, agents = patches(world = w1), var = "w1")
  onepMin2 <- minOneOf(world = ws, agents = patches(world = w1), var = "w2")
  compare1 <- cbind(a = as.numeric(allpMin[,1])==as.numeric(onepMin1[1]),b = as.numeric(allpMin[,2])==as.numeric(onepMin1[2]))
  rowTRUE <- compare1[compare1[,1] == TRUE & compare1[,2] == TRUE,,drop = FALSE]
  expect_equivalent(nrow(rowTRUE),1)
  compare2 <- onepMin2 == onepMin1
  expect_less_than(length(compare2[compare2 == TRUE]), 2)
})

test_that("minOneOf works with turtles",{
  t1 <- createTurtles(n = 10, coords = cbind(xcor = 1:10, ycor = 10:1), heading = c(1,2,3,4,4,2,3,4,4,3))
  minXcor1 <- withMin(agents = t1, var = "xcor")
  minXcor2 <- minOneOf(agents = t1, var = "xcor")
  expect_equivalent(minXcor1, minXcor2)
  minHeading1 <- withMin(agents = t1, var = "heading")
  minHeading2 <- minOneOf(agents = t1, var = "heading")
  expect_equivalent(length(minHeading2), 1)
  minH12 <- merge(minHeading1@data, minHeading2@data)
  expect_equivalent(nrow(minH12), 1)
  expect_error(minOneOf(agents = t1, var = "prevX"))
})

test_that("isNLclass works",{
  w1 <- createNLworld(0, 4, 0, 4)
  t1 <- createTurtles(n = 10, randomXYcor(w1, n = 10))
  expect_identical(isNLclass(agents = patch(w1, x = 0, y = 0), class = "patch"), TRUE)
  expect_identical(isNLclass(agents = patches(w1), class = "patchset"), TRUE)
  expect_identical(isNLclass(agents = turtle(t1, who = 0), class = "turtle"), TRUE)
  expect_identical(isNLclass(agents = t1, class = "turtleset"), TRUE)
  expect_identical(isNLclass(agents = patch(w1, x = 0, y = 0), class = "agent"), TRUE)
  expect_identical(isNLclass(agents = turtle(t1, who = 0), class = "agent"), TRUE)
  expect_identical(isNLclass(agents = patches(w1), class = "agentset"), TRUE)
  expect_identical(isNLclass(agents = t1, class = "agentset"), TRUE)

  expect_identical(isNLclass(agents = patch(w1, x = c(0,2), y = c(1,0)), class = "patch"), FALSE)
  expect_identical(isNLclass(agents = noPatches(), class = "patchset"), FALSE)
  expect_identical(isNLclass(agents = turtle(t1, who = c(0,2)), class = "turtle"), FALSE)
  expect_identical(isNLclass(agents = noTurtles(), class = "turtleset"), FALSE)
  expect_identical(isNLclass(agents = cbind(xcor = 2, ycor = 3), class = "agent"), FALSE)
  expect_identical(isNLclass(agents = patches(w1), class = "agent"), FALSE)
  expect_identical(isNLclass(agents = patch(w1, x = 0, y = 0), class = "agentset"), FALSE)
  expect_identical(isNLclass(agents = turtle(t1, who = 0), class = "agentset"), FALSE)

  t2 <- turtlesOwn(turtles = t1, tVar = "age")
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
  expect_identical(nrow(merge(t1@data, t3@data)), nrow(t1@data))

  # With matrix ncol = 3
  n4 <-neighbors(world = w1, agents = t1, nNeighbors = 8)
  p4 <- nOf(agents = n4, n = 1)
  expect_equivalent(nrow(p4), length(t1))
  p5 <- nOf(agents = n4, n = 2)
  expect_equivalent(nrow(p5), length(t1) * 2)

  # With matrix ncol = 2 "whoTurtles" and "id"
  t4 <- turtlesOn(world = w1, turtles = t1, agents = patches(w1), simplify = FALSE)
  expect_error(nOf(agents = t4, n = 2))
  t5 <- nOf(agents = t4, n = 1)
  expect_equivalent(nrow(merge(as.data.frame(t5), t4, by.x = "t5", by.y = "whoTurtles")), length(unique(t4[,"id"])))

  # With agentMatrix
  AM1 <- createTurtlesAM(n = 10, coords = randomXYcor(world = w1, n = 10))
  AM2 <- nOf(agents = AM1, n = 2)
  expect_equivalent(nrow(AM2), 2)
  AM2coords <- merge(AM2@.Data[,c("xcor", "ycor")], AM1@.Data[,c("xcor", "ycor")])
  expect_equivalent(nrow(AM2coords), 2)
  expect_equivalent(nrow(unique(AM2coords)), 2)
  AM2data <- merge(AM2@.Data[,3:ncol(AM2@.Data)], AM1@.Data[,3:ncol(AM1@.Data)])
  expect_equivalent(nrow(AM2data), 2)
  expect_equivalent(nrow(unique(AM2data)), 2)
  AM3 <- nOf(agents = AM1, n = 10)
  expect_identical(nrow(merge(AM1@.Data, AM3@.Data)), nrow(AM1@.Data))
})

test_that("oneOf works",{
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  p1 <- oneOf(agents = patches(world = w1))
  expect_equivalent(nrow(p1), 1)
  p11 <- as.matrix(merge(p1, patches(world = w1)))
  expect_equivalent(p1, p11)
  p2 <- oneOf(agents = patch(w1, x = 0, y = 0))
  p3 <- nOf(agents = patch(w1, x = 0, y = 0), n = 1)
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

  # With matrix ncol = 3
  n4 <-neighbors(world = w1, agents = t1, nNeighbors = 4)
  p4 <- oneOf(n4)
  expect_equivalent(nrow(p4), length(t1))

  # With matrix ncol = 2 "whoTurtles" and "id"
  t4 <- turtlesOn(world = w1, turtles = t1, agents = patches(w1), simplify = FALSE)
  t5 <- oneOf(agents = t4)
  expect_equivalent(length(t5), length(unique(t4[,"id"])))
  expect_equivalent(nrow(merge(as.data.frame(t5), t4, by.x = "t5", by.y = "whoTurtles")), length(unique(t4[,"id"])))

  # With agentMatrix
  AM1 <- createTurtlesAM(n = 10, coords = randomXYcor(world = w1, n = 10))
  AM2 <- oneOf(agents = AM1)
  expect_equivalent(nrow(AM2), 1)
  AM2coords <- merge(AM2@.Data[,c("xcor", "ycor"), drop = FALSE], AM1@.Data[,c("xcor", "ycor")])
  expect_equivalent(nrow(AM2coords), 1)
  AM2data <- merge(AM2@.Data[,3:ncol(AM2@.Data), drop = FALSE], AM1@.Data[,3:ncol(AM1@.Data)])
  expect_equivalent(nrow(AM2data), 1)
  AM3 <- nOf(agents = turtle(AM1, who = 0), n = 1)
  AM4 <- oneOf(agents = turtle(AM1, who = 0))
  expect_identical(AM3, AM4)
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
  t2 <- maxNof(agents = t1, n = 5, var = "heading")
  expect_equivalent(t2, turtle(t1, who = c(5,6,7,8,9)))
  t3 <- maxNof(agents = t1, n = 1, var = "heading")
  expect_equivalent(t3, turtle(t1, who = 9))
  t4 <- maxNof(agents = t1, n = 10, var = "heading")
  expect_equivalent(t4, t1)
  t5 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10), heading = 10)
  t6 <- maxNof(agents = t5, n = 5, var = "heading")
  expect_equivalent(length(t6), 5)
  t7 <- minNof(agents = t1, n = 0, var = "heading")
  expect_equivalent(t7, noTurtles())
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
  t2 <- minNof(agents = t1, n = 5, var = "heading")
  expect_equivalent(t2, turtle(t1, who = c(5,6,7,8,9)))
  t3 <- minNof(agents = t1, n = 1, var = "heading")
  expect_equivalent(t3, turtle(t1, who = 9))
  t4 <- minNof(agents = t1, n = 10, var = "heading")
  expect_equivalent(t4, t1)
  t5 <- createTurtles(n = 10, coords = randomXYcor(world = w1, n = 10), heading = 10)
  t6 <- minNof(agents = t5, n = 5, var = "heading")
  expect_equivalent(length(t6), 5)
  t7 <- minNof(agents = t1, n = 0, var = "heading")
  expect_equivalent(t7, noTurtles())
  expect_equivalent(length(t7), 0)
})

test_that("inRadius works",{
  # Patches to patches
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  p1 <- inRadius(agents = patch(w1, 0, 0), radius = 2, agents2 = patches(w1), world = w1)
  expect_identical(p1[[1]], cbind(pxcor = c(0, 0, 1, 0, 1, 2), pycor = c(2, 1, 1, 0, 0, 0)))
  p2 <- inRadius(agents = patches(w1), radius = 2, agents2 = patch(w1, 0, 0), world = w1)
  expect_equivalent(p2[[11]], cbind(pxcor = 0, pycor = 0))
  expect_equivalent(p2[[1]], noPatches())
  expect_identical(p2[[1]], p2[[2]])
  p3 <- inRadius(agents = patch(w1, 0, 0), radius = 2, agents2 = patch(w1, 0, 0), world = w1)
  expect_equivalent(p3[[1]], patch(w1, 0, 0))
  expect_equivalent(length(p3), 1)
  p4 <- inRadius(agents = patches(w1), radius = 10, agents2 = patches(w1), world = w1)
  expect_identical(p4[[1]], patches(w1))
  expect_equivalent(length(p4), nrow(patches(w1)))
  expect_identical(p4[[1]], p4[[2]])

  # Patches to turtles
  t1 <- createTurtles(n = 5, coords = cbind(xcor = 0:4, ycor = 0:4))
  t2 <- inRadius(agents = patch(w1, x = 0, y = 0), radius = 1, agents2 = t1, world = w1)
  expect_identical(t2[[1]], turtle(t1, 0))
  t3 <- inRadius(agents = patch(w1, x = 0, y = 0), radius = 2, agents2 = t1, world = w1)
  expect_identical(t3[[1]], turtle(t1, c(0, 1)))
  t4 <- inRadius(agents = patch(w1, x = 0, y = 0), radius = 2, agents2 = t1, world = w1, torus = TRUE)
  expect_identical(t4[[1]], turtle(t1, c(0, 1, 4)))
  t5 <- inRadius(agents = patches(w1), radius = 1, agents2 = t1, world = w1)
  expect_equivalent(length(t5), nrow(patches(w1)))
  expect_identical(t5[[5]], turtle(t1, 4))
  expect_identical(t5[[9]], turtle(t1, 3))
  t6 <- inRadius(agents = patches(w1), radius = 10, agents2 = t1, world = w1)
  expect_identical(t6[[1]], t1)
  expect_identical(t6[[25]], t1)
  t7 <- inRadius(agents = patches(w1), radius = 10, agents2 = t1, world = w1, torus = TRUE)
  expect_equivalent(t7[[1]], t1)
  expect_equivalent(t7[[25]], t1)

  # Turtles to patches
  p5 <- inRadius(agents = turtle(t1, 0), radius = 2, agents2 = patches(w1), world = w1)
  expect_equivalent(p5, p1)
  p6 <- inRadius(agents = t1, radius = 0.5, agents2 = patches(w1), world = w1)
  expect_equivalent(length(p6), length(t1))
  expect_equivalent(p6[[1]], turtle(t1, 0)@coords)
  p7 <- inRadius(agents = turtle(t1, 0), radius = 1, agents2 = patches(w1), world = w1, torus = TRUE)
  expect_equivalent(nrow(merge(p7[[1]], cbind(pxcor = c(0, 0, 0, 1, 4), pycor = c(4, 1, 0, 0, 0)))), nrow(p7[[1]]))
  p8 <- inRadius(agents = turtle(t1, c(0, 4)), radius = 1, agents2 = patches(w1), world = w1, torus = TRUE)
  expect_equivalent(nrow(merge(p8[[1]], cbind(pxcor = c(0, 0, 0, 1, 4), pycor = c(4, 1, 0, 0, 0)))), nrow(p8[[1]]))
  expect_equivalent(nrow(merge(p8[[2]], cbind(pxcor = c(0, 3, 4, 4, 4), pycor = c(4, 4, 4, 3, 0)))), nrow(p8[[2]]))
  p9 <- inRadius(agents = turtle(t1, 0), radius = 1, agents2 = patch(w1, 4, 4), world = w1)
  expect_equivalent(p9[[1]], noPatches())

  # Turtles to turtles
  t8 <- inRadius(agents = turtle(t1, 0), radius = 1, agents2 = t1, world = w1)
  expect_identical(t8[[1]], turtle(t1, 0))
  t9 <- inRadius(agents = turtle(t1, 0), radius = 2, agents2 = t1, world = w1)
  expect_identical(t9[[1]], turtle(t1, c(0, 1)))
  t10 <- inRadius(agents = turtle(t1, 0), radius = 2, agents2 = t1, world = w1, torus = TRUE)
  expect_identical(t10[[1]], turtle(t1, c(0, 1, 4)))
  t11 <- inRadius(agents = t1, radius = 10, agents2 = t1, world = w1)
  expect_identical(length(t11), length(t1))
  expect_identical(t11[[1]], t1)

  # Works without the world provided when torus = FALSE
  p1 <- inRadius(agents = patch(w1, 0, 0), radius = 2, agents2 = patches(w1))
  expect_identical(p1[[1]], cbind(pxcor = c(0, 0, 1, 0, 1, 2), pycor = c(2, 1, 1, 0, 0, 0)))
  p2 <- inRadius(agents = patches(w1), radius = 2, agents2 = patch(w1, 0, 0))
  expect_equivalent(p2[[11]], cbind(pxcor = 0, pycor = 0))
  expect_equivalent(p2[[1]], noPatches())
  expect_identical(p2[[1]], p2[[2]])
  p3 <- inRadius(agents = patch(w1, 0, 0), radius = 2, agents2 = patch(w1, 0, 0))
  expect_equivalent(p3[[1]], patch(w1, 0, 0))
  expect_equivalent(length(p3), 1)
  p4 <- inRadius(agents = patches(w1), radius = 10, agents2 = patches(w1))
  expect_identical(p4[[1]], patches(w1))
  expect_equivalent(length(p4), nrow(patches(w1)))
  expect_identical(p4[[1]], p4[[2]])
  t2 <- inRadius(agents = patch(w1, x = 0, y = 0), radius = 1, agents2 = t1)
  expect_identical(t2[[1]], turtle(t1, 0))
  t3 <- inRadius(agents = patch(w1, x = 0, y = 0), radius = 2, agents2 = t1)
  expect_identical(t3[[1]], turtle(t1, c(0, 1)))
  expect_error(inRadius(agents = patch(w1, x = 0, y = 0), radius = 2, agents2 = t1, torus = TRUE))
  t5 <- inRadius(agents = patches(w1), radius = 1, agents2 = t1)
  expect_equivalent(length(t5), nrow(patches(w1)))
  expect_identical(t5[[5]], turtle(t1, 4))
  expect_identical(t5[[9]], turtle(t1, 3))
  t6 <- inRadius(agents = patches(w1), radius = 10, agents2 = t1)
  expect_identical(t6[[1]], t1)
  expect_identical(t6[[25]], t1)
  expect_error(inRadius(agents = patches(w1), radius = 10, agents2 = t1, torus = TRUE))
  p5 <- inRadius(agents = turtle(t1, 0), radius = 2, agents2 = patches(w1))
  expect_equivalent(p5, p1)
  p6 <- inRadius(agents = t1, radius = 0.5, agents2 = patches(w1))
  expect_equivalent(length(p6), length(t1))
  expect_equivalent(p6[[1]], turtle(t1, 0)@coords)
  expect_error(inRadius(agents = turtle(t1, 0), radius = 1, agents2 = patches(w1), torus = TRUE))
  expect_error(inRadius(agents = turtle(t1, c(0, 4)), radius = 1, agents2 = patches(w1), torus = TRUE))
  p9 <- inRadius(agents = turtle(t1, 0), radius = 1, agents2 = patch(w1, 4, 4))
  expect_equivalent(p9[[1]], noPatches())
  t8 <- inRadius(agents = turtle(t1, 0), radius = 1, agents2 = t1)
  expect_identical(t8[[1]], turtle(t1, 0))
  t9 <- inRadius(agents = turtle(t1, 0), radius = 2, agents2 = t1)
  expect_identical(t9[[1]], turtle(t1, c(0, 1)))
  expect_error(inRadius(agents = turtle(t1, 0), radius = 2, agents2 = t1, torus = TRUE))
  t11 <- inRadius(agents = t1, radius = 10, agents2 = t1)
  expect_identical(length(t11), length(t1))
  expect_identical(t11[[1]], t1)
})

test_that("inCone works",{
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  t1 <- createTurtles(n = 5, coords = cbind(xcor = 0:4, ycor = 0:4), heading = c(0, 90, 180, 270, 0))

  # Turtles to patches
  t2 <- inCone(turtles = t1, radius = 1, angle = 5, agents = patches(w1), world = w1, torus = FALSE)
  expect_equivalent(length(t2), 5)
  expect_equivalent(nrow(t2[[5]]), 1)
  t3 <- inCone(turtles = t1, radius = 1, angle = 5, agents = patches(w1), world = w1, torus = TRUE)
  expect_equivalent(length(t3), 5)
  expect_equivalent(nrow(t3[[5]]), 2)
  t4 <- inCone(turtles = turtle(t1, who = 0), radius = 1, angle = 181, agents = patches(w1), world = w1, torus = FALSE)
  expect_identical(t4[[1]], patch(w1, x = c(0,0,1), y = c(1, 0, 0)))
  t5 <- inCone(turtles = turtle(t1, who = 0), radius = 1, angle = 181, agents = patches(w1), world = w1, torus = TRUE)
  expect_equivalent(nrow(merge(t5[[1]], patch(w1, x = c(0,0,1, 4), y = c(1, 0, 0, 0)))), 4)
  t6 <- inCone(turtles = turtle(t1, who = 1), radius = 1, angle = 181, agents = patch(w1, x = 0, y = 0), world = w1, torus = FALSE)
  expect_equivalent(t6[[1]], noPatches())
  t7 <- inCone(turtles = turtle(t1, who = c(1,2)), radius = 1, angle = 181, agents = patch(w1, x = 2, y = 2), world = w1, torus = FALSE)
  expect_equivalent(length(t7),2)
  expect_equivalent(t7[[1]], noPatches())
  expect_equivalent(t7[[2]], patch(w1, x = 2, y = 2))

  # Turtles to turtles
  t8 <- inCone(turtles = t1, radius = 3, angle = 360, agents = t1, world = w1, torus = FALSE)
  expect_identical(t8[[3]], t1)
  expect_identical(t8[[1]], turtle(t1, who = c(0,1,2)))
  t9 <- inCone(turtles = t1, radius = 3, angle = 360, agents = t1, world = w1, torus = TRUE)
  expect_identical(t9[[3]], t1)
  expect_identical(t9[[1]], t1)
  t10 <- inCone(turtles = turtle(t1, 1), radius = 1, angle = 360, agents = turtle(t1, 4), world = w1)
  expect_equivalent(t10[[1]], noTurtles())
  t11 <- inCone(turtles = turtle(t1, 1), radius = 3, angle = 180, agents = t1, world = w1)
  expect_identical(t11[[1]], turtle(t1, who = c(1,2,3)))
  t12 <- inCone(turtles = turtle(t1, 4), radius = 3, angle = 180, agents = t1, world = w1)
  expect_equivalent(t12[[1]], turtle(t1, 4))
  t13 <- inCone(turtles = turtle(t1, 4), radius = 3, angle = 180, agents = t1, world = w1, torus = TRUE)
  expect_equivalent(t13[[1]], turtle(t1, c(0,1,4)))

  # Works without the world provided when torus = FALSE
  t2 <- inCone(turtles = t1, radius = 1, angle = 5, agents = patches(w1), torus = FALSE)
  expect_equivalent(length(t2), 5)
  expect_equivalent(nrow(t2[[5]]), 1)
  expect_error(inCone(turtles = t1, radius = 1, angle = 5, agents = patches(w1), torus = TRUE))
  t4 <- inCone(turtles = turtle(t1, who = 0), radius = 1, angle = 181, agents = patches(w1), torus = FALSE)
  expect_identical(t4[[1]], patch(w1, x = c(0,0,1), y = c(1, 0, 0)))
  expect_error(inCone(turtles = turtle(t1, who = 0), radius = 1, angle = 181, agents = patches(w1), torus = TRUE))
  t6 <- inCone(turtles = turtle(t1, who = 1), radius = 1, angle = 181, agents = patch(w1, x = 0, y = 0), torus = FALSE)
  expect_equivalent(t6[[1]], noPatches())
  t7 <- inCone(turtles = turtle(t1, who = c(1,2)), radius = 1, angle = 181, agents = patch(w1, x = 2, y = 2), torus = FALSE)
  expect_equivalent(length(t7),2)
  expect_equivalent(t7[[1]], noPatches())
  expect_equivalent(t7[[2]], patch(w1, x = 2, y = 2))
  t8 <- inCone(turtles = t1, radius = 3, angle = 360, agents = t1, torus = FALSE)
  expect_identical(t8[[3]], t1)
  expect_identical(t8[[1]], turtle(t1, who = c(0,1,2)))
  expect_error(inCone(turtles = t1, radius = 3, angle = 360, agents = t1, torus = TRUE))
  t10 <- inCone(turtles = turtle(t1, 1), radius = 1, angle = 360, agents = turtle(t1, 4))
  expect_equivalent(t10[[1]], noTurtles())
  t11 <- inCone(turtles = turtle(t1, 1), radius = 3, angle = 180, agents = t1)
  expect_identical(t11[[1]], turtle(t1, who = c(1,2,3)))
  t12 <- inCone(turtles = turtle(t1, 4), radius = 3, angle = 180, agents = t1)
  expect_equivalent(t12[[1]], turtle(t1, 4))
  expect_error(inCone(turtles = turtle(t1, 4), radius = 3, angle = 180, agents = t1, torus = TRUE))
})

test_that("set works",{
  # Set work with patches
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  w1[] <- 1:25
  w1 <- set(world = w1, agents = patches(w1), val = 0)
  expect_equivalent(values(w1), rep(0, length(w1)))
  w1 <- set(world = w1, agents = patches(w1), val = 1:25)
  expect_equivalent(values(w1), 1:25)
  w1 <- set(world = w1, agents = patch(w1, 0, 0), val = 100)
  expect_equivalent(w1[0,0], 100)
  w1 <- set(world = w1, agents = patch(w1, c(-1, 0), c(-1, 0)), val = -10)
  expect_equivalent(w1[0,0], -10)
  w1_ <- set(world = w1, agents = patch(w1, c(-1, -2), c(-1, 0)), val = -20)
  expect_equivalent(w1, w1_)

  w1[] <- 1:25
  w2 <- w1
  w2[] <- 25:1
  w3 <- NLstack(w1, w2)
  w3 <- set(world = w3, agents = patches(w3), var = "w1", val = 0)
  expect_equivalent(values(w3)[,"w1"], rep(0, length(w1)))
  w3 <- set(world = w3, agents = patches(w3), var = "w1", val = 1:25)
  expect_equivalent(values(w3)[,"w1"], 1:25)
  w3 <- set(world = w3, agents = patch(w3, 0, 0), var = "w1", val = 100)
  expect_equivalent(w3[0,0][1], 100)

  # With multiple values
  w3 <- set(world = w3, agents = patch(w3, 0, 0), var = c("w1", "w2"), val = cbind(w1 = 0, w2 = 100))
  expect_equivalent(w3[0,0], cbind(w1 = 0, w2 = 100))
  w3 <- set(world = w3, agents = patch(w3, 0, 0), var = c("w2", "w1"), val = cbind(w2 = 0, w1 = 100))
  expect_equivalent(w3[0,0], cbind(w1 = 100, w2 = 0))
  w3 <- set(world = w3, agents = patches(w3), var = c("w1", "w2"), val = cbind(w1 = 101:125, w2 = 125:101))
  expect_equivalent(values(w3), cbind(w1 = 101:125, w2 = 125:101))
  w3 <- set(world = w3, agents = patches(w3), var = c("w2", "w1"), val = cbind(w2 = 101:125, w1 = 125:101))
  expect_equivalent(values(w3), cbind(w1 = 125:101, w2 = 101:125))
  w3 <- set(world = w3, agents = patches(w3), var = c("w1", "w2"), val = cbind(w1 = 101, w2 = 125))
  expect_equivalent(values(w3), cbind(w1 = rep(101, 25), w2 = rep(125, 25)))
  w3 <- set(world = w3, agents = patches(w3), var = c("w2", "w1"), val = cbind(w2 = 125, w1 = 101))
  expect_equivalent(values(w3), cbind(w1 = rep(101, 25), w2 = rep(125, 25)))
  w3 <- set(world = w3, agents = patch(w3,c(-1, 0), c(-1, 0)), var = c("w1", "w2"), val = cbind(w1 = 0, w2 = 1))
  expect_equivalent(w3[0,0], cbind(w1 = 0, w2 = 1))
  valW3 <- values(w3)
  expect_equivalent(length(valW3[is.na(valW3[,1]),1]), 0)
  expect_equivalent(length(valW3[is.na(valW3[,2]),2]), 0)

  # Set work with turtles
  t1 <- createTurtles(n = 5, coords = cbind(xcor = 0:4, ycor = 0:4), heading = c(0, 90, 180, 270, 0))
  t2 <- set(turtles = t1, agents = t1, var = "heading", val = 0)
  expect_equivalent(t2@data$heading, rep(0, length(t2)))
  t3 <- set(turtles = t1, agents = turtle(t1, 0), var = "xcor", val = 3)
  expect_equivalent(t3@coords[,1], c(3, 1, 2, 3, 4))
  t4 <- set(turtles = t1, agents = turtle(t1, c(0,1)), var = "xcor", val = 3)
  expect_equivalent(t4@coords[,1], c(3, 3, 2, 3, 4))

  # With multiple values
  t5 <- set(turtles = t1, agents = turtle(t1, c(0,1)), var = c("xcor", "heading"), val = cbind(xcor = c(100,100), heading = c(33, 66)))
  expect_equivalent(t5@coords[,1], c(100,100,2,3,4))
  expect_equivalent(t5@data$heading, c(33,66,180,270,0))
  t6 <- set(turtles = t1, agents = turtle(t1, c(0,1)), var = c("heading", "xcor"), val = cbind(heading = c(33, 66), xcor = c(100,100)))
  expect_identical(t5, t6)

  # Warning with who numbers
  expect_warning(set(turtles = t1, agents = turtle(t1, 1), var = "who", val = 0))
  expect_warning(set(turtles = t1, agents = turtle(t1, 1), var = c("who", "heading"), val = cbind(who = 0, heading = 0)))
  t7 <- set(turtles = t1, agents = turtle(t1, 1), var = "who", val = 100) # no warning because no duplicates who numbers
  t8 <- set(turtles = t1, agents = turtle(t1, 1), var = c("who", "heading"), val = cbind(who = 100, heading = 0))

  # With NAs
  w4 <- set(world = w3, agents = cbind(pxcor = c(NA, 1, NA), pycor = c(NA, 2, NA)), var = c("w2", "w1"), val = cbind(w2 = c(1,2,3), w1 = c(1,2,3)))
  w5 <- set(world = w3, agents = cbind(pxcor = 1, pycor = 2), var = c("w2", "w1"), val = cbind(w2 = 2, w1 = 2))
  expect_equivalent(w4, w5)
  w6 <- set(world = w3, agents = cbind(pxcor = 1, pycor = 2), var = c("w2", "w1"), val = cbind(w2 = NA, w1 = 2))
  expect_equivalent(w6[1,2][1,2], as.numeric(NA))
})

test_that("set works with NLworldMs",{
  # Set work with patches
  w1 <- createNLworldMatrix(data = 1:25, minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  w1 <- set(world = w1, agents = patches(w1), val = 0)
  expect_equivalent(as.numeric(t(w1)), rep(0, length(w1)))
  w1 <- set(world = w1, agents = patches(w1), val = 1:25)
  expect_equivalent(as.numeric(t(w1)), 1:25)
  w1 <- set(world = w1, agents = patch(w1, 0, 0), val = 100)
  expect_equivalent(of(world = w1, agents = patch(w1, 0, 0)), 100)
  w1 <- set(world = w1, agents = patch(w1, c(-1, 0), c(-1, 0)), val = -10)
  expect_equivalent(of(world = w1, agents = patch(w1, 0, 0)), -10)
  w1_ <- set(world = w1, agents = patch(w1, c(-1, -2), c(-1, 0)), val = -20)
  expect_equivalent(w1, w1_)

  w1 <- set(world = w1, agents = patches(w1), val = 1:25)
  w2 <- createNLworldMatrix(data = 25:1, minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)
  w3 <- NLworldArray(w1, w2)
  w3 <- set(world = w3, agents = patches(w3), var = "w1", val = 0)
  expect_equivalent(as.numeric(t(w3[,,"w1"])), rep(0, length(w1)))
  w3 <- set(world = w3, agents = patches(w3), var = "w1", val = 1:25)
  expect_equivalent(as.numeric(t(w3[,,"w1"])), 1:25)
  w3 <- set(world = w3, agents = patch(w3, 0, 0), var = "w1", val = 100)
  expect_equivalent(of(world = w3, var = "w1", agents = patch(w3, 0, 0)), 100)

  # With multiple values
  w3 <- set(world = w3, agents = patch(w3, 0, 0), var = c("w1", "w2"), val = cbind(w1 = 0, w2 = 100))
  expect_equivalent(of(world = w3, var = c("w1", "w2"), agents = patch(w3, 0, 0)), cbind(w1 = 0, w2 = 100))
  w3 <- set(world = w3, agents = patch(w3, 0, 0), var = c("w2", "w1"), val = cbind(w2 = 0, w1 = 100))
  expect_equivalent(of(world = w3, var = c("w1", "w2"), agents = patch(w3, 0, 0)), cbind(w1 = 100, w2 = 0))
  w3 <- set(world = w3, agents = patches(w3), var = c("w1", "w2"), val = cbind(w1 = 101:125, w2 = 125:101))
  expect_equivalent(of(world = w3, var = c("w1", "w2"), agents = patches(w3)), cbind(w1 = 101:125, w2 = 125:101))
  w3 <- set(world = w3, agents = patches(w3), var = c("w2", "w1"), val = cbind(w2 = 101:125, w1 = 125:101))
  expect_equivalent(of(world = w3, var = c("w1", "w2"), agents = patches(w3)), cbind(w1 = 125:101, w2 = 101:125))
  w3 <- set(world = w3, agents = patches(w3), var = c("w1", "w2"), val = cbind(w1 = 101, w2 = 125))
  expect_equivalent(of(world = w3, var = c("w1", "w2"), agents = patches(w3)), cbind(w1 = rep(101, 25), w2 = rep(125, 25)))
  w3 <- set(world = w3, agents = patches(w3), var = c("w2", "w1"), val = cbind(w2 = 125, w1 = 101))
  expect_equivalent(of(world = w3, var = c("w1", "w2"), agents = patches(w3)), cbind(w1 = rep(101, 25), w2 = rep(125, 25)))
  w3 <- set(world = w3, agents = patch(w3,c(-1, 0), c(-1, 0)), var = c("w1", "w2"), val = cbind(w1 = 0, w2 = 1))
  expect_equivalent(of(world = w3, var = c("w1", "w2"), agents = patch(w3, 0, 0)), cbind(w1 = 0, w2 = 1))
  valW3 <- of(world = w3, var = c("w1", "w2"), agents = patches(w3))
  expect_equivalent(length(valW3[is.na(valW3[,1]),1]), 0)
  expect_equivalent(length(valW3[is.na(valW3[,2]),2]), 0)

  # With NAs
  w4 <- set(world = w3, agents = cbind(pxcor = c(NA, 1, NA), pycor = c(NA, 2, NA)), var = c("w2", "w1"), val = cbind(w2 = c(1,2,3), w1 = c(1,2,3)))
  w5 <- set(world = w3, agents = cbind(pxcor = 1, pycor = 2), var = c("w2", "w1"), val = cbind(w2 = 2, w1 = 2))
  expect_equivalent(w4, w5)
  w6 <- set(world = w3, agents = cbind(pxcor = 1, pycor = 2), var = c("w2", "w1"), val = cbind(w2 = NA, w1 = 2))
  expect_equivalent(of(world = w6, var = "w2", agents = patch(w6, 1, 2)), as.numeric(NA))
})
