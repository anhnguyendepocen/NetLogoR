test_that("createNLworld works with default values", {
  w1 <- createNLworld()
  w2 <- createNLworld(-16, 16, -16, 16) # default values

  expect_identical(w1, w2)
})

test_that("convertNLworld works with RasterLayer and RasterStack", {
  r1 <- raster(nrows=10, ncols=10, xmn=-5, xmx=10, ymn=2, ymx=20)
  r1[]<-runif(100)
  r2 <- r1
  r2[]<-runif(100)
  rs <- stack(r1, r2)
  w1 <- convertNLworld(r1)
  ws <- convertNLworld(rs)
  ws1 <- ws[[1]]
  ws1@data@names <- w1@data@names
  expect_identical(w1,ws1)
})

test_that("maxPxcor works", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  m1 <- maxPxcor(w1)
  m2 <- w1@maxPxcor

  expect_identical(m1, m2)
  expect_identical(m1, 10)

  w2 <- w1
  w1[] <- runif(231)
  w2[] <- runif(231)
  ws <- NLstack(w1, w2)
  m3 <- maxPxcor(ws)
  expect_identical(m3, m2)
  expect_identical(m3, 10)

  # With NLworldMatrix and NLworldArray
  w4 <- createNLworldMatrix(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  m4 <- maxPxcor(w4)
  expect_identical(m1, m4)
  w5 <- w4
  w6 <- NLworldArray(w4, w5)
  m6 <- maxPxcor(w6)
  expect_identical(m1, m6)
})

test_that("maxPycor works", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  m1 <- maxPycor(w1)
  m2 <- w1@maxPycor

  expect_identical(m1, m2)
  expect_identical(m1, 15)

  w2 <- w1
  w1[] <- runif(231)
  w2[] <- runif(231)
  ws <- NLstack(w1, w2)
  m3 <- maxPycor(ws)
  expect_identical(m3, m2)
  expect_identical(m3, 15)

  # With NLworldMatrix and NLworldArray
  w4 <- createNLworldMatrix(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  m4 <- maxPycor(w4)
  expect_identical(m1, m4)
  w5 <- w4
  w6 <- NLworldArray(w4, w5)
  m6 <- maxPycor(w6)
  expect_identical(m1, m6)
})

test_that("minPxcor works", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  m1 <- minPxcor(w1)
  m2 <- w1@minPxcor

  expect_identical(m1, m2)
  expect_identical(m1, 0)

  w2 <- w1
  w1[] <- runif(231)
  w2[] <- runif(231)
  ws <- NLstack(w1, w2)
  m3 <- minPxcor(ws)
  expect_identical(m3, m2)
  expect_identical(m3, 0)

  # With NLworldMatrix and NLworldArray
  w4 <- createNLworldMatrix(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  m4 <- minPxcor(w4)
  expect_identical(m1, m4)
  w5 <- w4
  w6 <- NLworldArray(w4, w5)
  m6 <- minPxcor(w6)
  expect_identical(m1, m6)
})

test_that("minPycor works", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  m1 <- minPycor(w1)
  m2 <- w1@minPycor

  expect_identical(m1, m2)
  expect_identical(m1, -5)

  w2 <- w1
  w1[] <- runif(231)
  w2[] <- runif(231)
  ws <- NLstack(w1, w2)
  m3 <- minPycor(ws)
  expect_identical(m3, m2)
  expect_identical(m3, -5)

  # With NLworldMatrix and NLworldArray
  w4 <- createNLworldMatrix(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  m4 <- minPycor(w4)
  expect_identical(m1, m4)
  w5 <- w4
  w6 <- NLworldArray(w4, w5)
  m6 <- minPycor(w6)
  expect_identical(m1, m6)
})

test_that("worldWidth works", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  width1 <- worldWidth(w1)
  width2 <- w1@maxPxcor - w1@minPxcor + 1

  expect_identical(width1,width2)
  expect_identical(width1, 11)

  # With NLworldMatrix and NLworldArray
  w4 <- createNLworldMatrix(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  width4 <- worldWidth(w4)
  expect_identical(width1, width4)
  w5 <- w4
  w6 <- NLworldArray(w4, w5)
  width6 <- worldWidth(w6)
  expect_identical(width1, width6)
})

test_that("worldHeight works", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  height1 <- worldHeight(w1)
  height2 <- w1@maxPycor - w1@minPycor + 1

  expect_identical(height1,height2)
  expect_identical(height1, 21)

  # With NLworldMatrix and NLworldArray
  w4 <- createNLworldMatrix(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  height4 <- worldHeight(w4)
  expect_identical(height1, height4)
  w5 <- w4
  w6 <- NLworldArray(w4, w5)
  height6 <- worldHeight(w6)
  expect_identical(height1, height6)
})

test_that("clearPatches works", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 3, minPycor = 0, maxPycor = 3)
  w1 <- setValues(w1, NA)
  w2 <- setValues(w1, 1:16)
  w2_NA <- clearPatches(w2)
  expect_identical(w1, w2_NA)

  # NLworldStack
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 3, minPycor = 0, maxPycor = 3)
  w1 <- setValues(w1, runif(16))
  w2 <- setValues(w1, 1:16)
  w3 <- NLstack(w1, w2)
  w1_NA <- clearPatches(w1)
  w3_NA <- clearPatches(w3)
  expect_identical(w1_NA, w3_NA)

  # With NLworldMatrix
  w4 <- createNLworldMatrix(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 3, data = 1:20)
  w5 <- clearPatches(w4)
  w6 <- createNLworldMatrix(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 3)
  expect_equivalent(w5, w6)

  # With NLworldArray
  w7 <- createNLworldMatrix(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 3, data = 1:20)
  w8 <- w7
  w9 <- NLworldArray(w7, w8)
  w10 <- clearPatches(w9)
  expect_equivalent(w10, w6)
})

test_that("raster2world and world2raster work", {
  r1 <- raster(nrows=10, ncols=10, xmn=-5, xmx=10, ymn=2, ymx=20)
  r1[]<-runif(100)
  r2 <- r1
  r2[]<-runif(100)
  rs <- stack(r1, r2)
  w1 <- raster2world(r1, method = "ngb")
  w2 <- w1
  w3 <- NLworldArray(w1, w2)
  ws <- raster2world(rs, method = "ngb")
  expect_identical(w1[],ws[][,1])

  r1_ <- world2raster(w1)
  rs_ <- world2raster(ws)
})

test_that("spdf2turtles and turtles2spdf work", {
  t1 <- createTurtles(n = 10, coords = cbind(xcor = 1:10, ycor = 1:10), heading = 1:10)
  t1 <- turtlesOwn(turtles = t1, tVar = "age", tVal = 1:10)
  t1 <- turtlesOwn(turtles = t1, tVar = "sex", tVal = c(rep("M", 5), rep("F", 5)))
  t2 <- createTurtlesAM(n = 10, coords = cbind(xcor = 1:10, ycor = 1:10), heading = 1:10)
  t2 <- turtlesOwn(turtles = t2, tVar = "age", tVal = 1:10)
  t2 <- turtlesOwn(turtles = t2, tVar = "sex", tVal = c(rep("M", 5), rep("F", 5)))
  t1_ <- spdf2turtles(t1)
  expect_equivalent(t1_, t2)
  t2_ <- turtles2spdf(t2)
  expect_equivalent(t2_, SpatialPointsDataFrame(coords = t1@coords, data = t1@data[,c(1:6, 8,9)]))
  t2__ <- spdf2turtles(t2_)
  expect_equivalent(t2__, t2)
  t1__ <- turtles2spdf(t1_)
  expect_equivalent(t1__, SpatialPointsDataFrame(coords = t1@coords, data = t1@data[,c(1:6, 8,9)]))
})
