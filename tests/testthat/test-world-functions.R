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
})

test_that("worldWidth works", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  width1 <- worldWidth(w1)
  width2 <- w1@maxPxcor - w1@minPxcor + 1

  expect_identical(width1,width2)
  expect_identical(width1, 11)
})

test_that("worldHeight works", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  height1 <- worldHeight(w1)
  height2 <- w1@maxPycor - w1@minPycor + 1

  expect_identical(height1,height2)
  expect_identical(height1, 21)
})

test_that("clearPatches works", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 3, minPycor = 0, maxPycor = 3)
  w1 <- setValues(w1, NA)
  w2 <- setValues(w1, 1:16)
  w2_NA <- clearPatches(w2)

  expect_identical(w1, w2_NA)
})

test_that("clearPatches handles NLworld and NLworldStack", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 3, minPycor = 0, maxPycor = 3)
  w1 <- setValues(w1, runif(16))
  w2 <- setValues(w1, 1:16)
  w3 <- NLstack(w1, w2)
  w1_NA <- clearPatches(w1)
  w3_NA <- clearPatches(w3)

  expect_identical(w1_NA, w3_NA)
})

