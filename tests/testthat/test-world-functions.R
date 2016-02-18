test_that("createNLworld works with default values", {
  w1 <- createNLworld()
  w2 <- createNLworld(-16, 16, -16, 16) # default values

  expect_identical(w1, w2)
})

test_that("importNLworld works with RasterLayer and RasterStack", {
  rl <- raster(system.file("external/test.grd", package="raster"))
  rs <- stack(rl, rl) # the layer names changed
  wl <- convertNLworld(rl)
  ws <- convertNLworld(rs)
  ws1 <- ws[[1]]
  ws1@data@names <- rl@data@names # rename same as in wl
  expect_identical(wl,ws1)
})

test_that("maxPxcor works", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  m1 <- maxPxcor(w1)
  m2 <- w1@maxPxcor

  expect_identical(m1, m2)
  expect_identical(m1, 10)
})

test_that("maxPycor works", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  m1 <- maxPycor(w1)
  m2 <- w1@maxPycor

  expect_identical(m1, m2)
  expect_identical(m1, 15)
})

test_that("minPxcor works", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  m1 <- minPxcor(w1)
  m2 <- w1@minPxcor

  expect_identical(m1, m2)
  expect_identical(m1, 0)
})

test_that("minPycor works", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  m1 <- minPycor(w1)
  m2 <- w1@minPycor

  expect_identical(m1, m2)
  expect_identical(m1, -5)
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

test_that("clearTurtles works", {
  t1 <- SpatialPointsDataFrame(coords = matrix(c(1,2), nrow = 1, ncol = 2), data = data.frame(NA))
  clearTurtles(turtles = t1)
  expect_false(exists("t1"))
})

test_that("clearPacthes works", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 3, minPycor = 0, maxPycor = 3)
  w1 <- setValues(w1, NA)
  w2 <- setValues(w1, 1:16)
  w2_NA <- clearPatches(w2)

  expect_identical(w1, w2_NA)
})

test_that("clearPacthes handles NLworld and NLworldStack", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 3, minPycor = 0, maxPycor = 3)
  w2 <- setValues(w1, 1:16)
  w3 <- NLstack(w2, w2)
  w2_NA <- clearPatches(w2)
  w3_NA <- clearPatches(w3)

  expect_identical(w2_NA, w3_NA)
})

