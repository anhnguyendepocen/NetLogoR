test_that("createNLworld works with default values", {
  w1 <- createNLworld()
  w2 <- createNLworld(-16, 16, -16, 16) # default values

  expect_identical(w1, w2)
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
