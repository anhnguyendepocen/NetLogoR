test_that("createNLworld works with default values", {
  w1 <- createNLworld()
  w2 <- createNLworld(-16, 16, -16, 16) # default values

  expect_identical(w1, w2)
})

test_that("getMaxPxcor works", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  m1 <- getMaxPxcor(w1)
  m2 <- w1@maxPxcor

  expect_identical(m1, m2)
  expect_identical(m1, 10)
})

test_that("getMaxPycor works", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  m1 <- getMaxPycor(w1)
  m2 <- w1@maxPycor

  expect_identical(m1, m2)
  expect_identical(m1, 15)
})

test_that("getMinPxcor works", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  m1 <- getMinPxcor(w1)
  m2 <- w1@minPxcor

  expect_identical(m1, m2)
  expect_identical(m1, 0)
})

test_that("getMinPycor works", {
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  m1 <- getMinPycor(w1)
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
