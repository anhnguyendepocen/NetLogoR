test_that("createWorld handles different inputs", {
  w1 <- createWorld(0, 10, 0, 10, 1)
  w2 <- createWorld(0, 10, 0, 10)     # patchSize missing

  expect_identical(w1, w2)
})

test_that("createWorld works with default values", {
  w1 <- createWorld()
  w2 <- createWorld(-16, 16, -16, 16, 1) # default values

  expect_identical(w1, w2)
})

test_that("maxPxcor handles RasterStack", {
  w1 <- createWorld(0, 10, 0, 10)
  w1[] <- runif(100)
  w2 <- createWorld(0, 10, 0, 10)
  w2[] <- runif(100)
  w3 <- stack(w1,w2)

  maxW1 <- maxPxcor(w1)
  maxW3 <- maxPxcor(w3)

  expect_identical(maxW1, maxW3)
})
