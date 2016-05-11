test_that("Plotting NLworldMatrix", {
  w1 <- createNLworldMatrix(0, 2, 0, 2)
  library(SpaDES)
  w1[] <- c(1,3,6,2,8,10,3,8,2)
  expect_silent(Plot(w1, new=TRUE, axes=TRUE))

  t1 <- createTurtlesAM(n = 10, coords = randomXYcor(world = w1, n = 10), heading = 1:10)
  expect_silent(Plot(t1, new=TRUE))

  expect_silent(Plot(w1, new=TRUE, axes = TRUE))
  expect_silent(Plot(t1, addTo="w1"))

})

