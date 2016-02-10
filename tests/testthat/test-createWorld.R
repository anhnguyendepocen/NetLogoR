test_that("createWorld handles different inputs", {
  w1 <- createWorld(0, 10, 0, 10, 1)
  w2 <- createWorld(0, 10, 0, 10)     # patchSize missing

  expect_identical(w1, w2)
})
