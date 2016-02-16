test_that("createNLworld works with default values", {
  w1 <- createNLworld()
  w2 <- createNLworld(-16, 16, -16, 16) # default values

  expect_identical(w1, w2)
})
