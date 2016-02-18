test_that("NLstack() works similar as stack()",{
  rl1 <- raster(system.file("external/test.grd", package="raster"))
  rl1@data@names <- "test1"
  rl2 <- rl1
  rl2@data@names <- "test2"
  rl3 <- rl1
  rl3@data@names <- "test3"
  s <- stack(rl1, rl2, rl3)
  sNL <- convertNLworld(s)

  w1 <- convertNLworld(rl1)
  w2 <- convertNLworld(rl2)
  w3 <- convertNLworld(rl3)
  ws <- NLstack(w1, w2, w3)

  expect_identical(sNL, ws)
})
