test_that("NLstack() works similar as stack()",{
  rl1 <- raster(system.file("external/test.grd", package="raster"))
  rl1@data@names <- "test1"
  rl2 <- rl1
  rl2@data@names <- "test2"
  s <- stack(rl1, rl2)
  sNL <- convertNLworld(s)

  w1 <- convertNLworld(rl1)
  w2 <- convertNLworld(rl2)
  ws <- NLstack(w1, w2)

  expect_identical(sNL, ws)
})
