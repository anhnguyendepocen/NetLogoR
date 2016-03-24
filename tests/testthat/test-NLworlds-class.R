test_that("NLstack() works similar as stack()",{
  rl1 <- raster(nrows=108, ncols=21, xmn=0, xmx=10)
  rl1[] <- runif(2268)
  rl1@data@names <- "test1"
  rl2 <- rl1
  rl2[] <- runif(2268)
  rl2@data@names <- "test2"
  rl3 <- rl1
  rl3[] <- runif(2268)
  rl3@data@names <- "test3"
  s <- stack(rl1, rl2, rl3)
  sNL <- convertNLworld(s)

  w1 <- convertNLworld(rl1)
  w2 <- convertNLworld(rl2)
  w3 <- convertNLworld(rl3)
  ws <- NLstack(w1, w2, w3)

  expect_identical(sNL, ws)
})

test_that("[] works with NLworld and pxcor and pycor",{
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 1, minPycor = 0, maxPycor = 1)
  w1[] <- c(1, 2, 3, 4)

  w1_00 <- w1[0,0]
  expect_identical(w1_00, 3)

  w1_01_11 <- w1[c(0, 1),1]
  expect_identical(w1_01_11, c(1, 2))

  w1[1,c(0,1)] <- c(10, 20)
  expect_identical(values(w1)[c(2, 4)], c(10, 20))
})

test_that("[] works with NLworldStack and pxcor and pycor",{
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 1, minPycor = 0, maxPycor = 1)
  w1[] <- c(1, 2, 3, 4)
  w2 <- w1
  w2[] <- c(10, 20, 30, 40)
  ws <- NLstack(w1, w2)

  ws_00 <- ws[0,0]
  expect_identical(ws_00, cbind(w1 = 3, w2 = 30))

  ws_01_11 <- ws[c(0, 1),1]
  expect_identical(ws_01_11, cbind(w1 = c(1, 2), w2 = c(10, 20)))

  ws[1,c(0,1)] <- cbind(c(10, 20), c(100, 200)) # signature matrix for value
  expect_identical(values(ws)[c(2, 4),], cbind(w1 = c(10, 20), w2 = c(100, 200)))

  ws[1,1] <- c(-1, -4) # signature numeric for value
  expect_identical(as.numeric(values(ws)[2,]), c(-1, -4))
})

test_that("cellFromPxcorPycor works",{
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
  cellNum <- cellFromPxcorPycor(world = w1, pxcor = c(9,0,1), pycor = c(0, 0, 9))
  expect_identical(cellNum, c(100, 91, 2))

  w1[] <- runif(100)
  w2 <- w1
  w2[] <- runif(100)
  ws <- NLstack(w1, w2)
  # Same as for w1
  cellNum <- cellFromPxcorPycor(world = ws, pxcor = c(9,0,1), pycor = c(0, 0, 9))
  expect_identical(cellNum, c(100, 91, 2))

})

test_that("PxcorPycorFromCell works",{
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
  pCoords1 <- PxcorPycorFromCell(world = w1, cellNum = c(100, 91, 2))
  pCoords2 <- cbind(pxcor = c(9,0,1), pycor = c(0, 0, 9))
  expect_identical(pCoords1, pCoords2)

  w1[] <- runif(100)
  w2 <- w1
  w2[] <- runif(100)
  ws <- NLstack(w1, w2)
  # Same as for w1
  pCoords1 <- PxcorPycorFromCell(world = ws, cellNum = c(100, 91, 2))
  expect_identical(pCoords1, pCoords2)

})
