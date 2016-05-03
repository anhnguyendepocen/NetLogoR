test_that("NLstack() works similar as stack()",{
  rl1 <- raster(nrows=108, ncols=21, xmn=0, xmx=10)
  rl1[] <- runif(2268)
  rl1@data@names <- "w1"
  rl2 <- rl1
  rl2[] <- runif(2268)
  rl2@data@names <- "w2"
  rl3 <- rl1
  rl3[] <- runif(2268)
  rl3@data@names <- "w3"
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

test_that("createNLworldMatrix works similarly as createNLworld",{
  w1 <- createNLworld(minPxcor = -2, maxPxcor = 7, minPycor = -2, maxPycor = 5)
  w1[] <- 1:80
  w2 <- createNLworldMatrix(minPxcor = -2, maxPxcor = 7, minPycor = -2, maxPycor = 5, data = 1:80)
  expect_equivalent(values(w1), as.numeric(t(w2@.Data)))
  expect_identical(extent(w1), w2@extent)
  expect_identical(minPxcor(w1), w2@minPxcor)
  expect_identical(maxPxcor(w1), w2@maxPxcor)
  expect_identical(minPycor(w1), w2@minPycor)
  expect_identical(maxPycor(w1), w2@maxPycor)
  expect_identical(res(w1), w2@res)
  expect_equivalent(patches(w1), w2@pCoords)

  w3 <- createNLworldMatrix(minPxcor = -2, maxPxcor = 7, minPycor = -2, maxPycor = 5)
  expect_equivalent(rep(as.numeric(NA), length(w1)), as.numeric(t(w3@.Data)))

  w1 <- createNLworld()
  w2 <- createNLworldMatrix()
  expect_equivalent(as.numeric(values(w1)), as.numeric(t(w2@.Data)))
  expect_identical(extent(w1), w2@extent)
  expect_identical(minPxcor(w1), w2@minPxcor)
  expect_identical(maxPxcor(w1), w2@maxPxcor)
  expect_identical(minPycor(w1), w2@minPycor)
  expect_identical(maxPycor(w1), w2@maxPycor)
  expect_identical(res(w1), w2@res)
  expect_equivalent(patches(w1), w2@pCoords)
})

test_that("NLworldArray works similarly as NLstack",{
  w1 <- createNLworldMatrix(minPxcor = -2, maxPxcor = 7, minPycor = -4, maxPycor = 5, data = 1:100)
  w2 <- createNLworldMatrix(minPxcor = -2, maxPxcor = 7, minPycor = -4, maxPycor = 5, data = 101:200)
  w3 <- createNLworldMatrix(minPxcor = -3, maxPxcor = 6, minPycor = -4, maxPycor = 5, data = 1:100)
  w4 <- createNLworldMatrix(minPxcor = -2, maxPxcor = 6, minPycor = -4, maxPycor = 5, data = 1:90)
  expect_error(NLworldArray(w2, w3))
  expect_error(NLworldArray(w2, w4))
  w5 <- NLworldArray(w1, w2)
  expect_identical(w5@extent, w2@extent)
  expect_identical(w5@pCoords, w2@pCoords)
  expect_identical(w5@res, w2@res)
  expect_identical(w5@minPxcor, w2@minPxcor)
  expect_identical(w5@maxPxcor, w2@maxPxcor)
  expect_identical(w5@minPycor, w2@minPycor)
  expect_identical(w5@maxPycor, w2@maxPycor)
  expect_equivalent(w5@.Data[,,"w1"], w1@.Data)
  expect_equivalent(w5@.Data[,,"w2"], w2@.Data)

  w3 <- createNLworldMatrix(minPxcor = -2, maxPxcor = 7, minPycor = -4, maxPycor = 5, data = -1:-100)
  w6 <- NLworldArray(w1, w2, w3)
  expect_identical(w5@extent, w6@extent)
  expect_identical(w5@pCoords, w6@pCoords)
  expect_identical(w5@res, w6@res)
  expect_identical(w5@minPxcor, w6@minPxcor)
  expect_identical(w5@maxPxcor, w6@maxPxcor)
  expect_identical(w5@minPycor, w6@minPycor)
  expect_identical(w5@maxPycor, w6@maxPycor)
  expect_equivalent(w6@.Data[,,"w1"], w1@.Data)
  expect_equivalent(w6@.Data[,,"w2"], w2@.Data)
  expect_equivalent(w6@.Data[,,"w3"], w3@.Data)
})

test_that("[] works for NLworldMatrix",{
  w1 <- createNLworldMatrix(minPxcor = 0, maxPxcor = 1, minPycor = 0, maxPycor = 1, data = c(1, 2, 3, 4))
  expect_equivalent(w1[], c(1,2,3,4))

  w1_00 <- w1[0,0]
  expect_identical(w1_00, 3)
  w1_01_11 <- w1[c(0, 1),1]
  expect_identical(w1_01_11, c(1, 2))
  w1_01_11 <- w1[c(1, 0),1]
  expect_identical(w1_01_11, c(2, 1))

  w1[1,c(0,1)] <- c(10, 20)
  expect_identical(as.numeric(t(w1@.Data))[c(2, 4)], c(20, 10))
  w1[1,c(1,0)] <- c(100, 200)
  expect_identical(as.numeric(t(w1@.Data))[c(2, 4)], c(100, 200))
  w1[] <- c(10, 20, 30, 40)
  expect_equivalent(w1[], c(10, 20, 30, 40))
  w1[] <- -1
  expect_equivalent(w1[], c(-1, -1, -1, -1))
})

test_that("[] works with NLworldArray",{
  w1 <- createNLworldMatrix(minPxcor = 0, maxPxcor = 1, minPycor = 0, maxPycor = 1, data = c(1, 2, 3, 4))
  w2 <- w1
  w2[] <- c(10, 20, 30, 40)
  ws <- NLworldArray(w1, w2)
  expect_equivalent(ws[], cbind(c(1,2,3,4), c(10, 20, 30, 40)))

  ws_00 <- ws[0,0]
  expect_identical(ws_00, cbind(w1 = 3, w2 = 30))
  ws_01_11 <- ws[c(0, 1),1]
  expect_identical(ws_01_11, cbind(w1 = c(1, 2), w2 = c(10, 20)))
  w1_01_11 <- ws[c(1, 0),1]
  expect_identical(w1_01_11, cbind(w1 = c(2, 1), w2 = c(20, 10)))

  ws[1,c(0,1)] <- cbind(c(10, 20), c(100, 200))
  expect_identical(ws[1,c(0,1)], cbind(w1 = c(10, 20), w2 = c(100, 200)))
  ws[] <- cbind(c(15, 25, 35, 45), c(-1, -2, -3, -4))
  expect_equivalent(ws[], cbind(c(15, 25, 35, 45), c(-1, -2, -3, -4)))
  expect_equivalent(as.numeric(t(ws@.Data[,,1])), c(15, 25, 35, 45))
  ws[] <- cbind(-1, -2)
  expect_equivalent(ws[], cbind(c(-1, -1, -1, -1), c(-2, -2, -2, -2)))
})

test_that("cellFromPxcorPycor works for NLworldMs",{
  w3 <- createNLworldMatrix(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
  cellNum <- cellFromPxcorPycor(world = w3, pxcor = c(9,0,1), pycor = c(0, 0, 9))
  expect_equivalent(cellNum, c(100, 91, 2))
  cellNum <- cellFromPxcorPycor(world = w3, pxcor = c(1, 0, 9), pycor = c(9, 0, 0))
  expect_equivalent(cellNum, c(2, 91, 100))
  w4 <- w3
  w5 <- NLworldArray(w3, w4)
  cellNum <- cellFromPxcorPycor(world = w5, pxcor = c(9,0,1), pycor = c(0, 0, 9))
  expect_equivalent(cellNum, c(100, 91, 2))
})

test_that("PxcorPycorFromCell works for NLworldMs",{
  w3 <- createNLworldMatrix(minPxcor = 0, maxPxcor = 9, minPycor = 0, maxPycor = 9)
  pCoords1 <- PxcorPycorFromCell(world = w3, cellNum = c(100, 91, 2))
  pCoords2 <- cbind(pxcor = c(9,0,1), pycor = c(0, 0, 9))
  expect_equivalent(pCoords1, pCoords2)
  w4 <- w3
  w5 <- NLworldArray(w3, w4)
  pCoords1 <- PxcorPycorFromCell(world = w5, cellNum = c(100, 91, 2))
  expect_equivalent(pCoords1, pCoords2)
  pxcor <- sample(0:9, size = 5)
  pycor <- sample(0:9, size = 5)
  cellNum <- cellFromPxcorPycor(world = w5, pxcor = pxcor, pycor = pycor)
  pCoords <- PxcorPycorFromCell(world = w5, cellNum = cellNum)
  expect_equivalent(cbind(pxcor, pycor), pCoords)
})

