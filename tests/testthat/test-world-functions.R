test_that("maxPxcor works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  expect_identical(10, maxPxcor(w1))
  w2 <- w1
  w3 <- stackWorlds(w1, w2)
  expect_identical(10, maxPxcor(w3))
})

test_that("maxPycor works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  expect_identical(15, maxPycor(w1))
  w2 <- w1
  w3 <- stackWorlds(w1, w2)
  expect_identical(15, maxPycor(w3))
})

test_that("minPxcor works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  expect_identical(0, minPxcor(w1))
  w2 <- w1
  w3 <- stackWorlds(w1, w2)
  expect_identical(0, minPxcor(w3))
})

test_that("minPycor works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  expect_identical(-5, minPycor(w1))
  w2 <- w1
  w3 <- stackWorlds(w1, w2)
  expect_identical(-5, minPycor(w3))
})

test_that("worldWidth works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  expect_identical(11, worldWidth(w1))
  w2 <- w1
  w3 <- stackWorlds(w1, w2)
  expect_identical(11, worldWidth(w3))
})

test_that("worldHeight works", {
  w1 <- createWorld(minPxcor = 0, maxPxcor = 10, minPycor = -5, maxPycor = 15)
  expect_identical(21, worldHeight(w1))
  w2 <- w1
  w3 <- stackWorlds(w1, w2)
  expect_identical(21, worldHeight(w3))
})

test_that("clearPatches works", {
  # NLworldMatrix
  w1 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 3, data = 1:20)
  w2 <- clearPatches(w1)
  w3 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 3)
  expect_equivalent(w2, w3)

  # worldArray
  w4 <- createWorld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 3, data = 1:20)
  w5 <- w4
  w6 <- stackWorlds(w4, w5)
  w7 <- clearPatches(w6)
  expect_equivalent(w7, w3)
})

test_that("raster2world and world2raster work", {
  r1 <- raster(nrows=10, ncols=10, xmn=-5, xmx=10, ymn=2, ymx=20)
  r1[]<-runif(100)
  r2 <- r1
  r2[]<-runif(100)
  rs <- stack(r1, r2)
  w1 <- raster2world(r1, method = "ngb")
  w2 <- w1
  w3 <- stackWorlds(w1, w2)
  ws <- raster2world(rs, method = "ngb")
  expect_identical(w1[],ws[][,1])

  r1_ <- world2raster(w1)
  rs_ <- world2raster(ws)
})
