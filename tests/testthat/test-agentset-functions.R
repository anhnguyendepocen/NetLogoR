test_that("sortOn works",{
  w1 <- createNLworld(0, 4, 0, 4)
  w1[] <- 25:1
  p1 <- sortOn(world = w1, agents = patches(world = w1))
  expect_equivalent(cbind(p1[1,1], p1[1,2]), patch(w1, xcor = 4, ycor = 0))
  expect_equivalent(cbind(p1[25,1],p1[25,2]), patch(w1, xcor = 0, ycor = 4))

  w2 <- w1
  w2[] <- 1:25
  ws <- NLstack(w1, w2)
  p1 <- sortOn(world = ws, agents = patch(world = ws, xcor = c(0,1,2,3,4), ycor = c(4,4,4,4,4)), pVar = "w1")
  p2 <- sortOn(world = ws, agents = patch(world = ws, xcor = c(0,1,2,3,4), ycor = c(4,4,4,4,4)), pVar = "w2")
  expect_equivalent(p1, cbind(pxcor = c(4,3,2,1,0), pycor = c(4,4,4,4,4)))
  expect_equivalent(p2, cbind(pxcor = c(0,1,2,3,4), pycor = c(4,4,4,4,4)))
})

test_that("NLwith works",{
  w1 <- createNLworld(0, 4, 0, 4)
  w1[] <- 0
  w1[1,c(1,4)] <- c(1,1)
  p1 <- NLwith(world = w1, agents = patches(world = w1), val = 1)
  expect_identical(p1, patch(w1, xcor = c(1,1), ycor = c(4,1)))

  w2 <- w1
  w2[] <- 0
  w2[2,c(2,4)] <- c(1,1)
  ws <- NLstack(w1, w2)
  p1w1 <- NLwith(world = ws, agents = patches(world = ws), pVar = "w1", val = 1)
  p1w2 <- NLwith(world = ws, agents = patches(world = ws), pVar = "w2", val = 1)
  expect_identical(p1w1, patch(ws, xcor = c(1,1), ycor = c(4,1)))
  expect_identical(p1w2, patch(ws, xcor = c(2,2), ycor = c(4,2)))
})

test_that("withMax works",{
  w1 <- createNLworld(0, 4, 0, 4)
  w1[] <- runif(25)
  w1[1,c(1,4)] <- c(2,2)
  pMax <- withMax(world = w1, agents = patches(world = w1))
  expect_identical(pMax, patch(w1, xcor = c(1,1), ycor = c(4,1)))

  w2 <- w1
  w2[] <- runif(25)
  w2[2,c(2,4)] <- c(2,2)
  ws <- NLstack(w1, w2)
  pMaxw1 <- withMax(world = ws, agents = patches(world = ws), pVar = "w1")
  pMaxw2 <- withMax(world = ws, agents = patches(world = ws), pVar = "w2")
  expect_identical(pMaxw1, patch(ws, xcor = c(1,1), ycor = c(4,1)))
  expect_identical(pMaxw2, patch(ws, xcor = c(2,2), ycor = c(4,2)))
})

test_that("withMin works",{
  w1 <- createNLworld(0, 4, 0, 4)
  w1[] <- runif(25)
  w1[1,c(1,4)] <- c(-1,-1)
  pMin <- withMin(world = w1, agents = patches(world = w1))
  expect_identical(pMin, patch(w1, xcor = c(1,1), ycor = c(4,1)))

  w2 <- w1
  w2[] <- runif(25)
  w2[2,c(2,4)] <- c(-1,-1)
  ws <- NLstack(w1, w2)
  pMinw1 <- withMin(world = ws, agents = patches(world = ws), pVar = "w1")
  pMinw2 <- withMin(world = ws, agents = patches(world = ws), pVar = "w2")
  expect_identical(pMinw1, patch(ws, xcor = c(1,1), ycor = c(4,1)))
  expect_identical(pMinw2, patch(ws, xcor = c(2,2), ycor = c(4,2)))
})

test_that("maxOneOf works",{
  w1 <- createNLworld(0, 4, 0, 4)
  w1[] <- sample(1:5, size = 25, replace = TRUE)
  allpMax <- withMax(world = w1, agents = patches(world = w1))
  onepMax <- maxOneOf(world = w1, agents = patches(world = w1))
  compare <- cbind(a = as.numeric(allpMax[,1])==as.numeric(onepMax[1]),b = as.numeric(allpMax[,2])==as.numeric(onepMax[2]))
  rowTRUE <- compare[compare[,1] == TRUE & compare[,2] == TRUE,,drop = FALSE]
  expect_equivalent(nrow(rowTRUE),1)

  w2 <- w1
  w2[] <- 1 / values(w1)
  ws <- NLstack(w1, w2)
  onepMax1 <- maxOneOf(world = ws, agents = patches(world = w1), pVar = "w1")
  onepMax2 <- maxOneOf(world = ws, agents = patches(world = w1), pVar = "w2")
  compare1 <- cbind(a = as.numeric(allpMax[,1])==as.numeric(onepMax1[1]),b = as.numeric(allpMax[,2])==as.numeric(onepMax1[2]))
  rowTRUE <- compare1[compare1[,1] == TRUE & compare1[,2] == TRUE,,drop = FALSE]
  expect_equivalent(nrow(rowTRUE),1)
  compare2 <- onepMax2 == onepMax1
  expect_less_than(length(compare2[compare2 == TRUE]), 2)
})

test_that("minOneOf works",{
  w1 <- createNLworld(0, 4, 0, 4)
  w1[] <- sample(1:5, size = 25, replace = TRUE)
  allpMin <- withMin(world = w1, agents = patches(world = w1))
  onepMin <- minOneOf(world = w1, agents = patches(world = w1))
  compare <- cbind(a = as.numeric(allpMin[,1])==as.numeric(onepMin[1]),b = as.numeric(allpMin[,2])==as.numeric(onepMin[2]))
  rowTRUE <- compare[compare[,1] == TRUE & compare[,2] == TRUE,,drop = FALSE]
  expect_equivalent(nrow(rowTRUE),1)

  w2 <- w1
  w2[] <- 1 / values(w1)
  ws <- NLstack(w1, w2)
  onepMin1 <- minOneOf(world = ws, agents = patches(world = w1), pVar = "w1")
  onepMin2 <- minOneOf(world = ws, agents = patches(world = w1), pVar = "w2")
  compare1 <- cbind(a = as.numeric(allpMin[,1])==as.numeric(onepMin1[1]),b = as.numeric(allpMin[,2])==as.numeric(onepMin1[2]))
  rowTRUE <- compare1[compare1[,1] == TRUE & compare1[,2] == TRUE,,drop = FALSE]
  expect_equivalent(nrow(rowTRUE),1)
  compare2 <- onepMin2 == onepMin1
  expect_less_than(length(compare2[compare2 == TRUE]), 2)
})

