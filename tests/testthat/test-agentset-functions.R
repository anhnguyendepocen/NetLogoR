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
