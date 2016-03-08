test_that("createTurtles works with different missing inputs",{
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)

  t1 <- createTurtles(world = w1, n = 10)
  expect_identical(cbind(xcor = rep(2, 10), ycor = rep(2, 10)), t1@coords)
  t2 <- createTurtles(world = w1, n = 10, coords = cbind(xcor = rep(0, 10), ycor = rep(0, 10)))
  expect_identical(cbind(xcor = rep(0, 10), ycor = rep(0, 10)), t2@coords)

  t3 <- createTurtles(world = w1, n = 10, heading = 0)
  expect_identical(rep(0, 10), t3@data$heading)
  t4 <- createTurtles(world = w1, n = 10, heading = 1:10)
  expect_identical(1:10, t4@data$heading)
  t1head <- t1@data$heading >= 0 & t1@data$heading <= 360
  expect_identical(t1head, rep(TRUE, 10))

  t5 <- createTurtles(world = w1, n = 10, breed = "caribou")
  expect_identical(as.factor(rep("caribou", 10)), t5@data$breed)
  t6 <- createTurtles(world = w1, n = 10, breed = c(rep("caribou", 5), rep("moose", 5)))
  expect_identical(as.factor(c(rep("caribou", 5), rep("moose", 5))), t6@data$breed)
  t1breed <- t1@data$breed == rep("turtle", 10)
  expect_identical(t1breed, rep(TRUE, 10))

  expect_equivalent(length(unique(t1@data$color)), 10)
  expect_equivalent(t1@data$who, 0:9)
  expect_equivalent(t1@data$prevX, rep(NA, 10))
  expect_equivalent(t1@data$prevY, rep(NA, 10))
  expect_equivalent(length(t1), 10)
})

test_that("createOTurtles works",{
  w1 <- createNLworld(minPxcor = 0, maxPxcor = 4, minPycor = 0, maxPycor = 4)

  t1 <- createOTurtles(world = w1, n = 10)
  expect_identical(cbind(xcor = rep(2, 10), ycor = rep(2, 10)), t1@coords)
  expect_identical(seq(0, 360 - (360 / 10), by = 360 /10), t1@data$heading)

  t5 <- createOTurtles(world = w1, n = 10, breed = "caribou")
  expect_identical(as.factor(rep("caribou", 10)), t5@data$breed)
  t6 <- createOTurtles(world = w1, n = 10, breed = c(rep("caribou", 5), rep("moose", 5)))
  expect_identical(as.factor(c(rep("caribou", 5), rep("moose", 5))), t6@data$breed)
  t1breed <- t1@data$breed == rep("turtle", 10)
  expect_identical(t1breed, rep(TRUE, 10))

  expect_equivalent(length(unique(t1@data$color)), 10)
  expect_equivalent(t1@data$who, 0:9)
  expect_equivalent(t1@data$prevX, rep(NA, 10))
  expect_equivalent(t1@data$prevY, rep(NA, 10))
  expect_equivalent(length(t1), 10)
})
