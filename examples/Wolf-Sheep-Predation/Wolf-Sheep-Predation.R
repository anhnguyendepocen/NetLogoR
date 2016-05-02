a = Sys.time()
useFastClasses <- TRUE
maxTime <- 500
seed <- sample(1e5, 1)
seed <- 22242
set.seed(seed)

################################################################################
# Wolf sheep predation
# by Wilensky (1997) NetLogo Wolf Sheep Predation model.
# http://ccl.northwestern.edu/netlogo/models/WolfSheepPredation
#
# Converted into R using the NetLogoR package
# by Sarah Bauduin
#
#

## Packages required
#library(NetLogoR)
library(SpaDES)
library(profvis) # test the speed of the different functions


## Global variables (some represent the model buttons)
# Grass settings
grassOn <- TRUE # TRUE to include grass in the model, FALSE to only include wolves and sheep
grassTGrowth <- 30 # how long it takes for grass to regrow once it is eaten
numGreen <- numeric() # keep track of how much grass there is

# Sheep settings
nSheep <- 100 # initial sheep population size
gainFoodSheep <- 4 # amount of energy sheep get for every grass patch eaten
reproSheep <- 4 # probability in % of a sheep reproducing at each time step
numSheep <- nSheep # keep track of how many sheep there are

# Wolf settings
nWolf <- 50 # initial wolf population size
gainFoodWolf <- 20 # amount of energy wolves get for every sheep eaten
reproWolf <- 5 # probability in % of a wolf reproducing at each time step
numWolves <- nWolf # keep track of how many wolves there is

# torus = TRUE # just for reminder, to be used in the movement functions (e.g., fd())


## Setup
# Create the world
if(useFastClasses){
  grass <- createNLworldMatrix(minPxcor = -25, maxPxcor = 25, minPycor = -25, maxPycor = 25)
} else {
  grass <- createNLworld(minPxcor = -25, maxPxcor = 25, minPycor = -25, maxPycor = 25)
}

worldCoords <- cbind(pxcor=rep_len(attr(grass, "minPxcor"):attr(grass, "maxPxcor"),
                                   length.out=length(grass)),
                     pycor = rep(attr(grass, "maxPycor"):attr(grass, "minPycor"), each = ncol(grass)))
assign("worldCoords", worldCoords, envir=.GlobalEnv)

# If grassOn is TRUE, assign grass and countdown values to patches
# Because there are multiple patches variables, a NLworldStack is needed
# If grassOn is TRUE, the grass grows and the sheep eat it, if FALSE, the sheep don't need to eat
if(grassOn == TRUE){
  # Initialize patch values (grass and countdown) at random
  grassVal <- sample(c(0,1), size = count(patches(grass)), replace = TRUE) # 0 or 1 (i.e., green or brown in the NetLogo model)
  grass <- set(world = grass, agents = patches(grass), val = grassVal)
  countdown <- grass # countdown is a new NLworld with the same extent as grass
  countdownVal <- runif(n = count(patches(grass)), min = 0, max = grassTGrowth) # grass grow clock
  countdown <- set(world = countdown, agents = patches(countdown), val = countdownVal)

  if(useFastClasses){
    field <- NLworldArray(grass, countdown)
  } else {
    field <- NLstack(grass, countdown)
  }
}
# When no patches values are used, using grass, countdown or field as the world argument required by a function does not change anything
# because they all have the same extent and number of patches
# When patches values are used (e.g., when the sheep eat the grass), use only field as the world argument for the functions
# which update and retrieve the patches values
# When field is updated, the values on the individual NLworld grass and countdown are not updated, only the layers in field are

# Create the sheep
if(useFastClasses){
  sheep <- createTurtlesAM(n = nSheep, coords = randomXYcor(world = grass, n = nSheep), breed = "aSheep", color = rep("red", nSheep))
} else {
  sheep <- createTurtles(n = nSheep, coords = randomXYcor(world = grass, n = nSheep), breed = "aSheep", color = rep("red", nSheep))
}
# Add the energy variable
sheep <- turtlesOwn(turtles = sheep, tVar = "energy", tVal = runif(n = nSheep, min = 0, max = 2 * gainFoodSheep))

# Create the wolves
if(useFastClasses){
  wolves <- createTurtlesAM(n = nWolf, coords = randomXYcor(world = grass, n = nWolf), breed = "wolf", color = rep("black", nWolf))
} else {
  wolves <- createTurtles(n = nWolf, coords = randomXYcor(world = grass, n = nWolf), breed = "wolf", color = rep("black", nWolf))
}
# Add the energy variable
wolves <- turtlesOwn(turtles = wolves, tVar = "energy", tVal = runif(n = nWolf, min = 0, max = 2 * gainFoodWolf))

# Initialize the count of grass
if(grassOn == TRUE){
  pGreen <- NLwith(world = field, var = "grass", agents = patches(field), val = 1) # patches equal to 1 (green)
  numGreen <- count(pGreen)
}

# # Visualize the world
# dev() # open a new plotting window
# clearPlot()
# if(grassOn == TRUE){
#   Plot(field[[1]])
#   Plot(sheep, addTo = "field$grass")
#   Plot(wolves, addTo = "field$grass")
# } else {
#   grass <- set(world = grass, agents = patches(grass), val = 0) # cannot plot an empty world
#   Plot(grass)
#   Plot(sheep, addTo = "grass")
#   Plot(wolves, addTo = "grass")
# }


## Functions used in the go procedure
# Always return the object updated by the function
# When only one type of input is permitted (e.g., only sheep or only wolves), the function does not need to express arguments
# When a function can be used by both sheep and wolves, the argument "turtles" must be used when building the function
# and be replaced by either sheep or wolves when calling the function

move <- function(turtles){ # sheep and wolves
  # turtles <- right(turtles, angle = runif(n = count(turtles), min = 0, max = 50))
  # turtles <- left(turtles, angle = runif(n = count(turtles), min = 0, max = 50))
  # The two above functions can be replaced by this next one, as a negative value to turn right will turn left
  turtles <- right(turtles, angle = runif(n = count(turtles), min = -50, max = 50))
  turtles <- fd(world = grass, turtles = turtles, dist = 1, torus = TRUE)
  return(turtles)
}

# # Test move()
# if(useFastClasses){
#   plot(wolves@.Data[, "xcor"], wolves@.Data[, "ycor"], col = rainbow(count(wolves)), pch = 16)
#   for(i in 1:15){
#     wolves <- move(wolves)
#     points(wolves@.Data[, "xcor"], wolves@.Data[, "ycor"], col = rainbow(count(wolves)), pch = 16)
#   }
# } else {
#   plot(wolves, col = rainbow(count(wolves)), pch = 16)
#   for(i in 1:15){
#     wolves <- move(wolves)
#     points(wolves, col = rainbow(count(wolves)), pch = 16)
#   }
# }
# #

eatGrass <- function(){ # only sheep

  #microbenchmark(times = 10,
  pGreen <- NLwith(world = field, var = "grass", agents = patches(field), val = 1) # patches with grass equal to 1 (green)
  #pGreen1 <- which(field[,,"grass"] == 1, arr.ind=TRUE)-26 # patches with grass equal to 1 (green)
  #)

  sheepOnGreen <- turtlesOn(world = field, turtles = sheep, agents = pGreen) # sheep on green patches

  if(count(sheepOnGreen) != 0){
    # These sheep gain energy by eating
    energySheep <- of(agents = sheepOnGreen, var = "energy") # energy before eating
    sheep <- set(turtles = sheep, agents = sheepOnGreen, var = "energy", val = energySheep + gainFoodSheep) # update energy

    # If a sheep is on a green patch (value equal to 1), it eats the grass and turns it to brown (value to 0)
    pHere <- patchHere(world = field, turtles = sheepOnGreen)
    field <- set(world = field, agents = pHere, var = "grass", val = 0)

  }

  return(list(field, sheep)) # return the two objects updated in this function
}

# # Test eatGrass()
# if(useFastClasses){
#   grass <- createNLworldMatrix(minPxcor = 1, maxPxcor = 10, minPycor = 1, maxPycor = 10)
# } else {
#   grass <- createNLworld(1, 10, 1, 10)
# }
# grass <- set(world = grass, agents = patches(grass), val = c(rep(1, 50), rep(0, 50)))
# countdown <- grass
# countdown <- set(world = countdown, agents = patches(countdown), val = 0)
# if(useFastClasses){
#   field <- NLworldArray(grass, countdown)
#   sheep <- createTurtlesAM(n = 10, coords = cbind(xcor = 1:10, ycor = 1:10))
# } else {
#   field <- NLstack(grass, countdown)
#   sheep <- createTurtles(n = 10, coords = cbind(xcor = 1:10, ycor = 1:10))
# }
# sheep <- turtlesOwn(turtles = sheep, tVar = "energy", tVal = 1:10)
# if(useFastClasses == FALSE){
#   plot(field$grass)
#   points(sheep)
# }
# resultsEatGrass <- eatGrass()
# fieldEat <- resultsEatGrass[[1]]
# if(useFastClasses == FALSE){
#   plot(fieldEat$grass)
# }
# of(world = fieldEat, var = "grass", agents = patches(grass)) == c(rep(1,9), 0, rep(1, 8), 0, rep(1, 8), 0 , rep(1, 8), 0, rep(1, 8), 0, rep(1, 4), rep(0, 50))
# sheepEat <- resultsEatGrass[[2]]
# of(agents = sheepEat, var = "energy")[6:10] == (6:10 + gainFoodSheep)
# #

#whoEnergy <- numeric()
death <- function(turtles){ # sheep and wolves
  # When energy dips below 0, die
  whoEnergy <- of(agents = turtles, var = c("who", "energy"))
  if(useFastClasses) {
    who0 <- whoEnergy[which(whoEnergy[,"energy"] < 0), "who"] # "who" numbers of the turtles with their energy value below 0
  } else {
    who0 <- whoEnergy[which(whoEnergy$energy < 0), "who"] # "who" numbers of the turtles with their energy value below 0
  }
  if(length(who0) != 0){
    turtles <- die(turtles = turtles, who = who0)
  }

  return(turtles)
}

# # Test death()
# if(useFastClasses){
#   wolves <- createTurtlesAM(n = nWolf, coords = randomXYcor(world = grass, n = nWolf), breed = "wolf", color = rep("black", nWolf))
# } else {
#   wolves <- createTurtles(n = nWolf, coords = randomXYcor(world = grass, n = nWolf), breed = "wolf", color = rep("black", nWolf))
# }
# wolves <- turtlesOwn(turtles = wolves, tVar = "energy", tVal = runif(n = nWolf, min = 0, max = 2 * gainFoodWolf))
# count1 <- count(wolves)
# count2 <- count(wolves)
# for(i in 1:100){
#   energy <- runif(count(wolves), min = -10, max = 100)
#   wolves <- set(turtles = wolves, agents = wolves, var = "energy", val = energy)
#   count1 <- c(count1, count(wolves) - length(energy[energy < 0]))
#   wolves <- death(wolves)
#   count2 <- c(count2, count(wolves))
# }
# plot(1:length(count1), count1, pch = 16)
# points(1:length(count2), count2, pch = 16, col = "red")
# #

reproduce <- function(turtles, reproTurtles){ # sheep and wolves
  # Throw dice to see if the turtles will reproduce
  repro <- runif(n = count(turtles), min = 0, max = 100) < reproTurtles
  whoTurtles <- of(agents = turtles, var = "who") # "who" of the turtles before they reproduce
  reproWho <- whoTurtles[repro] # "who" of turtles which reproduce
  reproInd <- turtle(turtles, who = reproWho) # turtles which reproduce

  if(count(reproInd) != 0){ # if there is at least one turtle reproducing
    energyTurtles <- of(agents = reproInd, var = "energy")
    # Divide the energy between the parent and offspring
    turtles <- set(turtles = turtles, agents = reproInd, var = "energy", val = energyTurtles / 2)
    turtles <- hatch(turtles = turtles, who = reproWho, n = 1) # hatch one offspring per parent

    # Move the offspring by 1 step
    whoNewTurtles <- of(agents = turtles, var = "who") # "who" of the turtles after they reproduced
    whoOffspring <- which(!whoNewTurtles %in% whoTurtles) # "who" of offspring
    offspring <- turtle(turtles = turtles, who = whoOffspring)
    offspringMoved <- right(turtles = offspring, angle = runif(n = count(offspring), min = 0, max = 360))
    offspringMoved <- fd(world = grass, turtles = offspring, dist = 1, torus = TRUE)
    # Update the headings and coordinates of the offsprings inside the turtles
    valOffspring <- of(agents = offspringMoved, var = c("heading", "xcor", "ycor"))
    turtles <- set(turtles = turtles, agents = offspring, var = c("heading", "xcor", "ycor"), val = valOffspring)
  }

  return(turtles)
}

# # Test reproduce()
# if(useFastClasses){
#   wolves <- createTurtlesAM(n = nWolf, coords = randomXYcor(world = grass, n = nWolf), breed = "wolf", color = rep("black", nWolf))
# } else {
#   wolves <- createTurtles(n = nWolf, coords = randomXYcor(world = grass, n = nWolf), breed = "wolf", color = rep("black", nWolf))
# }
# wolves <- turtlesOwn(turtles = wolves, tVar = "energy", tVal = runif(n = nWolf, min = 0, max = 2 * gainFoodWolf))
# count1 <- count(wolves)
# count2 <- count(wolves)
# for(i in 1:100){
#   count1 <- c(count1,count(wolves) + count(wolves) * reproWolf / 100)
#   wolves <- reproduce(wolves, reproWolf)
#   count2 <- c(count2, count(wolves))
# }
# plot(1:length(count1), count1, pch = 16)
# points(1:length(count2), count2, pch = 16, col = "red")
# #

catchSheep <- function(){ # only wolves
  # "who" numbers of sheep that are on the same patches as the wolves
  sheepWolves <- turtlesOn(world = grass, turtles = sheep, agents = wolves, simplify = FALSE)
  if(nrow(sheepWolves) != 0){
    # sheepWolves[,"whoTurtles"] are the "who" numbers of sheep
    # sheepWolves[,"id"] represent the rank/order of the individual wolf in the wolves (! not the "who" numbers of the wolves)
    sheepGrabbed <- oneOf(agents = sheepWolves) # grab one random sheep

    sheep <- die(turtles = sheep, who = sheepGrabbed) # kill the grabbed sheep
    whoWolves <- of(agents = wolves, var = "who")
    whoGrabbingWolves <- whoWolves[unique(sheepWolves[,"id"])]
    grabbingWolves <- turtle(turtles = wolves, who = whoGrabbingWolves)
    energyGrabbingWolves <- of(agents = grabbingWolves, var = "energy")
    # Get energy from eating for the wolves who grabbed sheep
    wolves <- set(turtles = wolves, agents = grabbingWolves, var = "energy", val = energyGrabbingWolves + gainFoodWolf)
  }

  return(list(sheep, wolves))# return the two objects updated in this function
}

# # Test catchSheep()
# if(useFastClasses){
#   grass <- createNLworldMatrix(minPxcor = 1, maxPxcor = 10, minPycor = 1, maxPycor = 10)
# } else {
#   grass <- createNLworld(1, 10, 1, 10)
# }
# grass <- set(world = grass, agents = patches(grass), val = c(rep(1, 50), rep(0, 50)))
# countdown <- grass
# countdown <- set(world = countdown, agents = patches(countdown), val = 0)
# if(useFastClasses){
#   field <- NLworldArray(grass, countdown)
#   sheep <- createTurtlesAM(n = 10, coords = cbind(xcor = c(1,1,2,2,3,4,5,6,7,8), ycor = c(1,1,2,2,3,4,5,6,7,8)))
#   wolves <- createTurtlesAM(n = 5, coords = cbind(xcor = 1:5, ycor = 1:5))
# } else {
#   field <- NLstack(grass, countdown)
#   sheep <- createTurtles(n = 10, coords = cbind(xcor = c(1,1,2,2,3,4,5,6,7,8), ycor = c(1,1,2,2,3,4,5,6,7,8)))
#   wolves <- createTurtles(n = 5, coords = cbind(xcor = 1:5, ycor = 1:5))
# }
# wolves <- turtlesOwn(turtles = wolves, tVar = "energy", tVal = 1:5)
# catchSheepResults <- catchSheep()
# sheepCatch <- catchSheepResults[[1]]
# wolvesCatch <- catchSheepResults[[2]]
# count(sheepCatch) == 5
# count(wolvesCatch) == 5
# of(agents = wolvesCatch, var = "energy") == (1:5 + gainFoodWolf)
# #

growGrass <- function(){ # only patches
  # Identify patches with grass equal to 0 (brown) and countdown less or equal to 0
  pBrown <- NLwith(world = field, var = "grass", agents = patches(field), val = 0)
  pBrownCountdown <- of(world = field, var = "countdown", agents = pBrown) # countdown values for the patches equal to 0 (brown)

  pBrownCountdown0 <- which(pBrownCountdown <= 0) # patches with a countdown <= 0
  if(length(pBrownCountdown0) != 0){
    pGrow <- pBrown[pBrownCountdown0, , drop = FALSE] # patches with grass equal to 0 (brown) and countdown <= 0
    # Grow some grass on these patches and reset the countdown
    field <- set(world = field, var = c("grass", "countdown"), agents = pGrow,
                 val = cbind(grass = rep(1, count(pGrow)), countdown = rep(grassTGrowth, count(pGrow))))
  }

  pBrownCountdown1 <- which(!pBrownCountdown <= 0) # patches with a countdown > 0
  if(length(pBrownCountdown1) != 0){
    pWait <- pBrown[pBrownCountdown1, , drop = FALSE] # patches with grass equal to 0 (brown) and countdown > 0
    # Decrease the countdown for the patches which wait
    field <- set(world = field, var = "countdown", agents = pWait, val = pBrownCountdown[pBrownCountdown1] - 1)
  }

  return(field)
}

# # Test growGrass()
# grass <- createNLworld(1, 5, 1, 5)
# grass <- set(world = grass, agents = patches(grass), val = c(rep(1, 10), rep(0, 15)))
# countdown <- grass
# countdown <- set(world = countdown, agents = patches(countdown), val = c(rep(-1, 15), rep(1, 10)))
# field <- NLstack(grass, countdown)
# fieldGrow <- growGrass()
# of(world = fieldGrow, agents = patches(fieldGrow), var = "grass") == c(rep(1, 15), rep(0, 10))
# of(world = fieldGrow, agents = patches(fieldGrow), var = "countdown") == c(rep(-1, 10), rep(grassTGrowth, 5), rep(0, 10))
# #


## Go
profvisWolfSheep <- profvis({
time <- 0
while((NLany(sheep) | NLany(wolves)) & time < maxTime ){ # as long as there are sheep or wolves in the world (time steps maximum at 500)

  # Ask sheep
  if(count(sheep) != 0){
    sheep <- move(sheep)
    if(grassOn == TRUE){
      energySheep <- of(agents = sheep, var = "energy")
      sheep <- set(turtles = sheep, agents = sheep, var = "energy", val = energySheep - 1)
      eatGrassResults <- eatGrass() # in the results are stored both "field" and "sheep"
      field <- eatGrassResults[[1]] # reassign the object with their updated values
      sheep <- eatGrassResults[[2]]
    }
    sheep <- death(sheep)
    if(count(sheep) != 0){
      sheep <- reproduce(sheep, reproSheep)
    }
  }

  # Ask wolves
  if(count(wolves) != 0){
    wolves <- move(wolves)
    energyWolves <- of(agents = wolves, var = "energy")
    wolves <- set(turtles = wolves, agents = wolves, var = "energy", val = energyWolves - 1)
    catchSheepResults <- catchSheep() # in the results are stored both "sheep" and "wolves"
    sheep <- catchSheepResults[[1]] # reassign the object with their updated values
    wolves <- catchSheepResults[[2]]
    wolves <- death(wolves)
    if(count(wolves) != 0){
      wolves <- reproduce(wolves, reproWolf)
    }
  }

  # Ask grass
  if(grassOn == TRUE){
    field <- growGrass()
    pGreen <- NLwith(world = field, var = "grass", agents = patches(field), val = 1) # patches equal to 1 (green)
    npGreen <- count(pGreen)
    numGreen <- c(numGreen, npGreen) # add the new number of green patches
  }

  numSheep <- c(numSheep, count(sheep)) # add the new number of sheep
  numWolves <- c(numWolves, count(wolves)) # add the new numbr of wolves

  time <- time + 1
  # # Help for checking the model is working
  #print(time)
}
}) # end profvis

## Plot outputs
dev()
timeStep <- 1:length(numSheep)

if(grassOn == TRUE){

  plot(timeStep, numSheep, type = "l", col = "blue", lwd = 2, ylab = "Population size", xlab = "Time step",
       ylim = c(min = 0, max = max(c(max(numSheep), max(numWolves), max(numGreen / 4)))))
  lines(timeStep, numWolves, col = "red", lwd = 2)
  lines(timeStep, numGreen / 4, col = "green", lwd = 2)

  legend("topleft", legend = c("Sheep", "Wolves", "Grass / 4"), lwd = c(2, 2, 2), col = c("blue", "red", "green"),
         bg = "white")

} else {

  plot(timeStep, numSheep, type = "l", col = "blue", lwd = 2, ylab = "Population size", xlab = "Time step",
       ylim = c(min = 0, max = max(c(max(numSheep), max(numWolves)))))
  lines(timeStep, numWolves, col = "red", lwd = 2)

  legend("topleft", legend = c("Sheep", "Wolves"), lwd = c(2, 2), col = c("blue", "red"), bg = "white")
}

b = Sys.time()
print(paste(sum(numSheep+numWolves)/as.numeric(b-a), "sheep and wolves per second"))
print(b-a)
profvisWolfSheep
