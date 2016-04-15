# Run the WolfSheepPredation SpaDES module

# Parameters values
wolfSheepParams <- list(.plotInitialTime = 0, .plotInterval = 100, .saveInitialTime = 0, .saveInterval = 1,
                        grassOn = TRUE, grassTGrowth = 30,
                        nSheep = 100, gainFoodSheep = 4, reproSheep = 4,
                        nWolf = 50, gainFoodWolf = 20, reproWolf = 5)

library(SpaDES)

wolfSheepSim <- simInit(
  times = list(start = 0, end = 500), # maximum time steps
  params = list(WolfSheepPredation = wolfSheepParams),
  modules = list("WolfSheepPredation"),
  paths = list(modulePath = paste(getwd(), "/NetLogoR/examples/Wolf-Sheep-Predation", sep = ""))
)

spades(wolfSheepSim, debug = TRUE)
