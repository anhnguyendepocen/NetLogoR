################################################################################
# Butterfly Hilltopping model (Butterfly-1.nlogo)
# by Railsback and Grimm (2012), pages 47-59
#
# Converted into R using the NetLogoR package
# by Sarah Bauduin
#
#

library(NetLogoR)
library(SpaDES) # useful for plotting

# Create a world with the desired extent
elevation <- createNLworld(minPxcor = 0, maxPxcor = 149, minPycor = 0, maxPycor = 149)

# Define the patches values
# Elevation decreases linearly with distance from the center of the hill
# hills are at (30,30) and (120,100)
# The 1st hill is 100 units high, the 2nd hill is 50 units high
elev1 <- 100 - NLdist(world = elevation, agents = patches(elevation), agents2 = cbind(x = 30, y = 30))
elev2 <- 50 - NLdist(world = elevation, agents = patches(elevation), agents2 = cbind(x = 120, y = 100))
pElevation <- ifelse(elev1 > elev2, elev1, elev2)
# Assign the elevation values to the patches
elevation[] <- pElevation

# Visualize the world
# Plot(elevation) # Plot function from SpaDES
plot(elevation)

# Some of the patches have a negative elevation value
pElevation[pElevation < 0] <- 0 # all negative values are set to 0
elevation[] <- pElevation # reassign the patches values
# Plot(elevation) # better!
plot(elevation)

# Create turtles (one butterfly in this model)
t1 <- createTurtles(n = 1, coords = cbind(xcor = 85, ycor = 95)) # the butterfly's initial location is [85, 95]
# Visualize the turtle
# Plot(t1, addTo = "elevation") # need to add the turtle on the plotted world
points(t1, pch = 16, col = t1@data$color)

# Define the global variable needed
q <- 0.4 # q is the probability to move directly to the highest surrounding patch

# Create a go procedure with a for loop
# This can be done with a scheduler function (e.g., with the SpaDES package)
for(time in 1:1000){ # what is inside this loop will be iterated 1000 times

  # The "move" function can be written directly here or before in the script as a separate function and then called here
  # The output of the functions is the turtle t1 and it needs to be reassigned to t1 so that the updated turtle t1 is used
  # at each time step
  if(runif(n = 1, min = 0, max = 1) < q){ # conditional statement

    # Move the turtle t1 uphill considering 8 neighbor patches in the elevation world
    t1 <- uphill(world = elevation, turtles = t1, nNeighbors = 8)

  } else {

    # Or move the turtle t1 to one of its neighbor patches at random in the elevation world
    # Creating local variables instead of putting everything in the same one line code helps to understand
    # It is also useful for debugging
    allNeighbors <- neighbors(world = elevation, agents = t1, nNeighbors = 8)[[1]]
    # [[1]] is needed to access to the neighbors of t1 because neighbors() returns a list of length t1
    oneNeighbor <- oneOf(allNeighbors)
    t1 <- moveTo(turtles = t1, agents = oneNeighbor)

  }

  # Visualize each new position for t1
  # Plot(t1, addTo = "elevation") # plot the new position of the turtle instead of its track
  points(t1, pch = 16, col = t1@data$color)
}

