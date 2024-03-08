library(gstat)
library(sp)
library(ggplot2)

# Load the 'meuse' dataset
data(meuse)

# Convert the 'meuse' dataframe to a SpatialPointsDataFrame
coordinates(meuse) <- c("x", "y")

# Define the CRS (Coordinate Reference System) for the 'meuse' dataset
proj4string(meuse) <- CRS("+init=epsg:28992")  # Dutch Rijksdriehoekstelsel (RD) projection

# Visualize the 'meuse' dataset
plot(meuse, pch = 19, col = "blue", main = "Meuse River Basin")

# Fit a variogram to the zinc concentration in the 'meuse' dataset
v <- variogram(log(zinc) ~ 1, meuse)

# Plot the experimental variogram
plot(v, main = "Experimental Variogram - Zinc Concentration")

# Fit a variogram model to the experimental variogram
v.fit <- fit.variogram(v, vgm(psill = 1, model = "Exp", range = 500, nugget = 0.1))

# Plot the experimental variogram with the fitted model
plot(v, v.fit, main = "Experimental Variogram with Fitted Model - Zinc Concentration")

# Ordinary Kriging
# Define a grid for kriging prediction
grid <- expand.grid(x = seq(min(meuse@coords[,1]), max(meuse@coords[,1]), length.out = 100),
                    y = seq(min(meuse@coords[,2]), max(meuse@coords[,2]), length.out = 100))

# Convert the grid to a SpatialPointsDataFrame
coordinates(grid) <- c("x", "y")
gridded(grid) <- TRUE

# Perform kriging
kriged <- krige(log(zinc) ~ 1, meuse, grid, model = v.fit)

# Visualize kriging results
image(kriged, col = terrain.colors(50), main = "Kriging Prediction - Zinc Concentration")
points(meuse, pch = 19, col = "blue")
