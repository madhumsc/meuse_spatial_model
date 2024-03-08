library(ggplot2)
library(sp)
library(gstat)
library(viridis)

# Load the 'meuse' dataset
data(meuse)

# Convert 'meuse' to SpatialPointsDataFrame
coordinates(meuse) <- c("x", "y")
proj4string(meuse) <- CRS("+init=epsg:28992")  # Assuming the original CRS is EPSG:28992

# Create a grid for interpolation
bbox <- bbox(meuse)
x_range <- bbox[1, ]
y_range <- bbox[2, ]
x <- seq(from = x_range[1], to = x_range[2], by = 40)
y <- seq(from = y_range[1], to = y_range[2], by = 40)
grid <- expand.grid(x = x, y = y)
coordinates(grid) <- c("x", "y")
proj4string(grid) <- CRS("+init=epsg:28992")

# Interpolation
idw_result <- idw(zinc ~ 1, meuse, grid)  # Interpolate the 'zinc' variable

# Convert interpolated grid to data frame
grid_df <- as.data.frame(coordinates(grid))

# Convert contour data to data frame
contour_df <- as.data.frame(coordinates(idw_result))
names(contour_df) <- c("x", "y")
contour_df$zinc.pred <- idw_result$var1.pred

# Plotting
# Plotting
ggplot() +
  geom_tile(data = as.data.frame(grid), aes(x = x, y = y)) +
  geom_contour(data = as.data.frame(contour_df), aes(x = x, y = y, z = zinc.pred), color = "black") +
  geom_point(data = as.data.frame(meuse), aes(x = x, y = y, color = zinc)) +
  scale_color_viridis() +
  theme_minimal()


ggplot() +
  geom_tile(data = as.data.frame(grid), aes(x = x, y = y)) +
  geom_contour(data = as.data.frame(contour_df), aes(x = x, y = y, z = zinc.pred), color = "black") +
  geom_point(data = as.data.frame(meuse), aes(x = x, y = y, color = zinc)) +
  geom_point(data = as.data.frame(meuse), aes(x = x, y = y), color = "black", alpha = 0.5) + # Original map
  scale_color_viridis() +
  theme_minimal()

library(sf)

# Convert 'meuse' to sf object

library(ggplot2)
library(gstat)

# Assuming 'interpolated.values' is a stars object representing interpolated values
# Assuming 'map' is an sf object representing the original map
# Assuming 'no2.sf' is an sf object representing additional spatial data

# Plotting with ggplot2



