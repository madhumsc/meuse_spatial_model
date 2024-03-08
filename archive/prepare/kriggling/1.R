library("gstat")   # geostatistics
library("mapview") # map plot
library("sf")      # spatial vector data
library("stars")   # spatial-temporal data
library("terra")   # raster data handling 
library("ggplot2") # plotting
mapviewOptions(fgb = FALSE)


head(meuse)
#view(meuse)

# Convert to sf object
meuse_sf <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992)

# Plot using ggplot2
ggplot() +
  geom_sf(data = meuse_sf)


ggplot() + geom_sf(data = meuse_sf) + 
  geom_sf(data = meuse_sf, mapping = aes(col = zinc)) + 
  scale_color_viridis()

library(gstat)
library(ggplot2)
library(sf)

# Load the 'meuse' dataset
data(meuse)

# Convert to sf object
meuse_sf <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992)

# Define a grid
grid <- st_bbox(meuse_sf) |>
  st_as_stars(dx = 100) |> st_crop(meuse_sf)

# Perform IDW interpolation
interpolated.values <- idw(zinc ~ 1, meuse_sf, grid)

# Visualize the interpolated surface
ggplot() +
  geom_stars(data = interpolated.values, aes(fill = var1.pred, x = x, y = y)) + 
  geom_sf(data = meuse_sf, col = "red") + 
  scale_fill_viridis() +
  theme_minimal()
