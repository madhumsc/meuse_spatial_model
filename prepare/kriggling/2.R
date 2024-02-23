library(sp)
data(meuse)

# Convert to spatial dataframe
coordinates(meuse) <- c("x", "y")
proj4string(meuse) <- CRS("+init=epsg:28992")  # Assuming the original projection is EPSG 28992

# Define desired CRS
crs <- CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs")

# Transform to desired CRS
meuse <- spTransform(meuse, crs)

# Visualize the meuse dataset
plot(meuse)



# Create a grid for interpolation
grid_meuse <- meuse_extents |>
  st_as_stars(dx = 100) |>
  st_as_stars(crs = st_crs(meuse))

# Interpolate using IDW
interpolated_values_meuse <- idw(zinc ~ 1, meuse, grid_meuse)

# Visualize the interpolation
plot(interpolated_values_meuse)

# If you want to overlay the original points on the plot
points(meuse, col = "red")
