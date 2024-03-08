library(sp)
library(gstat)
library(sf)
library(tidyverse)
library(automap)

data("meuse")
coordinates(meuse) <- ~x+y
proj4string(meuse) <- CRS("+init=epsg:28992")

crs <- st_crs(28992)
meuse.sf <- st_as_sf(meuse, coords = c("x", "y"), crs = crs)

meuse.grid <- expand.grid(x = seq(min(meuse$x), max(meuse$x), length.out = 100),
                          y = seq(min(meuse$y), max(meuse$y), length.out = 100))

coordinates(meuse.grid) <- ~x+y
proj4string(meuse.grid) <- CRS("+init=epsg:28992")

meuse.grid.sf <- st_as_sf(meuse.grid, coords = c("x", "y"), crs = crs)
meuse.grid.sf <- st_intersection(meuse.grid.sf, st_as_sf(meuse, crs = crs))

# Interpolate values using Ordinary Kriging
kriging_result <- autoKrige(zinc ~ 1, meuse.sf, meuse.grid.sf)

ggplot() + 
  geom_raster(data = kriging_result$krige_output, aes(fill = var1.pred, x = x, y = y)) + 
  geom_sf(data = meuse.sf, col="red") + 
  scale_fill_viridis() 

