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


vcloud <- variogram(zinc ~ 1, meuse_sf, cloud=TRUE)
vcloud


ggplot(data=vcloud, aes(x=dist, y=gamma)) + geom_point()

plot(vcloud)


v2 <- variogram(zinc ~ 1, meuse_sf, cutoff = 100000, width = 10000)

plot(v2, plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(v2$dist)))


vgm()


show.vgms(par.strip.text=list(cex=0.75))

v.m <- fit.variogram(v1, vgm(psill=20, model = "Exp", range = 20000, nugget = 1))


plot(v1, v.m)


v1.ani <- variogram(zinc ~ 1, alpha = c(0, 45, 90, 135), meuse_sf)
v.m <- fit.variogram(v1, vgm(psill=20, model = "Exp", range = 20000, nugget = 1))
plot(v1.ani, v.m)
plot(v1.ani)



v1.ani <- variogram(zinc ~ 1, alpha = c(0, 45, 90, 135), meuse_sf)
fit.ani <- vgm(psill=20, model = "Exp", range = 25000, nugget = 3, anis = c(30, 10, 0, 0.5, 0.3))
plot(v1.ani, fit.ani)



v1 <- variogram(zinc ~ 1, meuse_sf)
v.m <- fit.variogram(v1, vgm(psill=20, model = "Exp", range = 20000, nugget = 1))
krigOK <- krige(zinc ~ 1,meuse_sf, grid, v.m)


