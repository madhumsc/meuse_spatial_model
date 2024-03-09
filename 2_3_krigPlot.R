library(gstat)
library(tidyverse)
library(viridis)
library(sf)
library(stars)
library(patchwork)
library(viridis)
library(ggplot2)
library(viridis)

meuse_sf <- st_as_sf(meuse, coords = c("x", "y"))
data(meuse)
class(meuse)
# Load necessary libraries


# Load the meuse dataset
data(meuse)

# Convert to sf object
meuse_sf <- st_as_sf(meuse, coords = c("x", "y"))

# Plot using ggplot2 with adjusted parameters
ggplot() +
  geom_sf(data = meuse_sf, aes(color = zinc), size = 2, alpha = 0.7) +
  scale_color_viridis() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        axis.title = element_text(size = 14))  +  # Adjust axis title size
  labs(x = "X Coordinate", y = "Y Coordinate")  # Add axis labels


data(meuse.grid)

library(gstat)

# Assuming you have loaded the meuse and meuse.grid datasets
data(meuse)
data(meuse.grid)

# Convert to spatial objects
coordinates(meuse) <- c("x", "y")
coordinates(meuse.grid) <- c("x", "y")

# Perform Inverse Distance Weighting (IDW)
zinc.idw <- idw(zinc ~ 1, meuse, meuse.grid)


zinc.idw = idw(zinc~1, meuse, meuse.grid)

# Extract predicted values
zinc_idw_pred <- data.frame(coordinates(meuse.grid), zinc = zinc.idw$var1.pred)

# Convert to data.frame
zinc_idw_df <- as.data.frame(zinc_idw_pred)

# Plot using ggplot2
# Plot using ggplot2 with aspect ratio adjustment
ggplot(zinc_idw_df, aes(x = x, y = y, fill = zinc)) +
  geom_tile() +
  scale_fill_viridis() +
  labs(fill = "Predicted Zinc") +
  theme_minimal() +
  coord_equal()


vgm1 = variogram(log(zinc)~1, meuse)
plot(variogram(log(zinc)~1, meuse, cloud=TRUE))

plot(vgm1)


vgm1.fit = fit.variogram(vgm1, model = vgm(1, "Sph", 900, 1))
vgm1.fit


plot(vgm1, vgm1.fit)

# Fit variogram
vgm5 <- variogram(log(zinc) ~ 1, meuse)
vgm5.fit <- fit.variogram(vgm5, model = vgm(psill = 1, model = "Sph", range = 900, nugget = 1))

# Perform kriging
lzn.okriging <- krige(log(zinc) ~ 1, meuse, meuse.grid, model = vgm5.fit)

# Extract predicted values
kriged_df <- data.frame(coordinates(meuse.grid), zinc_predicted = lzn.okriging$var1.pred)

# Plot using ggplot2
ggplot(kriged_df, aes(x = x, y = y, fill = zinc_predicted)) +
  geom_tile() +
  scale_fill_viridis() +
  labs(fill = "Predicted Zinc") +
  theme_minimal()+
  coord_equal()