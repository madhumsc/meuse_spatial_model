
## Load necessary libraries
###1
library(gstat)
library(tidyverse)
library(viridis)
library(sf)
library(stars)
library(patchwork)
library(ggplot2)
###2

##loads the dataset named "meuse" into the current R session.check the data type. show the head
###1
data("meuse")
class(meuse)
head(meuse)
###2


##Convert the "meuse" dataset to a spatial feature object (sf) with coordinates "x" and "y".
###1
meuse_sf <- st_as_sf(meuse, coords = c("x", "y"))
###2

# Plot using ggplot2 with adjusted parameters


###1
ggplot() +
  geom_sf(data = meuse_sf, aes(color = zinc), size = 2, alpha = 0.7) +
  scale_color_viridis() +
  theme_minimal() + # reduce unnecessary clutter
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        axis.title = element_text(size = 14))  +  # Adjust axis title size
  labs(x = "X Coordinate", y = "Y Coordinate")  # Add axis labels
###2

##Load the "meuse.grid" dataset containing gridded environmental variables for spatial analysis and modeling.
###1
data(meuse.grid)
###2


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


#ggplot(meuse,aes(y=zinc,x=dist))+geom_point()

#ggplot(meuse,aes(y=log(zinc),x=sqrt(dist)))+geom_point()

vgmUni=variogram(log(zinc)~sqrt(dist),alpha = c(0, 45, 90, 135), meuse)

vgmUK.fit = fit.variogram(vgmUni,  model = vgm(1, "Sph", 900, 1))

k11<- krige(log(zinc) ~ 1, meuse, meuse.grid, model = vgmUK.fit )



plot(vgmUni,vgmUK.fit )


# v1.ani <- variogram(NO2~1, alpha = c(0, 45, 90, 135), no2.sf)
# v.m <- fit.variogram(v1, vgm(psill=20, model = "Exp", range = 20000, nugget = 1))
# plot(v1.ani, v.m)


plot(vgmUni, vgmUK.fit)




lm.model<-lm(log(zinc)~sqrt(dist),data=meuse)

residual<-residuals(lm.model)

v1.residual <- variogram(residual~1,alpha = c(0, 45, 90, 135), meuse)

vm.resid<- fit.variogram(v1.residual ,  model = vgm(1, "Sph", 900, 1))



plot(v1.residual, vm.resid)



#k22<- krige(log(zinc) ~ sqrt(dist), meuse, meuse.grid, model = vm.resid )








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

p1<-ggplot(zinc_idw_df, aes(x = x, y = y, fill = zinc)) +
  geom_tile() +
  scale_fill_viridis() +
  labs(fill = "Predicted Zinc",title = "IDW Predictions of Zinc") +
  theme_minimal() +
  coord_equal()
p2<-ggplot(kriged_df, aes(x = x, y = y, fill = zinc_predicted)) +
  geom_tile() +
  scale_fill_viridis() +
  labs(fill = "Predicted Zinc",title = "Kriged Predictions of Zinc") +
  theme_minimal()+
  coord_equal()

p1|p2

# Calculate Kriging error
kriging_error <- meuse$zinc - lzn.okriging$var1.pred

# Create a data frame with coordinates and Kriging error
kriging_error_df <- data.frame(coordinates(meuse.grid), kriging_error = kriging_error)

# Plot Kriging error map
ggplot(kriging_error_df, aes(x = x, y = y, fill = kriging_error)) +
  geom_tile() +
  scale_fill_viridis(name = "Kriging Error") +
  labs(title = "Kriging Error Map") +
  theme_minimal() +
  coord_equal()

# Calculate IDW error
idw_error <- meuse$zinc - zinc.idw$var1.pred

# Create a data frame with coordinates and IDW error
idw_error_df <- data.frame(coordinates(meuse.grid), idw_error = idw_error)

# Plot IDW error map
ggplot(idw_error_df, aes(x = x, y = y, fill = idw_error)) +
  geom_tile() +
  scale_fill_viridis(name = "IDW Error") +
  labs(title = "IDW Error Map") +
  theme_minimal() +
  coord_equal()

p3<-ggplot(kriging_error_df, aes(x = x, y = y, fill = kriging_error)) +
  geom_tile() +
  scale_fill_viridis(name = "Kriging Error") +
  labs(title = "Kriging Error Map") +
  theme_minimal() +
  coord_equal()

p4<-ggplot(idw_error_df, aes(x = x, y = y, fill = idw_error)) +
  geom_tile() +
  scale_fill_viridis(name = "IDW Error") +
  labs(title = "IDW Error Map") +
  theme_minimal() +
  coord_equal()

p3|p4

