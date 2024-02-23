# Load necessary libraries
library(sp)
library(ggplot2)

# Load the 'meuse' dataset
data(meuse)

# View the structure of the dataset
str(meuse)

# Summary statistics of numeric variables
summary(meuse)

# Check for missing values
sum(is.na(meuse))

# Set up the plot layout
par(mfrow = c(3, 3), mar = c(2, 2, 1, 1))

# Create scatterplot matrix for numeric variables
for (i in 3:ncol(meuse)) {
  for (j in 3:ncol(meuse)) {
    plot(meuse[, c(i, j)], main = paste(colnames(meuse)[i], "vs", colnames(meuse)[j]))
  }
}

# Reset the plot layout
par(mfrow = c(1, 1))

# Scatterplot of zinc concentration vs. dist
ggplot(data = meuse, aes(x = dist, y = zinc)) +
  geom_point() +
  labs(x = "Distance to River (m)", y = "Zinc Concentration (ppm)") +
  ggtitle("Scatterplot of Zinc Concentration vs. Distance to River")

# Spatial plot of zinc concentration
sp::coordinates(meuse) <- c("x", "y")  # Set coordinates
sp::proj4string(meuse) <- CRS("+init=epsg:28992")  # Define CRS

# Plotting zinc concentrations
sp::spplot(meuse, "zinc", main = "Zinc Concentrations in the Meuse Dataset")
