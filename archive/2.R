# Load necessary libraries
library(sp)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(corrplot)

# Load the 'meuse' dataset
data(meuse)

# View the structure of the dataset
str(meuse)

# Summary statistics of numeric variables
summary(meuse)

# Check for missing values
sum(is.na(meuse))

# Set up a custom layout for the scatterplot matrix
num_vars <- names(meuse)[sapply(meuse, is.numeric)]
num_vars <- num_vars[!num_vars %in% c("x", "y")]  # Exclude x and y coordinates

n <- length(num_vars)
cols <- round(sqrt(n))
rows <- ceiling(n / cols)

# Create a new graphics device
dev.new()

# Create the scatterplot matrix
scatterplot_list <- lapply(num_vars, function(var) {
  ggplot(meuse, aes_string(x = var, y = var)) + geom_point() + ggtitle(var)
})

# Combine scatterplot matrix using patchwork
scatterplot_matrix <- wrap_plots(scatterplot_list, ncol = cols)

# Boxplots for numeric variables
boxplot_list <- lapply(num_vars, function(var) {
  ggplot(meuse, aes_string(x = 1, y = var)) + geom_boxplot() + ggtitle(paste("Boxplot of", var))
})

# Combine boxplots using patchwork
boxplot_grid <- wrap_plots(boxplot_list, ncol = cols)

# Histograms and density plots for numeric variables
histogram_density_list <- lapply(num_vars, function(var) {
  ggplot(meuse, aes_string(x = var)) + 
    geom_histogram(binwidth = 2, fill = "lightblue", color = "black") + 
    labs(x = var, y = "Frequency") + 
    ggtitle(paste("Histogram of", var)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
})

density_plot_list <- lapply(num_vars, function(var) {
  ggplot(meuse, aes_string(x = var)) + 
    geom_density(fill = "lightblue", color = "black") + 
    labs(x = var, y = "Density") + 
    ggtitle(paste("Density Plot of", var)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
})

# Combine histograms and density plots using patchwork
histogram_density_grid <- wrap_plots(histogram_density_list, ncol = cols)
density_plot_grid <- wrap_plots(density_plot_list, ncol = cols)

# Arrange plots
layout_matrix <- rbind(c(1, 1), c(2, 3), c(4, 5))
grid.arrange(
  scatterplot_matrix,
  boxplot_grid,
  histogram_density_grid,
  density_plot_grid,
  layout_matrix = layout_matrix
)

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
