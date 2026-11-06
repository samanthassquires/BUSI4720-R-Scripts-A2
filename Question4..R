# ==============================================================================
# QUESTION 4.1: DATA EXPLORATION
# ==============================================================================
# Load required libraries
library(readxl)

# Import the dataset from your Home folder
facility_data <- read_excel("clustering.xlsx")

# a. View the entire dataset in a grid tab
View(facility_data)

# b. Display the first six observations
head(facility_data)

# c. Create a scatter plot between Energy Consumption and Emissions-2025
plot(facility_data$Energy_Consumption, facility_data$Emissions_2025,
     main = "Energy Consumption vs. 2025 GHG Emissions",
     xlab = "Energy Consumption (GWh)",
     ylab = "GHG Emissions 2025 (Thousand Tonnes CO2e)",
     pch = 19, col = "darkblue")

# ==============================================================================
# QUESTION 4.2 & 4.3: K-MEANS CLUSTERING (K = 2)
# ==============================================================================
# Select the 5 operational variables for clustering
cluster_vars <- facility_data[, c("Emissions_2015", "Emissions_2020", 
                                  "Emissions_2025", "Energy_Consumption", 
                                  "Production_Index")]

# Standardize the data so different units don't skew the results
scaled_data <- scale(cluster_vars)

# a. Perform K-means clustering with K = 2
set.seed(123) # Ensures your results match perfectly every time you run it
kmeans_k2 <- kmeans(scaled_data, centers = 2, nstart = 20)

# b. Add the cluster assignment back to your original dataset
facility_data$Cluster_K2 <- as.factor(kmeans_k2$cluster)

# c. Print the number of facilities assigned to each cluster
cat("\n--- Number of facilities per cluster (K=2) ---\n")
print(table(facility_data$Cluster_K2))

# QUESTION 4.3: Plot the clusters for K = 2
plot(facility_data$Energy_Consumption, facility_data$Emissions_2025,
     col = facility_data$Cluster_K2, pch = 19,
     main = "K-Means Clustering Layout (K = 2)",
     xlab = "Energy Consumption (GWh)",
     ylab = "GHG Emissions 2025")

# Add the cluster centers (stars) to the plot
# First, unscale the centers to plot them correctly on our graph
unscaled_centers_k2 <- aggregate(cluster_vars, by=list(kmeans_k2$cluster), FUN=mean)
points(unscaled_centers_k2$Energy_Consumption, unscaled_centers_k2$Emissions_2025, 
       col = 1:2, pch = 8, cex = 2, lwd = 2)

# ==============================================================================
# QUESTION 4.4: K-MEANS CLUSTERING (K = 3)
# ==============================================================================
set.seed(123)
kmeans_k3 <- kmeans(scaled_data, centers = 3, nstart = 20)
facility_data$Cluster_K3 <- as.factor(kmeans_k3$cluster)

# Plot the clusters for K = 3
plot(facility_data$Energy_Consumption, facility_data$Emissions_2025,
     col = facility_data$Cluster_K3, pch = 19,
     main = "K-Means Clustering Layout (K = 3)",
     xlab = "Energy Consumption (GWh)",
     ylab = "GHG Emissions 2025")

unscaled_centers_k3 <- aggregate(cluster_vars, by=list(kmeans_k3$cluster), FUN=mean)
points(unscaled_centers_k3$Energy_Consumption, unscaled_centers_k3$Emissions_2025, 
       col = 1:3, pch = 8, cex = 2, lwd = 2)

# ==============================================================================
# QUESTION 4.5: ELBOW METHOD
# ==============================================================================
# Create an empty vector to store the Within-Cluster Sum of Squares (WSS)
wss <- numeric(10)

# Calculate WSS for K = 1 through K = 10
for (k in 1:10) {
  set.seed(123)
  kmeans_model <- kmeans(scaled_data, centers = k, nstart = 20)
  wss[k] <- kmeans_model$tot.withinss
}

# Print the WSS values
cat("\n--- Within-Cluster Sum of Squares (WSS) for K=1 to K=10 ---\n")
for(i in 1:10) { cat("K =", i, "-> WSS =", wss[i], "\n") }

# Plot the Elbow diagram
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     main = "Elbow Method for Optimal K",
     xlab = "Number of Clusters (K)",
     ylab = "Total Within-Cluster Sum of Squares (WSS)")