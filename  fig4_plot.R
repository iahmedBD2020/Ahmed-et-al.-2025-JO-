library(openxlsx)
library(cluster)
library(dendextend)
library(vegan)

# Read the Excel file
data <- read.xlsx("~/Desktop/2nd_paper_codes/Fig.4 /raw_all_cluster_new.xlsx")

# Remove the "Zacco platypus" column
data <- data[, !names(data) %in% "Zacco platypus"]

# Extract station names and species data
station_names <- data$Station
species_data <- data[, -1]

# Convert species data to binary matrix if necessary
species_data <- as.data.frame(lapply(species_data, function(x) as.integer(x > 0)))

# Compute Jaccard distance matrix
jaccard_dist <- vegdist(species_data, method = "jaccard")

# Perform hierarchical clustering using Ward's method
hc <- hclust(jaccard_dist, method = "ward.D2")

# Convert hclust object to a dendrogram object for better visualization
dend <- as.dendrogram(hc)

# Reorder station names based on the dendrogram's leaf order
ordered_station_indices <- order.dendrogram(dend)
ordered_station_names <- station_names[ordered_station_indices]

# Define the color assignment function based on station name
assign_station_color <- function(station) {
  if (grepl("Intake_Station", station)) {
    return("violetred1")  # Color for Intake stations
  } else if (grepl("Niskin_Station", station)) {
    return("darkgreen")   # Color for Niskin stations
  } else if (grepl("Bucket_Station", station)) {
    return("dodgerblue")  # Color for Bucket stations
  } else {
    return("black")  # Default color for others (if any)
  }
}

# Apply the function to each station name to assign colors based on ordered station names
station_colors <- sapply(ordered_station_names, assign_station_color)

# Create a data frame for clusters (use the clustering result)
clusters <- cutree(hc, k = 8)  # Define clusters

# Now, map the color for each branch according to its method
dend_colored <- dend
labels(dend_colored) <- ordered_station_names  # Add station names back to labels for clarity

# Color only the terminal branches (stations) based on their color assignments
dend_colored <- color_branches(dend_colored, col = station_colors)
# Remove station labels from the dendrogram
labels(dend_colored) <- rep("", length(labels(dend_colored)))  # Set all labels to an empty string

# Assign colors to the rectangles around the clusters
rect_colors <- c("red", "blue", "green", "orange", "purple", "brown", "pink", "cyan")

# Save to PDF
pdf("~/Desktop/2nd_paper_codes/Fig.4 /Figure 4.pdf", height = 10, width = 10)

# Adjust margins: bottom, left, top, right
par(mar = c(5, 6, 4, 3))  # Increase left margin for y-axis and bottom margin for x-axis

# Plot the dendrogram with only the terminal branches colored, without coloring the connecting branches
plot(dend_colored, 
     xlab = "Stations", 
     ylab = "Height", 
     sub = "", 
     cex.lab = 1.5, 
     font.lab = 2, 
     cex.axis = 1.2, 
     labels = FALSE)

# Add rectangles around clusters
rect.dendrogram(dend_colored, k = 8, border = rect_colors[1:8], lwd = 3)

# Add custom legend to indicate cluster colors
legend(
  "topright", 
  legend = paste("Cluster", 1:8), 
  fill = rect_colors[1:8], 
  cex = 1.13  # Reduce font size slightly
)

# Close the PDF device to save the plot
dev.off()

# Save to PNG
png("~/Desktop/2nd_paper_codes/Fig.4 /Figure 4.png", height = 10, width = 10, units = "in", res = 600)

# Adjust margins: bottom, left, top, right
par(mar = c(5, 6, 4, 3))  # Increase left margin for y-axis and bottom margin for x-axis

# Plot the dendrogram with only the terminal branches colored, without coloring the connecting branches
plot(dend_colored, 
     xlab = "Stations", 
     ylab = "Height", 
     sub = "", 
     cex.lab = 1.5, 
     font.lab = 2, 
     cex.axis = 1.2, 
     labels = FALSE)

# Add rectangles around clusters
rect.dendrogram(dend_colored, k = 8, border = rect_colors[1:8], lwd = 3)

# Add custom legend to indicate cluster colors
legend(
  "topright", 
  legend = paste("Cluster", 1:8), 
  fill = rect_colors[1:8], 
  cex = 1.13  # Reduce font size slightly
)

# Close the PNG device to save the plot
dev.off()
