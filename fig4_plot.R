# Load required libraries
library(openxlsx)
library(cluster)
library(dendextend)
library(vegan)

# Read the Excel file
data <- read.xlsx("~/Desktop/New_second_paper/Fig.4/raw_all_cluster_new.xlsx")

# Remove "Zacco platypus" column
data <- data[, !names(data) %in% "Zacco platypus"]

# Extract station names and species data
station_names <- data$Station
species_data <- data[, -1]

# Convert species data to binary matrix
species_data <- as.data.frame(lapply(species_data, function(x) as.integer(x > 0)))

# Compute Jaccard distance matrix
jaccard_dist <- vegdist(species_data, method = "jaccard")

# Perform hierarchical clustering using Ward's method
hc <- hclust(jaccard_dist, method = "ward.D2")

# Convert to dendrogram
dend <- as.dendrogram(hc)

# Define number of clusters
k <- 8

# -----------------------------
# Preserve old cluster numbering and order
# -----------------------------
# Cut the dendrogram to obtain k clusters
clusters <- cutree(hc, k = k)

# Get dendrogram leaf order (left to right)
dend_order <- order.dendrogram(dend)

# Reorder clusters based on leaf order
clusters_reordered <- clusters[dend_order]

# Reassign cluster numbers based on their left-to-right order
new_clusters <- as.numeric(factor(clusters_reordered, levels = unique(clusters_reordered)))

# Map new cluster numbers back to original station order
new_clusters_mapped <- new_clusters[order(dend_order)]

# -----------------------------
# Coloring by method
# -----------------------------
# Reorder station names to match dendrogram leaf order
ordered_station_names <- station_names[dend_order]

# Assign method colors
assign_station_color <- function(station) {
  if (grepl("Intake_Station", station)) return("violetred1")
  else if (grepl("Niskin_Station", station)) return("darkgreen")
  else if (grepl("Bucket_Station", station)) return("dodgerblue")
  else return("black")
}
station_colors <- sapply(ordered_station_names, assign_station_color)

# Apply colored branches (by method)
dend_colored <- dend
labels(dend_colored) <- ordered_station_names
dend_colored <- color_branches(dend_colored, col = station_colors)
labels(dend_colored) <- rep("", length(labels(dend_colored)))  # Hide labels

# Cluster rectangle colors
rect_colors <- c("red", "blue", "green", "orange", "purple", "brown", "pink", "cyan")

# -----------------------------
# Create data frame in original station order
cluster_assignment <- data.frame(Station = station_names, Cluster = new_clusters_mapped)

# Save to Excel in original order
write.xlsx(cluster_assignment, "~/Desktop/New_second_paper/Fig.4/cluster_assignment_original_order.xlsx", rowNames = FALSE)



# -----------------------------
# Plot to PDF
# -----------------------------
pdf("~/Desktop/New_second_paper/Fig.4/Fig.4.pdf", height = 12, width = 12.5)
par(mar = c(5, 6, 2, 2))
plot(dend_colored,
     xlab = "Stations",
     ylab = "Height",
     sub = "",
     cex.lab = 2,
     font.lab = 2,
     cex.axis = 1.2,
     labels = FALSE)
rect.dendrogram(dend_colored, k = k, border = rect_colors[1:k], lwd = 3)
legend("topright",
       legend = paste("Cluster", 1:k),
       fill = rect_colors[1:k],
       cex = 1.5,
       text.font = 2,
       bty = "n")
legend("top", 
       inset = -0.02,
       legend = c("Bucket", "Intake", "Niskin"),
       fill = c("dodgerblue", "violetred1", "darkgreen"),
       horiz = TRUE,
       cex = 1.5,
       text.font = 2,
       bty = "n")
dev.off()

# -----------------------------
# Plot to PNG
# -----------------------------
png("~/Desktop/New_second_paper/Fig.4/Fig.4.png", height = 12, width = 13, units = "in", res = 600)
par(mar = c(5, 6, 2, 2))
plot(dend_colored,
     xlab = "Stations",
     ylab = "Height",
     sub = "",
     cex.lab = 2,
     font.lab = 2,
     cex.axis = 1.2,
     labels = FALSE)
rect.dendrogram(dend_colored, k = k, border = rect_colors[1:k], lwd = 3)
legend("topright",
       legend = paste("Cluster", 1:k),
       fill = rect_colors[1:k],
       cex = 1.6,
       text.font = 2,
       bty = "n")
legend("top", 
       inset = -0.02,
       legend = c("Bucket", "Intake", "Niskin"),
       fill = c("dodgerblue", "violetred1", "darkgreen"),
       horiz = TRUE,
       cex = 1.6,
       text.font = 2,
       bty = "n")
dev.off()

# -----------------------------
# Confirmation
# -----------------------------
cat("Final figure with original cluster order and new styling saved successfully.\n")
