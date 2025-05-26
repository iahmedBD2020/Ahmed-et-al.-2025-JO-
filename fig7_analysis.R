# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(openxlsx)

# Load the data from CSV (preserving column names with spaces)
file_path <- "~/Desktop/New_second_paper/Fig.7/heatmap_newest.csv"
data <- read.csv(file_path, check.names = FALSE)
print(colnames(data)[1:10])  # Confirm column names

# Exclude 'Zacco platypus' column if it exists
data <- data %>% select(-contains("Zacco platypus"))

# Exclude '5_I_N' from the cluster data
valid_data <- data[data$Cluster != "5_I_N", ]

# Transform to presence/absence (1/0)
valid_data[, -1] <- as.data.frame(lapply(valid_data[, -1], function(x) ifelse(x > 0, 1, 0)))

# Aggregate data by clusters
agg_data <- valid_data %>%
  group_by(Cluster) %>%
  summarise(across(everything(), sum))

# Shorten species names to a maximum of 35 characters
colnames(agg_data)[-1] <- sapply(colnames(agg_data)[-1], function(x) {
  if(nchar(x) > 35) substring(x, 1, 35) else x
})

# Get the top 10 species for each cluster
top_species_per_cluster <- agg_data %>%
  gather(key = "Species", value = "Frequency", -Cluster) %>%
  group_by(Cluster) %>%
  top_n(10, Frequency) %>%
  pull(Species) %>%
  unique()

# Create the "Others" column only for species not in top 10
agg_data <- agg_data %>%
  mutate(Others = rowSums(select(., -c(Cluster, all_of(top_species_per_cluster))) > 0))

# Select only the top species and the "Others" column
agg_data <- agg_data %>%
  select(Cluster, all_of(top_species_per_cluster), Others)

# Ensure "Others" is last in species order
species_order <- c(setdiff(top_species_per_cluster, "Others"), "Others")

# Melt data for heatmap
melted_data <- melt(agg_data, id.vars = "Cluster")

# Apply same truncation and factor ordering
melted_data$variable <- factor(substr(melted_data$variable, 1, 35),
                               levels = substr(species_order, 1, 35))

# Define breaks and labels
breaks <- c(0, 1, 10, 25, 40, 60, 100, 200)
labels <- c("0", "1-10", "11-25", "26-40", "41-60", "61-100", "101-200")

# Bin the frequency values
melted_data$value_bin <- cut(melted_data$value, breaks = breaks, labels = labels, right = FALSE)

# Define color palette
colors <- rev(brewer.pal(length(labels), "RdYlBu"))

# Plot the heatmap
heatmap_plot <- ggplot(melted_data, aes(x = variable, y = as.factor(Cluster), fill = value_bin)) +
  geom_tile() +
  scale_fill_manual(
    values = setNames(colors, labels),
    name = "Frequency",
    labels = labels,
    limits = labels,
    drop = FALSE
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 90, hjust = 1, face = "italic", vjust = 0.5),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold")
  ) +
  labs(x = "Species", y = "Cluster")

# Print the heatmap
print(heatmap_plot)

# Save the plot
ggsave("~/Desktop/New_second_paper/Fig.7/Fig.7.png", plot = heatmap_plot, width = 12, height = 8, dpi = 600)

# Identify unique/nearly unique and dominant species
results <- list()
dominant_species_results <- list()

for(cluster in unique(agg_data$Cluster)) {
  
  species_in_cluster <- agg_data %>%
    filter(Cluster == cluster) %>%
    select(-Cluster, -Others) %>%
    pivot_longer(cols = everything(), names_to = "Species", values_to = "Frequency") %>%
    filter(Frequency > 0) %>%
    arrange(desc(Frequency)) %>%
    pull(Species)
  
  unique_species_per_cluster <- agg_data %>%
    pivot_longer(cols = -c(Cluster, Others), names_to = "Species", values_to = "Frequency") %>%
    group_by(Species) %>%
    summarise(Cluster_Count = sum(Frequency > 0)) %>%
    filter(Cluster_Count <= 2) %>%
    pull(Species)
  
  cluster_unique_species <- intersect(species_in_cluster, unique_species_per_cluster)
  
  dominant_species <- agg_data %>%
    filter(Cluster == cluster) %>%
    select(-Cluster, -Others) %>%
    pivot_longer(cols = everything(), names_to = "Species", values_to = "Frequency") %>%
    arrange(desc(Frequency)) %>%
    slice(1:3) %>%
    pull(Species)
  
  if(length(cluster_unique_species) > 0) {
    results[[cluster]] <- data.frame(
      Cluster = cluster,
      Type = "Unique/Nearly Unique",
      Species = paste(cluster_unique_species, collapse = ", ")
    )
  } else {
    results[[cluster]] <- data.frame(
      Cluster = cluster,
      Type = "Dominant",
      Species = paste(dominant_species, collapse = ", ")
    )
  }
  
  dominant_species_results[[cluster]] <- data.frame(
    Cluster = cluster,
    Dominant_Species = paste(dominant_species, collapse = ", ")
  )
}

# Combine results
final_results <- do.call(rbind, results)
dominant_species_df <- do.call(rbind, dominant_species_results)

# Truncate long species names in output
final_results$Species <- sapply(strsplit(final_results$Species, ", "), function(x) {
  paste(substr(x, 1, 35), collapse = ", ")
})
dominant_species_df$Dominant_Species <- sapply(strsplit(dominant_species_df$Dominant_Species, ", "), function(x) {
  paste(substr(x, 1, 35), collapse = ", ")
})

# Save Excel with 2 sheets
output_path <- "~/Desktop/New_second_paper/Fig.7/cluster_analysis_results_top10.xlsx"
write.xlsx(list(
  "Unique or Nearly Unique Species" = final_results,
  "Top 3 Dominant Species" = dominant_species_df
), output_path, rowNames = FALSE)

cat("Results have been saved to:", output_path, "\n")
