# Load required libraries
library(indicspecies)
library(writexl)
library(dplyr)
library(tidyr)

# Load the data from CSV
file_path <- "~/Desktop/New_second_paper/Table_2/heatmap_newest.csv"
data <- read.csv(file_path, check.names = FALSE)

# Convert species data to presence/absence (1 for present, 0 for absent)
species_data <- as.data.frame(ifelse(data[, -1] > 0, 1, 0))

# Extract the cluster information
clusters <- data$Cluster

# Exclude '5_I_N' from the cluster data
valid_data <- data[data$Cluster != "5_I_N", ]
species_data_valid <- species_data[data$Cluster != "5_I_N", ]
clusters_valid <- valid_data$Cluster

# Step 1: Aggregate the data by cluster to get species frequency
agg_data <- valid_data %>%
  group_by(Cluster) %>%
  summarise(across(everything(), sum))

# Step 2: Find the top 10 species in each cluster by frequency
top_species_per_cluster <- agg_data %>%
  pivot_longer(cols = -Cluster, names_to = "Species", values_to = "Frequency") %>%
  group_by(Cluster) %>%
  arrange(desc(Frequency)) %>%
  slice(1:10)  # Get the top 10 species per cluster

# Step 3: Run indicator species analysis for the top 10 species in each cluster
top_species_data <- species_data_valid[, unique(top_species_per_cluster$Species)]

# Perform indicator species analysis
result <- multipatt(top_species_data, clusters_valid, func = "IndVal.g", duleg = FALSE)

# Extract the summary of the indicator species analysis
result_summary <- result$sign

# Add rownames (species names) to the result dataframe
result_summary$Indicator_Species <- rownames(result_summary)

# Convert the p-values to numeric
result_summary$P_value <- as.numeric(result_summary$p.value)

# Create a cluster column based on the maximum value in the s. columns
result_summary$Cluster <- apply(result_summary[, grep("^s\\.", colnames(result_summary))], 1, function(row) {
  colnames(result_summary)[grep("^s\\.", colnames(result_summary))][which.max(row)]
})

# Step 4: Find top indicator species in each cluster (based on p-value and R-value)
top_indicator_species <- result_summary %>%
  filter(P_value < 0.05) %>%  # Select significant species
  arrange(P_value, desc(stat)) %>%  # Sort by p-value and R-value
  group_by(Cluster)  %>%
  slice(1:3)
# Select top 3 per cluster

# Step 5: Identify unique or fallback species among the top 10 species in each cluster
unique_species_results <- list()

for(cluster in unique(agg_data$Cluster)) {
  
  # Species present in the current cluster among the top 10 detected species
  species_in_cluster <- top_species_per_cluster %>%
    filter(Cluster == cluster) %>%
    pull(Species)
  
  # Check if these species are found in other clusters
  species_in_other_clusters <- top_species_per_cluster %>%
    filter(Cluster != cluster & Species %in% species_in_cluster) %>%
    pull(Species)
  
  # Unique species (those not found in other clusters)
  unique_species <- setdiff(species_in_cluster, species_in_other_clusters)
  
  if(length(unique_species) > 0) {
    # If unique species are found, use them
    unique_species_results[[cluster]] <- unique_species
  } else {
    # If no unique species, select species found in only one other cluster
    fallback_species <- top_species_per_cluster %>%
      filter(Species %in% species_in_cluster) %>%
      group_by(Species) %>%
      summarise(Cluster_Count = n()) %>%
      filter(Cluster_Count == 2) %>%
      pull(Species)
    
    unique_species_results[[cluster]] <- fallback_species
  }
}

# Step 6: Run indicator species analysis on the unique/fallback species
species_data_unique <- species_data_valid[, unlist(unique_species_results)]

# Perform the indicator species analysis for unique/fallback species
result_unique <- multipatt(species_data_unique, clusters_valid, func = "IndVal.g", duleg = FALSE)

# Extract the summary of the unique/fallback species indicator analysis
result_unique_summary <- result_unique$sign

# Add rownames (species names) to the unique species result dataframe
result_unique_summary$Indicator_Species <- rownames(result_unique_summary)

# Convert the p-values to numeric
result_unique_summary$P_value <- as.numeric(result_unique_summary$p.value)

# Create a cluster column for the unique species
result_unique_summary$Cluster <- apply(result_unique_summary[, grep("^s\\.", colnames(result_unique_summary))], 1, function(row) {
  colnames(result_unique_summary)[grep("^s\\.", colnames(result_unique_summary))][which.max(row)]
})

# Step 7: Save the final results to an Excel file
output_file <- "~/Desktop/New_second_paper/Table_2/top_and_unique_species_indicator_analysis_all_new_top_3.xlsx"
write_xlsx(list(
  "Top Indicator Species Analysis" = top_indicator_species,
  "Unique Species Indicator Analysis" = result_unique_summary
), output_file)

# Aggregated species frequency counts for Cluster 5
agg_cluster_5 <- agg_data %>% filter(Cluster == "s.5")
print(agg_cluster_5)  # Check the species and their frequencies in Cluster 5
