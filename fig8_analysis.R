# Load necessary libraries
library(vegan)
library(forcats)
library(ggplot2)
library(car)
library(writexl)
library(dplyr)

# Read the OTU table CSV file
otu_table <- read.csv("~/Desktop/New_second_paper/Fig.8/NMDS_83_by_cluster_new_ZP.csv", header = TRUE)

# Remove the "Zacco platypus" column
otu_table <- otu_table[, !names(otu_table) %in% "Zacco platypus"]

# Create a subset of the OTU table containing only species columns
species_data <- otu_table[, -(1:11)]

# Convert percentages to presence/absence
presence_absence <- ifelse(species_data > 0, 1, 0)

# Create a new data frame with presence/absence values
presence_absence_df <- data.frame(
  Station = otu_table$Station,
  Method = otu_table$Method,
  Cluster = otu_table$Cluster,
  presence_absence
)

# Remove rows with no variation (all zeros)
presence_absence_df_filtered <- presence_absence_df[rowSums(presence_absence_df[, -(1:3)]) > 0, ]

# Calculate Jaccard dissimilarity matrix for filtered presence/absence data
jaccard_dist <- vegdist(presence_absence_df_filtered[, -(1:3)], method = "jaccard")

# Perform NMDS using Jaccard distance
nmds_result <- metaMDS(jaccard_dist)

# Extract coordinates from NMDS result
nmds_data <- data.frame(nmds_result$points)

# Combine the NMDS coordinates with the metadata
nmds_data <- cbind(presence_absence_df_filtered[, c("Method", "Cluster")], nmds_data)

# Stress value
stress <- nmds_result$stress

# Convert Method to factor with desired order
nmds_data$Method <- factor(nmds_data$Method, levels = c("Intake", "Niskin", "Bucket"))

# Convert Cluster to factor
nmds_data$Cluster <- as.factor(nmds_data$Cluster)

# Define updated colors for methods and clusters
method_colors <- c("Intake" = "violetred1", "Niskin" = "darkgreen", "Bucket" = "dodgerblue")
cluster_colors <- c('1' = 'red', '2' = 'blue', '3' = 'green', '4' = 'orange', 
                    '5' = 'purple', '6' = 'brown', '7' = 'pink', '8' = 'cyan')

# Create NMDS plot
nmds_plot <- ggplot(nmds_data, aes(MDS1, MDS2)) +
  geom_point(aes(color = Method, shape = Cluster), size = 3) +  
  scale_color_manual(name = "Method", values = method_colors) +
  scale_shape_manual(name = "Cluster", values = 1:8) +  
  labs(x = "NMDS1", y = "NMDS2", 
       title = paste("NMDS Plot (Stress =", round(stress, 3), ")")) +  
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "right", 
        legend.box = "vertical") +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray", linewidth = 1) +
  stat_ellipse(aes(group = Cluster, color = Cluster), geom = "path", level = 0.95, alpha = 0.8, show.legend = FALSE) +
  scale_color_manual(values = c(cluster_colors, method_colors), breaks = names(method_colors)) +
  guides(color = guide_legend(order = 1, override.aes = list(shape = 15)),  
         shape = guide_legend(order = 2))

# Separate continuous and categorical variables
continuous_vars <- otu_table[, c("Chl_a", "Temperature", "Salinity", "Max_Depth", "MLD")]
categorical_vars <- otu_table[, c("Time", "Season")]

# Perform envfit to fit continuous variables as vectors
envfit_result_cont <- envfit(nmds_result, continuous_vars, permutations = 999)

# Perform envfit to fit categorical variables as centroids
envfit_result_cat <- envfit(nmds_result, categorical_vars, permutations = 999)

# Extract envfit vectors and print them to inspect
vectors <- as.data.frame(scores(envfit_result_cont, display = "vectors"))
vectors$variable <- rownames(vectors)

# Filter vectors for Temperature, Salinity, and Chl_a
selected_vars <- c("Temperature", "Salinity", "Chl_a")
vectors_filtered <- vectors[vectors$variable %in% selected_vars, ]

# Extract envfit centroids
centroids <- as.data.frame(scores(envfit_result_cat, display = "factors"))
centroids$variable <- rownames(centroids)

# Filter centroids for Season categories
centroids_filtered <- centroids[centroids$variable %in% c("SeasonFall", "SeasonSpring", "SeasonSummer", "SeasonWinter"), ]

# Rename the Season categories for clarity
centroids_filtered$variable <- dplyr::recode(centroids_filtered$variable, 
                                             "SeasonFall" = "Fall", 
                                             "SeasonSpring" = "Spring", 
                                             "SeasonSummer" = "Summer", 
                                             "SeasonWinter" = "Winter")

# Skip centroids with NA values to avoid warnings
if (!any(is.na(centroids_filtered$NMDS1) | is.na(centroids_filtered$NMDS2))) {
  
  nmds_plot <- nmds_plot + 
    geom_point(data = centroids_filtered, aes(x = NMDS1, y = NMDS2), color = "darkorange", size = 3) +
    
    # Apply nudge only for Temperature (move it up and right)
    geom_text(data = centroids_filtered[centroids_filtered$variable == "Temperature", ], 
              aes(x = NMDS1, y = NMDS2, label = variable),
              vjust = -1.5, color = "darkblue", size = 5, fontface = "bold", 
              nudge_x = 0.1,  # Move Temperature right
              nudge_y = 0.1) +  # Move Temperature up
    
    # Apply nudge for Winter (move it down)
    geom_text(data = centroids_filtered[centroids_filtered$variable == "Winter", ], 
              aes(x = NMDS1, y = NMDS2, label = variable),
              vjust = -1.5, color = "darkblue", size = 5, fontface = "bold", 
              nudge_y = -0.22) +  # Move Winter lower
    
    # Apply nudge for Fall, Spring, and Summer (move them down)
    geom_text(data = centroids_filtered[centroids_filtered$variable %in% c("Fall", "Spring", "Summer"), ], 
              aes(x = NMDS1, y = NMDS2, label = variable),
              vjust = -1.5, color = "darkblue", size = 5, fontface = "bold", 
              nudge_y = -0.22) +  # Move Fall, Spring, and Summer lower
    
    # Apply default positioning for all other labels
    geom_text(data = centroids_filtered[!centroids_filtered$variable %in% c("Temperature", "Winter", "Fall", "Spring", "Summer"), ], 
              aes(x = NMDS1, y = NMDS2, label = variable),
              vjust = -1.5, color = "darkblue", size = 5, fontface = "bold") +
    theme_minimal()
}

# Add environmental vectors to the NMDS plot
nmds_plot <- nmds_plot + 
  geom_segment(data = vectors_filtered, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.3, "cm")), color = "black") +
  geom_text(data = vectors_filtered, aes(x = NMDS1, y = NMDS2, label = variable),
            vjust = 1.5, color = "black", size = 5, fontface = "bold") +  # Increased size
  theme_minimal()

# Save the NMDS plot
ggsave(
  filename = "~/Desktop/New_second_paper/Fig.8/Fig 8.png",
  plot = nmds_plot,
  width = 7,
  height = 7,
  dpi = 600,
  bg = "white"
)

# Add Stress Value to Results
stress_value <- data.frame(Variable = "Stress", P.value = NA, R2 = stress)

# Combine results for saving
p_values_cont <- envfit_result_cont$vectors$pvals
r2_values_cont <- envfit_result_cont$vectors$r
p_values_cat <- envfit_result_cat$factors$pvals
r2_values_cat <- envfit_result_cat$factors$r

cont_results <- data.frame(Variable = names(p_values_cont), P.value = p_values_cont, R2 = r2_values_cont)
cat_results <- data.frame(Variable = names(p_values_cat), P.value = p_values_cat, R2 = r2_values_cat)

results_list <- list(
  "Continuous Variables" = cont_results,
  "Categorical Variables" = cat_results,
  "Stress Value" = stress_value
)

# Save all results to an Excel file
write_xlsx(results_list, "~/Desktop/New_second_paper/Fig.8/statistical_results.xlsx")
