# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggsignif)
library(gridExtra)
library(vegan)  # Assuming vegan is used for diversity calculation
library(writexl)  # For writing to Excel files
library(cowplot)  # For combining plots with a shared legend

# Load your data
data <- read.csv("~/Desktop/My Desktop/Latest Academic/Surface+Tap/New analysis/All_data_KH-23-2_included/Cluster_biodiversity/Final_without_ZA/Cluster_bio_ZA.csv")

# Prepare the data: Exclude 'Zacco.platypus' column and convert species columns to abundance data
species_data <- data %>%
  select(-Method, -Cluster, -Zacco.platypus)

# Initialize a list to store results for each method
index_results_list <- list()
wilcox_results <- data.frame()  # To store Wilcoxon test results

# Define the methods
methods <- unique(data$Method)

# Loop over each method
for(method in methods) {
  
  # Initialize a data frame to store results for this method
  index_results_df <- data.frame()
  
  # Loop over each cluster
  for(cluster in unique(data$Cluster)) {
    # Subset the data for the method within the cluster
    subset_data <- data %>%
      filter(Cluster == cluster, Method == method) %>%
      select(-Method, -Cluster, -Zacco.platypus)  # Exclude 'Zacco.platypus' here
    
    # Check if data is available
    if (nrow(subset_data) > 0) {
      # Calculate the Shannon and Simpson indices
      shannon <- diversity(subset_data, index = "shannon")
      simpson <- diversity(subset_data, index = "simpson")
      
      # Store the results in a data frame
      temp_df <- data.frame(
        Cluster = paste("Cluster", cluster),
        Method = method,
        Shannon = shannon,
        Simpson = simpson
      )
      
      index_results_df <- rbind(index_results_df, temp_df)
    }
  }
  
  # Store the results in the list
  index_results_list[[method]] <- index_results_df
}

# Combine all methods' data into a single data frame for plotting
combined_index_data <- bind_rows(index_results_list, .id = "Method")

# Function to perform Wilcoxon test and store results
perform_wilcox_test <- function(df, index_name) {
  comparisons <- combn(unique(df$Method), 2, simplify = FALSE)  # Generate all pairwise comparisons
  for (comparison in comparisons) {
    group1 <- df %>% filter(Method == comparison[1]) %>% pull(index_name)
    group2 <- df %>% filter(Method == comparison[2]) %>% pull(index_name)
    
    if (length(group1) > 1 && length(group2) > 1) {
      test_result <- wilcox.test(group1, group2, exact = FALSE)
      wilcox_results <<- rbind(wilcox_results, data.frame(
        Cluster = unique(df$Cluster),
        Index = index_name,
        Method1 = comparison[1],
        Method2 = comparison[2],
        P_Value = test_result$p.value
      ))
    }
  }
}

# Loop through each cluster and perform Wilcoxon tests for Shannon and Simpson indices
for (cluster in unique(combined_index_data$Cluster)) {
  cluster_data <- combined_index_data %>% filter(Cluster == cluster)
  perform_wilcox_test(cluster_data, "Shannon")
  perform_wilcox_test(cluster_data, "Simpson")
}

# Save the Wilcoxon test results to an Excel file
write_xlsx(wilcox_results, "~/Desktop/My Desktop/Latest Academic/Surface+Tap/New analysis/All_data_KH-23-2_included/Cluster_biodiversity/Final_without_ZA/wilcox_results_all_ZA.xlsx")

# Define updated colors for each method
color_mapping <- c("Bucket" = "dodgerblue", "Intake" = "violetred1", "Niskin" = "darkgreen")

# Function to plot diversity indices with Wilcoxon test comparison
plot_indices <- function(df, index_name) {
  comparisons <- combn(unique(df$Method), 2, simplify = FALSE)  # Generate all pairwise comparisons
  
  p <- ggplot(df, aes(x = Method, y = .data[[index_name]], fill = Method)) +
    geom_boxplot(outlier.shape = NA) +  # Avoid plotting outliers separately
    geom_jitter(position = position_jitter(width = 0.2, height = 0), alpha = 0.5) + 
    scale_fill_manual(values = color_mapping) +  # Updated colors
    labs(x = "", y = paste(index_name, "Value")) +  # Remove x-axis label
    theme_bw() +  # Use a white background theme
    theme(
      axis.title.x = element_blank(),  # Remove x-axis title
      axis.title.y = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      strip.text = element_text(size = 14, face = "bold"),  # Make facet labels (Cluster 1, Cluster 2, etc.) bold
      legend.position = "none"  # Hide legend in individual plots
    ) +
    geom_signif(comparisons = comparisons, 
                map_signif_level = TRUE, 
                test = "wilcox.test", 
                step_increase = 0.05,
                test.args = list(exact = FALSE)) +  # Use the exact = FALSE argument
    facet_wrap(~Cluster, scales = "free_y")
  
  return(p)
}

# Create the two plots for Shannon and Simpson indices
shannon_plot <- plot_indices(combined_index_data, "Shannon")
simpson_plot <- plot_indices(combined_index_data, "Simpson")

# Extract legend
legend <- get_legend(
  shannon_plot +
    guides(fill = guide_legend(ncol = 1)) +  # Vertically aligned legend
    theme(legend.position = "right") +  # Vertically aligned legend with bold title
    theme(
      legend.title = element_text(size = 16, face = "bold"),  # Larger and bold legend title
      legend.text = element_text(size = 14)  # Larger legend text
    )
)

# Combine the two plots into one figure with the shared legend on the right
combined_plot <- plot_grid(
  shannon_plot + labs(title = expression(bold("(a)"))),
  simpson_plot + labs(title = expression(bold("(b)"))),
  nrow = 1,
  align = 'h',
  rel_widths = c(1, 1, 0.3)
)

# Add the legend to the right
final_plot <- plot_grid(combined_plot, legend, ncol = 2, rel_widths = c(3, 0.4))

# Save and display the final combined plot with a shared legend
ggsave("~/Desktop/My Desktop/Latest Academic/Surface+Tap/New analysis/All_data_KH-23-2_included/Cluster_biodiversity/Final_without_ZA/Shannon_simpson_system_annotation_ZA.png", plot = final_plot, dpi = 600, bg = "white", width = 16, height = 12)
print(final_plot)
