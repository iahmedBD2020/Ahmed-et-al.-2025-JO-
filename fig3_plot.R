# Load necessary libraries
library(iNEXT)
library(tidyverse)
library(patchwork)
library(ggplot2)
library(vegan)  # For PERMANOVA and Jaccard distance
library(car)  # For Levene's test
library(ggpubr)  # For visualization and assumption checks
library(writexl)  # For writing Excel files

# Load the data
data <- read.csv("~/Desktop/My Desktop/Latest Academic/Surface+Tap/New analysis/All_data_KH-23-2_included/rarefaction/rarefaction_83.csv")

# Define a sequence of sample sizes for iNEXT analysis
t <- seq(1, 700, by = 10)

# Perform iNEXT analysis for q = 0, q = 1, and q = 2
out.inc.q0 <- iNEXT(data, q = 0, datatype = "incidence_freq", size = t)
out.inc.q1 <- iNEXT(data, q = 1, datatype = "incidence_freq", size = t)
out.inc.q2 <- iNEXT(data, q = 2, datatype = "incidence_freq", size = t)

# Define line and fill color mappings for the sample types
line_color_mapping <- c('Bucket' = 'skyblue', 'Intake' = 'violetred1', 'Niskin' = 'lightgoldenrod')
fill_color_mapping <- c('Bucket' = 'skyblue', 'Intake' = 'violetred1', 'Niskin' = 'lightgoldenrod')

# Function to create plots with customized line and shading colors
create_plot_with_shading <- function(out.inc, type, title, y_label) {
  ggiNEXT(out.inc, type = type) +
    theme_bw(base_size = 18) +
    scale_color_manual(values = line_color_mapping) +  # Line colors
    scale_fill_manual(values = fill_color_mapping) +   # Shading (fill) colors
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          text = element_text(size = 18),
          legend.box = "vertical") +
    labs(title = title, y = y_label)
}

# Create sample-size-based R/E curves with different shading
p1_q0 <- create_plot_with_shading(out.inc.q0, type = 1, title = "q0", y_label = "Richness (q0)")
p1_q1 <- create_plot_with_shading(out.inc.q1, type = 1, title = "q1", y_label = "Shannon (q1)")
p1_q2 <- create_plot_with_shading(out.inc.q2, type = 1, title = "q2", y_label = "Inverse Simpson (q2)")

# Combine the plots into a single pane with labels A, B, and C
combined_plot <- (p1_q0 + p1_q1 + p1_q2) + plot_annotation(tag_levels = 'A')

# Print and save the combined plot
print(combined_plot)
ggsave("~/Desktop/My Desktop/Latest Academic/Surface+Tap/New analysis/All_data_KH-23-2_included/rarefaction/Newest/Rarefaction_horizontal.pdf", combined_plot, width = 14, height = 8, units = "in", dpi = 600)

# Save the combined plot as a PNG file with a white background at 600 dpi
ggsave("~/Desktop/My Desktop/Latest Academic/Surface+Tap/New analysis/All_data_KH-23-2_included/rarefaction/Newest/Rarefaction_horizontal.png", 
       combined_plot, width = 14, height = 8, units = "in", dpi = 600, bg = "white")

# Combine estimates from q0, q1, and q2 into one dataframe
combined_estimates <- bind_rows(
  out.inc.q0$iNextEst$size_based %>% mutate(Diversity_Index = "q0"),
  out.inc.q1$iNextEst$size_based %>% mutate(Diversity_Index = "q1"),
  out.inc.q2$iNextEst$size_based %>% mutate(Diversity_Index = "q2")
)

# Separate data filtering for each diversity index
data_q0 <- combined_estimates %>% filter(Diversity_Index == "q0")
data_q1 <- combined_estimates %>% filter(Diversity_Index == "q1")
data_q2 <- combined_estimates %>% filter(Diversity_Index == "q2")

# Calculate Jaccard distance matrices
dist_q0 <- vegdist(data_q0$qD, method = "jaccard")
dist_q1 <- vegdist(data_q1$qD, method = "jaccard")
dist_q2 <- vegdist(data_q2$qD, method = "jaccard")

# Perform PERMANOVA on each distance matrix
permanova_q0 <- adonis2(dist_q0 ~ Assemblage, data = data_q0)
print(permanova_q0)

permanova_q1 <- adonis2(dist_q1 ~ Assemblage, data = data_q1)
print(permanova_q1)

permanova_q2 <- adonis2(dist_q2 ~ Assemblage, data = data_q2)
print(permanova_q2)

# Save PERMANOVA results in an Excel file
write_xlsx(list(
  "PERMANOVA_q0" = as.data.frame(permanova_q0$aov.tab),
  "PERMANOVA_q1" = as.data.frame(permanova_q1$aov.tab),
  "PERMANOVA_q2" = as.data.frame(permanova_q2$aov.tab)
), "~/Desktop/My Desktop/Latest Academic/Surface+Tap/New analysis/All_data_KH-23-2_included/rarefaction/Newest/PERMANOVA_Results.xlsx")

# Load necessary libraries
library(vegan)
library(dplyr)

# Function to run adonis2 for a specific subset and print the results
run_adonis_and_print <- function(data_subset, dist_subset, group1, group2, diversity_index) {
  adonis_result <- tryCatch({
    adonis2(dist_subset ~ Assemblage, data = data_subset, permutations = 999)
  }, error = function(e) {
    print(paste("Error in adonis2 for", group1, "vs", group2, "in", diversity_index))
    NULL
  })
  
  # Print the results if adonis2 returned a valid result
  if (!is.null(adonis_result)) {
    print(paste("Results for", group1, "vs", group2, "in", diversity_index))
    print(adonis_result)
  }
}

# Run adonis2 for each diversity index and pair

# Diversity index q0
data_q0_subset_bucket_intake <- data_q0 %>% filter(Assemblage %in% c("Bucket", "Intake"))
dist_q0_subset_bucket_intake <- vegdist(data_q0_subset_bucket_intake$qD, method = "jaccard")
run_adonis_and_print(data_q0_subset_bucket_intake, dist_q0_subset_bucket_intake, "Bucket", "Intake", "q0")

data_q0_subset_bucket_niskin <- data_q0 %>% filter(Assemblage %in% c("Bucket", "Niskin"))
dist_q0_subset_bucket_niskin <- vegdist(data_q0_subset_bucket_niskin$qD, method = "jaccard")
run_adonis_and_print(data_q0_subset_bucket_niskin, dist_q0_subset_bucket_niskin, "Bucket", "Niskin", "q0")

data_q0_subset_intake_niskin <- data_q0 %>% filter(Assemblage %in% c("Intake", "Niskin"))
dist_q0_subset_intake_niskin <- vegdist(data_q0_subset_intake_niskin$qD, method = "jaccard")
run_adonis_and_print(data_q0_subset_intake_niskin, dist_q0_subset_intake_niskin, "Intake", "Niskin", "q0")

# Diversity index q1
data_q1_subset_bucket_intake <- data_q1 %>% filter(Assemblage %in% c("Bucket", "Intake"))
dist_q1_subset_bucket_intake <- vegdist(data_q1_subset_bucket_intake$qD, method = "jaccard")
run_adonis_and_print(data_q1_subset_bucket_intake, dist_q1_subset_bucket_intake, "Bucket", "Intake", "q1")

data_q1_subset_bucket_niskin <- data_q1 %>% filter(Assemblage %in% c("Bucket", "Niskin"))
dist_q1_subset_bucket_niskin <- vegdist(data_q1_subset_bucket_niskin$qD, method = "jaccard")
run_adonis_and_print(data_q1_subset_bucket_niskin, dist_q1_subset_bucket_niskin, "Bucket", "Niskin", "q1")

data_q1_subset_intake_niskin <- data_q1 %>% filter(Assemblage %in% c("Intake", "Niskin"))
dist_q1_subset_intake_niskin <- vegdist(data_q1_subset_intake_niskin$qD, method = "jaccard")
run_adonis_and_print(data_q1_subset_intake_niskin, dist_q1_subset_intake_niskin, "Intake", "Niskin", "q1")

# Diversity index q2
data_q2_subset_bucket_intake <- data_q2 %>% filter(Assemblage %in% c("Bucket", "Intake"))
dist_q2_subset_bucket_intake <- vegdist(data_q2_subset_bucket_intake$qD, method = "jaccard")
run_adonis_and_print(data_q2_subset_bucket_intake, dist_q2_subset_bucket_intake, "Bucket", "Intake", "q2")

data_q2_subset_bucket_niskin <- data_q2 %>% filter(Assemblage %in% c("Bucket", "Niskin"))
dist_q2_subset_bucket_niskin <- vegdist(data_q2_subset_bucket_niskin$qD, method = "jaccard")
run_adonis_and_print(data_q2_subset_bucket_niskin, dist_q2_subset_bucket_niskin, "Bucket", "Niskin", "q2")

data_q2_subset_intake_niskin <- data_q2 %>% filter(Assemblage %in% c("Intake", "Niskin"))
dist_q2_subset_intake_niskin <- vegdist(data_q2_subset_intake_niskin$qD, method = "jaccard")
run_adonis_and_print(data_q2_subset_intake_niskin, dist_q2_subset_intake_niskin, "Intake", "Niskin", "q2")

# Calculate error for each asymptotic estimate and format as "mean ± error"
all_asymptotic_estimates <- all_asymptotic_estimates %>%
  mutate(Error = (qD.UCL - qD.LCL) / 2,
         Asymptotic_Estimate = paste0(qD, " ± ", round(Error, 2)))

# Select relevant columns to display
formatted_asymptotic_estimates <- all_asymptotic_estimates %>%
  select(Assemblage, Diversity_Index, Asymptotic_Estimate)

# Print the formatted table
print(formatted_asymptotic_estimates)

# Optional: Save the formatted results to an Excel file
write_xlsx(formatted_asymptotic_estimates, "~/Desktop/My Desktop/Latest Academic/Surface+Tap/New analysis/All_data_KH-23-2_included/rarefaction/Newest/Asymptotic_Estimates_with_Error.xlsx")
