# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(cowplot) # For extracting the legend

# Load the data for species relative abundance
file_path <- "~/Desktop/My Desktop/Latest Academic/Surface+Tap/New analysis/All_data_KH-23-2_included/Relative_abundance/By_method/RA.xlsx"
data <- read_excel(file_path)

# Convert abundance data to presence/absence data
presence_absence_data <- data
presence_absence_data[,-1] <- ifelse(data[,-1] > 0, 1, 0)

# Calculate species frequencies
species_freq <- colSums(presence_absence_data[,-1])

# Select top 50 species
top_species <- names(sort(species_freq, decreasing = TRUE)[1:50])

# Convert data to long format and group other species as "Others"
data_long <- presence_absence_data %>%
  pivot_longer(cols = -Method, names_to = "Species", values_to = "Presence") %>%
  mutate(Species = ifelse(Species %in% top_species, Species, "Others")) %>%
  group_by(Method, Species) %>%
  summarise(Presence = sum(Presence), .groups = 'drop') %>%
  group_by(Method) %>%
  mutate(Presence = Presence / sum(Presence)) %>%
  ungroup()

# Create a color palette for species
color_palette <- colorRampPalette(brewer.pal(12, "Paired"))(464)
species_colors <- setNames(color_palette, colnames(data)[-1])
species_colors["Others"] <- "grey"

# Reorder Species to have "Others" at the end
data_long$Species <- factor(data_long$Species, levels = c(setdiff(unique(data_long$Species), "Others"), "Others"))

# Plot the relative frequency by method (Plot A) without legend
p1_plot <- ggplot(data_long, aes(x = Method, y = Presence, fill = Species)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = species_colors) +
  labs(x = "Method", y = "Relative Frequency") +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, face = "italic", size = 14, family = "Arial"),
    axis.text.y = element_text(size = 16, family = "Arial"),
    axis.title.x = element_text(size = 16, face = "bold", family = "Arial"),
    axis.title.y = element_text(size = 16, face = "bold", family = "Arial"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none" # Remove legend from the main plot
  ) +
  ggtitle(expression(bold("(a)")))

p1_legend <- cowplot::get_legend(
  ggplot(data_long, aes(x = Method, y = Presence, fill = Species)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_fill_manual(values = species_colors) +
    guides(fill = guide_legend(ncol = 1)) +
    theme(
      legend.text = element_text(face = "italic", size = 12, family = "Arial"),
      legend.title = element_text(size = 14, face = "bold", family = "Arial"),
      legend.key.width = unit(0.7, "lines"),  # Narrower legend key width
      legend.key.height = unit(0.7, "lines"),
      legend.position = "right"
    )
)


# Combine the plot and legend side by side with adjusted width
p1_combined <- grid.arrange(
  p1_plot, 
  p1_legend, 
  ncol = 2, 
  widths = c(0.5, 0.5)
)

# Function to process family data and calculate relative composition
process_family_data <- function(file_path, method_name) {
  data <- read_excel(file_path)
  family_counts <- data %>%
    count(Family, sort = TRUE)
  top_families <- family_counts %>%
    top_n(10, n)
  data_cleaned <- data %>%
    mutate(Family_grouped = ifelse(Family %in% top_families$Family, Family, "Others"))
  family_summary <- data_cleaned %>%
    count(Family_grouped) %>%
    mutate(Relative_Composition = n / sum(n) * 100,
           Method = method_name)
  return(family_summary)
}

# Process the family data for all methods
bucket_family_data <- process_family_data("~/Desktop/My Desktop/Latest Academic/Surface+Tap/New analysis/All_data_KH-23-2_included/Family/Dominant_Families/Bucket_families.xlsx", "Bucket")
intake_family_data <- process_family_data("~/Desktop/My Desktop/Latest Academic/Surface+Tap/New analysis/All_data_KH-23-2_included/Family/Dominant_Families/Intake_families.xlsx", "Intake")
niskin_family_data <- process_family_data("~/Desktop/My Desktop/Latest Academic/Surface+Tap/New analysis/All_data_KH-23-2_included/Family/Dominant_Families/Niskin_families.xlsx", "Niskin")

# Combine the data for all methods
combined_family_data <- bind_rows(bucket_family_data, intake_family_data, niskin_family_data)

# Reorder the Family_grouped factor to show "Others" at the bottom
combined_family_data$Family_grouped <- factor(combined_family_data$Family_grouped, 
                                              levels = c(setdiff(unique(combined_family_data$Family_grouped), "Others"), "Others"))

# Create a more vivid color palette for families
family_colors <- colorRampPalette(brewer.pal(8, "Dark2"))(length(unique(combined_family_data$Family_grouped)))

# Create the stacked bar plot (Plot B) with slightly wider bars
p2 <- ggplot(combined_family_data, aes(x = Method, y = Relative_Composition, fill = Family_grouped, width = 0.8)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = family_colors) +
  labs(x = "Method", y = "Relative Composition (%)", fill = "Family") +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "italic", size = 14, family = "Arial"),
    axis.text.y = element_text(size = 14, family = "Arial"),
    axis.title.x = element_text(size = 16, face = "bold", family = "Arial"),
    axis.title.y = element_text(size = 16, face = "bold", family = "Arial"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(face = "italic", size = 12, family = "Arial"),
    legend.title = element_text(size = 14, face = "bold", family = "Arial"),
    plot.title = element_text(size = 26, face = "bold", family = "Arial")
  ) +
  ggtitle(expression(bold("(b)")))

# Arrange plots vertically with 70% for p1_combined and 30% for p2
fig2 <- grid.arrange(
  p1_combined, 
  p2, 
  ncol = 1,
  heights = c(0.7, 0.3)
)

# Save the combined plot as a TIFF file with A4 size dimensions
ggsave("~/Desktop/My Desktop/Latest Academic/Surface+Tap/New analysis/All_data_KH-23-2_included/Relative_abundance/By_method/Fig2_A4.tiff",
       plot = fig2, width = 210, height = 297, dpi = 600, units = "mm", device = "tiff")

# Optional: Save as PNG as well
ggsave("~/Desktop/My Desktop/Latest Academic/Surface+Tap/New analysis/All_data_KH-23-2_included/Relative_abundance/By_method/Fig2_A4.png",
       plot = fig2, width = 210, height = 297, dpi = 600, units = "mm")

# Create a data frame with family names and their assigned colors
family_color_mapping <- data.frame(
  Family = unique(combined_family_data$Family_grouped),
  Color = family_colors
)

# Print the family-color mapping
print(family_color_mapping)

