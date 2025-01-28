### This is the document for caculate Speicies Richenss Index

library(dplyr)

Richness_data_orignal <- readRDS("/Users/muqianqianli/Library/CloudStorage/GoogleDrive-limqq@umich.edu/My Drive/RA-Winter-2025/data/processed_iNaturalist_csymd-copy.rds")

# collapse the data to grid-by-year level, counting distinct species

collapsed_data <- Richness_data_orignal %>%
  mutate(year = format(time, "%Y")) %>%  # Extract the year from the 'time' column
  group_by(cell_id, year) %>%
  summarise(
    Richness = n_distinct(species),       # Count distinct species (Richness)
    visits_per_year = sum(visits, na.rm = TRUE)  # Sum of visits per year
  ) %>%
  ungroup()
# here Richness is our grid_year richness index
saveRDS(collapsed_data,"/Users/muqianqianli/Library/CloudStorage/GoogleDrive-limqq@umich.edu/My Drive/RA-Winter-2025/data/richenss.rds")

# draw the bin scatter richness visit plot
library(ggplot2)

collapsed_data$Richness <- as.numeric(as.character(collapsed_data$Richness))
collapsed_data$visits_per_year <- as.numeric(as.character(collapsed_data$visits_per_year))

collapsed_data <- na.omit(collapsed_data) # remove the NA

collapsed_data$Richness <- as.numeric(as.character(collapsed_data$Richness))
collapsed_data$visits_per_year <- as.numeric(as.character(collapsed_data$visits_per_year))

# Remove NA values for accurate calculations
collapsed_data <- na.omit(collapsed_data)

# Create bins for 'Richness' and calculate the average 'visits_per_year' per bin
binned_data <- collapsed_data %>%
  mutate(Richness_bin = cut(Richness, breaks = 30, include.lowest = TRUE)) %>%
  group_by(Richness_bin) %>%
  summarise(Avg_visits = mean(visits_per_year, na.rm = TRUE)) %>%
  mutate(Midpoint = as.numeric(sapply(Richness_bin, function(bin) {
    bounds <- as.numeric(gsub("\\(|\\[|\\]|\\)", "", strsplit(as.character(bin), ",")[[1]]))
    if (length(bounds) == 2) mean(bounds) else NA_real_
  })))

# Create the binscatter plot with a polynomial fit to check for non-linear (concave) relationship
ggplot(binned_data, aes(x = Midpoint, y = Avg_visits)) +
  geom_point() +  # Points for the binned data
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +  # Polynomial regression line
  labs(x = "Richness (Binned)", y = "Average Visits per Year", title = "Nonlinear Relationship Between Richness and Visits") +
  theme_minimal()
