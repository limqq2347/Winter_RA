### This is the document for calculate Speicies Richness Index
# we need to calculate the richenss first

library(dplyr)
library(ggplot2)

# locate the path where data is at : /nfs/turbo/bus-ericzou/proj-corporate-bio/data/temporary_delete/GBIF/combined_iNaturalist_csymd.rds
combined_iNaturalist <- readRDS("/Users/muqianqianli/Winter_RA/data/combined_iNaturalist_csymd.rds")

# make the data the structure we want
# Process the data to retain and format required columns
# we want the date format as 2001-02-21 
# and we want to save species, cell_id, time, counts = number_organisms_csymd, visits = number_observer_csymd
# counts_per_visit = organisms_per_visit
# to get the structure we need 

processed_data <- combined_iNaturalist %>%
  mutate(
    time = paste(year, month, day, sep = "-") %>% as.Date(format = "%Y-%m-%d")
  ) %>%
  select(species, cell_id, time, 
         counts = number_organisms_csymd, 
         visits = number_observer_csymd, 
         counts_per_visit = organisms_per_visit)

saveRDS(processed_data,"/Users/muqianqianli/Winter_RA/data/Richness.rds")
# the Richness.rds is in the /nfs/turbo/bus-ericzou/proj-corporate-bio/data/temporary_delete/GBIF/Richness.rds
# processed_data <- readRDS("/nfs/turbo/bus-ericzou/proj-corporate-bio/data/temporary_delete/GBIF/Richness.rds")

# collapse the data to grid-by-year level, counting distinct species

collapsed_data <- processed_data %>%
  mutate(year = format(time, "%Y")) %>%  # Extract the year from the 'time' column
  group_by(cell_id, year) %>%
  summarise(
    Richness = n_distinct(species),       # Count distinct species (Richness)
    visits_per_year = sum(visits, na.rm = TRUE)  # Sum of visits per year
  ) %>%
  ungroup()

# draw the bin scatter richness visit plot

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
ggplot(binned_data, aes(x = Avg_visits, y = Midpoint)) +
  geom_point() +  # Points for the binned data
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +  # Polynomial regression line
  labs(x = "Average Visits per Year", y = "Richness (Binned)", title = "Nonlinear Relationship Between Visits and Richness") +
  theme_minimal()

