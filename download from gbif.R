library("rgbif")
library(lme4)
library(sf)
library(dplyr)
library(ggplot2)
library(readr)
library(maps)
library(maps)
setwd("/home/cwclaire/seas-zhukai/proj-corporate-bio/data/temporary_delete/GBIF")



# List of file names in your working directory
file_names <- paste0("iNaturalist", 1:9, ".txt")

# Function to read each file and select only the required columns
read_and_select <- function(file) {
  data <- read.csv(file)  # adjust 'sep' if necessary
  data <- data[, c("decimalLongitude", "decimalLatitude")]
  return(data)
}

# Use lapply to read each file and keep only the specified columns
data_list <- lapply(file_names, read_and_select)
# Combine all datasets into one
coor <- do.call(rbind, data_list)

# Preview the merged data
head(coor)

grid_points <- st_as_sf(coor, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# Step 2: Define a bounding box around the USA points


# Step 3: Set the CRS to a projected coordinate system that uses meters
# This is crucial for creating an accurate grid of 40 km x 40 km cells
# Albers Equal Area projection is commonly used for the contiguous USA
usa_crs <- 5070 # EPSG code for USA Albers Equal Area projection
grid_points <- st_transform(grid_points, crs = usa_crs)

bounding_box <- st_bbox(grid_points)

# Step 4: Create the grid
grid_size <- 40000 # 40 km in meters
grid <- st_make_grid(bounding_box, cellsize = c(grid_size, grid_size), square = TRUE)
grid <-  st_sf(grid) %>% mutate(cell_id = row_number())

saveRDS(grid, file = "grid.rds")
# Step 5: Plot the grid over the points
#ggplot() +
#  geom_sf(data = grid, fill = NA, color = "grey") +
#  geom_sf(data = points, color = "blue", size = 0.5) +
#  labs(title = "40 km x 40 km Grid over USA Points",
#       x = "Longitude", y = "Latitude") +
#  theme_minimal()

# Define a loop to process files iNaturalist1 through iNaturalist9
for (i in 1:9) {
  
  # Construct the file names dynamically
  input_file <- paste0("iNaturalist", i, ".txt")
  output_file <- paste0("iNaturalist", i, "_sorted.rda")
  
  # Read the CSV file
  data <- read.csv(input_file)
  
  # Filter data for the specified years
  data <- data %>%
    filter(year >= 1980 & year != 2024)
  
  # Convert to spatial data frame with coordinates
  data_points <- st_as_sf(data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
  
  # Transform CRS
  data_points <- st_transform(data_points, crs = usa_crs)
  
  # Spatial join with grid data
  iNaturalist_sorted <- st_join(data_points, grid, join = st_within)
  
  
  # Step 5: Drop duplicates by specific columns
  iNaturalist_sorted <- iNaturalist_sorted %>%
    distinct(cell_id,  year, month, genus, identifiedBy, day, .keep_all = TRUE)
  
  # Drop geometry to convert to a regular data frame
  iNaturalist_sorted <- st_drop_geometry(iNaturalist_sorted)
  
  # Select relevant columns
  iNaturalist_sorted <- iNaturalist_sorted %>%
    select(cell_id, year, month, genus, identifiedBy, day, kingdom, phylum, class, order, family, stateProvince)
  
  # Rename the data frame dynamically
  assign(paste0("iNaturalist", i, "_sorted"), iNaturalist_sorted)
  
  # Save the result with the unique name
  save(list = paste0("iNaturalist", i, "_sorted"), file = output_file)
}

# Define a loop to process files iNaturalist1 through iNaturalist9

# Initialize an empty list to store each loaded data frame
all_data <- list()

# Loop to load each .rda file and append to the list
for (i in 1:9) {
  
  # Construct the file name dynamically
  file_name <- paste0("iNaturalist", i, "_sorted.rda")
  
  # Load the .rda file (it will load with its saved name, e.g., iNaturalist1_sorted)
  load(file_name)
  
  # Dynamically get the name of the loaded object
  data <- get(paste0("iNaturalist", i, "_sorted"))
  
  # Append the data frame to the list
  all_data[[i]] <- data
}

# Combine all data frames in the list into a single data frame
combined_iNaturalist <- bind_rows(all_data)

# Step 1: Create `obs` column
combined_iNaturalist <- combined_iNaturalist %>%
  mutate(obs = 1)

# Step 2: Calculate `number_creature_cgmy`
combined_iNaturalist <- combined_iNaturalist %>%
  group_by(cell_id, month, year, genus) %>%
  mutate(number_creature_cgmy = sum(obs)) %>%
  ungroup()

# Step 3: Calculate `number_observer_cmy`
combined_iNaturalist <- combined_iNaturalist %>%
  group_by(cell_id, month, year) %>%
  mutate(number_observer_cmy = n_distinct(identifiedBy)) %>%
  ungroup()

# Step 4: Calculate `number_day_cmy`
combined_iNaturalist <- combined_iNaturalist %>%
  group_by(cell_id, month, year) %>%
  mutate(number_day_cmy = n_distinct(day)) %>%
  ungroup()

combined_iNaturalist<- combined_iNaturalist %>%
  distinct(cell_id,  year, month, genus, number_creature_cgmy, number_observer_cmy, number_day_cmy , .keep_all = TRUE)

combined_iNaturalist$year_month <- with(combined_iNaturalist, paste(year, month, sep = "-"))

combined_iNaturalist$cell_id_year <- with(combined_iNaturalist, paste(cell_id,year, sep = "-"))

saveRDS(combined_iNaturalist, file = "combined_iNaturalist.rds")
combined_iNaturalist <- readRDS("combined_iNaturalist.rds")
class_data <- combined_iNaturalist %>% filter(class == "Insecta")
grid_joined <- grid %>%
  left_join(class_data, by = "cell_id")
set.seed(111)
sampled_data <- class_data %>%
  group_by(genus,cell_id) %>%            # Group by genus
  sample_frac(0.1) %>%           # Randomly sample 10% of each genus group
  ungroup()                      # Remove grouping for final dataset
# Ensure no zeros in number_creature_cgmy before log transformation
sampled_data <- sampled_data %>%
  filter(number_creature_cgmy > 0) %>%  # Remove rows where the outcome is zero or negative
  mutate(log_number_creature_cgmy = log(number_creature_cgmy))  # Create log-transformed outcome

# Fit the GLMM with the log-transformed outcome
model <- lmer(  log_number_creature_cgmy ~ number_observer_cmy + number_day_cmy +
    (1 | month) + cell_id+year,  data = sampled_data,na.action = na.omit,control = lmerControl(check.nobs.vs.nRE = "ignore"))

model <- lmer(  log_number_creature_cgmy ~ number_observer_cmy + number_day_cmy +
                  (1 | month) + as.factor(cell_id)+as.factor(year),  data = sampled_data,na.action = na.omit,control = lmerControl(check.nobs.vs.nRE = "ignore"))
model <- lm(  log_number_creature_cgmy ~ number_observer_cmy + number_day_cmy +
                  as.factor(month)+as.factor(cell_id)+as.factor(year),  data = sampled_data,na.action = na.omit)
# Extract the genus coefficients
genus_coeff <- coef(summary(model))["genus", "Estimate", drop=FALSE]

# Store in the list with the class name as the key
genus_coefficients[[class_level]] <- genus_coeff
