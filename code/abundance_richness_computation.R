library(dplyr)
library(fixest) # For fixed effects regression
library(purrr)

# In this code I already subset the data we have into different class for our 

# subset all the class in rds file, for example class_Acatharia.rds 
# Orignal we have 220 classes
# Define the directory containing the class_XXX.rds files

input_dir <- "/Users/muqianqianli/Library/CloudStorage/GoogleDrive-limqq@umich.edu/My Drive/RA-Winter-2025/data/class/"
output_dir <- "/Users/muqianqianli/Library/CloudStorage/GoogleDrive-limqq@umich.edu/My Drive/RA-Winter-2025/data/fe2/"

# add the day_of_week we want in the column
file_names <- list.files(input_dir, pattern = "\\.rds$", full.names = TRUE)

# Loop over each file
for (file_name in file_names) {
  # Read the .rds file
  data <- readRDS(file_name)
  
  # Check if the necessary columns are present
  if("year" %in% names(data) && "month" %in% names(data) && "day" %in% names(data)) {
    # Create the date column from year, month, and day
    data$date <- as.Date(with(data, paste(year, month, day, sep = "-")), "%Y-%m-%d")
    
    # Add the day_of_week column
    data$day_of_week <- weekdays(data$date)
    
    # Save the modified data back to the same .rds file
    saveRDS(data, file_name)
  } else {
    cat("The file", file_name, "does not have the required columns.\n")
  }
}

# load one example data to check the format
class_Insecta<- readRDS("/Users/muqianqianli/Library/CloudStorage/GoogleDrive-limqq@umich.edu/My Drive/RA-Winter-2025/data/class/class_Insecta.rds")

# load the combined_iNaturalist_csymd.rds because we want to remove
# Alaska, Hawaii and sea region in the class.
combined_iNaturalist_csymd <- readRDS("/Users/muqianqianli/Library/CloudStorage/GoogleDrive-limqq@umich.edu/My Drive/RA-Winter-2025/data/combined_iNaturalist_csymd.rds")

inat_data <- combined_iNaturalist_csymd %>% select(cell_id, year, month, day, species, kingdom, stateProvince, phylum)

output_folder <- "/Users/muqianqianli/Library/CloudStorage/GoogleDrive-limqq@umich.edu/My Drive/RA-Winter-2025/data/class_with_state/"

# Process each file
for (file_path in file_names) {
  # Load the class data
  class_data <- readRDS(file_path)
  
  # Merge with the iNaturalist data
  merged_data <- left_join(class_data, inat_data, by = c("cell_id", "year", "month", "day", "species"))
  
  # Exclude Alaska, Hawaii, and any records without a defined stateProvince if necessary
  merged_data <- filter(merged_data, !stateProvince %in% c("Alaska", "Hawaii", NA))
  
  # Save the merged file
  output_file_path <- paste0(output_folder, basename(file_path))
  saveRDS(merged_data, output_file_path)
}


# check the class_with_state
class_Diplura<- readRDS("/Users/muqianqianli/Library/CloudStorage/GoogleDrive-limqq@umich.edu/My Drive/RA-Winter-2025/data/class_with_state/class_Diplura.rds")

# update the input_dir
input_dir <- "/Users/muqianqianli/Library/CloudStorage/GoogleDrive-limqq@umich.edu/My Drive/RA-Winter-2025/data/class_with_state/"
  
# Get a list of all .rds files in the input directory
rds_files <- list.files(input_dir, pattern = "^class_.*\\.rds$", full.names = TRUE)

for (input_file in rds_files) {
  # Extract the identifier and load data
  file_identifier <- sub("class_(.*)\\.rds", "\\1", basename(input_file))
  data <- readRDS(input_file)
  
  # Check if dependent variable has variation
  if (length(unique(data$counts_per_visit)) > 1) {
    # Fit the fixed effects model
    model <- feols(log(counts_per_visit) ~ 1 | species + month + day_of_week + cell_year, data = data)
    
    # Extract fixed effects
    fixed_effects <- fixef(model)
    cell_year_effects <- fixed_effects$cell_year
    
    # Save output
    output_file <- paste0(output_dir, "cell_year_fixed_effects_", file_identifier, ".csv")
    write.csv(as.data.frame(cell_year_effects), output_file, row.names = TRUE)
    cat("Processed and saved:", output_file, "\n")
  } else {
    cat("Skipped due to constant dependent variable:", input_file, "\n")
  }
}

# Define the directory containing the CSV files where we save the abundance
directory <- "/Users/muqianqianli/Library/CloudStorage/GoogleDrive-limqq@umich.edu/My Drive/RA-Winter-2025/data/fe2"

# notice that after using the model our class decrease only have 155
# it is because we exclude the counts_per_visit <= 1 

csv_files <- list.files(directory, pattern = "^cell_year_fixed_effects_.*\\.csv$", full.names = TRUE)

# Function to standardize cell_year_effects within each class
standardize_effects <- function(df) {
  df %>%
    group_by(class) %>%
    mutate(cell_year_effects_standardized = scale(cell_year_effects)) %>%
    ungroup()
}

# Loop over each file, apply the standardization, and save the file
for (file in csv_files) {
  data <- read.csv(file)
  data_standardized <- standardize_effects(data)
  
  # Save the modified dataframe back to CSV
  write.csv(data_standardized, file, row.names = FALSE)
}

# Loop through each file
for (file in csv_files) {
  # Extract the class name from the filename
  file_name <- basename(file)
  class_name <- sub("cell_year_fixed_effects_(.*)\\.csv", "\\1", file_name)
  
  # Read the CSV file
  data <- read.csv(file)
  
  # Add the new 'class' column
  data$class <- class_name
  
  # Save the updated data back to the file
  write.csv(data, file, row.names = FALSE)
}


# List all CSV files in the folder
csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty list to store data frames
data_list <- list()

# Loop through each file and read the data
for (file in csv_files) {
  # Read the CSV file
  data <- read.csv(file)
  
  # Append the data frame to the list
  data_list[[file]] <- data
}

# Combine all data frames into one
combined_data <- bind_rows(data_list, .id = "source")

# Save the combined data to a new CSV file
write.csv(combined_data, file = "/Users/muqianqianli/Library/CloudStorage/GoogleDrive-limqq@umich.edu/My Drive/RA-Winter-2025/data/fe_output/combined_data.csv", row.names = FALSE)

# now we have the combined_data with cell_year effects (fixed effect)
combined_data <- read.csv("/Users/muqianqianli/Library/CloudStorage/GoogleDrive-limqq@umich.edu/My Drive/RA-Winter-2025/data/fe_output/combined_data.csv")

processed_data <- combined_data %>%
  # Remove the 'source' column
  select(-source) %>%
  # Split 'X' column into 'cell_id' and 'year'
  mutate(cell_id = sub("_.*", "", X),  # Extract part before '_'
         year = sub(".*_", "", X)) %>% # Extract part after '_'
  # Rename 'cell_year_effects' to 'fe'
  rename(relative_abundance_index = cell_year_effects_standardized) %>%
  # Remove the original 'X' column
  select(-X)

# Save the processed data to a new CSV file
write.csv(processed_data, file = "/Users/muqianqianli/Library/CloudStorage/GoogleDrive-limqq@umich.edu/My Drive/RA-Winter-2025/data/fe_output/processed_data.csv", row.names = FALSE)

# load the proceed_data
processed_data <- read.csv("/Users/muqianqianli/Library/CloudStorage/GoogleDrive-limqq@umich.edu/My Drive/RA-Winter-2025/data/fe_output/processed_data.csv")

# draw the abundance line chart
library(ggplot2)
processed_data <- na.omit(processed_data)

# Aggregate relative_abundance_index by year
yearly_data <- processed_data %>%
  group_by(year) %>%
  summarise(aggregated_index = sum(relative_abundance_index, na.rm = TRUE))

# Plotting the aggregated relative_abundance_index over years
ggplot(yearly_data, aes(x = year, y = aggregated_index)) +
  geom_line() + # Line chart
  labs(title = "Aggregated Relative Abundance Index by Year",
       x = "Year",
       y = "Aggregated Relative Abundance Index") +
  theme_minimal()
#
# Aggregate cell_year_effects by year
yearly_effects_data <- processed_data %>%
  group_by(year) %>%
  summarise(aggregated_effects = sum(cell_year_effects, na.rm = TRUE))

# Plotting the aggregated cell_year_effects over years
ggplot(yearly_effects_data, aes(x = year, y = aggregated_effects)) +
  geom_line() + # Line chart
  labs(title = "Aggregated Cell Year Effects by Year",
       x = "Year",
       y = "Aggregated Cell Year Effects") +
  theme_minimal()

combined_iNaturalist_csymd <- readRDS("/Users/muqianqianli/Library/CloudStorage/GoogleDrive-limqq@umich.edu/My Drive/RA-Winter-2025/data/combined_iNaturalist_csymd.rds")

fixed_effect_iNaturalist <- readRDS("/Users/muqianqianli/Library/CloudStorage/GoogleDrive-limqq@umich.edu/My Drive/RA-Winter-2025/data/fixed_effect_iNaturalist.rds")

combined_iNaturalist_csymd_selected <- combined_iNaturalist_csymd %>%
  select(cell_id, year, day,class, species, kingdom, stateProvince, phylum, number_organisms_csymd, number_observer_csymd, organisms_per_visit)

#saveRDS(combined_iNaturalist_csymd_selected, "/Users/muqianqianli/Library/CloudStorage/GoogleDrive-limqq@umich.edu/My Drive/RA-Winter-2025/data/combined_iNaturalist_csymd_selected.rds")

#combined_data <- inner_join(processed_data, combined_iNaturalist_csymd_selected, by = c("cell_id", "year"))

#load the original iNaturalist data to combine the 
fixed_effect_iNaturalist <- readRDS("/Users/muqianqianli/Library/CloudStorage/GoogleDrive-limqq@umich.edu/My Drive/RA-Winter-2025/data/fixed_effect_iNaturalist.rds")

output <- fixed_effect_iNaturalist %>%
  group_by(cell_id, year, species, class) %>%
  summarize(`organisms observed` = sum(obs, na.rm = TRUE), .groups = "drop") %>%
  select(cell_id, year, species, class, `organisms observed`)

# remove the class = Na and  ""
data_cleaned <- subset(output, !is.na(class) & class != "")

final_output <- data_cleaned %>%
  group_by(cell_id, year, class) %>% 
  summarize(
    richness = n_distinct(species, na.rm = TRUE),  # Count unique species in each cell_id
    `organisms observed` = sum(`organisms observed`, na.rm = TRUE),  # Sum organisms observed
    .groups = "drop"
  ) %>%
  filter(!is.na(class) & class != "")  # Remove rows where class is NA or an empty string

# Print the final output

combined_data <- final_output %>%
  full_join(processed_data, by = c("cell_id", "year", "class"))

# Save the combined dataset to an .rds file
saveRDS(combined_data, "/Users/muqianqianli/Library/CloudStorage/GoogleDrive-limqq@umich.edu/My Drive/RA-Winter-2025/data/fe.rds")

fe <- readRDS("/Users/muqianqianli/Library/CloudStorage/GoogleDrive-limqq@umich.edu/My Drive/RA-Winter-2025/data/fe.rds")

# calculate 
