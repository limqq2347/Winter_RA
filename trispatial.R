# Set the folder path containing the CSV files
library(haven)
library(maps)
library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
folder_path <- "/home/cwclaire/eric-zou/proj-corporate-bio/data/temporary_delete/TRI"


file_path <- file.path(folder_path, "tri_temp.dta")  # Combine folder path with the file name

tri_temp <- read_dta(file_path)

missing_coords <- tri_temp %>%
  filter(is.na(longitude) | is.na(latitude))

if (nrow(missing_coords) > 0) {
  cat("Rows with missing coordinates removed:\n")
  print(missing_coords)
}

# Step 2: Remove rows with missing coordinates
tri_temp <- tri_temp %>%
  filter(!is.na(longitude) & !is.na(latitude))

# Step 1: Adjust totalreleases for "Dioxin" classification
tri_temp <- tri_temp %>%
  mutate(
    totalreleases = ifelse(classification == "Dioxin", totalreleases * 0.00220462, totalreleases)
  )
usa_crs <- 5070
grid_points <- st_as_sf(tri_temp, coords = c("longitude", "latitude"), crs = 4326)
usa_map <- map("state", plot = FALSE, fill = TRUE)

usa_map_sf <- st_as_sf(usa_map, crs = 4326)
grid_points <- st_transform(grid_points, crs = usa_crs)
usa_map_sf <- st_transform(usa_map_sf, crs = usa_crs)

# Get bounding box of USA
usa_bbox <- st_bbox(usa_map_sf)

grid_size <- 40000 # 40 km in meters
grid <- st_make_grid(usa_bbox, cellsize = c(grid_size, grid_size), square = TRUE)
grid <-  st_sf(grid) %>% mutate(cell_id = row_number())

tri_grid_with_points <- st_join(grid_points, grid, join = st_within)

tri_grid_with_points <- tri_grid_with_points %>%
  group_by(cell_id, year) %>%
  mutate(ems_tot= sum(totalreleases)) %>%
  ungroup()


tri_grid_with_points <- tri_grid_with_points %>%
  group_by(cell_id, year) %>%
  mutate(ems_tot_onsite= sum(onsitereleasetotal)) %>%
  ungroup()
subset_df <- tri_grid_with_points[tri_grid_with_points$cell_id == 5014, ] 
subset_df2 <- tri_grid_with_points[tri_grid_with_points$cell_id == 5353, ]

tri_grid_with_points  <- tri_grid_with_points %>%
  distinct(cell_id, year, .keep_all = TRUE)

tri_grid_no_geom <- st_drop_geometry(tri_grid_with_points)

grid_joined <- grid  %>%
  left_join(tri_grid_no_geom, by = "cell_id")
grid_joined <- st_transform(grid_joined, crs = 4326)

grid_2018 <- grid_joined %>% filter(year == 2018)
grid_2019 <- grid_joined %>% filter(year == 2019)
grid_2020 <- grid_joined %>% filter(year == 2020)
grid_2021 <- grid_joined %>% filter(year == 2021)
grid_2022 <- grid_joined %>% filter(year == 2022)
grid_2023 <- grid_joined %>% filter(year == 2023)


ggplot() +
  # Base USA map
  geom_sf(data = usa_map_sf, fill = "lightgray", color = "black") +
  
  # Mesh-style grid with color steps
  geom_sf(data = grid_2018, aes(fill = ems_tot_onsite), color = NA) +
  
  # Discrete color steps with uneven breaks
  scale_fill_steps(
    low = "white", high = "#8B0000", n.breaks = 8,
    #breaks = c(-0.14, -0.13, -0.12, -0.09,  40), # Uneven breaks
    name = "Standardized EMS"
  ) +
  
  theme_minimal() +
  
  # Adjust legend appearance
  theme(
    legend.position = "bottom",
    legend.key.height = unit(1, "cm"), # Make legend keys taller
    legend.key.width = unit(2, "cm"),  # Adjust key width if needed
    axis.text = element_blank(), 
    axis.title = element_blank(), 
    panel.grid = element_blank()
  ) +
  
  ggtitle("Grid Joined with USA Base Map")

# Create a histogram of ems_tot_onsite from grid_2018
hist(
  grid_2018$ems_tot_onsite,         # Variable to plot
  breaks = 10,                      # Number of bins
  main = "Histogram of ems_tot_onsite",  # Title of the histogram
  xlab = "ems_tot_onsite",          # Label for the x-axis
  col = "skyblue",                  # Fill color for the bars
  border = "black"                  # Color of the bar borders
)

# Add a grid for better readability
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
