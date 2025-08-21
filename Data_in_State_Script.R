# Load necessary packages
library(sf)
library(ggplot2)
library(dplyr)
library(readr)

# Load CSV data with latitude and longitude columns
csv_data <- read.csv("C:/Users/PJNat/Downloads/DQQ_Questionnaire_Form_Hausa_2024_12_12_22_15_16_928049.csv")

csv_data <-subset(csv_data, !csv_data$X_gps_latitude=="n/a")
csv_data<- csv_data %>% filter (X_gps_longitude !="n/a")
# Convert CSV data to an sf object (Spatial Points DataFrame)
points_sf <- st_as_sf(csv_data, coords = c("X_gps_longitude", "X_gps_latitude"), crs = 4326)

# Load the boundary shapefile
boundary_shapefile <- st_read("C:/Users/PJNat/Desktop/TAMASA_GIS/Nigeria Shp/nigstates36.shp")

# Plot points on top of the boundary shapefile
ggplot() +
  geom_sf(data = boundary_shapefile, fill = "lightgray", color = "black") +
  geom_sf(data = points_sf, color = "blue", size = 1) +
  theme_minimal() +
  labs(title = "Plot of Points with Boundary Shapefile")

# Filter points that fall within a specific state by name (e.g., "StateName")
specific_state <- boundary_shapefile %>%
  filter(NAME == "Nasarawa") # Replace 'NAME' with the appropriate field name in your shapefile

# Find points that intersect with the specific state
points_in_state <- st_intersection(points_sf, specific_state)

ggplot() +
  geom_sf(data = boundary_shapefile, fill = "lightgray", color = "black") +
  geom_sf(data = points_in_state, color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Plot of Points with Boundary Shapefile")

# Export the selected points to a new CSV file
write_csv(as.data.frame(points_in_state), "C:/Users/PJNat/Dropbox/Projects/IITA_Consult/Data/Respondent_in_State.csv")