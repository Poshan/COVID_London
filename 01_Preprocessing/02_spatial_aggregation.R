# ==============================================================================
# 02_spatial_aggregation.R
# Aggregate mobility data to MSOA level (spatially) and weekly (temporally)
#
# Input: Pre-processed movement data, MSOA boundaries
# Output: Weekly aggregated activity by MSOA
# ==============================================================================

# Load required libraries ------------------------------------------------------
library(sf)
library(dplyr)
library(tidyr)
library(arrow)
library(lubridate)

# Configuration ----------------------------------------------------------------
# Define ID column for MSOA
ID_COL <- "MSOA11CD"

# Set working directory
# setwd('')

# Disable spherical geometry for performance
sf_use_s2(FALSE)

# Load input data --------------------------------------------------------------
message("Loading input data...")

# Activity data
merged_data <- read_parquet("data/mobility_data/pre_processed_movement.parquet")

# Location mapping
distinct_LONLAT <- read_parquet("data/mobility_data/distinct_LONLAT.parquet")

# MSOA boundaries
msoa <- read_sf('data/Boundary/MSOA_2011_London_gen_MHW.shp') |> 
  st_transform(crs = "OGC:CRS84")

# Prepare spatial data ---------------------------------------------------------
message("Preparing spatial data...")

# Remove any NA locations
distinct_LONLAT <- distinct_LONLAT |> 
  drop_na(XLAT)

# Convert locations to spatial features
distinct_LONLAT.sf <- st_as_sf(
  distinct_LONLAT, 
  crs = "OGC:CRS84", 
  coords = c("XLON", "XLAT")
)

# Map activity locations to MSOAs ---------------------------------------------
message("Mapping activity locations to MSOAs...")

# Spatial join to find which MSOA each activity location belongs to
activitylocation_msoa <- st_join(
  st_make_valid(msoa), 
  st_make_valid(distinct_LONLAT.sf)
)

# Remove points that don't fall within any MSOA
activitylocation_msoa <- drop_na(activitylocation_msoa)

# Keep only necessary columns
activitylocation_msoa <- activitylocation_msoa |> 
  select(all_of(c(ID_COL, "LONLAT_ID")))

# Add MSOA codes to activity data
df_with_msoa <- left_join(
  merged_data, 
  activitylocation_msoa, 
  by = "LONLAT_ID"
) |> 
  drop_na()

message("Matched ", nrow(df_with_msoa), " activity records to MSOAs")

# Spatial aggregation ----------------------------------------------------------
message("Aggregating by MSOA and day...")

# Calculate mean activity per MSOA per day
daily_msoa_activity <- aggregate(
  mean_column ~ MSOA11CD + AGG_DAY_PERIOD,
  data = df_with_msoa, 
  FUN = mean
)

# Convert date column to Date type
daily_msoa_activity$AGG_DAY_PERIOD <- as.Date(daily_msoa_activity$AGG_DAY_PERIOD)

# Temporal aggregation ---------------------------------------------------------
message("Aggregating to weekly level...")

# Add week column
daily_msoa_activity$week <- lubridate::week(daily_msoa_activity$AGG_DAY_PERIOD)

# Handle year-end edge case (week 53 -> 52)
daily_msoa_activity <- daily_msoa_activity |>
  mutate(week = if_else(week == 53, 52, week))

# Sort data for proper temporal ordering
daily_msoa_activity <- daily_msoa_activity |>
  arrange(MSOA11CD, AGG_DAY_PERIOD) |>
  group_by(MSOA11CD)

# Find last day of each week for each MSOA
last_day_per_week <- daily_msoa_activity |>
  group_by(MSOA11CD, week) |>
  filter(AGG_DAY_PERIOD == max(AGG_DAY_PERIOD)) |>
  ungroup() |>
  select(MSOA11CD, week, AGG_DAY_PERIOD)

# Calculate weekly mean activity
weekly_summary <- daily_msoa_activity |>
  group_by(MSOA11CD, week) |>
  summarise(
    mean_weekly_activity = mean(mean_column, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  # Add the last day of each week for reference
  left_join(last_day_per_week, by = c("MSOA11CD", "week"))

# Save processed data ----------------------------------------------------------
output_file <- "data/processed_data/processed_activity.parquet"
write_parquet(weekly_summary, output_file)

message("Weekly aggregation complete!")
message("Output saved to: ", output_file)
message("Total MSOAs: ", n_distinct(weekly_summary$MSOA11CD))
message("Total weeks: ", n_distinct(weekly_summary$week))

# Optional: Create aggregated grids --------------------------------------------
# Function to aggregate data into regular grids of specified size

#' Aggregate activity data into regular spatial grids
#' 
#' @param boundary SF object defining study area boundary
#' @param activity_data Dataframe with columns: LONLAT_ID, AGG_DAY_PERIOD, mean_column
#' @param point_locations SF object with activity point locations
#' @param grid_size Size of grid cells in meters
#' @return List with grid polygons and aggregated activity data
aggregate_to_grid <- function(boundary, activity_data, point_locations, grid_size) {
  
  message("Creating ", grid_size, "m grid...")
  
  # Transform to projected CRS for metric calculations
  point_locations_proj <- st_transform(point_locations, crs = 32630)
  boundary_proj <- st_transform(boundary, crs = 32630)
  
  # Create regular grid
  grid <- st_make_grid(
    boundary_proj, 
    cellsize = c(grid_size, grid_size)
  ) |>
    st_as_sf() |>
    st_intersection(boundary_proj) |>
    st_as_sf()
  
  # Assign grid IDs
  grid$grid_ID <- seq_len(nrow(grid))
  
  # Map points to grid cells
  point_grid_mapping <- st_join(
    point_locations_proj, 
    grid, 
    left = FALSE, 
    join = st_intersects
  ) |> 
    st_drop_geometry()
  
  # Add grid IDs to activity data
  activity_with_grid <- activity_data |>
    left_join(point_grid_mapping, by = "LONLAT_ID") |>
    drop_na()
  
  # Aggregate by grid cell
  grid_aggregated <- activity_with_grid |>
    group_by(grid_ID, AGG_DAY_PERIOD) |>
    summarise(
      grid_mean = mean(mean_column, na.rm = TRUE), 
      .groups = 'drop'
    ) |>
    drop_na(grid_mean)
  
  return(list(
    grid = grid, 
    aggregated_data = grid_aggregated
  ))
}

# Example: Create 1000m grid (uncomment to use)
# gla_boundary <- st_read("data/Boundary/GLA_boundary.gpkg")
# grid_1000m <- aggregate_to_grid(
#   gla_boundary, 
#   merged_data, 
#   distinct_LONLAT.sf, 
#   1000
# )
# 
# write_parquet(
#   grid_1000m$aggregated_data, 
#   "data/processed_data/activity_in_1000m_grids.parquet"
# )
# st_write(
#   grid_1000m$grid, 
#   "data/processed_data/1000m_grids.shp"
# )
