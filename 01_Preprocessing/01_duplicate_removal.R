#!/usr/bin/env Rscript
# ==============================================================================
# 01_duplicate_removal.R
# Remove duplicates from raw mobility data and prepare unique locations
# 
# Input: Raw Mapbox activity data (not included in the repository)
# Output: Pre-processed movement data with unique location IDs
# ==============================================================================

# Load required libraries ------------------------------------------------------
library(sf)
library(dplyr)
library(arrow)

# Configuration ----------------------------------------------------------------
# Set working directory (adjust as needed)
setwd("")

# Set precision for activity values
options(digits = 19)

# Load raw data ----------------------------------------------------------------
message("Loading raw activity data...")
df <- read.csv("London_activity_data.csv")

# Create unique location identifiers ------------------------------------------
message("Creating unique location identifiers...")

# Extract distinct longitude/latitude combinations
distinct_LONLAT <- df |> 
  distinct(XLON, XLAT)

# Assign unique IDs to locations
distinct_LONLAT$LONLAT_ID <- seq_len(nrow(distinct_LONLAT))

# Save location mapping
write_parquet(distinct_LONLAT, "distinct_LONLAT.parquet")
message("Saved ", nrow(distinct_LONLAT), " unique locations")

# Add location IDs to original dataframe
df_with_ids <- inner_join(df, distinct_LONLAT, by = c("XLON", "XLAT"))

# Analyze duplicates (optional) -----------------------------------------------
# This section checks for duplicate activities at the same location and time

check_duplicates <- FALSE  # Set to TRUE to analyze duplicates

if (check_duplicates) {
  message("Analyzing duplicate activities...")
  
  # Find duplicate entries
  duplicates <- df[duplicated(df_with_ids[c("AGG_DAY_PERIOD", "LONLAT_ID")]) |
                   duplicated(df_with_ids[c("AGG_DAY_PERIOD", "LONLAT_ID")], 
                              fromLast = TRUE), ]
  
  # Calculate variation in duplicate values
  duplicate_stats <- duplicates |>
    group_by(AGG_DAY_PERIOD, XLAT, XLON) |>
    mutate(
      activity_range = max(ACTIVITY_INDEX_TOTAL) - min(ACTIVITY_INDEX_TOTAL),
      activity_sd = sd(ACTIVITY_INDEX_TOTAL)
    ) |>
    filter(activity_range > 0 | activity_sd > 0)
  
  if (nrow(duplicate_stats) > 0) {
    message("Found ", nrow(duplicate_stats), " locations with varying duplicate values")
    print(summary(duplicate_stats$activity_range))
  }
}

# Aggregate duplicate values --------------------------------------------------
message("Aggregating duplicate activities by taking mean...")

df_aggregated <- df_with_ids |>
  group_by(AGG_DAY_PERIOD, LONLAT_ID) |>
  summarize(
    mean_activity = mean(ACTIVITY_INDEX_TOTAL, na.rm = TRUE),
    .groups = 'drop'
  )

# Alternative aggregation methods (uncommented as needed):
# Sum aggregation
# df_sum <- df_with_ids |>
#   group_by(AGG_DAY_PERIOD, LONLAT_ID) |>
#   summarize(mean_activity = sum(ACTIVITY_INDEX_TOTAL, na.rm = TRUE))

# Maximum aggregation  
# df_max <- df_with_ids |>
#   group_by(AGG_DAY_PERIOD, LONLAT_ID) |>
#   summarize(mean_activity = max(ACTIVITY_INDEX_TOTAL, na.rm = TRUE))

# Fill missing date-location combinations ------------------------------------
message("Ensuring complete date-location coverage...")

# Create all possible date-location combinations
all_combinations <- expand.grid(
  AGG_DAY_PERIOD = unique(df_aggregated$AGG_DAY_PERIOD),
  LONLAT_ID = unique(df_aggregated$LONLAT_ID)
)

# Merge with aggregated data (missing combinations become NA)
final_data <- left_join(all_combinations, df_aggregated,
                       by = c("AGG_DAY_PERIOD", "LONLAT_ID"))

# Save processed data ----------------------------------------------------------
output_file <- "England/data/processed_data/pre_processed_movement.parquet"
write_parquet(df_aggregated, output_file)

message("Processing complete!")
message("Output saved to: ", output_file)
message("Total records: ", nrow(df_aggregated))
message("Date range: ", min(df_aggregated$AGG_DAY_PERIOD), " to ", 
        max(df_aggregated$AGG_DAY_PERIOD))
