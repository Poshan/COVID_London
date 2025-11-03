# ==============================================================================
# 05_create_indicators.R
# Create spatial lag indicators and mobility-weighted factors
#
# Input: Spatial graphs, OD matrix, processed data
# Output: Dataset with spatial lag and mobility lag indicators
# ==============================================================================

# Load required libraries and dependencies ------------------------------------
source("create_graph.R")  # Must be run first to create spatial structures

# Create spatial lag indicator ------------------------------------------------
message("Creating spatial lag indicator...")

# Convert spatial weights matrix to standard matrix format
spatial_weights_matrix <- as.matrix(msoa_W)

# Get number of time periods
num_time_periods <- length(unique(london_df$date))

# Create identity matrix for temporal structure
# Each time period is independent (no temporal correlation in this step)
temporal_identity <- diag(1, nrow = num_time_periods)

# Create space-time weight matrix using Kronecker product
# This creates a block-diagonal matrix where each block is the spatial weights
spacetime_weights <- kronecker(temporal_identity, spatial_weights_matrix)

# Calculate spatial lag: weighted average of neighbors' activity
london_df$spatial_lag <- spacetime_weights %*% london_df$weekly_mean_activity

message("Spatial lag created for ", num_time_periods, " time periods")

# Create mobility-based lag indicator -----------------------------------------
message("Creating mobility-weighted lag indicator...")

# Convert mobility-based neighborhood to weighted list
mobility_weights_list <- nb2listw(
  neighbors_list_i,  # From create_graph.R
  glist = weights_list_i,  # OD flow weights
  style = "W",  # Row standardization
  zero.policy = TRUE
)

# Alternative weight styles (uncomment to use):
# style = "C" for globally standardized weights
# style = "B" for binary weights
# style = "S" for variance stabilizing

# Convert to matrix format
mobility_weights_matrix <- as.matrix(listw2mat(mobility_weights_list))

# Create space-time mobility weight matrix
spacetime_mobility_weights <- kronecker(
  diag(1, num_time_periods), 
  mobility_weights_matrix
)

# Calculate mobility lag: activity weighted by movement patterns
london_df$MobilityLag <- spacetime_mobility_weights %*% london_df$weekly_mean_activity

message("Mobility lag created using OD flow weights")

# Add within-region movement indicator ----------------------------------------
message("Adding within-region movement flows...")

# Extract diagonal of OD matrix (within-area flows)
within_area_flows <- diag(as.matrix(od_matrix))

# Repeat for all time periods
london_df$within_flow <- rep(
  within_area_flows,
  length(unique(london_df$week))
)

# Save enhanced dataset --------------------------------------------------------
output_file <- "data/processed_data/lagged/df_lagged.parquet"


