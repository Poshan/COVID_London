# =============================================================================
# create_graph.R
# =============================================================================
# Purpose: Create spatial and mobility-based graph structures for INLA modeling
# =============================================================================

# Load required libraries ------------------------------------------------------
library(sf)
library(dplyr)
library(tidyr)
library(spdep)
library(arrow)
library(INLA)
library(lubridate)
library(Matrix)

# Load data --------------------------------------------------------------------
message("Loading processed data...")

# Load main dataset with COVID cases and activity
london_df <- read_parquet("data/processed_data/processed_data_with_covid_cases.parquet")

# Alternative: Load NHS version if available
# london_df <- read_parquet("data/processed_data/processed_data_with_NHS_covid_cases.parquet")
# london_df$caserate[london_df$caserate < 0] <- 0  # Handle negative case rates

# Load spatial boundaries ------------------------------------------------------
message("Loading spatial boundaries...")

# MSOA boundaries for London
msoa <- read_sf('data/Boundary/MSOA_2011_London_gen_MHW.shp') |> 
  st_transform('OGC:CRS84') |> 
  filter(MSOA11CD %in% unique(london_df$areaCode))

# LAD (Local Authority District) boundaries for hierarchical effects
lad <- st_read("data/Boundary/LAD.gpkg")

message("  MSOAs: ", nrow(msoa))
message("  LADs: ", nrow(lad))

# Create MSOA ID mapping -------------------------------------------------------
message("Creating MSOA numeric IDs...")

# Create consistent numeric IDs for MSOAs
msoa_list <- msoa$MSOA11CD

create_numeric_id <- function(codes, reference_list) {
  match(codes, reference_list)
}

london_df$new_id <- create_numeric_id(london_df$areaCode, msoa_list)

# ==============================================================================
# GEOGRAPHIC NEIGHBORHOOD STRUCTURES
# ==============================================================================

message("\n=== Creating Geographic Neighborhood Structures ===")

# MSOA-level neighborhood ------------------------------------------------------
message("Creating MSOA neighborhood structure...")

# Create queen contiguity (shared boundary = neighbor)
msoa_neighbors <- poly2nb(st_make_valid(msoa), queen = TRUE)

# Create spatial weights matrix (row-standardized)
msoa_W <- nb2mat(msoa_neighbors, style = "W", zero.policy = TRUE)

# Create INLA graph object
msoa_graph <- inla.read.graph(msoa_W)
inla.write.graph(msoa_graph, "data/graphs/msoa_graph.txt")

message("  MSOA graph created: ", length(msoa_neighbors), " areas")
message("  Average neighbors per MSOA: ", 
        round(mean(sapply(msoa_neighbors, length)), 1))

# Create precision matrix for BYM2 model ---------------------------------------
message("Creating precision matrix for spatial effects...")

# Diagonal matrix with row sums
D <- Diagonal(x = rowSums(msoa_W))

# Precision matrix Q = D - W
Q_spatial <- D - msoa_W

# Convert to sparse format for INLA
Q_spatial <- as(Q_spatial, "TsparseMatrix")

# LAD-level neighborhood (for hierarchical effects) ---------------------------
message("Creating LAD neighborhood structure...")

lad_neighbors <- poly2nb(st_make_valid(lad), queen = TRUE)
lad_W <- nb2mat(lad_neighbors, style = "W", zero.policy = TRUE)
lad_graph <- inla.read.graph(lad_W)
inla.write.graph(lad_graph, "data/graphs/lad_graph.txt")

message("  LAD graph created: ", length(lad_neighbors), " areas")

# ==============================================================================
# MOBILITY-BASED NEIGHBORHOOD STRUCTURES
# ==============================================================================

message("\n=== Creating Mobility-Based Neighborhood Structures ===")

# Load origin-destination matrix
od_matrix <- read_parquet("data/processed_data/origin_destination_matrix.parquet")
od_matrix_values <- od_matrix[-1]  # Remove origin ID column

# Convert to matrix and set diagonal to 0 (exclude within-area flows)
od_mat <- as.matrix(od_matrix_values)
diag(od_mat) <- 0

# Function to create outgoing mobility neighbors -------------------------------
create_outgoing_neighbors <- function(od_matrix, threshold = 0) {
  #' Create neighbor structure based on outgoing mobility flows
  #' 
  #' @param od_matrix Matrix of origin-destination flows
  #' @param threshold Minimum flow to consider as neighbor
  #' @return List with neighbors and weights
  
  if (!is.matrix(od_matrix)) {
    od_matrix <- as.matrix(od_matrix)
  }
  
  # Apply threshold
  od_matrix[od_matrix <= threshold] <- NA
  
  neighbors_list <- list()
  weights_list <- list()
  
  # For each origin (row), find destinations with flows
  for (origin in 1:nrow(od_matrix)) {
    row_flows <- od_matrix[origin, ]
    
    # Find valid destinations
    valid_mask <- !is.na(row_flows)
    destinations <- which(valid_mask)
    flows <- row_flows[valid_mask]
    
    if (length(destinations) == 0) {
      # No neighbors - self-neighbor with 0 weight
      neighbors_list[[origin]] <- origin
      weights_list[[origin]] <- 0
    } else {
      neighbors_list[[origin]] <- as.integer(destinations)
      weights_list[[origin]] <- as.numeric(flows)
    }
  }
  
  return(list(neighbors = neighbors_list, weights = weights_list))
}

# Create outgoing mobility neighbors with threshold
message("Creating outgoing mobility neighbors (threshold = 5)...")
outgoing_mobility <- create_outgoing_neighbors(od_mat, threshold = 5)
neighbors_list_i <- outgoing_mobility$neighbors
weights_list_i <- outgoing_mobility$weights

# Set class attributes for spdep compatibility
class(neighbors_list_i) <- "nb"
attr(neighbors_list_i, "region.id") <- as.character(1:length(neighbors_list_i))
attr(neighbors_list_i, "zero.policy") <- TRUE

# Check if neighbor structure is symmetric
is_symmetric <- is.symmetric.nb(neighbors_list_i)
message("  Mobility neighbors created")
message("  Symmetric structure: ", is_symmetric)
message("  Average destinations per origin: ", 
        round(mean(sapply(neighbors_list_i, length)), 1))

# Create mobility-weighted matrix
mobility_weights_matrix <- nb2mat(
  neighbors_list_i, 
  glist = weights_list_i, 
  style = "W", 
  zero.policy = TRUE
)

# Create INLA graph from mobility weights
mobility_graph <- inla.read.graph(mobility_weights_matrix)
inla.write.graph(mobility_graph, "data/graphs/mobility_graph.txt")

# ==============================================================================
# PREPARE TEMPORAL AND HIERARCHICAL INDICES
# ==============================================================================

message("\n=== Preparing Temporal and Hierarchical Indices ===")

# Week indices -----------------------------------------------------------------
london_df$week <- week(as.Date(london_df$date))

# Create duplicate week columns for INLA model specification
london_df$week1 <- london_df$week  # For LAD-specific temporal effects
london_df$week2 <- london_df$week  # Additional copy if needed

# MSOA indices -----------------------------------------------------------------
# Create duplicate MSOA ID columns for different random effects
london_df$new_id1 <- london_df$new_id  # For spatial structured effect
london_df$new_id2 <- london_df$new_id  # For spatial unstructured effect

# Space-time interaction index
london_df$msoa_week <- interaction(london_df$new_id, london_df$week)
london_df$msoa_week <- as.numeric(london_df$msoa_week)

# LAD indices ------------------------------------------------------------------
message("Adding LAD hierarchical structure...")

# Join LAD codes to data
london_df <- london_df |>
  left_join(
    select(msoa, MSOA11CD, LAD11CD), 
    by = c("areaCode" = "MSOA11CD")
  )

# Create numeric LAD IDs
lad_list <- lad$LAD11CD
london_df$lad_id <- create_numeric_id(london_df$LAD11CD, lad_list)

# Month index (for seasonal effects if needed)
london_df$month <- as.integer(format(as.Date(london_df$date), "%m"))

# Remove geometry column if present
london_df <- london_df |> 
  select(-any_of("geometry"))
