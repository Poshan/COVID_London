# ==============================================================================
# 04_od_matrix_prep.R
# Process Origin-Destination matrices for mobility network analysis
#
# Input: ONS Origin-Destination estimates, MSOA boundaries
# Output: Processed OD matrix with serial numbering
# ==============================================================================

# Load required libraries ------------------------------------------------------
library(tidyr)
library(sf)
library(dplyr)
library(arrow)

# Load OD matrix data ----------------------------------------------------------
message("Loading Origin-Destination matrix...")

# Source: ONS Travel to work estimates during coronavirus
# https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/traveltoworkestimatesusingassumedtravelbehavioursduringcoronavirus
od_matrix <- read.csv('data/OD_matrix_COVID/ons-des-prod-mobility-outenc-ingress_encrypt_estODMatrix_msoa_MidPandemic_df.csv')

# Load MSOA boundaries for London ---------------------------------------------
msoa <- read_sf('data/Boundary/MSOA_2011_London_gen_MHW.shp') |> 
  st_transform('OGC:CRS84')

london_msoas <- unique(msoa$MSOA11CD)
message("Processing ", length(london_msoas), " London MSOAs")

# Filter OD matrix for London area --------------------------------------------
message("Filtering OD matrix for London...")

# Keep only flows within London (both origin and destination in London)
od_matrix <- od_matrix |>
  filter(MSOA11CD %in% london_msoas) |>
  select(all_of(c(london_msoas, "MSOA11CD")))

# Rename origin column
names(od_matrix)[names(od_matrix) == 'MSOA11CD'] <- 'origin'

# Reorder columns with origin first
od_matrix <- od_matrix |> 
  select(origin, everything())

# Create serial numbering system ----------------------------------------------
message("Creating serial numbering system...")

# Map MSOA codes to serial numbers for easier matrix manipulation
mapping_df <- data.frame(
  region_id = names(od_matrix)[-1],  # Exclude 'origin' column
  serial_number = seq_along(names(od_matrix)[-1])
)

# Function to convert MSOA codes to serial numbers
code_to_serial <- function(code, mapping) {
  mapping$serial_number[match(code, mapping$region_id)]
}

# Replace origin codes with serial numbers
od_matrix$new_id <- code_to_serial(od_matrix$origin, mapping_df)
od_matrix <- od_matrix |> select(-origin)

# Replace column names with serial numbers
new_colnames <- mapping_df$serial_number[
  match(names(od_matrix)[-length(names(od_matrix))], mapping_df$region_id)
]
names(od_matrix)[-length(names(od_matrix))] <- as.integer(new_colnames)

# Reorganize matrix with new numbering
od_matrix <- od_matrix[, c("new_id", new_colnames)]
names(od_matrix)[1] <- "origin"

# Sort by origin and set row names
od_matrix <- od_matrix[order(od_matrix$origin), ]
rownames(od_matrix) <- od_matrix$origin

# Save processed OD matrix -----------------------------------------------------
output_file <- "data/processed_data/origin_destination_matrix.parquet"
write_parquet(od_matrix, output_file)

message("OD matrix saved to: ", output_file)
message("Matrix dimensions: ", nrow(od_matrix), " x ", ncol(od_matrix))

# Analyze OD matrix characteristics (optional) --------------------------------
analyze_od_matrix <- TRUE  # Set to FALSE to skip analysis

if (analyze_od_matrix) {
  message("\nAnalyzing OD matrix characteristics...")
  
  # Convert to long format for analysis
  od_long <- od_matrix |>
    pivot_longer(
      cols = -origin,
      names_to = "dest",
      values_to = "count"
    ) |>
    mutate(
      origin = as.integer(origin),
      dest = as.integer(dest)
    )
  
  # Calculate flow statistics
  
  # Incoming flows (sum by destination)
  incoming_flows <- od_long |>
    group_by(dest) |>
    summarise(
      total_incoming = sum(count, na.rm = TRUE),
      .groups = 'drop'
    ) |>
    left_join(mapping_df, by = c("dest" = "serial_number"))
  
  # Outgoing flows (sum by origin)
  outgoing_flows <- od_long |>
    group_by(origin) |>
    summarise(
      total_outgoing = sum(count, na.rm = TRUE),
      .groups = 'drop'
    ) |>
    left_join(mapping_df, by = c("origin" = "serial_number"))
  
  # Within-area flows (diagonal elements)
  within_flows <- od_long |>
    filter(origin == dest) |>
    left_join(mapping_df, by = c("origin" = "serial_number"))
  
  # Get MSOA names for better interpretation
  msoa_names <- msoa |>
    st_drop_geometry() |>
    select(MSOA11CD, MSOA11NM)
  
  # Add names to flow statistics
  incoming_flows <- incoming_flows |>
    left_join(msoa_names, by = c("region_id" = "MSOA11CD"))
  
  outgoing_flows <- outgoing_flows |>
    left_join(msoa_names, by = c("region_id" = "MSOA11CD"))
  
  within_flows <- within_flows |>
    left_join(msoa_names, by = c("region_id" = "MSOA11CD"))
  
  # Report top areas
  message("\nTop 5 MSOAs by incoming movement:")
  top_incoming <- incoming_flows |> 
    arrange(desc(total_incoming)) |> 
    head(5)
  print(top_incoming[, c("MSOA11NM", "total_incoming")])
  
  message("\nTop 5 MSOAs by outgoing movement:")
  top_outgoing <- outgoing_flows |> 
    arrange(desc(total_outgoing)) |> 
    head(5)
  print(top_outgoing[, c("MSOA11NM", "total_outgoing")])
  
  message("\nTop 5 MSOAs by within-area movement:")
  top_within <- within_flows |> 
    arrange(desc(count)) |> 
    head(5)
  print(top_within[, c("MSOA11NM", "count")])
  
  # Save flow statistics
  write_parquet(incoming_flows, "data/processed_data/od_incoming_flows.parquet")
  write_parquet(outgoing_flows, "data/processed_data/od_outgoing_flows.parquet")
  write_parquet(within_flows, "data/processed_data/od_within_flows.parquet")
}

# Optional: Create flow visualization with flowmapblue ------------------------
create_flowmap <- FALSE  # Set to TRUE to create interactive flow map

if (create_flowmap) {
  library(flowmapblue)
  
  message("\nCreating flow map visualization...")
  
  # Prepare location data
  locations <- msoa |>
    select(MSOA11CD) |>
    mutate(
      id = MSOA11CD,
      name = MSOA11CD,
      centroid = st_centroid(geometry),
      lon = st_coordinates(centroid)[, 1],
      lat = st_coordinates(centroid)[, 2]
    ) |>
    st_drop_geometry() |>
    select(id, name, lon, lat) |>
    left_join(mapping_df, by = c("id" = "region_id")) |>
    mutate(id = as.character(serial_number))
  
  # Prepare flow data
  flows <- od_long |>
    mutate(
      origin = as.character(origin),
      dest = as.character(dest)
    )
  
  # Create interactive map
  flowmapblue(
    locations, 
    flows,
    mapboxAccessToken = "pk.eyJ1IjoicG9zaGFuIiwiYSI6InZ5a0dsLVkifQ.DGY81ha1q5VQpj3QA-REew",
    clustering = TRUE,
    darkMode = FALSE,
    animation = FALSE
  )
}

message("\nOD matrix processing complete!")
