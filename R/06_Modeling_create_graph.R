library(sf)
library(dplyr)
library(tidyr)
library(spdep)
library(arrow)
library(INLA)


##set working directory

##load london_df
london_df <- read_parquet("England/data/processed_data/london_weekly_data_with_caserate.parquet")

##graph based on geography -------------------------------------
msoa <- read_sf('England/data/Boundary/MSOA_2011_London_gen_MHW.shp') |> 
  st_transform('OGC:CRS84') |> 
  filter(MSOA11CD %in% unique(london_df$areaCode))

##change the id in the msao
list_msoa <- msoa$MSOA11CD
change_id <- function(code){
  return(match(code, list_msoa))
}
london_df$new_id <- change_id(london_df$areaCode)

plot(msoa$geometry)

##queen type neighborhood structure

msoa.neg = poly2nb(st_make_valid(msoa), queen = TRUE)
##weight matrix from the neighborhood structure with Binary style / Row standardized (W)/ global standardized (C)/ Variance stabilizing (S)

msoa_W <- nb2mat(msoa.neg, style = "W", zero.policy = TRUE)

##create graph from the weight matrix
msoa.graph <- inla.read.graph(msoa_W)

##plot the graphs


## graph based on OD-matrices -----------------------------
od_matrix <- read_parquet("England/data/processed_data/origin_destination_matrix.parquet")
### Outgoing mobility graph (not applied only for demonstration purporse) -------------------------------

## function to compute the neighbors and weights based on OD-matrices
outgoing_neighbors <- function(od_matrix, threshold) {

  ## Parameters:
  ### od_matrix: containing the OD values including origin column
  ### threshold: minimum value of a movement value to consider the regions neighbors

  # values less than threshold set to NA
  od_matrix[od_matrix <= threshold] <- NA

  # Remove the origin column as the rownames are same as the origin
  od_matrix |> subset(select = -c(origin)) -> od_matrix

  # Initialize lists for neighbors and weights
  neighbors_list <- list()
  weights_list <- list()

  # Loop over each row (origin) in the dataframe
  for (origin in rownames(od_matrix)) {
    # Get the row corresponding to the origin, removing NA values
    row_values <- od_matrix[origin, , drop = FALSE]

    # Get the destinations with non-NA values and their counts
    destinations <- as.integer(names(row_values)[!is.na(row_values)])
    destinations <- destinations[destinations != as.integer(origin)] # Remove the origin itself

    ### compute ratio instead of "W" method instead of raw movement data
    weights <- row_values[!is.na(row_values)]

    # Add the origin and its destinations to the lists
    neighbors_list[[as.integer(origin)]] <- destinations
    weights_list[[as.integer(origin)]] <- as.numeric(weights)
  }

  return(list(neighbors = neighbors_list, weights = weights_list))
}
# outgoing <- outgoing_neighbors(od_matrix,30)
# neighbors_list_o <- outgoing$neighbors
# class(neighbors_list_o) <- "nb"
# is.symmetric.nb(neighbors_list_o)


### Incoming mobility graph -------------------------------
# Function to find neighbors based on threshold
incoming_neighbors <- function(od_matrix, threshold) {
  od_matrix[od_matrix <= threshold ] <- NA
  
  od_matrix |> subset(select = -c(origin)) -> od_matrix
  
  neighbors_list <- list()
  weights_list <- list()
  
  # Iterate through each destination column
  for (destination in colnames(od_matrix)) {

    ##all the column values
    col_values <- od_matrix[,destination, drop=FALSE]
    
    #get all the origins that have non-NA values and the counts
    origins <- as.integer(rownames(col_values)[!is.na(col_values[, 1])])
    weights <- col_values[!is.na(col_values)]
    
    # Add the destinations and their corresponding origins to the lists
    if (length(origins) == 0) {
      neighbors_list[[as.integer(destination)]] <- as.integer(destination)
      weights_list[[as.integer(destination)]] <- as.integer(0)
    } else {
      neighbors_list[[as.integer(destination)]] <- origins
      weights_list[[as.integer(destination)]] <- as.integer(weights)
    }

  }
  
  return(list(neighbors = neighbors_list, weights = weights_list))
}

incoming <-incoming_neighbors(od_matrix, 10)
neighbors_list_i <- incoming$neighbors
weights_list_i <- incoming$weights


class(neighbors_list_i) <- "nb"
attr(neighbors_list_i, "region.id") <- as.character(1:length(neighbors_list_i))
attr(neighbors_list_i, "zero.policy") <- TRUE

## check symmetricity
is.symmetric.nb(neighbors_list_i)


### Create graph ------
weights_matrix <- nb2mat(neighbors_list_i, glist = weights_list_i, style = "W", zero.policy = TRUE)

mobility_graph_wt <- inla.read.graph(weights_matrix)

plot(mobility_graph_wt)


