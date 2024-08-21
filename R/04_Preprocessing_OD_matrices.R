library(tidyr)
library(dplyr)
library(arrow)
##read london_df
london_df <- read_parquet('England/data/processed_data/london_weekly_data_with_caserate.parquet')


## load origin destination matrix and filter within the spatial range ---------

od_matrix <- read.csv('England/data/OD_matrix_COVID/ons-des-prod-mobility-outenc-ingress_encrypt_estODMatrix_msoa_MidPandemic_df.csv')

# filter the OD matrices within the LONDON area
london_msoas <- unique(london_df$areaCode)
#in origin
od_matrix |>filter(MSOA11CD %in% london_msoas) -> od_matrix
##in destination
od_matrix |> select(any_of(c(london_msoas, "MSOA11CD"))) -> od_matrix

#change the column name to origin
names(od_matrix)[names(od_matrix) == 'MSOA11CD'] <- 'origin'

##bring origin to the front
od_matrix |> select(origin, everything()) -> od_matrix

## change the MSOA names to serial numbers -------------------------------------
# Create the mapping dataframe (to map the MSOA code to serial number)
mapping_df <- data.frame(
  region_id = names(od_matrix)[-1],
  serial_number = 1: length(names(od_matrix)[-1]) 
)

# Function to change origin values
change_id <- function(code, mapping_df) {
  return(mapping_df$serial_number[match(code, mapping_df$region_id)])
}

##rownames
od_matrix$new_id <- change_id(od_matrix$origin, mapping_df)
od_matrix <- od_matrix |> select(-origin)


# Replace the column names with serial numbers
new_colnames <- mapping_df$serial_number[match(names(od_matrix)[-length(names(od_matrix))], 
                                               mapping_df$region_id)]
names(od_matrix)[-length(names(od_matrix))] <- as.integer(new_colnames)


# Reorder columns to move new_id to the first column and rename it to origin
od_matrix <- od_matrix[, c("new_id", new_colnames)]
names(od_matrix)[1] <- "origin"

od_matrix <- od_matrix[order(od_matrix$origin),]
rownames(od_matrix) <- od_matrix$origin

write_parquet(od_matrix, "England/data/processed_data/origin_destination_matrix.parquet")


