library(sf)
library(dplyr)
library(spacetime)
library(sp)
library(arrow)

setwd('/palma/scratch/tmp/pniraula/London/')

options(digits = 19)
df <- read.csv('Activity_data/London_activity_data.csv')

#remove the bounds, as it makes the data huge
#df_1 <- select(df, -BOUNDS)
df_1 <- df

##Because the geography field in the data doesnot represent all the unique geog
##using the combination of the lon and lat to create a new dataframe and 
##unique id LONLAT_ID 

df_1 |> distinct(XLON, XLAT) -> distinct_LONLAT

distinct_LONLAT$LONLAT_ID <- 1: length(distinct_LONLAT$XLON)
write_parquet(distinct_LONLAT, "distinct_LONLAT.parquet")

inner_join(df_1, distinct_LONLAT, by= c("XLON", "XLAT")) -> df_2

################################################################################
################################################################################
####          check how the duplicate values look like                       ###
################################################################################
################################################################################

# #checking the duplicates
duplicates <- df_1[duplicated(df_2[c("AGG_DAY_PERIOD", "LONLAT_ID")]) |
                     duplicated(df_2[c("AGG_DAY_PERIOD", "LONLAT_ID")], fromLast = TRUE), ]

# ##finding how different is the duplicate values
grouped_data <- duplicates %>%
  group_by(AGG_DAY_PERIOD, XLAT, XLON) %>%
  mutate(activity_range = max(ACTIVITY_INDEX_TOTAL) - min(ACTIVITY_INDEX_TOTAL),
         activity_sd = sd(ACTIVITY_INDEX_TOTAL))
#
# # Filter rows where there are differences in activity levels
differences_data <- grouped_data %>%
  filter(activity_range > 0 | activity_sd > 0)
#
# # Print the rows with differences
print(differences_data)

################################################################################
################################################################################
####          mean the values in the duplicated time and places              ###
################################################################################
################################################################################

# Group by relevant columns
# df_mean <- df_1 %>%
#   group_by(AGG_DAY_PERIOD, XLAT, XLON) %>%
#   summarize(mean_column = mean(ACTIVITY_INDEX_TOTAL, na.rm = TRUE))
# 
# df_mean$location <- paste(format(df_mean$XLAT, digits=19), format(df_mean$XLON, digits = 19), sep=',')


df_mean <- df_2 %>%
  group_by(AGG_DAY_PERIOD, LONLAT_ID) %>%
  summarize(mean_column = mean(ACTIVITY_INDEX_TOTAL, na.rm = TRUE))


################################################################################
################################################################################
####     To make sure there is datapoint for all the dates and all locations ###
###########     Filling the not available dates and geography by na ############
################################################################################
################################################################################

# Create a dataframe with all combinations of dates and geographies
all_combinations <- expand.grid(
  AGG_DAY_PERIOD = unique(df_mean$AGG_DAY_PERIOD),
  LONLAT_ID = unique(df_mean$LONLAT_ID)
)

# Left join with the original dataframe
merged_data <- left_join(all_combinations, df_mean, by = c("AGG_DAY_PERIOD", "LONLAT_ID"))

# Replace missing values in ACTIVITY_INDEX_TOTAL with 99
##merged_data$mean_column[is.na(merged_data$mean_column)] <- 99

##join with the unique_LONLAT dataframe if required to get the longitude and latitude

write_parquet(merged_data, "pre_processed_movement.parquet")

################################################################################
###############################################################################
################Preprocessing complete##########################################
################################################################################
###############################################################################



################################################################################
################################################################################
##A class for spatio-temporal data with full space-time grid; for n spatial 
##locations and m times, n x m observations are available 
################################################################################
################################################################################

##read the pre-processed parquet file
merged_data <- read_parquet("pre_processed_movement.parquet")

###give unique location an id



##get all the unique locations (n)
# unique_geography_df <- merged_data %>% distinct(location, .keep_all = TRUE)
unique_geography_df <- df_1 |> distinct(GEOGRAPHY, .keep_all = TRUE)
unique_geography_df$XLAT <- as.numeric(unique_geography_df$XLAT, length = 21)
unique_geography_df$XLON <- as.numeric(unique_geography_df$XLON, length = 21)
unique_geography_df <- unique_geography_df[,2:4]

write_parquet(unique_geography_df, "NYC/geography.parquet")





