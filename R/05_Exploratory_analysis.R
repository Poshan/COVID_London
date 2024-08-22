library(ggplot2)
library(dplyr)
library(tidyr)
library(arrow)
library(lubridate)
library(flexplot)
# 
london_df <- read_parquet("England/data/processed_data/london_weekly_data_with_caserate.parquet")


## Temporal plots -------------------------------------------------------------
### Calculate overall weekly mean caserate ----------------------------------
weekly_mean_caserate <- london_df |>
  group_by(date) |>
  summarise(mean_caserate = mean(caserate, na.rm = TRUE))

london_df <- left_join(london_df, weekly_mean_caserate, by = "date")
### Calculate overall weekly mean activity ----------------------------------
weekly_mean_activitys_act <- london_df |>
  group_by(date) |>
  summarise(mean_activity = mean(weekly_mean_activity, na.rm = TRUE))

london_df <- left_join(london_df, weekly_mean_activitys_act, by = "date")


### Temporal plot of caserate --------------------------------------------------
# Plot using ggplot
p<-ggplot(london_df, aes(x = date, y = caserate, group = areaCode, color = areaCode)) +
  geom_line() +
  geom_line(aes(y = mean_caserate), color = "black", linetype = "dashed") +
  labs(x = "date", y = "Case Rate") +
  # ggtitle("COVID-19 cases per 100000 population") +
  theme_minimal() +
  theme(legend.position = "none",
        # axis.title.x=element_blank(),
        # axis.text.x = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_x_date(date_breaks = "3 month") + 
  geom_vline(xintercept = as.Date(c("2020-03-23", "2020-05-10", "2020-06-01", 
                                    "2020-07-04", "2020-09-14", "2020-11-05")),
                                      linetype = "dotted", color = "red") +
  annotate("text", x = as.Date("2020-03-23"), y = max(london_df$caserate)*0.9, label = "Lockdown 1", hjust = -0.1) +
  annotate("text", x = as.Date("2020-05-10"), y = max(london_df$caserate)*0.9, label = "Ease\n Down \n 1", hjust = -0.1) +
  annotate("text", x = as.Date("2020-06-01"), y = max(london_df$caserate)*0.9, label = "Ease\n Down \n 2", hjust = -0.1) +
  annotate("text", x = as.Date("2020-07-04"), y = max(london_df$caserate)*0.9, label = "EaseDown 3", hjust = -0.1) +
  annotate("text", x = as.Date("2020-09-14"), y = max(london_df$caserate)*0.9, label = "Voluntary \n Restrictions", hjust = -0.1) +
  annotate("text", x = as.Date("2020-11-05"), y =  max(london_df$caserate)*0.9, label = "Lockdown 2", hjust = -0.1)

ggsave("England/plots/temporal_series_caserate.png", plot = p, width = 10, height = 6, dpi = 300)

### Temporal plot of mean activity----------------------------------------------
p2<-ggplot(london_df, aes(x = date, y = weekly_mean_activity, group = areaCode, color = areaCode)) +
  geom_line() +
  geom_line(aes(y = mean_activity), color = "black", linetype = "dashed") +
  labs(x = "Date", y = "Activity index") +
  # ggtitle("Weekly Mean Activity") +
  theme_minimal()+
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  scale_x_date(date_breaks = "3 month") +
  geom_vline(xintercept = as.Date(c("2020-03-23", "2020-05-10", "2020-06-01", 
                                                                           "2020-07-04", "2020-09-14", "2020-11-05")),
                                                    linetype = "dotted", color = "red") +
  annotate("text", x = as.Date("2020-03-23"), y = max(london_df$weekly_mean_activity)*0.9, label = "Lockdown 1", hjust = -0.1) +
  annotate("text", x = as.Date("2020-05-10"), y = max(london_df$weekly_mean_activity)*0.9, label = "Ease\n Down \n 1", hjust = -0.1) +
  annotate("text", x = as.Date("2020-06-01"), y = max(london_df$weekly_mean_activity)*0.9, label = "Ease\n Down \n 2", hjust = -0.1) +
  annotate("text", x = as.Date("2020-07-04"), y = max(london_df$weekly_mean_activity)*0.9, label = "EaseDown 3", hjust = -0.1) +
  annotate("text", x = as.Date("2020-09-14"), y = max(london_df$weekly_mean_activity)*0.9, label = "Voluntary \n Restrictions", hjust = -0.1) +
  annotate("text", x = as.Date("2020-11-05"), y =  max(london_df$weekly_mean_activity)*0.9, label = "Lockdown 2", hjust = -0.1)

ggsave("England/plots/temporal_series_activity.png", plot = p2, width = 10, height = 6, dpi = 300)


## Identify regions with extreme cases -----------------------------------------
## using z score
london_df <- london_df |> 
  group_by(wave) |>
  mutate(mean_cases = mean(caserate),
         sd_cases = sd(caserate),
         z_score_cases = (caserate-mean_cases)/sd_cases) |>
  ungroup()

extreme_cases <- london_df |>
  filter(abs(z_score_cases) > 3)


## Spatial Plot ----------------------------------------------------------------
###spatial plots
library(sf)
read_sf('England/data/Boundary/MSOA_2011_London_gen_MHW.shp') |> 
  st_transform('OGC:CRS84') |> 
  filter(MSOA11CD %in% unique(london_df$areaCode)) -> 
  msoa 


##join geometry in the london_df df
london_df |> inner_join(msoa, by = c("areaCode"="MSOA11CD")) -> london_df
st_as_sf(london_df) -> london_df

#
date_from <- "2020-12-06"
date_upto <- "2020-12-08"

plot_caserate_activity_spatial <- function (date_from, date_upto){  
  p1<-ggplot(subset(london_df, date > date_from & date < date_upto)) +
    geom_sf(aes(fill = weekly_mean_activity), linewidth=0.0001, alpha=0.9) + 
    scale_fill_viridis_c(
      # trans = "sqrt",
      # breaks=c(0, 100, 200, 500, 1700),
      # name = "caserate"
    )+
    facet_wrap(~ date, ncol = 3) +  # Arrange plots in a grid of 5 columns
    labs(title = "mean activity") +
    theme_minimal() +
    theme(legend.position.inside = c(0.5, 0),
          axis.text = element_blank(),  # Remove axis text for better visualization
          axis.title = element_blank())   # Remove axis titles
  
  
  p2 <-ggplot(subset(london_df, date > date_from & date < date_upto)) +
    geom_sf(aes(fill = caserate), linewidth=0.0001, alpha=0.9) + 
    scale_fill_viridis_c(
      # trans = "sqrt",
      # breaks=c(0, 100, 200, 500, 1700),
      # name = "caserate"
    )+
    facet_wrap(~ date, ncol = 3) +  # Arrange plots in a grid of 5 columns
    labs(title = "caserate") +
    theme_minimal() +
    theme(legend.position.inside = c(0.5, 0),
          axis.text = element_blank(),  # Remove axis text for better visualization
          axis.title = element_blank())   # Remove axis titles
  return (list(activity_plot = p1, caserate_plot = p2))
}  
plots <- plot_caserate_activity_spatial(date_from, date_upto)

ggsave("England/plots/VR_caserate.png", plots$caserate_plot)
ggsave("England/plots/VR_activity.png", plots$activity_plot)


## flexplot --------------------------------------------------------------------
london_df |> 
  flexplot(caserate ~ weekly_mean_activity | wave, data = _, method = "poisson")

## Activity_aggregated at different levels ------------------------------------

##plot a days activity
##plot a days activity
activity_grid_1000 <- read_parquet("England/data/processed_data/activity_in_1000m_grids.parquet")
grid_1000 <- st_read("England/data/processed_data/1000m_grids.shp")
activity_grid_1000 |> filter(AGG_DAY_PERIOD == "2020-03-08") |> left_join(grid_1000, by = c("grid_ID" = "grid_ID")) ->day_df

library(ggplot2)
ggplot(st_as_sf(day_df)) +
  geom_sf(aes(fill = grid_mean), linewidth=0.0001, alpha=0.9) + 
  scale_fill_viridis_c(
    # trans = "sqrt",
    # breaks=c(0, 100, 200, 500, 1700),
    # name = "caserate"
  )+
  # facet_wrap(~ date, ncol = 3) +  # Arrange plots in a grid of 5 columns
  labs(title = "Activity") +
  theme_minimal() +
  theme(legend.position.inside = c(0.5, 0),
        axis.text = element_blank(),  # Remove axis text for better visualization
        axis.title = element_blank())   # Remove axis titles



