library(ggplot2)
library(dplyr)
library(tidyr)
library(arrow)
library(lubridate)
library(flexplot)
# England
london_df <- read_parquet("data/processed_data/lagged/df_lagged.parquet")

## Temporal plots -------------------------------------------------------------
### Calculate overall weekly mean caserate ----------------------------------
london_df |>
  group_by(date) |>
  summarise(mean_caserate = mean(caserate, na.rm = TRUE)) -> 
  weekly_mean_caserate 

london_df <- left_join(london_df, weekly_mean_caserate, by = "date")
### Calculate overall weekly mean activity ----------------------------------
weekly_mean_activitys_act <- london_df |>
  group_by(date) |>
  summarise(mean_activity = mean(weekly_mean_activity, na.rm = TRUE))

london_df <- left_join(london_df, weekly_mean_activitys_act, by = "date")

##figure 1 
### Temporal plot of caserate --------------------------------------------------
# Plot using ggplot
p <- ggplot(london_df, aes(x = date, y = caserate, group = areaCode, color = areaCode)) +
  geom_line(size = 0.1, alpha = 0.5) +
  geom_line(aes(y = mean_caserate), color = "black", linetype = "dashed") +
  labs(x = "Date", y = "weekly incidence/100,000 population") +
  # ggtitle("Weekly Mean Activity") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        panel.grid.minor = element_line(color = "grey80", size = 0.5)) + # Ensure minor grid lines are added
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%b %Y",
               date_minor_breaks = "1 month") +
  # geom_vline(xintercept = as.Date(c("2020-03-23", "2020-05-10", "2020-06-01", 
  #                                   "2020-07-04", "2020-09-14", "2020-11-05")),
  geom_vline(xintercept = as.Date(c("2020-03-23", "2020-05-10", "2020-09-14", "2020-11-05")),
             linetype = "dotted", color = "red") +
  annotate("text", x = as.Date("2020-03-30"), y = max(london_df$caserate)*0.9, label = "Lockdown 1", hjust = -0.1) +
  annotate("text", x = as.Date("2020-06-20"), y = max(london_df$caserate)*0.9, label = "Easing phases", hjust = -0.1) +
  annotate("text", x = as.Date("2020-09-14"), y = max(london_df$caserate)*0.9, label = "Voluntary \n Restrictions", hjust = -0.1) +
  annotate("text", x = as.Date("2020-11-05"), y = max(london_df$caserate)*0.9, label = "Lockdown 2", hjust = -0.1)
# Show the plot
print(p)

# Save the plot with specified size and resolution
ggsave("plots/temporal_series_caserate.png", plot = p, width = 10, height = 6, dpi = 300)


##figure 3 
### Temporal plot of mean activity----------------------------------------------
p2<-ggplot(london_df, aes(x = date, y = weekly_mean_activity, group = areaCode, color = areaCode)) +
  geom_line(size = 0.1, alpha = 0.5) +
  geom_line(aes(y = mean_activity), color = "black", linetype = "dashed") +
  labs(x = "Date", y = "Activity index") +
  # ggtitle("Weekly Mean Activity") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        panel.grid.minor = element_line(color = "grey80", size = 0.5)) + # Ensure minor grid lines are added
  scale_x_date(date_breaks = "3 month", 
               date_labels = "%b %Y",
               date_minor_breaks = "1 month") +
  geom_vline(xintercept = as.Date(c("2020-03-23", "2020-05-10", "2020-09-14", "2020-11-05")),
             linetype = "dotted", color = "red") +
  annotate("text", x = as.Date("2020-03-30"), y = max(london_df$weekly_mean_activity)*0.9, label = "Lockdown 1", hjust = -0.1) +
  annotate("text", x = as.Date("2020-06-20"), y = max(london_df$weekly_mean_activity)*0.9, label = "Easing Phases", hjust = -0.1) +
  annotate("text", x = as.Date("2020-09-14"), y = max(london_df$weekly_mean_activity)*0.9, label = "Voluntary\nRestrictions", hjust = -0.1) +
  annotate("text", x = as.Date("2020-11-05"), y = max(london_df$weekly_mean_activity)*0.9, label = "Lockdown 2", hjust = -0.1)

print(p2)
ggsave("plots/temporal_series_activity.png", plot = p2, width = 10, height = 6, dpi = 300)



## Spatial Plot ----------------------------------------------------------------
###spatial plots
library(sf)
read_sf('data/Boundary/MSOA_2011_London_gen_MHW.shp') |> 
  st_transform('OGC:CRS84') |> 
  filter(MSOA11CD %in% unique(london_df$areaCode)) -> 
  msoa 


##join geometry in the london_df df
london_df |> inner_join(msoa, by = c("areaCode"="MSOA11CD")) -> london_df
st_as_sf(london_df) -> london_df

#

# 
# plot_caserate_activity_spatial <- function (date_from, date_upto){  
#   p1<-ggplot(subset(london_df, date > date_from & date < date_upto)) +
#     geom_sf(aes(fill = weekly_mean_activity), linewidth=0.0001, alpha=0.9) + 
#     scale_fill_viridis_c(
#       trans = "sqrt",
#       breaks=c(0,0.2,0.3,0.4,0.5),
#       name = "caserate"
#     )+
#     facet_wrap(~ date, ncol = 3) +  # Arrange plots in a grid of 5 columns
#     labs(title = "mean activity") +
#     theme_minimal() +
#     theme(legend.position.inside = c(0.5, 0),
#           axis.text = element_blank(),  # Remove axis text for better visualization
#           axis.title = element_blank())   # Remove axis titles
#   
#   
#   p2 <-ggplot(subset(london_df, date > date_from & date < date_upto)) +
#     geom_sf(aes(fill = caserate), linewidth=0.0001, alpha=0.9) + 
#     scale_fill_viridis_c(
#       trans = "sqrt",
#       breaks=c(0, 500, 1000, 1500),
#       name = "caserate"
#     )+
#     facet_wrap(~ date, ncol = 3) +  # Arrange plots in a grid of 5 columns
#     labs(title = "caserate") +
#     theme_minimal() +
#     theme(legend.position.inside = c(0.5, 0),
#           axis.text = element_blank(),  # Remove axis text for better visualization
#           axis.title = element_blank())   # Remove axis titles
#   return (list(activity_plot = p1, caserate_plot = p2))
# }  
# date_from <- "2020-03-23"
# date_upto <- "2020-05-10"
# plots <- plot_caserate_activity_spatial(date_from, date_upto)
# 
# ggsave("plots/VR_caserate.png", plots$caserate_plot)
# ggsave("plots/VR_activity.png", plots$activity_plot)
# 
# threshold <- quantile(london_df$weekly_mean_activity, 0.95, na.rm = T)
# p1<-ggplot(filter(london_df, (weekly_mean_activity <= threshold & date > date_from & date < date_upto))) +
#   geom_sf(aes(fill = weekly_mean_activity), linewidth=0.0001, alpha=0.9) + 
#   scale_fill_viridis_c(
#     # trans = "sqrt",
#     # breaks=c(0, 100, 200, 500, 1700),
#     # name = "caserate"
#   )+
#   facet_wrap(~ date, ncol = 3) +  # Arrange plots in a grid of 5 columns
#   labs(title = "mean activity") +
#   theme_minimal() +
#   theme(legend.position.inside = c(0.5, 0),
#         axis.text = element_blank(),  # Remove axis text for better visualization
#         axis.title = element_blank()) 
# 
# p2 <-ggplot(subset(london_df, date > date_from & date < date_upto)) +
#   geom_sf(aes(fill = caserate), linewidth=0.0001, alpha=0.9) + 
#   scale_fill_viridis_c(
#     # trans = "sqrt",
#     # breaks=c(0, 100, 200, 500, 1700),
#     # name = "caserate"
#   )+
#   facet_wrap(~ date, ncol = 3) +  # Arrange plots in a grid of 5 columns
#   labs(title = "caserate") +
#   theme_minimal() +
#   theme(legend.position.inside = c(0.5, 0),
#         axis.text = element_blank(),  # Remove axis text for better visualization
#         axis.title = element_blank())   # Remove axis titles


####to look at the possible extreme cases in the first lockdown and volunteered restrction period 
##study area figur ein paper

###drop geometry if london_Df is a sf ovject

if ("sf" %in% class(london_df)){
  london_df |> st_drop_geometry() -> london_df
}


# Calculate cumulative cases per wave for each region
cumulative_wave_df <- london_df |>
  arrange(areaCode, date) |>
  group_by(areaCode, Population, wave) |>
  summarise(cumulative_cases = sum(weekly_cases, na.rm = TRUE)) |>  # Ensure only one geometry per region
  ungroup()

cumulative_wave_df |> left_join(msoa, by = c("areaCode" = "MSOA11CD")) |> st_as_sf() -> cumulative_wave_df

# # Plotting with facets for each wave
# ggplot(filter(cumulative_wave_df, wave %in% c("lockdown 1", "Easedown 2", "Volunteered restrictions", "lockdown 2"))) +
#   geom_sf(aes(fill = cumulative_cases)) +
#   scale_fill_viridis_c(option = "plasma", na.value = "grey50") +
#   labs(title = "Cumulative COVID-19 Cases by Wave",
#        fill = "Cumulative Cases") +
#   theme_minimal() +
#   theme(axis.text.x = element_blank(), 
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(), 
#         axis.title = element_blank()) +
#   facet_wrap(~ wave, ncol = 2)



# wave_abbreviations <- c(
#   "lockdown 1" = "ld1",
#   "Easedown 2" = "ED",
#   "Volunteered restrictions" = "VR",
#   "lockdown 2" = "ld2"
# )
# 
# 
# 
# 
# unique_waves <- c("lockdown 1", "Easedown 2", "Volunteered restrictions", "lockdown 2")
# 
# for (wave in unique_waves) {
#   print(wave)
#   plot_data <- filter(cumulative_wave_df, wave == !!wave)
#   
#   p<-ggplot(plot_data) +
#     geom_sf(aes(fill = 100000 * cumulative_cases/Population)) +
#     scale_fill_viridis_c() +
#     labs(fill = "") +
#     theme_minimal() +
#     theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
#           axis.ticks = element_blank(), axis.title = element_blank())
#   print(p)
#   filename = paste0("plots/cum_sum_", wave_abbreviations[toString(wave)],".png")
#   ggsave(filename, p)
# }


##TAbles in appendix

top_5_caserates_df <- london_df |>
  group_by(wave) |>  # Group by wave
  arrange(desc(caserate)) |>  # Arrange by descending order of caserate
  slice_head(n = 50) |>  # Select the top 5 rows for each group
  select(areaCode, wave, date, caserate)  # Select relevant columns

top_5_caserates_df |> 
  left_join(msoa, by = c("areaCode"="MSOA11CD")) |> 
  select(any_of(c("areaCode", "MSOA11NM","wave","date", "caserate"))) ->
  top_5_caserates_df



top_5_activity_df_s <- london_df |>
  group_by(wave) |>  # Group by wave
  arrange(desc(weekly_sum_activity)) |>  # Arrange by descending order of caserate
  slice_head(n = 50) |>  # Select the top 5 rows for each group
  select(areaCode, wave,date, weekly_sum_activity)  # Select relevant columns

# View the results

top_5_activity_df_s |> 
  left_join(msoa, by = c("areaCode"="MSOA11CD")) |> 
  select(any_of(c("areaCode", "MSOA11NM","wave","date", "weekly_sum_activity"))) ->
  top_5_activity_df_s


top_5_activity_df <- london_df |>
  group_by(wave) |>  # Group by wave
  arrange(desc(weekly_mean_activity)) |>  # Arrange by descending order of caserate
  slice_head(n = 50) |>  # Select the top 5 rows for each group
  select(areaCode, wave, date, weekly_mean_activity)  # Select relevant columns

# View the results

top_5_activity_df |> 
  left_join(msoa, by = c("areaCode"="MSOA11CD")) |> 
  select(any_of(c("areaCode", "MSOA11NM","wave", "date","weekly_mean_activity"))) ->
  top_5_activity_df


##join top of all three to evaluate
top_5_caserates_df |> 
  left_join(top_5_activity_df_s, by = c("areaCode", "date")) |>
  left_join(top_5_activity_df, by = c("areaCode", "date")) ->
  all_combined




## flexplot --------------------------------------------------------------------
london_df$caserate[london_df$caserate<0] <- 0
london_df |> 
  flexplot(caserate ~ weekly_mean_activity | wave, data = _, method = "poisson")

## Activity_aggregated at different levels ------------------------------------
# 
# ##plot a days activity
# ##plot a days activity
# activity_grid_1000 <- read_parquet("data/processed_data/activity_in_1000m_grids.parquet")
# grid_1000 <- st_read("data/processed_data/1000m_grids.shp")
# activity_grid_1000 |> filter(AGG_DAY_PERIOD == "2020-10-05") |> left_join(grid_1000, by = c("grid_ID" = "grid_ID")) ->day_df
# 
# # "2020-10-05" "2020-09-28"
# 
# library(ggplot2)
# ggplot(st_as_sf(day_df)) +
#   geom_sf(aes(fill = grid_mean), linewidth=0.0001, alpha=0.9) + 
#   scale_fill_viridis_c(
#     # trans = "sqrt",
#     # breaks=c(0, 100, 200, 500, 1700),
#     # name = "caserate"
#   )+
#   # facet_wrap(~ date, ncol = 3) +  # Arrange plots in a grid of 5 columns
#   labs(title = "Activity") +
#   theme_minimal() +
#   theme(legend.position.inside = c(0.5, 0),
#         axis.text = element_blank(),  # Remove axis text for better visualization
#         axis.title = element_blank())   # Remove axis titles
# 
# 
# 
# 
# ###list of MSOAs with high activity index in different phases
# ## Identify regions with extreme cases -----------------------------------------
# ## using z score
# london_df <- london_df |> 
#   group_by(wave) |>
#   mutate(mean_activity = mean(weekly_mean_activity),
#          sd_activity = sd(weekly_mean_activity),
#          z_score_cases = (weekly_mean_activity-mean_activity)/sd_activity) |>
#   ungroup()
# 
# extreme_activity <- london_df |>
#   filter(abs(z_score_cases) > 3)
# 
# 
# 
# library(sf)
# read_sf('data/Boundary/MSOA_2011_London_gen_MHW.shp') |> 
#   st_transform('OGC:CRS84') |> 
#   filter(MSOA11CD %in% unique(london_df$areaCode)) -> 
#   msoa 
# 
# 
# 
# msoa_names <- msoa |> 
#   st_drop_geometry() |>
#   select(any_of(c("MSOA11CD", "MSOA11NM")))
# 
# extreme_activity |> 
#   filter(wave == "lockdown 2") |>
#   select(any_of(c("areaCode", "date", "weekly_mean_activity"))) |>
#   left_join(msoa_names, by= c("areaCode"="MSOA11CD")) |>
#   arrange(desc(weekly_mean_activity))


# ##map showing highest top 5 activity index regions in different phases
# 
# plot_top_activity <- function(date){
#   ggplot(msoa |> 
#          left_join(extreme_activity |> 
#                      filter(date == date), by = c("MSOA11CD"="areaCode"))) +
#   geom_sf(aes(fill = weekly_mean_activity), linewidth=0.0001, alpha=0.9) + 
#   scale_fill_viridis_c(
#     # trans = "sqrt",
#     # breaks=c(0, 100, 200, 500, 1700),
#     # name = "caserate"
#   )+
#   # facet_wrap(~ date, ncol = 3) +  # Arrange plots in a grid of 5 columns
#   labs(title = paste0("Activity", date)) +
#   theme_minimal() +
#   theme(legend.position.inside = c(0.5, 0),
#         axis.text = element_blank(),  # Remove axis text for better visualization
#         axis.title = element_blank())  
# }
# 
# plot_top_activity(2020-12-21)

## Figure 2
cumulative_wave_df$cumulative_cases <- 0.0001 + cumulative_wave_df$cumulative_cases 
cumulative_wave_df$log_cases <- log10(cumulative_wave_df$cumulative_cases)
###plots for paper


selected_cum_df<-cumulative_wave_df |> 
  filter(wave %in% c("lockdown 1", "Easedown 1", "Volunteered restrictions", "lockdown 2"))

selected_cum_df$wave_f <- factor(selected_cum_df$wave, levels = c("lockdown 1", "Easedown 1", "Volunteered restrictions", "lockdown 2"))

library(scales)
# p <- ggplot(data = selected_cum_df) +
#   geom_sf(aes(fill = cumulative_cases))+ 
#   facet_wrap(~wave_f, nrow = 2) +  
#   scale_fill_viridis_c(option = "H", trans = "sqrt",
#                        labels = label_number(accuracy = 1, big.mark = ""))+ # Ensure facets are arranged in two rows
#   theme_minimal() +
#   theme(
#     axis.title = element_blank(),
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     strip.text = element_blank()  # Remove facet titles
#   ) +
#   labs(fill = "Cumulative weekly incidence \n per 100,000 population")


p <- ggplot(selected_cum_df) +
  geom_sf(aes(fill = cumulative_cases)) +
  facet_wrap(~wave_f, nrow = 2,
             labeller = as_labeller(c(
               "lockdown 1" = "Lockdown 1", "Easedown 1" = "Easing Phase", "Volunteered restrictions" = "Voluntary restrictions", "lockdown 2" = "Lockdown 2"
             ))) +
  scale_fill_viridis_c(option = "H", trans = "sqrt",
                       labels = label_number(accuracy = 1, big.mark = "")) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text()) +
  labs(fill = "Cumulative weekly incidence \n per 100,000 population")

library(grid)

p1<-p +
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(direction = "horizontal",
                               title.position = "top",
                               barwidth = unit(8, "cm"),
                               barheight = unit(3, "mm")))


# Show the plot
print(p)


ggsave("plots/cum_sum_cases.png", plot = p1, dpi = 300)




##additional plots
## Correlation between caserate and variables of interest for all areaCode for each week ---------------
# 
# london_df |> 
#   group_by(date) |> 
#   summarise(cor = cor(caserate, weekly_mean_activity),
#             cor_moblag = cor(caserate, MobilityLag)) -> 
#   correlation_temporal_df
# 
# 
# correlation_temporal_df_long <- correlation_temporal_df |>
#   pivot_longer(
#     cols = starts_with("cor"), 
#     names_to = "correlation_type", 
#     values_to = "correlation"
#   )
# 
# # Plot all correlations
# p3<-ggplot(correlation_temporal_df_long, aes(x = date, y = correlation, color = correlation_type)) +
#   geom_line() +
#   geom_point() +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
#   labs(title = "Temporal Changes in Correlation between caserate and activity",
#        x = "Date",
#        y = "Correlation",
#        color = "Correlation Type") +
#   theme_minimal() +
#   theme(legend.position = "bottom")
# 
# ggsave("England/plots/correlation_evolution.png", p3)
# 

## Correlation between the caserate and the variables for all weeks for each areaCode ----------
# london_df |> 
#   group_by(areaCode) |> 
#   summarise(cor = cor(caserate, weekly_mean_activity),
#             cor_weeklag1 = cor(caserate, weekly_mean_activity_lag1),
#             cor_moblag = cor(caserate, MobilityLag)) -> 
#   correlation_spatial_df
# 
# 
# library(sf)
# read_sf('data/Boundary/MSOA_2011_London_gen_MHW.shp') |> 
#   st_transform('OGC:CRS84') |> 
#   filter(MSOA11CD %in% unique(london_df$areaCode)) -> 
#   msoa 
# 
# correlation_spatial_df |> 
#   left_join(msoa, by = c("areaCode" = "MSOA11CD")) |> 
#   st_as_sf() ->
#   correlation_spatial_df.sf
# 
# 
# p4<-ggplot(correlation_spatial_df.sf) +
#   geom_sf(aes(fill = cor), linewidth=0.0001, alpha=0.9) + 
#   scale_fill_viridis_c(
#     # trans = "sqrt",
#     # breaks=c(0, 100, 200, 500, 1700),
#     # name = "caserate"
#   )+
#   ##facet_wrap(~ date, ncol = 3) +  # Arrange plots in a grid of 5 columns
#   labs(title = "Spatial variation of correlation between caserate and activity") +
#   theme_minimal() +
#   theme(legend.position.inside = c(0.5, 0),
#         axis.text = element_blank(),  # Remove axis text for better visualization
#         axis.title = element_blank())
# 
# p5<-ggplot(correlation_spatial_df.sf) +
#   geom_sf(aes(fill = cor_weeklag1), linewidth=0.0001, alpha=0.9) + 
#   scale_fill_viridis_c(
#     # trans = "sqrt",
#     # breaks=c(0, 100, 200, 500, 1700),
#     # name = "caserate"
#   )+
#   ##facet_wrap(~ date, ncol = 3) +  # Arrange plots in a grid of 5 columns
#   labs(title = "Spatial variation of correlation between caserate and activity") +
#   theme_minimal() +
#   theme(legend.position.inside = c(0.5, 0),
#         axis.text = element_blank(),  # Remove axis text for better visualization
#         axis.title = element_blank())
# 
# 
# p6<-ggplot(correlation_spatial_df.sf) +
#   geom_sf(aes(fill = cor_moblag), linewidth=0.0001, alpha=0.9) + 
#   scale_fill_viridis_c(
#     # trans = "sqrt",
#     # breaks=c(0, 100, 200, 500, 1700),
#     # name = "caserate"
#   )+
#   ##facet_wrap(~ date, ncol = 3) +  # Arrange plots in a grid of 5 columns
#   labs(title = "Spatial variation of correlation between caserate and activity") +
#   theme_minimal() +
#   theme(legend.position.inside = c(0.5, 0),
#         axis.text = element_blank(),  # Remove axis text for better visualization
#         axis.title = element_blank())
# 
# 
# ## Identify regions with extreme cases -----------------------------------------
# ## using z score
# london_df <- london_df |> 
#   group_by(wave) |>
#   mutate(mean_cases = mean(caserate),
#          sd_cases = sd(caserate),
#          z_score_cases = (caserate-mean_cases)/sd_cases) |>
#   ungroup()
# 
# extreme_cases <- london_df |>
#   filter(abs(z_score_cases) > 3)


