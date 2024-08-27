### Helper functions -----

#### R squared computation ---------------
R_squared <- function(y, fv){
  mean_y <- mean(y)
  TSS <- sum((y-mean_y)^2)
  res <- y - fv
  RSS <- sum(res^2)
  Rsq <- 1- (RSS/TSS)
  return(Rsq)
}

#### Function to plot spatial random effect --------
plot_spatial_random_effect <- function(result, spatial_data, id_col) {
  spatial_effects <- result$summary.random[[id_col]]
  # Extract the formula
  # model_formula <- result$.args$formula |> 
  #   deparse() |> 
  #   paste(collapse = " ") |>  # Collapse the character vector into a single string
  #   str_replace_all(regex("\\s+"), " ") |>  # Replace multiple whitespaces with a single space
  #   str_trim() |>
  #   str_wrap(width=80)
  
  # Merge spatial effects with spatial data
  spatial_data <- spatial_data %>%
    mutate(areaCode_id = row_number()) %>%
    left_join(spatial_effects, by = c("areaCode_id" = "ID"))
  
  plot <- ggplot(spatial_data) +
    geom_sf(aes(fill = mean), linewidth = 0.2, alpha = 0.9) + 
    scale_fill_viridis_c() + 
    labs(title = "Spatial random effect") +
    theme_minimal() +
    theme(legend.position = "right",  
          axis.text = element_blank(),  
          axis.title = element_blank())
  return (plot)
}

#### Function to plot temporal random effect ------------------------
plot_temporal_random_effect <- function(model, id_col){
  tmp_eff <- model$summary.random[[id_col]]
  temporal_df <- data.frame(
    time = tmp_eff$ID,
    mean = tmp_eff$mean,
    lower = tmp_eff$`0.025quant`,
    upper = tmp_eff$`0.975quant`
  )
  
  
  # Plot the temporal effect
  plot <- ggplot(temporal_df, aes(x = time, y = mean, group=1)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    labs(title = "Temporal Effect",
         x = "Week",
         y = "Effect Size") +
    theme_minimal()
  
  return (plot)
}

#### Observed Vs Predicted Temporal plots ----------------------
comparision_plot_temporal <- function(areaCode){
  ###plot the results
  model <- inla_runs[[4]]
  model$result$week <- as.Date(model$result$week)
  plot <- ggplot(filter(model$result, msoa == areaCode), aes(x=week, y=mean, group=1))+
    geom_line(col = "blue")+
    geom_line(aes(x=week, y= obs), col="red")+
    geom_ribbon(aes(ymin=`0.025quant`, ymax=`0.975quant`), alpha=0.3) +
    labs(x = "Date", y = "Caserate") +
    ggtitle(areaCode) +
    # theme_minimal()+
    theme(legend.position.inside = c(0.5, 0),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))+
    scale_x_date(date_breaks = "2 month")
  ggsave(filename = paste0("England/plots/temporal_observed_fitted", toString(areaCode), ".png"), plot, dpi = 200)
  return(plot)
}


#### Observed Vs Predicted Spatial Plots ------------------------
comparision_plot_spatial <- function(date, breaks = 10){
  library(classInt)  # For natural breaks
  model <- inla_runs[[4]]
  model$result |> 
    left_join(msoa, by=c("msoa"="MSOA11CD")) |> 
    st_as_sf() -> 
    df_for_plots
  
  # Reshape the data to include a 'type' column 
  long_df <- df_for_plots|>
    filter(week == date)|>
    select(msoa, mean, obs, geometry)|>
    pivot_longer(cols = c(mean, obs), names_to = "type", values_to = "caserate")
  
  # Compute natural breaks for caserate
  n_breaks <- breaks  # Define how many breaks you want to compute; adjust as needed
  natural_breaks <- classIntervals(long_df$caserate, n = n_breaks, style = "sd")
  breaks <- natural_breaks$brks
  
  # Create the plot with facets and dynamic scaling
  combined_plot <- ggplot(long_df) +
    geom_sf(aes(fill = caserate, geometry = geometry), linewidth = 0.0001, alpha = 0.9) + 
    scale_fill_viridis_c(breaks = breaks) +
    facet_wrap(~ type, ncol = 2, labeller = as_labeller(c(mean = "Fitted Case Rate per 100,000", obs = "Observed Case Rate per 100,000"))) +
    labs(title = paste("Case Rate Analysis for Week:", date)) +
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.text = element_blank(),
      axis.title = element_blank()
    )
  ggsave(filename = paste0("England/plots/spatial_plot_observed_fitted", toString(date), ".png"), combined_plot, dpi = 200)
  return(combined_plot)
}

#### Identify regions with highest and lowest caserates -----------------
selected_area_code <- function(london_df, n=10){
  ####identify top 10 areaCode with highest average number of cases
  
  # Calculate average caserate per areaCode
  average_caserates <- london_df %>%
    group_by(areaCode) %>%
    summarize(average_caserate = mean(caserate, na.rm = TRUE)) %>%
    ungroup()
  
  # Find the top 10 areaCodes with highest average caserates
  top_10_highest <- average_caserates %>%
    arrange(desc(average_caserate)) %>%
    slice_head(n = n)
  top_10_highest <- top_10_highest$areaCode
  
  # Find the top 10 areaCodes with lowest average caserates
  top_10_lowest <- average_caserates %>%
    arrange(average_caserate) %>%
    slice_head(n = n)
  top_10_lowest <- top_10_lowest$areaCode
  
  return(c(top_10_highest, top_10_lowest))
}