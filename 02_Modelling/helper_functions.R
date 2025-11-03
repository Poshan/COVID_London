# =============================================================================
# helper_functions.R
# =============================================================================
# Purpose: Helper functions for model evaluation and visualization
# Author: Poshan Niraula
# Date: 2024
# =============================================================================

# Load required libraries
library(ggplot2)
library(dplyr)
library(sf)
library(viridis)
library(scales)
library(patchwork)

# ==============================================================================
# MODEL EVALUATION METRICS
# ==============================================================================

#' Calculate Conditional Predictive Ordinate (CPO)
#' 
#' @param model INLA model object
#' @param method "mean" or "sum" for aggregation
#' @return Numeric CPO value
cpo <- function(model, method = "mean") {
  cpo_vals <- model$cpo$cpo
  
  # Diagnostic information
  n_fail <- sum(model$cpo$failure > 0, na.rm = TRUE)
  n_zero <- sum(cpo_vals <= 0, na.rm = TRUE)
  n_na <- sum(is.na(cpo_vals))
  
  cat("CPO Diagnostics:\n")
  cat("  Failures:", n_fail, "\n")
  cat("  Zero/negative values:", n_zero, "\n")
  cat("  Missing values:", n_na, "\n")
  
  # Remove problematic values
  valid_cpo <- cpo_vals[cpo_vals > 0 & !is.na(cpo_vals) & is.finite(cpo_vals)]
  
  cat("  Valid CPO values:", length(valid_cpo), "out of", length(cpo_vals), "\n")
  
  if(length(valid_cpo) == 0) {
    warning("No valid CPO values!")
    return(NA)
  }
  
  # Compute metric
  if(method == "mean") {
    result <- -mean(log(valid_cpo))
  } else if(method == "sum") {
    result <- -sum(log(valid_cpo))
  }
  
  return(round(result, 3))
}

#' Calculate R-squared
#' 
#' @param observed Vector of observed values
#' @param fitted Vector of fitted values
#' @return Numeric R-squared value
R_squared <- function(observed, fitted) {
  mean_obs <- mean(observed, na.rm = TRUE)
  TSS <- sum((observed - mean_obs)^2, na.rm = TRUE)
  RSS <- sum((observed - fitted)^2, na.rm = TRUE)
  R2 <- 1 - (RSS / TSS)
  return(R2)
}

# ==============================================================================
# SPATIAL EFFECT VISUALIZATION
# ==============================================================================

#' Plot spatial random effects
#' 
#' @param model INLA model object
#' @param spatial_data SF object with spatial boundaries
#' @param id_col Column name for spatial unit ID
#' @return ggplot object
plot_spatial_random_effect <- function(model, spatial_data, id_col = "new_id") {
  # Extract spatial effects (structured component for BYM2)
  n_areas <- nrow(spatial_data)
  spatial_effects <- model$summary.random[[id_col]][1:n_areas, ]
  
  # Merge with spatial data
  spatial_data <- spatial_data %>%
    mutate(area_id = row_number()) %>%
    left_join(spatial_effects, by = c("area_id" = "ID"))
  
  # Create map
  plot <- ggplot(spatial_data) +
    geom_sf(aes(fill = mean), linewidth = 0.2, alpha = 0.9) + 
    scale_fill_viridis_c(option = "viridis", name = "Effect") + 
    labs(title = "Spatial Random Effects") +
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
  
  return(plot)
}

#' Plot BYM2 model components (structured and unstructured)
#' 
#' @param model INLA model object with BYM2 spatial effect
#' @param spatial_data SF object with spatial boundaries
#' @param id_col Column name for spatial unit ID
#' @return List with plots and statistics
plot_bym2_components <- function(model, spatial_data, id_col = "new_id") {
  n_areas <- nrow(spatial_data)
  
  # Extract both components from BYM2
  all_effects <- model$summary.random[[id_col]]
  structured <- all_effects[1:n_areas, ]
  unstructured <- all_effects[(n_areas + 1):(2 * n_areas), ]
  
  # Add to spatial data
  spatial_data$structured <- structured$mean
  spatial_data$unstructured <- unstructured$mean
  spatial_data$total <- structured$mean + unstructured$mean
  
  # Create three maps
  p1 <- ggplot(spatial_data) +
    geom_sf(aes(fill = structured), linewidth = 0.2) +
    scale_fill_viridis_c(option = "plasma") +
    labs(title = "Structured (Spatial)", fill = "Effect") +
    theme_minimal() +
    theme(axis.text = element_blank(), 
          axis.title = element_blank(),
          panel.grid = element_blank())
  
  p2 <- ggplot(spatial_data) +
    geom_sf(aes(fill = unstructured), linewidth = 0.2) +
    scale_fill_viridis_c(option = "viridis") +
    labs(title = "Unstructured (IID)", fill = "Effect") +
    theme_minimal() +
    theme(axis.text = element_blank(), 
          axis.title = element_blank(),
          panel.grid = element_blank())
  
  p3 <- ggplot(spatial_data) +
    geom_sf(aes(fill = total), linewidth = 0.2) +
    scale_fill_viridis_c(option = "magma") +
    labs(title = "Total Spatial Effect", fill = "Effect") +
    theme_minimal() +
    theme(axis.text = element_blank(), 
          axis.title = element_blank(),
          panel.grid = element_blank())
  
  # Combine plots
  combined <- p1 + p2 + p3 + plot_layout(ncol = 3)
  
  # Extract mixing parameter
  rho <- model$summary.hyperpar["Phi for new_id", "mean"]
  
  return(list(
    plot = combined,
    rho = rho,
    structured_var = var(structured$mean),
    unstructured_var = var(unstructured$mean),
    proportion_structured = rho
  ))
}

# ==============================================================================
# TEMPORAL EFFECT VISUALIZATION
# ==============================================================================

#' Plot temporal random effects
#' 
#' @param model INLA model object
#' @param id_col Column name for temporal ID
#' @return ggplot object
plot_temporal_random_effect <- function(model, id_col = "week") {
  # Extract temporal effects
  tmp_eff <- model$summary.random[[id_col]]
  
  temporal_df <- data.frame(
    time = tmp_eff$ID,
    mean = tmp_eff$mean,
    lower = tmp_eff$`0.025quant`,
    upper = tmp_eff$`0.975quant`
  )
  
  # Create plot
  plot <- ggplot(temporal_df, aes(x = time, y = mean)) +
    geom_line(linewidth = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = "Temporal Random Effect",
      x = "Week",
      y = "Effect Size"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold")
    )
  
  return(plot)
}

#' Plot LAD-specific temporal effects
#' 
#' @param model INLA model object
#' @param london_df Data frame with LAD information
#' @param week_col Column name for week effects
#' @return ggplot object
plot_lad_spatiotemporal_effect <- function(model, london_df, week_col = "week1") {
  # Extract effects
  lad_effects <- model$summary.random[[week_col]]
  
  n_weeks <- length(unique(london_df$week))
  n_lads <- length(unique(london_df$lad_id))
  
  # Create dataframe
  lad_effects_df <- data.frame(
    week_id = rep(1:n_weeks, n_lads),
    lad_id = rep(1:n_lads, each = n_weeks),
    mean = lad_effects$mean,
    lower = lad_effects$`0.025quant`,
    upper = lad_effects$`0.975quant`
  )
  
  # Add LAD names
  lad_lookup <- london_df |> 
    select(lad_id, LAD11CD) |> 
    distinct()
  
  lad_effects_df <- lad_effects_df |> 
    left_join(lad_lookup, by = "lad_id")
  
  # Plot
  plot <- ggplot(lad_effects_df, 
                 aes(x = week_id, y = mean, group = LAD11CD, color = LAD11CD)) +
    geom_line(alpha = 0.7) +
    labs(
      title = "LAD-specific Temporal Effects",
      x = "Week",
      y = "Effect Size"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  return(plot)
}

# ==============================================================================
# OBSERVED VS PREDICTED COMPARISONS
# ==============================================================================

#' Plot temporal comparison of observed vs fitted values
#' 
#' @param area_codes Vector of MSOA codes to plot
#' @param model Model results list with 'result' dataframe
#' @param msoa_names Optional dataframe with MSOA names
#' @return ggplot object
comparison_plot_temporal <- function(area_codes, model, msoa_names = NULL) {
  # Convert dates if needed
  model$result$week <- as.Date(model$result$week)
  
  # Filter data for selected areas
  plot_data <- model$result |>
    filter(msoa %in% area_codes)
  
  # Add area names if available
  if (!is.null(msoa_names)) {
    plot_data <- plot_data |>
      left_join(msoa_names, by = c("msoa" = "MSOA11CD"))
    group_var <- "MSOA11NM"
  } else {
    group_var <- "msoa"
  }
  
  # Create plot
  plot <- ggplot(plot_data, aes_string(x = "week", group = group_var, color = group_var)) +
    # Fitted values (solid lines)
    geom_line(aes(y = mean, linetype = "Fitted"), linewidth = 0.8) +
    # Observed values (dashed lines)
    geom_line(aes(y = obs, linetype = "Observed"), linewidth = 0.8) +
    # Confidence intervals (optional)
    # geom_ribbon(aes(ymin = `0.025quant`, ymax = `0.975quant`), alpha = 0.1) +
    
    scale_linetype_manual(values = c("Fitted" = "solid", "Observed" = "dashed")) +
    scale_color_brewer(palette = "Set1") +
    
    labs(
      x = "Date",
      y = "Weekly incidence per 100,000",
      color = "Area",
      linetype = "Type",
      title = "Observed vs Fitted Weekly Incidence"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold")
    ) +
    scale_x_date(date_breaks = "2 month", date_labels = "%b %Y")
  
  return(plot)
}

#' Plot spatial comparison of observed vs fitted values
#' 
#' @param dates Vector of dates to plot
#' @param model Model results list
#' @param msoa SF object with MSOA boundaries
#' @return ggplot object
comparison_plot_spatial <- function(dates, model, msoa) {
  # Merge results with spatial data
  df_spatial <- model$result |>
    left_join(msoa, by = c("msoa" = "MSOA11CD")) |>
    st_as_sf()
  
  # Filter and reshape for faceting
  df_long <- df_spatial |>
    filter(week %in% dates) |>
    select(msoa, week, mean, obs, geometry) |>
    pivot_longer(
      cols = c(mean, obs),
      names_to = "type",
      values_to = "caserate"
    )
  
  # Create faceted map
  plot <- ggplot(df_long) +
    geom_sf(aes(fill = caserate), linewidth = 0.05, alpha = 0.9) +
    scale_fill_viridis_c(
      option = "plasma",
      trans = "sqrt",
      labels = label_number(accuracy = 1),
      name = "Cases per\n100,000"
    ) +
    facet_grid(
      week ~ type,
      labeller = labeller(type = c(mean = "Fitted", obs = "Observed"))
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      strip.text = element_text(size = 10, face = "bold"),
      legend.position = "bottom"
    ) +
    guides(
      fill = guide_colorbar(
        direction = "horizontal",
        title.position = "top",
        barwidth = unit(10, "cm"),
        barheight = unit(0.5, "cm")
      )
    )
  
  return(plot)
}

# ==============================================================================
# DATA EXPLORATION UTILITIES
# ==============================================================================

#' Identify areas with highest and lowest case rates
#' 
#' @param data Dataframe with areaCode and caserate columns
#' @param n Number of areas to select from each extreme
#' @return Vector of area codes
select_extreme_areas <- function(data, n = 10) {
  # Calculate average case rate per area
  area_averages <- data |>
    group_by(areaCode) |>
    summarize(
      avg_caserate = mean(caserate, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Get top n highest
  top_n_highest <- area_averages |>
    arrange(desc(avg_caserate)) |>
    slice_head(n = n) |>
    pull(areaCode)
  
  # Get top n lowest
  top_n_lowest <- area_averages |>
    arrange(avg_caserate) |>
    slice_head(n = n) |>
    pull(areaCode)
  
  return(list(
    highest = top_n_highest,
    lowest = top_n_lowest,
    all = c(top_n_highest, top_n_lowest)
  ))
}



#' Get MSOA name from code
#' 
#' @param area_code MSOA code
#' @param lookup_df Dataframe with MSOA11CD and MSOA11NM columns
#' @return Character string with MSOA name
get_area_name <- function(area_code, lookup_df) {
  result <- lookup_df[lookup_df$MSOA11CD == area_code, "MSOA11NM"]
  
  if (nrow(result) == 0) {
    return(area_code)  # Return code if name not found
  } else {
    return(as.character(result$MSOA11NM))
  }
}

message("Helper functions loaded successfully")
