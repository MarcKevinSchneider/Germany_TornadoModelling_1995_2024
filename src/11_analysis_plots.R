#' @name 11_analysis_plots.R
#' @date 19.02.2026
#' @author Marc Kevin Schneider & Lukas Esselmann
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Analysis of the data and model. Also plots for the results.

# ================================================================
# 1. Load packages
# ================================================================

library(terra)
library(dplyr)
library(lubridate)
library(ggplot2)
library(sf)
library(catboost)
library(rnaturalearth)
library(rnaturalearthdata)

# ================================================================
# 2. Load data
# ================================================================

data_path <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Daten/"

tornado_sf <- sf::st_read(paste0(data_path, "ESWD_Tornado_FinalVersion_1995_2024.gpkg"))

pred_dir <- paste0(data_path, "EventBased_Improved_Predictions_CatBoost/")
fig_dir  <- paste0(data_path, "EventBased_Plots/")

dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)


# ================================================================
# 3. Plot preparation
# ================================================================

# germany outline
germany <- rnaturalearth::ne_countries(country = "Germany", scale = "medium", returnclass = "sf")
germany <- sf::st_transform(germany, 4326)

# germany states outline
states <- rnaturalearth::ne_states(country = "Germany", returnclass = "sf")
states <- sf::st_transform(states, 4326)

# fixed extents for the plots
germany_bbox <- st_bbox(germany)

# ================================================================
# 4. Loop through all tornado events
# ================================================================

for (i in 1:nrow(tornado_sf)) {
  
  event <- tornado_sf[i, ]
  # extract coords etc
  event_time <- event$TIME_EVENT
  lat <- event$LATITUDE
  lon <- event$LONGITUDE
  
  file_name <- paste0("Tornado_", format(event_time, "%Y%m%d_%H%M"), "_", round(lat, 2), 
                      "_", round(lon, 2),".tif")
  
  raster_path <- paste0(pred_dir, file_name)
  
  if (!file.exists(raster_path)) {
    cat("Missing raster:", raster_path, "\n")
    next
  }
  
  print(paste0("Processing:", event_time))

  # load pred raster
  r <- rast(raster_path)
  #plot(r)
  
  # classify based on the threshold we identified previously (0.5)
  r_binary <- classify(r, matrix(c(-Inf, 0.5, 0, 0.5, Inf, 1), ncol = 3, byrow = TRUE))
  
  names(r_binary) <- "Prediction"
  
  # outline to vector
  germany_vect <- vect(germany)
  
  # crop and mask
  r_binary <- crop(r_binary, germany_vect)
  r_binary <- mask(r_binary, germany_vect)
  
  # convert to dataframe
  r_df <- as.data.frame(r_binary, xy = TRUE, na.rm = FALSE)
  
  # to factor
  r_df$Prediction <- factor(r_df$Prediction, levels = c(0, 1), labels = c("No Tornado", "Tornado"))
  
  # plot
  p <- ggplot() +
    
    geom_raster(
      data = r_df,
      aes(x = x, y = y, fill = Prediction)
    ) +
    # plot the bundesländer
    geom_sf(
      data = states,
      fill = NA,
      color = "grey40",
      linewidth = 0.4
    ) +
    # plot the outline of germany
    geom_sf(
      data = germany,
      fill = NA,
      color = "black",
      linewidth = 0.7
    ) +
    # tornado as a red point
    geom_sf(
      data = event,
      color = "red",
      size = 3
    ) +
    # fill based on wehter tornado or not
    scale_fill_manual(
      values = c("lightgrey", "blue"),
      name = "Prediction"
    ) +
    # extent of the plot
    coord_sf(
      xlim = c(germany_bbox["xmin"], germany_bbox["xmax"]),
      ylim = c(germany_bbox["ymin"], germany_bbox["ymax"]),
      expand = FALSE
    ) +
    # title
    labs(
      title = paste0(
        "Observed Tornado vs Prediction\n",
        format(event_time, "%Y-%m-%d %H:%M UTC")
      ),
      # tornado strength
      subtitle = paste0(
        event$PLACE, ", ", event$STATE,
        "  |  F-scale: ", event$F_SCALE
      )
    ) +
    
    theme_minimal()
  
  # save plot
  out_file <- paste0(fig_dir, "TornadoPlot_", format(event_time, "%Y%m%d_%H%M"), 
                     "_", round(lat, 2), "_", round(lon, 2), ".png")
  
  ggsave(filename = out_file, plot = p, width = 8, height = 7, dpi = 300)
}


