#' @name 10_evaluate_catboost_model.R
#' @date 17.02.2026
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Evaluation of the catboost model using various metrics

# ================================================================
# 1. Load packages
# ================================================================

library(terra)
library(dplyr)
library(lubridate)
library(pROC)
library(caret)
library(PresenceAbsence)
library(Metrics)

# sourcing the evaluation functions
eval_path <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Scripts/Evaluation_Functions.R"
source(eval_path)

# ================================================================
# 2. Load tornado and random points and format
# ================================================================

# loading the tornado data
path <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Daten/"
eval_points <- read.table(paste0(path, "WithCoords_Merged_Tornado_RandomPoints_Parameters_1995_2024.csv"),
                        sep = ",", header=TRUE)

eval_points$valid_time <- lubridate::ymd_hms(eval_points$valid_time)


# ================================================================
# 3. Extract the probabilities for each point
# ================================================================

raster_dir <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Daten/EventBased_Improved_Predictions_CatBoost"  

# raster prediction files
raster_files <- list.files(raster_dir, pattern = "^Tornado_.*\\.tif$", full.names = TRUE)

# helper function to extract raster probability for a given point
extract_probability <- function(lat, lon, datetime) {
  # match raster by datetime
  fname <- raster_files[grepl(format(datetime, "%Y%m%d_%H%M"), basename(raster_files))]
  if(length(fname) == 0) return(NA)
  
  r <- rast(fname)
  
  # extract tornado probability
  prob <- terra::extract(r, cbind(lon, lat))
  #print(prob)
  return(prob[[1]])
}

# for all evaluation points
eval_points$pred_prob <- mapply(extract_probability,
                                lat = eval_points$latitude,
                                lon = eval_points$longitude,
                                datetime = eval_points$valid_time)

#print(eval_points$pred_prob)

# ================================================================
# 4. Evaluating the model
# ================================================================

# Actual evaluation now

# remove rows with NA predictions
eval_points <- eval_points %>% filter(!is.na(eval_points$pred_prob))

# from https://gitup.uni-potsdam.de/macroecology/mecofun/-/blob/master/R/evalSDM.R?ref_type=heads
# uses the code from parts of the evalSDM function
# finds the optimal threshold for maximizing sensitivity and specificity
thresh.dat <- data.frame(ID=seq_len(length(eval_points$tornado_occurred)), 
                         obs = eval_points$tornado_occurred,
                         pred = eval_points$pred_prob)

thresh.mat <- PresenceAbsence::optimal.thresholds(DATA= thresh.dat, 
                                                  req.sens=0.85, 
                                                  req.spec = 0.85, 
                                                  FPC=1, FNC=1)

# maximizing for sensitivity and specificity here
# provides the best threshold in this case here
thresh <- thresh.mat[thresh.mat$Method=="MaxSens+Spec",2]

# binary classification
eval_points$pred_class <- ifelse(eval_points$pred_prob >= thresh, 1, 0)

# calc metrics
metrics <- eval_funcs(eval_points)

# write to csv
write.csv(metrics, file.path(path, "catboost_evaluation_17022026.csv"), row.names = FALSE)

###############################################

