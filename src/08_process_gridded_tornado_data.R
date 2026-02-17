#' @name 08_process_gridded_tornado_data.R
#' @date 14.02.2026
#' @author Marc Kevin Schneider & Lukas Esselmann
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Script for the actual processing of the convective parameter data

# ================================================================
# 1. Load packages + functions
# ================================================================

library(data.table)
library(ncdf4)
library(terra)
library(catboost)

source("C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Scripts/01_compute_convective_parameters_new.R")

# ================================================================
# 2. Paths
# ================================================================

data_path <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Daten/"
model_path <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Models/"

# directory for the gridded data and resulting .tiff data
in_dir  <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Daten/ERA5_Germany_Gridded_EventBased/"
out_dir <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Daten/ERA5_thundeR_Processed_EventBased/"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ================================================================
# 3. Load tornado dataset
# ================================================================

tornadoes <- fread(paste0(data_path, "ESWD_Tornado_FinalVersion_1995_2024.csv"))
# ensure UTC
tornadoes[, TIME_EVENT := as.POSIXct(TIME_EVENT, tz="UTC")]

# Load trained catboost model
cat_model <- catboost.load_model(paste0(model_path, "catboost_model_80_20_split.cbm"))

# get the predictor names
predictor_names <- read.csv(paste0(model_path, "catboost_used_predictors.csv"))
predictor_names <- predictor_names$Feature

# ================================================================
# 4. Loop over tornadoes
# ================================================================

for (i in 1:nrow(tornadoes)) {
  # extract information about the tornadoes
  event_time <- tornadoes$TIME_EVENT[i]
  lat_event  <- tornadoes$LATITUDE[i]
  lon_event  <- tornadoes$LONGITUDE[i]
  
  # build filename
  safe_time <- format(event_time, "%Y-%m-%d_%H%M%S")
  era5_file <- paste0(in_dir, "era5_germany_", safe_time, ".nc")
  
  if (!file.exists(era5_file)) {
    print(paste0("Missing: ", era5_file))
    next
  }
  
  print(paste0("Processing: ", event_time))
  
  # open netcdf file
  nc <- nc_open(era5_file)
  
  # compute the parameters
  raster_stack <- compute_convective_parameters(nc, predictor_names)
  
  # new file name
  out_file <- paste0(out_dir, "Tornado_", format(event_time, "%Y%m%d_%H%M"),
                     "_", round(lat_event,2), "_", round(lon_event,2), ".tif")
  
  # write as a raster stack
  writeRaster(raster_stack, filename = out_file, overwrite = TRUE)
  
  # close netcdf to save ram
  nc_close(nc)
  
  print(paste0("Successfully wrote TIFF for: ", event_time))
}
