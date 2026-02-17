#' @name 09_catboost_prediction.R
#' @date 16.02.2026
#' @author Marc Kevin Schneider & Lukas Esselmann
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Script for predicting tornado occurrence using the catboost model

# ================================================================
# 1. Load packages
# ================================================================

library(terra)
library(catboost)

# ================================================================
# 2. Read the model and predictor names
# ================================================================

save_path <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Models/"

# load trained catboost model
cat_model <- catboost.load_model(
  paste0(save_path, "catboost_model_80_20_split.cbm")
)

# read the predictor names of the model
predictor_names <- read.csv(paste0(model_path, "catboost_used_predictors.csv"))
predictor_names <- predictor_names$Feature


# ================================================================
# 3. Helper function for the prediction
# ================================================================

predict_catboost_raster <- function(r, model) {
  
  # convert raster to data frame
  df <- as.data.frame(r, na.rm = FALSE)
  
  # keep row index to rebuild later
  valid_cells <- complete.cases(df)
  
  # initialize result vector
  pred_vec <- rep(NA, nrow(df))
  
  if (any(valid_cells)) {
    
    # create CatBoost pool
    test_pool <- catboost.load_pool(
      data = as.matrix(df[valid_cells, ])
    )
    
    # predict probabilities
    pred_prob <- catboost.predict(
      model,
      test_pool,
      prediction_type = "Probability"
    )
    
    pred_vec[valid_cells] <- pred_prob
  }
  
  # convert back to raster
  pred_raster <- setValues(r[[1]], pred_vec)
  
  return(pred_raster)
}

# ================================================================
# 4. Actual prediction
# ================================================================

input_dir  <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Daten/ERA5_thundeR_Processed_EventBased/"
output_dir <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Daten/EventBased_Improved_Predictions_CatBoost"

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# get all tif files
tif_files <- list.files(input_dir, pattern = "^Tornado_.*\\.tif$", full.names = TRUE)


for (f in tif_files) {
  
  print(paste0("Processing: ", basename(f)))
  
  # read raster
  r <- rast(f)
  
  # Check layer count
  if (nlyr(r) != length(predictor_names)) {
    warning("Skipping (wrong layer count): ", basename(f))
    next
  }
  
  # set names
  names(r) <- predictor_names
  
  # predict using the model
  pred_prob <- predict_catboost_raster(r, cat_model)
  
  # filename
  out_name <- sub("_stack\\.tif$", "_catboost_prob.tif", basename(f))
  output_name <- file.path(output_dir, out_name)
  
  # write as a raster
  writeRaster(pred_prob, output_name, overwrite = TRUE)
}

