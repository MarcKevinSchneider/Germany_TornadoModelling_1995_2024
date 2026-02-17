#' @name 05_calculate_convective_parameters.R
#' @date 07.03.2025
#' @author Marc Kevin Schneider & Lukas Esselmann
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Calculating the convective parameters for the ERA5 sounding data
#' using the thundeR package


# ================================================================
# 1. Load packages
# ================================================================

library(thunder)
library(dplyr)
library(readr)
library(fs)
library(tidyr)
library(tidyverse)

# ================================================================
# 2. Read data
# ================================================================

input_path <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Daten/ERA5_ProcessedFiles/Unsorted_Processed_Tornado_ERA5_Concat_1995_2024.csv"
output_folder <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Daten/"

if (!dir_exists(output_folder)) {dir_create(output_folder)}

df <- read_csv(input_path)

# ================================================================
# 2. Helper function for calculating the convective parameters
# ================================================================

compute_tornado_params <- function(group_data) {
  pressure <- group_data$pressure_level
  altitude <- group_data$altitude
  temp <- group_data$temperature
  dpt <- group_data$dew_point
  wd <- group_data$wind_direction
  ws <- group_data$wind_speed
  accuracy <- 3
  
  result <- sounding_compute(pressure, altitude, temp, dpt, wd, ws, accuracy)
  
  result_df <- as.data.frame(t(result)) %>%
    select(where(~ any(!is.na(.))))  
  
  return(result_df)
}

# ================================================================
# 3. Calculate and format the tornado data
# ================================================================

results <- df %>%
  group_by(valid_time, latitude, longitude) %>%
  group_modify(~ compute_tornado_params(.x)) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), round, 3))

# initial save
output_path <- file.path(output_folder, "Full_Tornado_Parameters_1995_2024.csv")
write_csv(results, output_path)

# ================================================================
# 4. Calculate and format the random points data
# ================================================================

input_rs <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Daten/ERA5_ProcessedFiles/Unsorted_Processed_RandomPoints_ERA5_Concat_1995_2024.csv"
df_rs <- read_csv(input_rs)

results_rs <- df_rs %>%
  group_by(valid_time, latitude, longitude) %>%
  group_modify(~ compute_tornado_params(.x)) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), round, 3))

# initial save
output_path <- file.path(output_folder, "RandomPoints_Parameters_1995_2024.csv")
write_csv(results_rs, output_path)

# ================================================================
# 5. Format both and merge
# ================================================================

path <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Daten/"

# read both csv files again
tornado_param <- read.csv(paste0(path, "Full_Tornado_Parameters_1995_2024.csv"))
rs_param <- read.csv(paste0(path, "RandomPoints_Parameters_1995_2024.csv"))

# remove time and coordinates so that the model doesnt learn from them
tornado_param_adj <- tornado_param[,-c(1,2,3)]
rs_param_adj <- rs_param[,-c(1,2,3)]

# add the binary classification of if tornadoes occurred or not
rs_param_adj <- rs_param_adj %>% mutate(tornado_occurred = 0)
tornado_param_adj <- tornado_param_adj %>% mutate(tornado_occurred = 1)

# uncomment the four lines above when coordinates for the tornado points are needed
#rs_param_adj <- rs_param %>% mutate(tornado_occurred = 0)
#tornado_param_adj <- tornado_param %>% mutate(tornado_occurred = 1)

# merge both
merged_data <- bind_rows(rs_param_adj, tornado_param_adj)

# ================================================================
# 6. Save the data
# ================================================================

#output_path <- file.path(output_folder, "WithCoords_Merged_Tornado_RandomPoints_Parameters_1995_2024.csv")
#saveRDS(merged_data, paste0(path, "With_Coords_Merged_Tornado_RandomPoints_Parameters_1995_2024.rds"))

output_path <- file.path(output_folder, "Merged_Tornado_RandomPoints_Parameters_1995_2024.csv")
write_csv(merged_data, output_path)
saveRDS(merged_data, paste0(path, "Merged_Tornado_RandomPoints_Parameters_1995_2024.rds"))

