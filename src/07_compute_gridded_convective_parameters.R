#' @name 07_compute_gridded_convective_parameters.R
#' @date 14.02.2026
#' @author Marc Kevin Schneider & Lukas Esselmann
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Functions for computing the thundeR convective parameters for the gridded
#' ERA5 Germany data

# ================================================================
# 1. Load packages
# ================================================================

library(terra)
library(ncdf4)
library(thunder)
library(data.table)

# ================================================================
# 2. Helper functions for calculating the convective parameters
# ================================================================

# helper function for calculating the dewpoint
dewpoint_from_specific_humidity <- function(temp_C, q, p_hPa) {
  # temp_C: °C, q: kg/kg, p_hPa: hPa
  e <- q * p_hPa * 100 / (0.622 + q)  # Pa
  Td <- 243.5 * log(e/610.94)/(17.67 - log(e/610.94))
  return(Td)
}


# helper function for computing the gridded convective parameters
compute_convective_parameters <- function(nc, predictor_names) {
  
  # all the variables
  pressure <- ncvar_get(nc, "pressure_level")
  t <- ncvar_get(nc, "t")
  q <- ncvar_get(nc, "q")
  u <- ncvar_get(nc, "u")
  v <- ncvar_get(nc, "v")
  z <- ncvar_get(nc, "z")
  
  # coords
  lon <- ncvar_get(nc, "longitude")
  lat <- ncvar_get(nc, "latitude")
  
  # x and y length
  nlon <- length(lon)
  nlat <- length(lat)
  
  # altitude
  altitude <- z / 9.80665
  # kelvin to celsius
  temp     <- t - 273.15
  
  # expand pressure to 3D
  # otherwise the code crashes
  p_3d <- array(rep(pressure, each = nlon * nlat), dim = dim(temp))
  
  # dewpoint, windspeed and wind direction
  dpt <- dewpoint_from_specific_humidity(temp, q, p_3d)
  ws  <- sqrt(u^2 + v^2) * 1.94384
  wd  <- (atan2(-u, -v) * 180 / pi) %% 360
  
  # empty array for storing the data
  out_array <- array(NA, dim = c(nlon, nlat, length(predictor_names)))
  
  # loops over all pixels
  for (i in 1:nlon) {
    for (j in 1:nlat) {
      
      # tries to calculate the sounding data
      sounding <- tryCatch(
        sounding_compute(
          pressure  = pressure,
          altitude  = altitude[i,j,],
          temp      = temp[i,j,],
          dpt       = dpt[i,j,],
          wd        = wd[i,j,],
          ws        = ws[i,j,],
          accuracy  = 3
        ),
        error = function(e) list()
      )
      
      # removes variables that are not in the predictor names list
      # also orders them correctly so that catboost can use them
      for (v_idx in seq_along(predictor_names)) {
        var_name <- predictor_names[v_idx]
        if (var_name %in% names(sounding)) {
          out_array[i, j, v_idx] <- sounding[[var_name]]
        }
      }
    }
  }
  
  # extent for the raster stack
  e <- ext(min(lon), max(lon), min(lat), max(lat))
  
  # creates the correct raster format 
  # was weirdly systematic before, had to implement this to get it right
  raster_list <- lapply(1:length(predictor_names), function(k) {
    
    # Transpose [Lon, Lat] -> [Lat, Lon]
    mat <- t(out_array[,,k])
    
    # convert array to raster
    r <- rast(mat, crs = "EPSG:4326", extent = e)
    
    # Uncomment only if map is vertically flipped
    # r <- flip(r, direction="vertical")
    
    return(r)
  })
  
  # stacks the raster list
  raster_stack <- rast(raster_list)
  
  # renames the rasters accordingly
  names(raster_stack) <- predictor_names
  
  return(raster_stack)
}


