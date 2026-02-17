#' @name 04_extract_sounding_data.R
#' @date 24.02.2025
#' @author Marc Kevin Schneider & Lukas Esselmann
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Formatting the ERA5 data and extracting the sounding data

# ================================================================
# 1. Load packages
# ================================================================

import pandas as pd
import numpy as np
import cdsapi
from datetime import datetime
import xarray as xr

# ================================================================
# 2. Helper functions
# ================================================================

# helper function for preprocessing the era5 files
def preprocess(ds):
    # necessary since the size of the dataset below will get too big otherwise
    # happens because the era5 data was only downloaded for specific points and xarray tries to create a coordinate grid out of the data for all 
    # 460 tornado events and all 37 vertical levels == 120GB size (oof ouch owie)
    # converts latitude and longitude into scalar coordinates
    ds = ds.set_coords(["latitude", "longitude"])
    # removes spatial dimensions
    ds = ds.squeeze("latitude").squeeze("longitude")  
    return ds


# helper function for calculating dew point
def calculate_dew_point(q, t, p):
    # pressure is in hPa, convert to Pascals for q calculations
    e = (q * p) / (0.622 + 0.378 * q)
    ln_e = np.log(e / 6.112)
    dew_point = (243.5 * ln_e) / (17.67 - ln_e)
    return dew_point


# ================================================================
# 3. Reading the data
# ================================================================

# tornado points
ds_era5 = xr.open_mfdataset(
    "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Daten/ERA5_MitRandomPoints/era5_*.nc",
    combine="nested",
    concat_dim="valid_time",
    # using the function from above to cut down on the size
    preprocess=preprocess,
)

# random points dataset
ds_rs = xr.open_mfdataset(
    "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Daten/ERA5_MitRandomPoints/random_*.nc",
    combine="nested",
    concat_dim="valid_time",
    #using the function from above to cut down on the size
    preprocess=preprocess,
)

# ================================================================
# 4. Converting for use in thundeR
# ================================================================

# earth acceleration due to gravity
g = 9.80665


for ds in [ds_era5, ds_rs]:
    # temperature from kelvin to celsius
    ds["t"] = ds["t"] - 273.15

    # calculates altitude (meters)
    ds["altitude"] = ds["z"] / g

    # calculates dew point
    ds["dew_point"] = xr.apply_ufunc(
        calculate_dew_point,
        ds["q"],  # Specific humidity
        ds["t"],
        ds["pressure_level"],
        vectorize=True,
        dask= "parallelized"
    )

    # calculates wind speed in knots
    ds["wind_speed"] = np.sqrt(ds["u"] ** 2 + ds["v"] ** 2) * 1.94384

    # calculates wind direction (azimuth in degrees)
    ds["wind_direction"] = (np.arctan2(ds["v"], ds["u"]) * (180 / np.pi) + 180) % 360

# renaming so that its easier for me
ds_era5 = ds_era5.rename_vars({"t": "temperature", "z": "gpt_height", "u":"u_wind", "v":"v_wind", "q":"sp_humidity"})
ds_rs = ds_rs.rename_vars({"t": "temperature", "z": "gpt_height", "u":"u_wind", "v":"v_wind", "q":"sp_humidity"})


# save both 
output_file = "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Daten/ERA5_ProcessedFiles/Processed_Tornado_ERA5_Concat_1995_2024.nc"
ds_era5.to_netcdf(output_file)

output_file = "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Daten/ERA5_ProcessedFiles/Processed_RandomPoints_ERA5_Concat_1995_2024.nc"
ds_rs.to_netcdf(output_file)


# ================================================================
# 5. Further formatting
# ================================================================

ds_era5 = xr.open_dataset(r"C:\Users\kevis\OneDrive\Desktop\Unisachen\Master\Fernerkundung_GIS_WS2024\Daten\ERA5_ProcessedFiles\Processed_Tornado_ERA5_Concat_1995_2024.nc")
ds_rs = xr.open_dataset(r"C:\Users\kevis\OneDrive\Desktop\Unisachen\Master\Fernerkundung_GIS_WS2024\Daten\ERA5_ProcessedFiles\Processed_RandomPoints_ERA5_Concat_1995_2024.nc")

# drop unnecessary varaibels
ds_era5 = ds_era5.drop_vars(["number", "expver"])
ds_rs = ds_rs.drop_vars(["number", "expver"])


# ================================================================
# 6. Convert to sounding data
# ================================================================

# converting the dataset to a dataframe
df_era5 = ds_era5.to_dataframe().reset_index()
df_rs = ds_rs.to_dataframe().reset_index()

# sorting by time (newest to oldest), pressure level (lowest to highest) and latitude (lowest to highest)
# means we have a vertical structure in our csv
df_era5 = df_era5.sort_values(by=["valid_time", "pressure_level", "latitude"], ascending=[False, False, False])
df_rs = df_rs.sort_values(by=["valid_time","latitude", "pressure_level"], ascending=[False, False, False])


# ================================================================
# 7. Save as .csv
# ================================================================

csv_file = r"C:\Users\kevis\OneDrive\Desktop\Unisachen\Master\Fernerkundung_GIS_WS2024\Daten\ERA5_ProcessedFiles\Processed_Tornado_ERA5_Concat_1995_2024.csv"
df_era5.to_csv(csv_file, index=False)

csv_file = r"C:\Users\kevis\OneDrive\Desktop\Unisachen\Master\Fernerkundung_GIS_WS2024\Daten\ERA5_ProcessedFiles\Processed_RandomPoints_ERA5_Concat_1995_2024.csv"
df_rs.to_csv(csv_file, index=False)


