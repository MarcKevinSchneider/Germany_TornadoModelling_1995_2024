#' @name 02_download_ERA5_point_data.R
#' @date 16.02.2025
#' @author Marc Kevin Schneider & Lukas Esselmann
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Downloading the ERA5 point data for the tornado cases and random points

# ================================================================
# 1. Load packages
# ================================================================

import pandas as pd
import numpy as np
import cdsapi
from datetime import datetime
import xarray as xr

# ================================================================
# 2. Read tornado file
# ================================================================

# tornado file
csv_file = r"C:\Users\kevis\OneDrive\Desktop\Unisachen\Master\Fernerkundung_GIS_WS2024\Daten\ESWD_Tornado_FinalVersion_1995_2024.csv" 
data = pd.read_csv(csv_file)


# ================================================================
# 3. Helper functions for the ERA5 download
# ================================================================

# for download
def download_era5_for_event(time_event, latitude, longitude, output_file):
    #date and time
    event_datetime = datetime.strptime(time_event, "%Y-%m-%d %H:%M:%S")
    year = event_datetime.strftime("%Y")
    month = event_datetime.strftime("%m")
    day = event_datetime.strftime("%d")
    hour = event_datetime.strftime("%H")  
    
    dataset = "reanalysis-era5-pressure-levels"
    request = {
        "product_type": "reanalysis",
        "variable": [
            "temperature",
            "geopotential",
            "u_component_of_wind",
            "v_component_of_wind",
            "specific_humidity",
        ],
        "year": year,
        "month": month,
        "day": day,
        "time": [hour],
        "pressure_level": [
            "1000", "975", "950", "925", "900", "875", "850", "825", "800", 
            "775", "750", "700", "650", "600", "550", "500", "450", "400", 
            "350", "300", "250", "225", "200", "175", "150", "125", "100", 
            "70", "50", "30", "20", "10", "7", "5", "3", "2", "1"
        ],
        "format": "netcdf",
        "area": [
            latitude + 0.01, longitude - 0.01,
            latitude - 0.01, longitude + 0.01,
        ]
    }

    
    client = cdsapi.Client()
    # request ERA5 data
    client.retrieve(dataset, request, output_file)
    print(f"Downloaded data for {time_event} at ({latitude}, {longitude}).")

# for generating a random point
def generate_random_point():
    # rough bounding area of Germany
    lat_min, lat_max = 47.27, 55.09
    lon_min, lon_max = 5.87, 15.04

    # generates a random coordinate within the bounds of Germany 
    latitude = np.random.uniform(lat_min, lat_max)
    longitude = np.random.uniform(lon_min, lon_max)

    return latitude, longitude

# ================================================================
# 4. Downloading the data
# ================================================================

for index, row in data.iterrows():
    time_event = row["TIME_EVENT"]
    latitude = float(row["LATITUDE"])
    longitude = float(row["LONGITUDE"])
    
    # download the data for the actual event
    output_file = f"C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Daten/ERA5_MitRandomPoints/era5_{index+1}_{time_event.replace(':', '').replace(' ', '_')}.nc"
    download_era5_for_event(time_event, latitude, longitude, output_file)

    # downloads the data for 10 randomly sampled points within Germany
    for i in range(10):
        rand_lat, rand_lon = generate_random_point()
        random_output_file = f"C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Daten/ERA5_MitRandomPoints/random_{index+1}_{i+1}_{time_event.replace(':', '').replace(' ', '_')}.nc"
        download_era5_for_event(time_event, rand_lat, rand_lon, random_output_file)