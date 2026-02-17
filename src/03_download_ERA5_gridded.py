#' @name 03_download_ERA5_gridded.R
#' @date 16.02.2025
#' @author Marc Kevin Schneider & Lukas Esselmann
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Downloading the gridded ERA5 data

# ================================================================
# 1. Load packages
# ================================================================

import pandas as pd
import cdsapi
from datetime import datetime
from pathlib import Path

# ================================================================
# 2. Reading data
# ================================================================

# Germany bounding box
LAT_MIN, LAT_MAX = 47.27, 55.09
LON_MIN, LON_MAX = 5.87, 15.04

# input tornado CSV
csv_file = r"C:\Users\kevis\OneDrive\Desktop\Unisachen\Master\Fernerkundung_GIS_WS2024\Daten\ESWD_Tornado_FinalVersion_1995_2024.csv"
data = pd.read_csv(csv_file)

# output directory
out_dir = Path(r"C:\Users\kevis\OneDrive\Desktop\Unisachen\Master\Fernerkundung_GIS_WS2024\Daten\ERA5_Germany_Gridded_EventBased")
out_dir.mkdir(parents=True, exist_ok=True)


# ================================================================
# 3. Helper function for ERA5 download
# ================================================================

client = cdsapi.Client()

def download_era5_for_event(time_event, output_file):

    event_datetime = datetime.strptime(time_event, "%Y-%m-%d %H:%M:%S")
    year  = event_datetime.strftime("%Y")
    month = event_datetime.strftime("%m")
    day   = event_datetime.strftime("%d")
    hour  = event_datetime.strftime("%H")

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
            "1000","975","950","925","900","875","850","825","800",
            "775","750","700","650","600","550","500","450","400",
            "350","300","250","225","200","175","150","125","100",
            "70","50","30","20","10","7","5","3","2","1"
        ],
        "format": "netcdf",
        "area": [
            LAT_MAX,   # North
            LON_MIN,   # West
            LAT_MIN,   # South
            LON_MAX    # East
        ]
    }

    client.retrieve(dataset, request, output_file)
    print(f"Downloaded ERA5 Germany grid for {time_event}")

# ================================================================
# 4. Download the data for the tornadoes
# ================================================================

# Loop over tornadoes
for index, row in data.iterrows():

    time_event = row["TIME_EVENT"]

    # clean filename
    safe_time = time_event.replace(":", "").replace(" ", "_")

    output_file = out_dir / f"era5_germany_{safe_time}.nc"

    # to avoid redownloads
    if output_file.exists():
        print(f"File already exists: {output_file}")
        continue

    download_era5_for_event(time_event, str(output_file))
