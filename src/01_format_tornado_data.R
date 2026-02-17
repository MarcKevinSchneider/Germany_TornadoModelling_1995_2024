#' @name 01_format_tornado_data.R
#' @date 06.01.2025
#' @author Marc Kevin Schneider & Lukas Esselmann
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Initial formatting of the European Severe Weather Database (ESWD) tornado data from
#' 1995 to 2024


# ================================================================
# 1. Load data
# ================================================================

library(lubridate)

path <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Daten/"
eswd <- read.csv(paste0(path, "ESWD_Tornado_1995_2024.csv"))

print(colnames(eswd))

# ================================================================
# 2. Initial formatting
# ================================================================

eswd <- eswd[,c("QC_LEVEL", "TIME_EVENT", "TIME_ACCURACY", "STATE", "PLACE", 
                 "LATITUDE", "LONGITUDE", "PLACE_ACCURACY", "SURFACE_INITIAL_LOCATION",
                 "SURFACE_CROSSED", "F_SCALE")]

print(unique(eswd["SURFACE_INITIAL_LOCATION"]))

# ================================================================
# 3. Removing waterspouts
# ================================================================

# filters rows where both SURFACE_INITIAL_LOCATION and SURFACE_CROSSED are not "WATER" or "LAKE"
# this removes waterspouts from the dataset, we dont want those
eswd <- eswd[!(eswd$SURFACE_INITIAL_LOCATION == "WATER" & eswd$SURFACE_CROSSED == "WATER"), ]
eswd <- eswd[!(eswd$SURFACE_INITIAL_LOCATION == "LAKE" & eswd$SURFACE_CROSSED == "LAKE"), ]


# ================================================================
# 4. Removing weak tornadoes
# ================================================================

# removes rows where F_SCALE is 0, 0.5 (for the IF-scale) or NA
# this removes all super weak tornadoes or those that dont have a rating
eswd <- eswd[!(eswd$F_SCALE %in% c(0, 0.5) | is.na(eswd$F_SCALE)), ]

# average tornado count per year
#print(nrow(eswd)/30)


# ================================================================
# 5. Initial save
# ================================================================

# saving the filtered data
write.csv(eswd, file=paste0(path, "ESWD_Tornado_Filtered_1995_2024.csv"), row.names = FALSE)

# average F/IF-category
#print(mean(as.double(eswd$F_SCALE)))

# ================================================================
# 6. Further formatting
# ================================================================

data <- read.csv(paste0(path, "ESWD_Tornado_Filtered_1995_2024.csv"))
 
# deleting the unnecessary columns
data$QC_LEVEL <- NULL
data$TIME_ACCURACY <- NULL
data$SURFACE_INITIAL_LOCATION <- NULL
data$SURFACE_CROSSED <- NULL
data$PLACE_ACCURACY <- NULL

write.csv(data, paste0(path, "ESWD_Tornado_Filtered_Cleaned_1995_2024.csv"), row.names = FALSE)


# ================================================================
# 7. Final formatting
# ================================================================

tor <- read.csv(paste0(path, "ESWD_Tornado_Filtered_Cleaned_1995_2024.csv"))

head(tor)

# time column to time format
tor$TIME_EVENT <- as.POSIXct(tor$TIME_EVENT, format = "%Y-%m-%d %H:%M:%S")

# rounding to nearest hour
tor$TIME_EVENT <- round_date(tor$TIME_EVENT, unit = "hour")


head(tor)


write.csv(tor, paste0(path, "ESWD_Tornado_FinalVersion_1995_2024.csv"), row.names = FALSE)

