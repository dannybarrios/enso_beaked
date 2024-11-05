## rangeuseratios_function_glmm
## Creating range use ratios for tagged goose-beaked whales and comparing them between ENSO period and season
## Author: Daniel M. Barrios
## Updated: 24 Jun 2024
# Load necessary libraries

library(dplyr)
library(lubridate)
library(ggplot2)
library(here)
library(sf)
library(adehabitatHR)

Zc <- read.csv("ZcTags_4hr_use_seafloor_geomorph_oceangographic_enso_300m_var_MAY23_v1.csv")

#read in coast shapefile
coast <- st_read("Coastline.shp") %>%
  st_transform(., crs = "ESRI:102007")

# range-use ratio function
calculate_range_use_ratio <- function(data, tag, coast) {
  
  # for testing 
  #data <- Zc
  #tag <- "ZcTag002"
  
  tag_data <- filter(data, animal == tag)
  df_sp <- tag_data[, c("lon", "lat", "animal")]
  
  coordinates(df_sp) <- c("lon", "lat")
  proj4string(df_sp) <- CRS("+init=epsg:4326")
  df_sp <- spTransform(df_sp, CRS("+proj=aea +lat_1=8 +lat_2=18 +lat_0=13 +lon_0=-157 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
  
  df_mcp95 <- mcp(df_sp, percent = 95)
  mcp95_sf <- as(df_mcp95, "sf") 
  mcp95_sf <- st_transform(mcp95_sf, crs = "ESRI:102007")
  mcp95_diff <- st_difference(mcp95_sf, st_union(coast))
  
  tag_data$datetime_utc <- as.POSIXct(tag_data$datetime_utc, format = "%m/%d/%Y %H:%M", tz = "UTC")
  tag_data$Date1 <- as.Date(tag_data$datetime_utc)
  tag_data$week <- cut(tag_data$Date1, breaks = "7 days")
  
  dwf_sp <- tag_data[, c("lon", "lat", "week")]
  coordinates(dwf_sp) <- c("lon", "lat")
  proj4string(dwf_sp) <- CRS("+init=epsg:4326")
  dwf_sp <- spTransform(dwf_sp, CRS("+proj=aea +lat_1=8 +lat_2=18 +lat_0=13 +lon_0=-157 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
  
  dwf_mcp50 <- mcp(dwf_sp, percent = 50)
  mwcp50_sf <- as(dwf_mcp50, "sf") 
  mwcp50_sf <- st_transform(mwcp50_sf, crs = "ESRI:102007")
  mwcp50_diff <- st_difference(mwcp50_sf, st_union(coast))
  
  area_entire <- st_area(mcp95_diff)
  area_weekly <- st_area(mwcp50_diff)
  
  ratioweekly <- area_weekly / area_entire
  ratioweekly <- as.data.frame(ratioweekly)
  ratioweekly$tag <- tag
  
  # add week start date, month, and year
  week_dates <- as.data.frame(unique(tag_data$week))
  colnames(week_dates) <- c("week")
  week_dates$week_start <- as.Date(sub("\\((.+),.*", "\\1", week_dates$week))
  week_dates$month <- as.integer(format(week_dates$week_start, "%m"))
  week_dates$year <- format(week_dates$week_start, "%Y")
  
  
  ratioweekly <- cbind(ratioweekly, week_dates)
  
  return(ratioweekly)
}

# list of tags
tags <- c("ZcTag013", "ZcTag002", "ZcTag003", "ZcTag006", "ZcTag008", "ZcTag009", "ZcTag012", "ZcTag018", "ZcTag033", "ZcTag044")

Zc_filtered <- Zc %>%
  filter(!(animal == "ZcTag006" & Periodmeiwithflav == "Neutral"))

Zc <- Zc_filtered

# calculate range-use ratios for each tag and combine into one data frame
range_use_ratios <- lapply(tags, calculate_range_use_ratio, data = Zc, coast = coast)
range_use_ratios_df <- bind_rows(range_use_ratios)

# View the combined data with month and year
head(range_use_ratios_df)

# save file
#write.csv(range_use_ratios_df, "ZcTags_weekly_rangeuseratios_2024Aug07.csv", row.names = F)