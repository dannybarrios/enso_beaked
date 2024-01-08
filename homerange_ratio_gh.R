## homerange_ratio_gh.R
## Calculating range-use ratios for Cuvier's beaked whale data
## Authors: Daniel M. Barrios and Michaela A. Kratofil
## Updated: 8 January 2024

#MCP script
library(dplyr)
library(lubridate)
library(ggplot2)
library(here)
library(sf)
library(adehabitatHR)

## read in data ## ========================================================== ##
Zc <- read.csv("ZcTags_4hr_use_seafloor_geomorph_oceangographic_enso_300m_var_MAY23_v1.csv")
# remove land from mcps using the coastline shapefile
coast <- st_read("Coastline.shp") %>%
  st_transform(., crs = "ESRI:102007")

#create MCP for each tag
#first for ZcTag013

zc_013 <- filter(Zc, animal=="ZcTag013")
#take out unnecessary columns
df_sp <- zc_013[,c("lon","lat","animal")]

## create MCP for the tag

coordinates(df_sp) <- c("lon","lat")
proj4string(df_sp) <- CRS( "+init=epsg:4326")
df_sp <- spTransform(df_sp, CRS("+proj=aea +lat_1=8 +lat_2=18 +lat_0=13 +lon_0=-157 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
#df_sp

# get 95% MCP
df_mcp95 <- mcp(df_sp, percent = 95)

# make the MCP an object
mcp95_sf <- as(df_mcp95, "sf") 
mcp95_sf <- st_transform(mcp95_sf, crs = "ESRI:102007")
#mcp95_sf

# remove land  using the coastline shapefile
mcp95_diff <- st_difference(mcp95_sf, st_union(coast))
plot(mcp95_diff)



#weekly/7-day
zc_013_filtered$week <- cut(zc_013_filtered$Date, breaks = "7 days")

#weekly range

dwf_sp <- zc_013_filtered[,c("lon","lat","week")]
coordinates(dwf_sp) <- c("lon","lat")
proj4string(dwf_sp) <- CRS( "+init=epsg:4326")
dwf_sp <- spTransform(dwf_sp, CRS("+proj=aea +lat_1=8 +lat_2=18 +lat_0=13 +lon_0=-157 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#get 50% MCP
dwf_mcp50 <- mcp(dwf_sp, percent = 50)
#mapview::mapview(df_mcp95)
# make it an sf object
mwcp50_sf <- as(dwf_mcp50, "sf") 
mwcp50_sf <- st_transform(mwcp50_sf, crs = "ESRI:102007")
#mcp95_sf
# remove land 
mwcp50_diff <- st_difference(mwcp50_sf, st_union(coast))
plot(mwcp50_diff)


#ratios
area_entire <- st_area(mcp95_diff)
# Calculate the area of the daily range (mdcp50_diff)
area_weekly <- st_area(mwcp50_diff)

# calculate the ratio of area size for daily range to entire range, as in Webber et al. (2020)
ratioweekly <- area_weekly / area_entire
# Print or use the ratio as needed
ratioweekly <- as.data.frame(ratioweekly)

write.csv(ratioweekly, "ratioweekly13.csv")


#repeat this process for each tag
#002
zc_002 <- filter(Zc, animal=="ZcTag002")

#003
zc_003 <- filter(Zc, animal=="ZcTag003")

#006
zc_006 <- filter(Zc, animal=="ZcTag006")

#008
zc_008 <- filter(Zc, animal=="ZcTag008")

#009
zc_009 <- filter(Zc, animal=="ZcTag009")

#012
zc_012 <- filter(Zc, animal=="ZcTag012")

#018
zc_018 <- filter(Zc, animal=="ZcTag018")

#033
zc_033 <- filter(Zc, animal=="ZcTag033")

#044
zc_044 <- filter(Zc, animal=="ZcTag044")