lon1 <- -164
lon2 <- -150
lat1 <- 18
lat2 <- 24
library(dplyr) # for tidying
library(lubridate) # for working with datetimes 
library(readr) # for reading in data
library(sf) # for working with spatial data 
library(stars) # for working with rasters
library(ggplot2) # for plotting/mapping 
library(ggspatial) # mapping aesthetics 
library(here)
library(maptools)
library(dplyr)
library(lubridate)
library(sf)
library(stars)
library(raster)
library(oce)
library(lunar)
library('lubridate')
library('remotes')
library('stars')
library('dplyr')
library('tidyr')
library('ggplot2')
library('terra')
library('rgdal')
library('maptools')
library('readr') # for reading in data
library('sf') # for working with spatial data 
library('ggplot2') # for plotting/mapping 
library('ggspatial') # mapping aesthetics 
library('marmap') # bathymetry data
library('metR') # contour aesthetics
library('sp')
library('rgeos')
library('grid')
library('rgdal')
library('rnaturalearth') # for getting land data
library('rnaturalearthhires') # for getting land data
library('kableExtra')
library('gt')
library('quadprog')
library('Webshot2')
library('ggforce')
library('ggpubr')
bathy <- getNOAA.bathy(lon1, lon2, lat1, lat2, resolution = 1)
bf <- fortify.bathy(bathy)


#graph by enso

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/R practice/06_Session6_Geocomp/00_Shapefiles_Rasters")
sess <- "06_Session6_Geocomp/"
shprast <- paste0(sess, "00_Shapefiles_Rasters/")
coast <- st_read("Coastline.shp")

#transform into meters
coast_sf <- st_transform(coast, crs = 3750)
## get raster files for depth, aspect, and slope. can ignore warning messages


setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/ENSO_paper/Sightings")
sight <- read.csv("Allsightings_biasremoved_monthsbelow40hrHIadjusted_beaked_onimeivalues.csv")

Md <- filter(sight, Species=="Blainville's beaked whale")
Zc <- filter(sight, Species=="Cuvier's beaked whale")

eff <- read.csv("Effort_thruNov2022_GIS_2023MAY_Enso_comb_beaked.csv")

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/ENSO_paper/csvs")


#add mei values to effort
mei <- read.csv("MEI_values_csv.csv")
MEI_long <- reshape(mei, direction = "long", varying = list(names(mei)[2:13]), v.names = "mei", times = 1:12, timevar = "month", idvar = "Year")
MEI_long$month <- as.numeric(MEI_long$month)

eff$MEI <- MEI_long$mei[match(paste(eff$Year, eff$month), paste(MEI_long$Year, MEI_long$month))]
# create a column that defines all
eff$Periodmei <- ifelse(eff$MEI <= -0.5, "LN",
                        ifelse(eff$MEI >= 0.5, "EN", "Neutral"))

#split into enso periods
effen <- filter(eff, Periodmei=="EN")
effln <- filter(eff, Periodmei=="LN")
effneut <- filter(eff, Periodmei=="Neutral")

#create tracks for each period
eff_tracks_en <- effen %>%
  st_as_sf(., coords = c("Long_dd","Lat_dd"), crs = 4326) %>%
  group_by(Date_, Vessel) %>% #because some days there are more than one vessels operating on one day
  summarise(do_union = F) %>%
  st_cast("LINESTRING")
eff_tracks_ln <- effln %>%
  st_as_sf(., coords = c("Long_dd","Lat_dd"), crs = 4326) %>%
  group_by(Date_, Vessel) %>% #because some days there are more than one vessels operating on one day
  summarise(do_union = F) %>%
  st_cast("LINESTRING")
eff_tracks_neut <- effneut %>%
  st_as_sf(., coords = c("Long_dd","Lat_dd"), crs = 4326) %>%
  group_by(Date_, Vessel) %>% #because some days there are more than one vessels operating on one day
  summarise(do_union = F) %>%
  st_cast("LINESTRING")

#bind them again with mei column
eff_tracks <- bind_rows(
  data.frame(eff_tracks_neut, Periodmei = "Neutral"),
  data.frame(eff_tracks_en, Periodmei = "EN"),
  data.frame(eff_tracks_ln, Periodmei = "LN")
)


#Plotting effort lines
effplot <- ggplot() +
  geom_sf(data = coast, color = NA, fill = "gray") +
  geom_sf(data = eff_tracks, aes(color = Periodmei, geometry = geometry), size = 0.1) +
  coord_sf(
    crs = 4326,
    xlim = c(-156.6, -155.4),
    ylim = c(19.1, 20.2)
  ) +
  geom_contour(data = bf,
               aes(x = x, y = y, z = z),
               breaks = c(-200, -1000, -2000),
               color = "darkgrey") +
  scale_y_continuous(breaks = c(19.2, 19.5, 19.8, 20.1)) +
  scale_x_continuous(breaks = c(-155.6, -155.8, -156, -156.2, -156.4)) +
  geom_text(aes(x = -155.6, y = 19.6, label = paste0("Hawai\u02BBi")), fontface = "bold", size = 6, color = "black") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 22, color = "black"),
    rect = element_rect(color = "white", size = 1.5),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    legend.text = element_text(size = 22, color = "black"),
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(size = 24, color = "black")
  ) +
  annotation_scale(location = 'bl', text_cex = unit(1, "cm")) +
  annotation_north_arrow(location = 'tr', which_north = 'true', style = north_arrow_fancy_orienteering()) +
  scale_color_manual(values = viridis(3))

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/ENSO_paper/Sightings/sighting_plots")

ggsave("effort_mei.jpeg", plot=effplot,
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)
effplot <- effplot + theme(legend.position = "none")

#plotting Blainville's sightings
Mdplot <- ggplot() +
  #geom_sf(data = blobs_sf, fill = "blue") +
  geom_sf(data = coast, color = NA, fill = "gray") +
  geom_contour(data = bf,
               aes(x = x, y = y, z = z),
               breaks = c(-200, -1000, -2000),
               color = "darkgrey") +
  geom_point(data = Md, aes(x = longitud, y = latitude, color = Period_mei))+

  coord_sf(
    crs = 4326,
    xlim = c(-156.6, -155.4),
    ylim = c(19.1, 20.2)
  ) +
  scale_y_continuous(breaks = c(19.2, 19.5, 19.8, 20.1)) +
  scale_x_continuous(breaks = c(-155.6, -155.8, -156, -156.2, -156.4)) +
  geom_text(aes(x = -155.6, y = 19.6, label = paste0("Hawai\u02BBi")), fontface = "bold", size = 7, color = "black") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 22, color = "black"),
    rect = element_rect(color = "white", size = 1.5),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    legend.text = element_text(size = 22, color="black"), # Add this line to increase the legend font size
    legend.background = element_rect(fill = "white", color = NA),
    legend.title=element_text(size=24, color= "black")
  ) +
  annotation_scale(location = 'bl', text_cex = unit(1, "cm")) +
  annotation_north_arrow(location = 'tr', which_north = 'true',
                         style = north_arrow_fancy_orienteering())+
  scale_color_manual(values = viridis(3))+
  labs(color = "ENSO period") +
  guides(color = guide_legend(title = "ENSO period"))




setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/ENSO_paper/Sightings/sighting_plots")
ggsave("Md_mei_sightings.jpeg", plot=Mdplot,
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)





#plotting Cuvier's sightings
Zcplot <- ggplot() +
  #geom_sf(data = blobs_sf, fill = "blue") +
  geom_sf(data = coast, color = NA, fill = "gray") +
  geom_contour(data = bf,
               aes(x = x, y = y, z = z),
               breaks = c(-200, -1000, -2000),
               color = "darkgrey") +
  geom_point(data = Zc, aes(x = longitud, y = latitude, color = Period_mei))+
  
  coord_sf(
    crs = 4326,
    xlim = c(-156.6, -155.4),
    ylim = c(19.1, 20.2)
  ) +
  scale_y_continuous(breaks = c(19.2, 19.5, 19.8, 20.1)) +
  scale_x_continuous(breaks = c(-155.6,  -156, -156.4)) +
  geom_text(aes(x = -155.6, y = 19.6, label = paste0("Hawai\u02BBi")), fontface = "bold", size = 7, color = "black") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 22, color = "black"),
    rect = element_rect(color = "white", size = 1.5),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    legend.text = element_text(size = 22, color="black"), # Add this line to increase the legend font size
    legend.background = element_rect(fill = "white", color = NA),
    legend.title=element_text(size=24, color= "black")
  ) +
  annotation_scale(location = 'bl', text_cex = unit(1, "cm")) +
  annotation_north_arrow(location = 'tr', which_north = 'true',
                         style = north_arrow_fancy_orienteering())+
  scale_color_manual(values = viridis(3))+
  labs(color = "ENSO period") +
  guides(color = guide_legend(title = "ENSO period"))




setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/ENSO_paper/Sightings/sighting_plots")
ggsave("Zc_mei_sightings.jpeg", plot=Zcplot,
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)
Zcplot <- Zcplot + theme(legend.position = "none")

#combining
library(patchwork)

# adjust the plots
top <- effplot + theme(axis.text.x = element_blank(), axis.title.x = element_blank())
middle <- Mdplot + theme(axis.text.x = element_blank(), axis.title.x = element_blank())


# combine the plots into a single stacked figure with 3 panels but 1 legend
combined_plot <- top/middle/Zcplot

ggsave("combined_plot.jpeg", plot = combined_plot,
       path = NULL, width = 12, height = 15, device = "jpeg", dpi = 600)

