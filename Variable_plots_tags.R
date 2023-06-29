#plotting tag info
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
library('doBy')
lon1 <- -160
lon2 <- -150
lat1 <- 18
lat2 <- 23

#read in tag info
setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/ENSO_paper/Tags")
cuv <- read.csv("ZcTags_4hr_use_seafloor_geomorph_oceangographic_enso_300m_var_MAY23_v1.csv")
blv <- read.csv("MdTags_4hr_use_seafloor_geomorph_oceangographic_var_enso_MAY23_v1.csv")
unique(blv$animal)
#filter out pseudoreplicates and gaps of 1+ days
blv <- subset(blv, animal != "MdTag005" & animal != "MdTag008")
cuv <- filter(cuv, !is.na(seg_id1))
blv <- filter(blv, !is.na(seg_id1))

# Load library
setwd("//crcdata1/RedirectedFolders/dbarrios/Desktop/kde_essentials")
#Load in shapefile and map materials for point plot
# Load shapefile
coastline <- read_sf('coastline.shp')
#buffer to make lines in shapefile polygons
coastline_buff <- st_buffer(coastline, .1) # buffer unit of 50 as example
coastline_buf <- as(coastline_buff, 'Spatial')
bathy <- getNOAA.bathy(lon1, lon2, lat1, lat2, resolution = 1)
bf <- fortify.bathy(bathy)

#convert enso periods to factors to retain order
cuv$Periodmei <- factor(cuv$Periodmei, levels = c("EN", "Neutral", "LN"))
blv$Periodmei <- factor(blv$Periodmei, levels = c("EN", "Neutral", "LN"))

#days of data
#filter blv and cuv into each period and double check
Mden <- filter(blv, Periodmei=="EN")
Mdln <- filter(blv, Periodmei=="LN")
Mdnt <- filter(blv, Periodmei=="Neutral")
table(Mden$animal)
table(Mdln$animal)
table(Mdnt$animal)

Zcen <- filter(cuv, Periodmei=="EN")
Zcln <- filter(cuv, Periodmei=="LN")
Zcnt <- filter(cuv, Periodmei=="Neutral")
table(Zcen$animal)
table(Zcln$animal)
table(Zcnt$animal)

#find days of each tag for each, record number
unique_dates <- Zcen %>%
  group_by(date_utc) %>%
  summarize(num_animals = n_distinct(animal)) %>%
  ungroup()
total_unique_dates <- sum(unique_dates$num_animals)
unique_dates <- Zcnt %>%
  group_by(date_utc) %>%
  summarize(num_animals = n_distinct(animal)) %>%
  ungroup()
total_unique_dates <- sum(unique_dates$num_animals)
unique_dates <- Zcln %>%
  group_by(date_utc) %>%
  summarize(num_animals = n_distinct(animal)) %>%
  ungroup()

total_unique_dates <- sum(unique_dates$num_animals)
unique_dates <- Mden %>%
  group_by(date_utc) %>%
  summarize(num_animals = n_distinct(animal)) %>%
  ungroup()
total_unique_dates <- sum(unique_dates$num_animals)
unique_dates <- Mdnt %>%
  group_by(date_utc) %>%
  summarize(num_animals = n_distinct(animal)) %>%
  ungroup()
total_unique_dates <- sum(unique_dates$num_animals)
unique_dates <- Mdln %>%
  group_by(date_utc) %>%
  summarize(num_animals = n_distinct(animal)) %>%
  ungroup()
total_unique_dates <- sum(unique_dates$num_animals)


#summaries of variables
summaryBy(dist_ridge ~ Periodmei, data = blv, na.rm=TRUE, FUN = c(min, max, mean, median, sd))
summaryBy(dist_ridge ~ Periodmei, data = cuv, na.rm=TRUE, FUN = c(min, max, mean, median, sd)) 
summaryBy(lat ~ Periodmei, data = blv, na.rm=TRUE, FUN = c(min, max, mean, median, sd))
summaryBy(lat ~ Periodmei, data = cuv, na.rm=TRUE, FUN = c(min, max, mean, median, sd))
summaryBy(sst_combf ~ Periodmei, data = blv, na.rm=TRUE, FUN = c(min, max, mean, median, sd))
summaryBy(sst_combf ~ Periodmei, data = cuv, na.rm=TRUE, FUN = c(min, max, mean, median, sd))
summaryBy(ssh_combf ~ Periodmei, data = blv, na.rm=TRUE, FUN = c(min, max, mean, median, sd))
summaryBy(ssh_combf ~ Periodmei, data = cuv, na.rm=TRUE, FUN = c(min, max, mean, median, sd))
summaryBy(dist_shore ~ Periodmei, data = blv, FUN = c(min, max, mean, median, sd))
summaryBy(depth ~ Periodmei, data = blv, na.rm=TRUE, FUN = c(min, max, mean, median, sd))
summaryBy(dist_shore ~ Periodmei, data = cuv, FUN = c(min, max, mean, median, sd))
summaryBy(depth ~ Periodmei, data = cuv, na.rm=TRUE, FUN = c(min, max, mean, median, sd))
summaryBy(hor_curr_mag ~ Periodmei, data = cuv, na.rm=TRUE, FUN = c(min, max, mean, median, sd))
summaryBy(hor_curr_mag ~ Periodmei, data = blv, na.rm=TRUE, FUN = c(min, max, mean, median, sd))
summaryBy(saln_combf ~ Periodmei, data = blv, na.rm=TRUE, FUN = c(min, max, mean, median, sd))
summaryBy(saln_combf ~ Periodmei, data = cuv, na.rm=TRUE, FUN = c(min, max, mean, median, sd))

#graphing Blainville's points
ggplot() +
  geom_sf(data = coastline, color = NA, fill = "gray") +
  geom_contour(data = bf,
               aes(x = x, y = y, z = z),
               breaks = c(-200, -1000, -2000),
               color = "darkgrey") +
  geom_point(data = blv, shape = 16, size = 2, aes(x = lon, y = lat, color = Periodmei)) +
  coord_sf(
    crs = 4326,
    xlim = c(-157.6, -154.6),
    ylim = c(18.2, 21.6)
  ) +
  scale_y_continuous(breaks = c(19, 19.5, 20, 20.5, 21, 21.5)) +
  scale_x_continuous(breaks = c(-161, -160, -159, -158, -157, -156, -155)) +
  geom_text(aes(x = -155.48, y = 19.6, label = paste0("Hawai\u02BBi")), fontface = "bold", size = 7, color = "black") +
  geom_text(aes(x = -156.28, y = 20.77, label = paste0("Maui")), fontface = "bold", size = 5.5, color = "black") +
  geom_text(aes(x = -157, y = 21.14, label = paste0("Moloka\u02BBi")), fontface = "bold", size = 4, color = "black") +
  geom_text(aes(x = -156.92, y = 20.839, label = "Lāna‘i"), fontface = "bold", size = 3, color = "black") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    rect = element_rect(color = "black", size = 1.5),
    legend.text = element_text(size = 14)
  ) +
  annotation_scale(location = 'bl', text_cex = unit(1, "cm")) +
  annotation_north_arrow(location = 'tr', which_north = 'true',
                         style = north_arrow_fancy_orienteering()) +
  guides(color = guide_legend(order = 1))



setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO")

ggsave("Md_ENSO_mei_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)


#graph Cuvier's points
cuv$Periodmei <- factor(cuv$Periodmei, levels = c("EN", "Neutral", "LN"))

ggplot() +
  geom_sf(data = coastline, color = NA, fill = "gray") +
  geom_contour(data = bf,
               aes(x = x, y = y, z = z),
               breaks = c(-200, -1000, -2000),
               color = "darkgrey") +
  geom_point(data = cuv, shape = 16, size = 2, aes(x = lon, y = lat, color = Periodmei)) +
  coord_sf(
    crs = 4326,
    xlim = c(-157.6, -154.6),
    ylim = c(18.2, 21.6)
  ) +
  scale_y_continuous(breaks = c(19, 19.5, 20, 20.5, 21, 21.5)) +
  scale_x_continuous(breaks = c(-161, -160, -159, -158, -157, -156, -155)) +
  geom_text(aes(x = -155.48, y = 19.6, label = paste0("Hawai\u02BBi")), fontface = "bold", size = 7, color = "black") +
  geom_text(aes(x = -156.28, y = 20.77, label = paste0("Maui")), fontface = "bold", size = 5.5, color = "black") +
  geom_text(aes(x = -157, y = 21.14, label = paste0("Moloka\u02BBi")), fontface = "bold", size = 4, color = "black") +
  geom_text(aes(x = -156.92, y = 20.839, label = "Lāna‘i"), fontface = "bold", size = 3, color = "black") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    rect = element_rect(color = "black", size = 1.5),
    legend.text = element_text(size = 14)
  ) +
  annotation_scale(location = 'bl', text_cex = unit(1, "cm")) +
  annotation_north_arrow(location = 'tr', which_north = 'true',
                         style = north_arrow_fancy_orienteering()) +
  guides(color = guide_legend(order = 1))



setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO")

ggsave("Zc_ENSO_mei_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

#cp vs ep for Cuvier's
###en
cuven <- filter(cuv, Periodmei=="EN")
cuvcp <- cuven[1:153,]
cuvep <- cuven[154:338,]

summary(cuvcp$dist_shore)
summary(cuvep$dist_shore)

summary(cuvcp$lat)
summary(cuvep$lat)

ggplot() +
  geom_sf(data = coastline, color = NA, fill = "gray") +
  geom_contour(data = bf,
               aes(x = x, y = y, z = z),
               breaks = c(-200, -1000, -2000),
               color = "darkgrey") +
  geom_point(data = cuvcp, shape = 16, size = 2, color="red", aes(x = lon, y = lat)) +
  geom_point(data = cuvep, shape = 16, size = 2, color="blue", aes(x = lon, y = lat)) +
  coord_sf(
    crs = 4326,
    xlim = c(-157.6, -154.6),
    ylim = c(18.2, 21.6)
  ) +
  scale_y_continuous(breaks = c(19, 19.5, 20, 20.5, 21, 21.5)) +
  scale_x_continuous(breaks = c(-161, -160, -159, -158, -157, -156, -155)) +
  geom_text(aes(x = -155.48, y = 19.6, label = paste0("Hawai\u02BBi")), fontface = "bold", size = 7, color = "black") +
  geom_text(aes(x = -156.28, y = 20.77, label = paste0("Maui")), fontface = "bold", size = 5.5, color = "black") +
  geom_text(aes(x = -157, y = 21.14, label = paste0("Moloka\u02BBi")), fontface = "bold", size = 4, color = "black") +
  geom_text(aes(x = -156.92, y = 20.839, label = "Lāna‘i"), fontface = "bold", size = 3, color = "black") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    rect = element_rect(color = "black", size = 1.5),
    legend.text = element_text(size = 14)
  ) +
  annotation_scale(location = 'bl', text_cex = unit(1, "cm")) +
  annotation_north_arrow(location = 'tr', which_north = 'true',
                         style = north_arrow_fancy_orienteering()) +
  guides(color = guide_legend(order = 1))

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/ENSO_paper/Tags/cpep")
ggsave("Zc_ENSO_mei_points_ep_vs_cp_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)


#chla
summary(cuv$chla_monthly)
summary(blv$chla_monthly)
ggplot(cuv, aes(x=Periodoni, y = chla_monthly)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("chla_monthly")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0.033, 0.172)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/chla")
ggsave("Zc_ENSO_oni_chla_monthly_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

summaryBy(chla_monthly ~ Periodmei, data = blv, na.rm=TRUE, FUN = c(min, max, mean, median, sd))
summaryBy(chla_monthly ~ Periodmei, data = cuv, na.rm=TRUE, FUN = c(min, max, mean, median, sd))


ggplot(blv, aes(x=Periodoni, y = chla_monthly)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("chla_monthly")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0.033, 0.172)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/chla")

ggsave("Md_ENSO_oni_chla_monthly_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

summary(cuv$chla_monthly)
ggplot(cuv, aes(x=Periodmei, y = chla_monthly)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("chla_monthly")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0.033, 0.172)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/chla")
ggsave("Zc_ENSO_mei_chla_monthly_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(blv, aes(x=Periodmei, y = chla_monthly)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("chla_monthly")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0.033, 0.172)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/chla")

ggsave("Md_ENSO_chla_monthly_mei_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)


#salinity
summary(cuv$saln_combf)
summary(blv$saln_combf)
ggplot(cuv, aes(x=Periodoni, y = saln_combf)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("saln_combf")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(34.2, 35.7)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/salinity")
ggsave("Zc_ENSO_oni_saln_combf_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)


ggplot(blv, aes(x=Periodoni, y = saln_combf)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("saln_combf")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(34.2, 35.7)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/salinity")

ggsave("Md_ENSO_oni_saln_combf_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

summary(cuv$saln_combf)
ggplot(cuv, aes(x=Periodmei, y = saln_combf)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("saln_combf")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(34.2, 35.7)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/salinity")
ggsave("Zc_ENSO_mei_saln_combf_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(blv, aes(x=Periodmei, y = saln_combf)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("saln_combf")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(34.2, 35.7)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/salinity")

ggsave("Md_ENSO_saln_combf_mei_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)


#dist_shore
summary(cuv$dist_shore)
summary(blv$dist_shore)
ggplot(cuv, aes(x=Periodoni, y = dist_shore)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Distance to shore (km)")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/dist_shore")
ggsave("Zc_ENSO_oni_dist_shore_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(blv, aes(x=Periodoni, y = dist_shore)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Distance to shore (km)")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 200)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/dist_shore")

ggsave("Md_ENSO_oni_dist_shore_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

summary(cuv$dist_shore)
ggplot(cuv, aes(x=Periodmei, y = dist_shore)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Distance to shore (km)")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/dist_shore")
ggsave("Zc_ENSO_mei_dist_shore_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(blv, aes(x=Periodmei, y = dist_shore)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Distance to shore (km)")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 200)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/dist_shore")

ggsave("Md_ENSO_dist_shore_mei_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)
#depth
summary(cuv$depth)
summary(blv$depth)
ggplot(cuv, aes(x=Periodoni, y = depth)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Seafloor depth (m)")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5785)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/depth")
ggsave("Zc_ENSO_oni_depth_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(blv, aes(x=Periodoni, y = depth)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Seafloor depth (m)")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5785)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/depth")

ggsave("Md_ENSO_oni_depth_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

summary(cuv$depth)
ggplot(cuv, aes(x=Periodmei, y = depth)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Seafloor depth (m)")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5785)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size



setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/depth")
ggsave("Zc_ENSO_mei_depth_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(blv, aes(x=Periodmei, y = depth)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Seafloor depth (m)")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5785)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size


summary(blv$depth)
setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/depth")

ggsave("Md_ENSO_depth_mei_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

#sst
summary(cuv$sst_combf)
summary(blv$sst_combf)
ggplot(cuv, aes(x=Periodoni, y = sst_combf)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("sst")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(23.5, 28.09)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/sst")
ggsave("Zc_ENSO_oni_sst_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(blv, aes(x=Periodoni, y = sst_combf)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("sst")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(23.5, 28.09)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/sst")

ggsave("Md_ENSO_oni_sst_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

summary(cuv$sst)
ggplot(cuv, aes(x=Periodmei, y = sst_combf)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("sst")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(23.5, 28.09)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/sst")
ggsave("Zc_ENSO_mei_sst_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(blv, aes(x=Periodmei, y = sst_combf)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("sst")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(23.5, 28.09)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/sst")

ggsave("Md_ENSO_sst_mei_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)
#ssh
summary(cuv$ssh_combf)
summary(blv$ssh_combf)
ggplot(cuv, aes(x=Periodoni, y = ssh_combf)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("SSH")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/ssh")
ggsave("Zc_ENSO_oni_ssh_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(blv, aes(x=Periodoni, y = ssh_combf)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("SSH")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/ssh")

ggsave("Md_ENSO_oni_ssh_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

summary(cuv$ssh)
ggplot(cuv, aes(x=Periodmei, y = ssh_combf)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("SSH")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/ssh")
ggsave("Zc_ENSO_mei_ssh_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(blv, aes(x=Periodmei, y = ssh_combf)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("SSH")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/ssh")

ggsave("Md_ENSO_ssh_mei_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)
#lat
summary(cuv$lat)
ggplot(cuv, aes(x=Periodoni, y = lat)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Latitude")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(18.5, 22)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/lat")
ggsave("Zc_ENSO_oni_lat_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(blv, aes(x=Periodoni, y = lat)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Latitude")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(18.5, 22)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/lat")

ggsave("Md_ENSO_oni_lat_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

summary(cuv$lat)
ggplot(cuv, aes(x=Periodmei, y = lat)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Latitude")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(18.5, 22)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/lat")
ggsave("Zc_ENSO_mei_lat_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(blv, aes(x=Periodmei, y = lat)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Latitude")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(18.5, 22)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/lat")

ggsave("Md_ENSO_lat_mei_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

#dist_ridge
summary(cuv$dist_ridge)
ggplot(cuv, aes(x=Periodoni, y = dist_ridge)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Distance to the ridge (km)")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 110)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/dist_ridge")
ggsave("Zc_ENSO_oni_dist_ridge_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(blv, aes(x=Periodoni, y = dist_ridge)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Distance to the ridge (km)")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 110)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/dist_ridge")

ggsave("Md_ENSO_oni_dist_ridge_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

summary(cuv$dist_ridge)
ggplot(cuv, aes(x=Periodmei, y = dist_ridge)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Distance to the ridge (km)")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 110)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/dist_ridge")
ggsave("Zc_ENSO_mei_dist_ridge_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(blv, aes(x=Periodmei, y = dist_ridge)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Distance to the ridge (km)")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 110)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/dist_ridge")

ggsave("Md_ENSO_mei_dist_ridge_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

#hor_curr_mag
summary(cuv$hor_curr_mag)
summary(blv$hor_curr_mag)
ggplot(cuv, aes(x=Periodoni, y = hor_curr_mag)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("hor_curr_mag")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/hor_curr_mag")
ggsave("Zc_ENSO_oni_hor_curr_mag_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(blv, aes(x=Periodoni, y = hor_curr_mag)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("hor_curr_mag")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/hor_curr_mag")

ggsave("Md_ENSO_oni_hor_curr_mag_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

summary(cuv$hor_curr_mag)
ggplot(cuv, aes(x=Periodmei, y = hor_curr_mag)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("hor_curr_mag")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/hor_curr_mag")
ggsave("Zc_ENSO_mei_hor_curr_mag_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(blv, aes(x=Periodmei, y = hor_curr_mag)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("hor_curr_mag")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5)) +
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

setwd("//crcdata1/RedirectedFolders/dbarrios/My Documents/beaked whales project/final_plots/blvcuv/ENSO/hor_curr_mag")

ggsave("Md_ENSO_hor_curr_mag_mei_boxplot_gc_v1_MAY23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)
