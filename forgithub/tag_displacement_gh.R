##tag_displacement_gh.R
##Calculating displacement between tag points for beaked whale data

##Author: Daniel M. Barrios
## Updated: 23 June 2023

library(adehabitatLT)
library(dplyr)
library(lubridate)
library(ggplot2)
library(geosphere)

#set working directory and read in csvs for both species
# filter gaps of 1+ days
cuv <- read.csv("ZcTags_4hr_use_seafloor_geomorph_oceangographic_enso_300m_var_MAY23_v1.csv")
cuv <- filter(cuv, !is.na(seg_id1))
blv <- read.csv("MdTags_4hr_use_seafloor_geomorph_oceangographic_var_enso_MAY23_v1.csv")
blv <- filter(blv, !is.na(seg_id1))

#clean up data
cuv$lat <- as.numeric(cuv$lat)
cuv$lon <- as.numeric(cuv$lon)
cuv$timestamp <- as.POSIXct(cuv$timestamp, format = "%m/%d/%Y %H:%M")

#develop trajectory information using as.ltraj
traj <- as.ltraj(cuv[, c("lon", "lat")], date = cuv$timestamp, id = cuv$animal, proj4string = CRS("+proj=longlat +datum=WGS84"))

#arrange data by animal and datetime                            
cuv <- cuv %>%
  arrange(animal, timestamp)
#group data by animal and calaculate the displacement
#distVincentyEllipsoid calculates distance between points in meters
#the 2:n part selects rows, starting at the 2nd one

cuv <- cuv %>%
  group_by(animal) %>%
  mutate(displacement = c(0, distVincentyEllipsoid(cuv[1:(n()-1), c("lon", "lat")], cuv[2:n(), c("lon", "lat")])))

#group data by animal and date, calculate displacement sums for each
daily_displacement <- cuv %>%
  group_by(animal, date = as.Date(timestamp)) %>%
  summarize(daily_displacement = sum(displacement))
daily_displacement$month <- month(daily_displacement$date)
daily_displacement$year <- year(daily_displacement$date)
#remove the first row because it is 0
daily_displacement <- daily_displacement[2:nrow(daily_displacement), ]
daily_displacement$daily_displacement <- daily_displacement$daily_displacement*.001

#read in MEI values and add them as we did in other r codes
mei <- read.csv("MEI_values_csv.csv")
MEI_long <- reshape(mei, direction = "long", varying = list(names(mei)[2:13]), v.names = "mei", times = 1:12, timevar = "month", idvar = "Year")
MEI_long$month <- as.numeric(MEI_long$month)
daily_displacement$MEI <- MEI_long$mei[match(paste(daily_displacement$year, daily_displacement$month), paste(MEI_long$Year, MEI_long$month))]
# Create the "Period" column based on MEI values
daily_displacement$Periodmei <- ifelse(daily_displacement$MEI <= -0.5, "LN",
                        ifelse(daily_displacement$MEI >= 0.5, "EN", "Neutral"))

#filter by period to get summary information for Cuvier's
cuvln <- filter(daily_displacement, Periodmei=="LN")
cuven<- filter(daily_displacement, Periodmei=="EN")
cuvneu<- filter(daily_displacement, Periodmei=="Neutral")

#plot
ggplot(daily_displacement, aes(x=Periodmei, y = daily_displacement)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Daily displacement (km)")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 124 )) +
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

ggsave("Zc_ENSO_daily_displacement_mei_boxplot_gc_v1_JUN23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(daily_displacement, aes(x=Periodmei, y = daily_displacement)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Daily displacement (km)")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 66)) +
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

ggsave("Zc_ENSO_daily_displacement_trunc_mei_boxplot_gc_v1_JUN23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)







#do the same for Blainville's beaked whale
blv$lat <- as.numeric(blv$lat)
blv$lon <- as.numeric(blv$lon)

blv$timestamp <- as.POSIXct(blv$timestamp, format = "%m/%d/%Y %H:%M")

duplicated_dates <- duplicated(blv$timestamp)

blv <- blv[!duplicated_dates, ]


traj <- as.ltraj(blv[, c("lon", "lat")], date = blv$timestamp, id = blv$animal, proj4string = CRS("+proj=longlat +datum=WGS84"))



blv <- blv %>%
  arrange(animal, timestamp)

blv <- blv %>%
  group_by(animal) %>%
  mutate(displacement = c(0, distVincentyEllipsoid(blv[1:(n()-1), c("lon", "lat")], blv[2:n(), c("lon", "lat")])))


daily_displacement <- blv %>%
  group_by(animal, date = as.Date(timestamp)) %>%
  summarize(daily_displacement = sum(displacement))
daily_displacement$month <- month(daily_displacement$date)
daily_displacement$year <- year(daily_displacement$date)
daily_displacement$daily_displacement <- daily_displacement$daily_displacement*.001



daily_displacement$MEI <- MEI_long$mei[match(paste(daily_displacement$year, daily_displacement$month), paste(MEI_long$Year, MEI_long$month))]
# Create the "Period" column based on MEI values
daily_displacement$Periodmei <- ifelse(daily_displacement$MEI <= -0.5, "LN",
                                       ifelse(daily_displacement$MEI >= 0.5, "EN", "Neutral"))
blvln <- filter(daily_displacement, Periodmei=="LN")
blven<- filter(daily_displacement, Periodmei=="EN")
blvneu<- filter(daily_displacement, Periodmei=="Neutral")
table(daily_displacement$Periodmei)

summary(blvln$daily_displacement)
summary(blven$daily_displacement)
summary(blvneu$daily_displacement)

sd(blvln$daily_displacement)
sd(blven$daily_displacement)
sd(blvneu$daily_displacement)

ggplot(daily_displacement, aes(x=Periodmei, y = daily_displacement)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Daily displacement (km)")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 132)) +
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

ggsave("Md_ENSO_daily_displacement_mei_boxplot_gc_v1_JUN23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)
ggplot(daily_displacement, aes(x=Periodmei, y = daily_displacement)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Daily displacement (km)")+
  xlab("ENSO Cycle Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 82)) +
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

ggsave("Md_ENSO_daily_displacement_trunc_mei_boxplot_gc_v1_JUN23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)