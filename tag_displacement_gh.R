## tag_displacement_gh.R
## Calculating displacement between tag points for Cuvier's beaked whale data
## Replace with Blainville's csvs and follow the same process for that species

## Author: Daniel M. Barrios and Michaela A. Kratofil
## Updated: 8 January 2024

library(adehabitatLT)
library(dplyr)
library(lubridate)
library(ggplot2)
library(geosphere)

#set working directory and read in csvs for both species
# filter gaps of 1+ days
cuv <- read.csv("ZcTags_4hr_use_seafloor_geomorph_oceangographic_enso_300m_var_MAY23_v1.csv")
#cuv <- filter(cuv, !is.na(seg_id1))

#standardize dates if needed

cuv <- cuv %>%
  mutate(date_utc = as.Date(datetime_utc, format = "%m/%d/%Y %H:%M"))

#remove the first and last days from each animal track (since they are incomplete)

cuv_filtered <- cuv %>%
  group_by(animal) %>%
  arrange(date_utc) %>%
  filter(!(date_utc %in% c(first(date_utc), last(date_utc)))) %>%
  ungroup()

#double check
cuv <- cuv_filtered

#then remove dates with less than 3 original locations
#call original location csv files
cuvos <- read.csv("ZcTag002-044_DouglasFiltered_KS_r10d3lc2_2020MAYv2.csv")

#removing original dates with points of less than 3 since they wont be accurate 
cuvos <- cuvos %>%
  mutate(date = as.Date(datetime_utc, format = "%m/%d/%Y %H:%M"))
date_counts <- cuvos %>%
  group_by(animal, date) %>%
  summarize(count = n())
# see which stretches/animals occur less than three times in cuvos
dates_to_delete <- date_counts %>%
  filter(count < 3)
# take out dates of less than 3 original points
cuv_filtered <- cuv %>%
  left_join(dates_to_delete, by = c("animal", "date_utc" = "date")) %>%
  filter(is.na(count)) %>%
  .[ , -which(names(.) == "count")]
#remove days with 0 original points from dataset
dates_to_remove <- cuv_filtered %>%
  anti_join(cuvos, by = c("animal", "date_utc"="date")) %>%
  dplyr::select(animal, date_utc)

# Remove rows from cuv_filtered where the date is in dates_to_remove
cuv_filtered <- cuv_filtered %>%
  filter(!(animal %in% dates_to_remove$animal & date_utc %in% dates_to_remove$date_utc))

# cuv_filtered now contains the rows from cuv where the dates are 3 or more
cuv <- cuv_filtered

#looking at temporal clustering
cuv <- cuv %>%
  mutate(datetime_utc = as_datetime(datetime_utc, format = "%m/%d/%Y %H:%M"))
cuvos <- cuvos %>%
  mutate(datetime_utc = as_datetime(datetime_utc, format = "%m/%d/%Y %H:%M"))

#the time window is the amount of hours that could define temporal clustering
time_window <- 4
#find days where all points are within 7 hours
cuvos_filtered <- cuvos %>%
  group_by(animal, date) %>%
  mutate(time_range = difftime(max(datetime_utc), min(datetime_utc), units = "hours")) %>%
  filter(time_range >= time_window | n() == 1) %>%
  ungroup() %>%
  dplyr::select(animal, date)

cuvos_filtered1 <- cuvos %>%
  group_by(animal, date) %>%
  mutate(time_range = difftime(max(datetime_utc), min(datetime_utc), units = "hours")) %>%
  filter(time_range <= time_window | n() == 1) %>%
  ungroup() %>%
  dplyr::select(animal, date)
#filter cuv dataset using those days with all times within 7 hours
cuv_filtered <- cuv %>%
  semi_join(cuvos_filtered, by = c("animal", "date_utc" = "date"))


#clean up data
summary(cuv)
cuv$lat <- as.numeric(cuv$lat)
cuv$lon <- as.numeric(cuv$lon)
cuv$timestamp <- as.POSIXct(cuv$datetime_utc, format = "%m/%d/%Y %H:%M", tz = "UTC") # MAK added the tz = "UTC
summary(cuv$timestamp)
tz(cuv$timestamp) # make sure that the UTC time zone was assigned 

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

# test on one animal that has some big displacement values 
chk <- filter(cuv, animal == "ZcTag013")
chk$disp_km <- chk$displacement/1000

# add the date column to make sure that it correctly assigned dates from timestamps
chk$day <- as.Date(chk$datetime_utc)

# subset one day 
sub <- filter(chk, day == as.Date("2010-12-19"))
sub$dist <- as.numeric(NA)
colnames(sub)

sub <- as.data.frame(sub)

# calculate displacement between each point row-wise via for-loop
for (i in 1:nrow(sub)) {
  
  lon1 = sub[i, "lon"]
  lat1 = sub[i, "lat"]
  
  lon2 = sub[i + 1, "lon"]
  lat2 = sub[i + 1, "lat"]
  
  sub[i, "dist"] <- distVincentyEllipsoid(p1 = c(lon1,lat1), p2 = c(lon2,lat2))
  
}

# no longer a big displacement value between the first and second locations for this particular day. 
# also manually checked the distances using the Douglas Filtered KML file and Google Earth Ruler
# now try to make a function to apply the for-loop above to each calendar day, and then for each animal 
calc_dist <- function(x){
  x <- as.data.frame(x)
  
  for (i in 1:nrow(x)) {
    
    lon1 = x[i, "lon"]
    lat1 = x[i, "lat"]
    
    lon2 = x[i + 1, "lon"]
    lat2 = x[i + 1, "lat"]
    
    x[i, "dist"] <- distVincentyEllipsoid(p1 = c(lon1,lat1), p2 = c(lon2,lat2))
    
  }
  
  return(x)
  
  
}

# try on one animal first 
library(purrr)

chk_tbl <- chk %>%
  group_by(day) %>%
  tidyr::nest() %>%
  mutate(
    disp_day = purrr::map(data, ~calc_dist(x = .x))
  )

# now bind back 
chk_new <- chk_tbl %>%
  dplyr::select(disp_day) %>%
  tidyr::unnest()

# convert and summarize
chk_new$dist_km <- chk_new$dist/1000

chk_sum <- chk_new %>%
  group_by(day) %>%
  summarise(
    tot_disp = sum(dist_km, na.rm = T)
  )

# now try to apply to the whole dataset. 
cuv$day <- as.Date(cuv$timestamp, tz="UTC")
cuv <- cuv %>%
  dplyr::select(animal, timestamp, day, lon, lat, displacement)

# nest by animal, and day, then apply the function 
cuv_tbl <- cuv %>%
  group_by(animal, day) %>%
  tidyr::nest() %>%
  mutate(
    disp_day = purrr::map(data, ~calc_dist(x = .x))
  )

# now unnest and check, summarize new displacements 
cuv_new <- cuv_tbl %>%
  dplyr::select(animal, day, disp_day) %>%
  tidyr::unnest()

# convert and summarize
cuv_new$dist_km <- cuv_new$dist/1000

cuv_sum <- cuv_new %>%
  group_by(animal, day) %>%
  summarise(
    tot_disp = sum(dist_km, na.rm = T)
  )

## ------------------------------------------------------------------------------------ ##

#read in MEI values and add them as we did in other r codes
mei <- read.csv("MEI_values_csv.csv")
MEI_long <- reshape(mei, direction = "long", varying = list(names(mei)[2:13]), v.names = "mei", times = 1:12, timevar = "month", idvar = "Year")
MEI_long$month <- as.numeric(MEI_long$month)
cuv_sum <- cuv_sum %>%
  mutate(year = year(day), month = month(day))

cuv_sum$MEI <- MEI_long$mei[match(paste(cuv_sum$year, cuv_sum$month), paste(MEI_long$Year, MEI_long$month))]
# Create the "Period" column based on MEI values
cuv_sum$Periodmei <- ifelse(cuv_sum$MEI <= -0.5, "LN",
                                       ifelse(cuv_sum$MEI >= 0.5, "EN", "Neutral"))

cuv_sum <- cuv_sum %>%
  filter(tot_disp != 0)

#filter by period to get summary information for Cuvier's
cuvln <- filter(cuv_sum, Periodmei=="LN")
cuven<- filter(cuv_sum, Periodmei=="EN")
cuvneu<- filter(cuv_sum, Periodmei=="Neutral")

summary(cuvln$tot_disp)
summary(cuven$tot_disp)
summary(cuvneu$tot_disp)

sd(cuvln$tot_disp)
sd(cuven$tot_disp)
sd(cuvneu$tot_disp)

#looking at flavors of EN
ep <- filter(cuven, animal=="ZcTag044")
cp <- filter(cuven, animal=="ZcTag008")

summary(ep$tot_disp)
summary(cp$tot_disp)

sd(ep$tot_disp)
sd(cp$tot_disp)

#plot
cuv_dd <- ggplot(cuv_sum, aes(x=Periodmei, y = tot_disp)) +
  geom_boxplot(position = position_dodge(0.75)) +
  theme(axis.ticks.y = element_blank()) +
  ylab("")+
  xlab("ENSO Period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 84)) +
  theme(axis.text.x = element_text(size = 31, angle = 0, color = "black"),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 38, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA), 
        legend.text = element_text(size = 14)) # Add this line to increase the legend font size

ggsave("Zc_ENSO_daily_displacement_mei_boxplot_gc_v2_AUG23.jpg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

#combine again with new information
cuvv <- rbind(cuven, cuvln, cuvneu)

#add deployment duration

dd <- read.csv("deployment_duration.csv")
cuvv <- cuvv %>%
  left_join(dd, by = "animal") %>%
  mutate(deployment_duration = ifelse(is.na(deployment_duration), "No Data", deployment_duration))

#save csvs

write.csv(cuvv, "cuv_dailydisplacement.csv")
