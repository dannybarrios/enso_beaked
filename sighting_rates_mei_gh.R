##sighting_rates_mei_gh.R
##Calculating and analyzing sighting rates for beaked whale species in relation to enso values

##Author: Daniel M. Barrios
## Updated: 23 June 2023
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

#set working directory (or whatever your preferred method is)
sight <- read.csv("Allsightings_biasremoved_monthsbelow40hrHIadjusted_beaked.csv")
eff <- read.csv("Effort_thruNov2022_GIS_2023MAY_Enso_comb_beaked.csv")

#filter for just on effort
eff <- filter(eff, On_Off_effort=="on"| On_Off_effort=="On"| On_Off_effort=="on "|On_Off_effort==" on" )
sight$datetimeHST <- as.POSIXct(strptime(sight$datetimeHST, "%m/%d/%Y"))
sight <- sight %>% mutate(year = year(datetimeHST))
eff <- filter(eff, depth < -375)

sight2 <- sight
sight2$island <- sight$Island_group

seff <- aggregate(list(Island = eff$Island), by = list(year = eff$Year, month=eff$month, island = eff$Island), FUN = length)

# aggregate total sightings for each species and island in all months
blainville <- aggregate(Species ~ year + month +  island, subset(sight2, Species == "Blainville's beaked whale"), length)
cuvier <- aggregate(Species ~ year + month + island, subset(sight2, Species == "Cuvier's beaked whale"), length)

# merge  sightings dataframes with all combinations of month year and island
seff <- merge(seff, blainville, by = c("year", "month", "island"), all.x = TRUE)
seff <- merge(seff, cuvier, by = c("year", "month", "island"), all.x = TRUE)

# fill missing sightings with 0
seff$Species.x[is.na(seff$Species.x)] <- 0
seff$Species.y[is.na(seff$Species.y)] <- 0

seff <- rename(seff, Blainville = Species.x, Cuvier = Species.y)
eff$year <- eff$Year
eff$month

# find  total effort count by month and island
eff_count <- eff %>%
  group_by(year, month, Island) %>%
  summarise(EffCount = n())

seff$Island <- seff$island
# merge effort count with the seff dataframe
seff <- merge(seff, eff_count, by = c("year", "month", "Island"))
#multiply effort count by 5 minutes, divide by 60 for # hours
seff$eff_hours <- (seff$EffCount * 5) / 60

#before calculating sighting rates, combine hours less than 40 for HI

seff$blv_sight_100hr <- (seff$Blainville ) / (seff$eff_hours / 100)
seff$cuv_sight_100hr <- (seff$Cuvier ) / (seff$eff_hours / 100)

#read in MEI values
MEI <- read.csv("MEI_values_csv.csv", header = TRUE, check.names = FALSE)

# reshape into long format with year, month, and mei value columns
MEI_long <- reshape(MEI, direction = "long", varying = list(names(MEI)[2:13]), v.names = "MEI", times = 1:12, timevar = "month", idvar = "Year")
MEI_long$month <- as.numeric(MEI_long$month)

# match based on year/month
seff$MEI <- MEI_long$MEI[match(paste(seff$year, seff$month), paste(MEI_long$Year, MEI_long$month))]

# create a column that defines all, double check to make sure it worked
seff$Period <- ifelse(seff$MEI <= -0.5, "LN",
                        ifelse(seff$MEI >= 0.5, "EN", "Neutral"))

#strength: does not always work?
#seff$Strength <- ifelse(seff$MEI %in% c(0.5:0.9, -0.5:-0.9), "Weak",
#                          ifelse(seff$MEI %in% c(-1:-1.7, 1:1.7), "Moderate",
#                                 ifelse(abs(seff$MEI) >= 1.8, "Strong", "Neutral")))

seff$Period <- factor(seff$Period, levels = c("EN", "Neutral", "LN"))

#monthly filter for island HI
meih <- filter(seff, Island=="Hawaii")

ggplot(meih, aes(x = Period, y = blv_sight_100hr)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Blainville's sightings/100 hr") +
  xlab("ENSO period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6.2), 
                     breaks = seq(0, 6, by = 1)) +
  geom_jitter(width=.1)+
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 25, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.text = element_text(size = 14))

ggsave("Md_monthly_sighting_rates_ENSO_HI_MEI.jpeg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(meih, aes(x = Period, y = cuv_sight_100hr)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Cuvier's sightings/100 hr") +
  xlab("ENSO period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6.2), 
                     breaks = seq(0, 6, by = 1)) +
  geom_jitter(width=.1)+
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 25, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.text = element_text(size = 14))

ggsave("Zc_monthly_sighting_rates_ENSO_HI_MEI.jpeg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

#yearly sighting rates
seffy <- aggregate(list(Island = eff$Island), by = list(year = eff$Year, island = eff$Island), FUN = length)

# aggregate total sightings for each species and island in all months
blainville <- aggregate(Species ~ year +   island, subset(sight2, Species == "Blainville's beaked whale"), length)
cuvier <- aggregate(Species ~ year +  island, subset(sight2, Species == "Cuvier's beaked whale"), length)

# merge  sightings dataframes with all combinations of year and island
seffy <- merge(seffy, blainville, by = c("year", "island"), all.x = TRUE)
seffy <- merge(seffy, cuvier, by = c("year", "island"), all.x = TRUE)

# fill missing sightings with 0
seffy$Species.x[is.na(seffy$Species.x)] <- 0
seffy$Species.y[is.na(seffy$Species.y)] <- 0

seffy <- rename(seffy, Blainville = Species.x, Cuvier = Species.y)
eff$year <- eff$Year

# find  total effort count by month and island
eff_count <- eff %>%
  group_by(year, Island) %>%
  summarise(EffCount = n())

seffy$Island <- seffy$island
# merge effort count with the seffy dataframe
seffy <- merge(seffy, eff_count, by = c("year", "Island"))
#multiply effort count by 5 minutes, divide by 60 for # hours
seffy$eff_hours <- (seffy$EffCount * 5) / 60

#sightings per 100 hours
seffy$blv_sight_100hr <- (seffy$Blainville ) / (seffy$eff_hours / 100)
seffy$cuv_sight_100hr <- (seffy$Cuvier ) / (seffy$eff_hours / 100)

seffy$Avg_MEI <- sapply(seffy$year, function(year) {
  average_MEI <- mean(seff$MEI[seff$year == year])
  ifelse(is.na(average_MEI), 0, round(average_MEI, 1))
})


# match based on year/month

# create a column that defines all, double check to make sure it worked
seffy$Period <- ifelse(seffy$Avg_MEI <= -0.5, "LN",
                      ifelse(seffy$Avg_MEI >= 0.5, "EN", "Neutral"))

#strength: does not always work?
#seffy$Strength <- ifelse(seffy$MEI %in% c(0.5:0.9, -0.5:-0.9), "Weak",
#                          ifelse(seffy$MEI %in% c(-1:-1.7, 1:1.7), "Moderate",
#                                 ifelse(abs(seffy$MEI) >= 1.8, "Strong", "Neutral")))



seffy$Period <- factor(seffy$Period, levels = c("EN", "Neutral", "LN"))
summary(seffy$blv_sight_100hr)

#write csv

#yearly plot for island of hawaii
meihy <- filter(seffy, Island=="Hawaii")

ggplot(meihy, aes(x = Period, y = blv_sight_100hr)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Blainville's sightings/100 hr") +
  xlab("ENSO period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3), 
                     breaks = seq(0, 3.2, by = 1)) +
  geom_jitter(width=.1)+
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 25, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.text = element_text(size = 14))

ggsave("Md_yearly_sighting_rates_ENSO_HI_MEI.jpeg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(meihy, aes(x = Period, y = cuv_sight_100hr)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Cuvier's sightings/100 hr") +
  xlab("ENSO period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4.2), 
                     breaks = seq(0, 4, by = 1)) +
  geom_jitter(width=.1)+
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 25, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.text = element_text(size = 14))

ggsave("Zc_yearly_sighting_rates_ENSO_HI_MEI.jpeg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

write.csv(seffy, "beaked_yearly_sighting_rates.csv" )



#PDO
#read in pre-made value and period csvs\
pdov <- read.csv("pdo_value_monthly.csv", header = TRUE, check.names = FALSE)
pdop <- read.csv("pdo_period.csv", header = TRUE, check.names = FALSE)

# reshape into long format with year, month, and pdo columns
pdo_long_v <- reshape(pdov, direction = "long", varying = list(names(pdov)[2:13]), v.names = "PDO_values", times = 1:12, timevar = "month")
pdo_long_p <- reshape(pdop, direction = "long", varying = list(names(pdop)[2:13]), v.names = "PDO_period", times = 1:12, timevar = "month")

# match based on year/month
seff$pdo_val <- pdo_long_v$PDO_values[match(paste(seff$year, seff$month), paste(pdo_long_v$Year, pdo_long_v$month))]
seff$pdo_period <- pdo_long_p$PDO_period[match(paste(seff$year, seff$month), paste(pdo_long_p$Year, pdo_long_p$month))]

write.csv(seff, "beaked_monthly_sighting_rates.csv")


#PDO
#plotting
meih <- filter(seff, Island=="Hawaii")

#make it a factor so neutral is kept in the middle
meih$Period <- factor(meih$Period, levels = c("EN", "Neutral", "LN"))

ggplot(meih, aes(x = Period, y = blv_sight_100hr)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Blainville's sightings/100 hr") +
  xlab("ENSO period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.52), 
                     breaks = seq(0, 0.50, by = 0.1)) +
  geom_jitter(width=.1)+
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 25, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.text = element_text(size = 14))

ggsave("Md_monthly_sighting_rates_ENSO_HI_MEI.jpeg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(meih, aes(x = Period, y = cuv_sight_100hr)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Cuvier's sightings/100 hr") +
  xlab("ENSO period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, .52), 
                     breaks = seq(0, .50, by = 0.1)) +
  geom_jitter(width=.1)+
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 25, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.text = element_text(size = 14))

ggsave("Zc_monthly_sighting_rates_ENSO_HI_MEI.jpeg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)


#make it a factor so neutral is kept in the middle
meih$Period <- factor(meih$Period, levels = c("Cold", "Neutral", "Warm"))

#PDO 
ggplot(meih, aes(x = pdo_period, y = blv_sight_100hr)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Blainville's sightings/100 hr") +
  xlab("PDO period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.52), 
                     breaks = seq(0, 0.50, by = 0.1)) +
  geom_jitter(width=.1)+
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 25, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.text = element_text(size = 14))

ggsave("Md_monthly_sighting_rates_PDO_HI.jpeg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(meih, aes(x = pdo_period, y = cuv_sight_100hr)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Cuvier's sightings/100 hr") +
  xlab("PDO period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, .52), 
                     breaks = seq(0, .50, by = 0.1)) +
  geom_jitter(width=.1)+
  theme(axis.text.x = element_text(size = 25, angle = 0, color = "black"),
        axis.text.y = element_text(size = 25, color = "black"),
        axis.title = element_text(size = 30, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .3),
        axis.line.y = element_line(color = "black", size = .3),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.text = element_text(size = 14))

ggsave("Zc_monthly_sighting_rates_PDO_HI.jpeg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)



#final sighting rates

#add MEI to sightings and effort
summary(eff$depth)
eff$depth <- eff$depth*-1
eff <- filter(eff,depth>=375)
# match the MEI values to the eff dataframe based on year and month
eff$MEI <- MEI_long$MEI[match(paste(eff$Year, eff$month), paste(MEI_long$Year, MEI_long$month))]

eff$Period <- ifelse(eff$MEI <= -0.5, "LN",
                     ifelse(eff$MEI >= 0.5, "EN", "Neutral"))

sight$MEI <- MEI_long$MEI[match(paste(sight$year, sight$month), paste(MEI_long$Year, MEI_long$month))]
sight$Period_mei <- ifelse(sight$MEI <= -0.5, "LN",
                     ifelse(sight$MEI >= 0.5, "EN", "Neutral"))
#filter by species
Md <- filter(sight, Species=="Blainville's beaked whale") 
Zc <- filter(sight, Species=="Cuvier's beaked whale")

#replace Md with Zc where necessary for Zc sighting rates
Mdln<- filter(Md, Period_mei=="LN")
Mden<- filter(Md, Period_mei=="EN")
Mdneutral<- filter(Md, Period_mei=="Neutral")

effln<- filter(eff, Period=="LN")
effen<- filter(eff, Period=="EN")
effneutral<- filter(eff, Period=="Neutral")

#overall sighting rates: in final counts df, read "effby100" column
#la nina sighting rates
counts_ln <- Mdln %>% 
  group_by(Species) %>% 
  count()
#then to find effort per 100 hr
constantln <- (count(effln)*5)/60
constantln <- as.numeric(constantln)
counts_ln$effby100 <- ((counts_ln$n)/constantln)*100
counts_ln$ENSO <- "LN"

#el nino sighting rates
counts_en <- Mden %>% 
  group_by(Species) %>% 
  count()
#then to find effort per 100 hr
constanten <- (count(effen)*5)/60
constanten <- as.numeric(constanten)
counts_en$effby100 <- ((counts_en$n)/constanten)*100
counts_en$ENSO <- "EN"

#neutral phase sighting rates
counts_neut <- Mdneutral %>% 
  group_by(Species) %>% 
  count()
#thneut to find effort per 100 hr
constantneut <- (count(effneutral)*5)/60
constantneut <- as.numeric(constantneut)
counts_neut$effby100 <- ((counts_neut$n)/constantneut)*100
counts_neut$ENSO <- "Neutral"
