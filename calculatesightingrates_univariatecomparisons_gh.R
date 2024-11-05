## ## calculatesightingrates_univariatecomparisons
## Calculating sighting rates for two species of beaked whale for ENSO, PDO, NPGO, and season. 
## Also comparing those indices with sea state. 
## Analyzing using figures, univariate comparisons, and summary stats. 
## Author: Daniel M. Barrios, Michaela A. Kratofil
## Updated: 01 Sep 2024

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(doBy)
library(here)
library(grid)
library(patchwork)

#Sightings and effort have been restricted to HI Island, depths greater than 375 m, etc, periods off-effort have been removed, 
#and encounters with other species have been removed from effort (e.g. only while searching). 

# set working directory (or whatever your preferred method is)

## read in sightings and effort file and prep for analyses ## ------------------- ##

sight <- read.csv("Allsightings_biasremoved_perbelow30hradj_oneffort_375m_HI_Island.csv")
eff <- read.csv("Effort_Oct2022_GIS_Enso_comb_beaked_oneffort_375m_HI_Island.csv")

summary(sight)
summary(eff)

# ensure only on effort
unique(eff$On_Off_effort)

# format date time for effort and sighting in the same time zone 
eff$datetime_utc <- as.POSIXct(eff$datetime_utc, tz="UTC", format = "%m/%d/%Y %H:%M")
sight$datetime_utc <- as.POSIXct(sight$datetimeUTC, tz="UTC", format = "%m/%d/%Y %H:%M")

# check: the effort data doesn't extend to the same date as the sightings data, 
# so something is going on here with the effort data... 
summary(eff$datetime_utc)
summary(sight$datetime_utc)

# aggregate effort data by island, year, month, getting the total number of 5-min effort points
# per level of aggregation
sight$Island
eff$Island
seff <- eff %>%
  group_by(year = Year, month = month) %>%
  summarise(EffCount = n())

# aggregate total sightings for each species and island in all months
blainville <- aggregate(Species ~ year + month, subset(sight, Species == "Blainville's beaked whale"), length)
goosebeaked <- aggregate(Species ~ year + month , subset(sight, Species == "Cuvier's beaked whale"), length)

# merge  sightings dataframes with all combinations of month year and island
seff <- merge(seff, blainville, by = c("year", "month"), all.x = TRUE)
seff <- merge(seff, goosebeaked, by = c("year", "month"), all.x = TRUE)

# fill missing sightings with 0
seff$Species.x[is.na(seff$Species.x)] <- 0
seff$Species.y[is.na(seff$Species.y)] <- 0

seff <- rename(seff, Blainville = Species.x, goosebeaked = Species.y)
# multiply effort count by 5 minutes, divide by 60 for # hours
seff$eff_hours <- (seff$EffCount * 5) / 60

#before calculating sighting rates, double check that all month/periods have more than 30 hr effort
seff$blv_sight_100hr <- (seff$Blainville ) / (seff$eff_hours / 100)
seff$cuv_sight_100hr <- (seff$goosebeaked ) / (seff$eff_hours / 100)

## get MEI values to assess patterns of sighting rates ~ ENSO ## ------------- ##
MEI_long <- read.csv("MEI_values_csv_long.csv", header = TRUE, check.names = FALSE)


# match based on year/month
seff$MEI <- MEI_long$MEI[match(paste(seff$year, seff$month), paste(MEI_long$YEAR, MEI_long$month))]

# create a column that defines all, double check to make sure it worked
seff$Period <- ifelse(seff$MEI <= -0.5, "LN",
                        ifelse(seff$MEI >= 0.5, "EN", "Neutral"))

#order from negative to positive
seff$Period <- factor(seff$Period, levels = c("LN", "Neutral", "EN"))

#write.csv(seff, "beakedwhales_sightingrates.csv")

## make summary tables of sighting rates and effort across ENSO phases ## ---- ##
# saving tables of this to refer/check 
blv_sum <- summaryBy(blv_sight_100hr ~ Period, data = seff, FUN = c(length, min, max, mean, median, sd, sum))
#write.csv(blv_sum, ("Md_sighting_rates_by_ENSO_phase.csv"), row.names = F)

cuv_sum <- summaryBy(cuv_sight_100hr ~ Period, data = seff, FUN = c(length, min, max, mean, median, sd, sum))
#write.csv(cuv_sum, ("Zc_sighting_rates_by_ENSO_phase.csv"), row.names = F)

eff_sum <- summaryBy(eff_hours ~ Period, data = seff, FUN = c(length, min, max, mean, median, sd, sum))
#write.csv(eff_sum, "Search_effort_hours_by_ENSO_phase.csv", row.names = F)

# assign categorical period for MEI data, then get a summary of this 
MEI_long$Period <- ifelse(MEI_long$MEI <= -0.5, "LN",
                      ifelse(MEI_long$MEI >= 0.5, "EN", "Neutral"))

mei_sum <- summaryBy(month ~ Period, data = MEI_long, FUN = c(length, min, max, mean, median, sd, sum))
#write.csv(mei_sum, "MEI_months_by_ENSO_phase.csv", row.names = F)


## statistical test of sighting rates ~ ENSO phase ## ------------------------- ##
# Blainville's 
md_enso_kt <- kruskal.test(blv_sight_100hr ~ Period, data = seff)
md_enso_kt

# goosebeaked 
zc_enso_kt <- kruskal.test(cuv_sight_100hr ~ Period, data = seff)
zc_enso_kt

## summary plots ## ----------------------------------------------------------- ##

#changing abbreviations to full names for figures
seff$Period <- ifelse(seff$Period == "LN", "La Niña",
                      ifelse(seff$Period == "Neutral", "Neutral", "El Niño"))

seff$Period <- factor(seff$Period, levels = c("La Niña", "Neutral", "El Niño")) 

# Md sighting rate box plots
summary(seff$blv_sight_100hr)
ggplot(seff, aes(x = Period, y = blv_sight_100hr)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Blainville's sightings/100 hr") +
  xlab("ENSO period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7),
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

# ggsave("Md_monthly_sighting_rates_ENSO_HI_MEI.jpeg", plot=last_plot(),
#        path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

ggplot(seff, aes(x = Period, y = cuv_sight_100hr)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("goosebeaked's sightings/100 hr") +
  xlab("ENSO period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 9), 
                     breaks = seq(0, 8, by = 1)) +
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

# ggsave("Zc_monthly_sighting_rates_ENSO_HI_MEI.jpeg", plot=last_plot(),
#        path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

summary(seff$cuv_sight_100hr)
#final plots
plot_a <- ggplot(seff, aes(x = Period, y = blv_sight_100hr)) +
  geom_boxplot(color = "black", fill = "white", size=.25) +
  geom_jitter(width = .1, color = "black", size=.25) +
  ylab("Sightings/100 hr") +
  xlab("") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6.2), breaks = seq(0, 6, by = 1)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 9, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .25),
        axis.line.y = element_line(color = "black", size = .25),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.ticks=element_line(size=0.25),
        legend.text = element_text(size = 14))+
  annotate("text", x = "El Niño", y =5.8 ,  label = "a", hjust=-3)

plot_b <- ggplot(seff, aes(x = Period, y = cuv_sight_100hr)) +
  geom_boxplot(color = "black", fill = "white", size=.25) +
  geom_jitter(width = .1, color = "black", size=.25) +
  ylab("Sightings/100 hr") +
  xlab("ENSO period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 9), breaks = seq(0, 8, by = 2)) +
  theme(axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 9, color = "black", margin = margin(t = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "black", size = .25),
        axis.line.y = element_line(color = "black", size = .25),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.ticks=element_line(size=0.25),
        legend.text = element_text(size = 14))+
  annotate("text", x = "El Niño", y = 8.8, label = "b", hjust=-3)
plot_b
# combine plots
combined_plot <- plot_a + plot_b +
  plot_layout(ncol = 1, nrow = 2, heights = c(1, 1), widths = c(1, 1))

combined_plot
ggsave("Fig3_sightingrates_boxplot_comp_JUN24.jpg",combined_plot, 
       width = 81, height = 169, units = "mm", dpi = 600)

ggsave("Fig3_sightingrates_boxplot_comp_JUN24.tiff",combined_plot, 
       width = 81, height = 169, units = "mm", dpi = 600)


## PDO ## -------------------------------------------------------------------- ##
#read in pre-made value and period csvs
pdo_long_v <- read.csv("pdo_value_monthly_long.csv", header = TRUE, check.names = FALSE) 

# match based on year/month
seff$pdo_val <- pdo_long_v$PDO_values[match(paste(seff$year, seff$month), paste(pdo_long_v$Year, pdo_long_v$month))]

# assign categorical PDO periods based on the index value 
seff$pdo_period <- ifelse(seff$pdo_val <= -0.5, "Cold",
                          ifelse(seff$pdo_val >= 0.5, "Warm", "Neutral"))

# now make summary tables of sighting rates by PDO
summary_data <- aggregate(seff$blv_sight_100hr, by = list(seff$pdo_period), FUN = summary) # delete this if you aren't using it
summary_data <- aggregate(seff$cuv_sight_100hr, by = list(seff$pdo_period), FUN = summary) # delete this if you aren't using it
blv_pdo_sum <- summaryBy(blv_sight_100hr ~ pdo_period, data = seff, FUN = c(length, min, max, mean, median, sd))
#write.csv(blv_pdo_sum, "Md_sighting_rates_by_PDO_phase.csv", row.names = F)

cuv_pdo_sum <- summaryBy(cuv_sight_100hr ~ pdo_period, data = seff, FUN = c(length, min, max, mean, median, sd))
#write.csv(cuv_pdo_sum, "Zc_sighting_rates_by_PDO_phase.csv", row.names = F)

eff_pdo_sum <- summaryBy(eff_hours ~ pdo_val, data = seff, FUN = c(length, min, max, mean, median, sd, sum))
#write.csv(eff_sum, "Search_effort_hours_by_ENSO_phase.csv", row.names = F)

## statistical test of sighting rates ~ PDO phase ## ------------------------- ##

# Blainville's 
md_pdo_kt <- kruskal.test(blv_sight_100hr ~ pdo_period, data = seff)
md_pdo_kt

# goosebeaked 
zc_pdo_kt <- kruskal.test(cuv_sight_100hr ~ pdo_period, data = seff)
zc_pdo_kt

## summary plots ## ----------------------------------------------------------- ##
#make it a factor so neutral is kept in the middle
seff$pdo_period <- factor(seff$pdo_period, levels = c("Cold", "Neutral", "Warm"))

ggplot(seff, aes(x = pdo_period, y = blv_sight_100hr)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Blainville's sightings/100 hr") +
  xlab("PDO period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6.8),
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

ggsave("Md_monthly_sighting_rates_PDO_HI.jpeg", plot=last_plot(),
       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

# something is wrong with the scale_y_continuous as is
ggplot(seff, aes(x = pdo_period, y = cuv_sight_100hr)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("goosebeaked's sightings/100 hr") +
  xlab("PDO period") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12.5),
                     breaks = seq(0, 12, by = 2)) +
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

## npgo ## -------------------------------------------------------------------- ##

# read in NPGO file 
npgo <- read.csv("npgo_values_monthly.csv", header = TRUE, check.names = FALSE)

# assign NPGO values to the sighting rates dataset based on month and year
seff$npgo <- npgo$NPGO_val[match(paste(seff$year, seff$month), paste(npgo$year, npgo$month))]

# assign categorical NPGO value
seff$npgo_val <- ifelse(seff$npgo <= -0.01, "Cold",
                          ifelse(seff$npgo >= 0.01, "Warm", "Neutral"))

# get summary tables 
blv_npgo_sum <- summaryBy(blv_sight_100hr ~ npgo_val, data = seff, FUN = c(length, min, max, mean, median, sd))
#write.csv(blv_npgo_sum, "Md_sighting_rates_by_NPGO_phase.csv", row.names = F)

cuv_npgo_sum <- summaryBy(cuv_sight_100hr ~ npgo_val, data = seff, FUN = c(length, min, max, mean, median, sd))
#write.csv(cuv_npgo_sum, "Zc_sighting_rates_by_NPGO_phase.csv", row.names = F)

eff_pdo_sum <- summaryBy(eff_hours ~ npgo_val, data = seff, FUN = c(length, min, max, mean, median, sd, sum))
#write.csv(eff_sum, "Search_effort_hours_by_ENSO_phase.csv", row.names = F)



## statistical test of sighting rates ~ NPGO phase ## ------------------------- ##

# Blainville's 
md_npgo_kt <- kruskal.test(blv_sight_100hr ~ npgo_val, data = seff)
md_npgo_kt

# goosebeaked 
zc_npgo_kt <- kruskal.test(cuv_sight_100hr ~ npgo_val, data = seff)
zc_npgo_kt

## summary plots by NPGO 
ggplot(seff, aes(x = npgo_val, y = blv_sight_100hr)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Blainville's sightings/100 hr") +
  xlab("NPGO period") +
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 12), 
  #                    breaks = seq(0, .50, by = 0.1)) +
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

ggplot(seff, aes(x = npgo_val, y = cuv_sight_100hr)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("goosebeaked's sightings/100 hr") +
  xlab("NPGO period") +
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 12), 
  #                    breaks = seq(0, .50, by = 0.1)) +
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

#write.csv(seff, "hawaii_md_zc_sighting_rates_by_consecutive_times_searching_eff_only.csv")

## seasons ## ----------------------------------------------------------------- ##

# assign season (oceanographic season) based on month 
seff$season <- NA
seff$season <- ifelse(seff$month %in% c(11, 12, 1), "Fall",
                      ifelse(seff$month %in% c(2, 3, 4), "Winter",
                             ifelse(seff$month %in% c(5, 6, 7), "Spring",
                                    ifelse(seff$month %in% c(8, 9, 10), "Summer", NA))))

# create factor with specific order of levels
seff <- seff %>%
  mutate(season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter")))

# create summary tables 
blv_szn_sum <- summaryBy(blv_sight_100hr ~ season, data = seff, FUN = c(length, min, max, mean, median, sd))
#write.csv(blv_szn_sum,  "Md_sighting_rates_by_season.csv", row.names = F)

cuv_szn_sum <- summaryBy(cuv_sight_100hr ~ season, data = seff, FUN = c(length, min, max, mean, median, sd))
#write.csv(cuv_szn_sum, "Zc_sighting_rates_by_season.csv", row.names = F)

eff_szn_sum <- summaryBy(eff_hours ~ season, data = seff, FUN = c(length, min, max, mean, median, sd, sum))

#write.csv(eff_szn_sum, "searching_effort_hours_by_season.csv", row.names = F)

## statistical test of sighting rates ~ Season ## ------------------------- ##

# Blainville's 
md_szn_kt <- kruskal.test(blv_sight_100hr ~ season, data = seff)
md_szn_kt

# goosebeaked 
zc_szn_kt <- kruskal.test(cuv_sight_100hr ~ season, data = seff)
zc_szn_kt

## assess sea state across ENSO phases ## ------------------------------------ ##

# match based on year/month
eff$MEI <- MEI_long$MEI[match(paste(eff$Year, eff$month), paste(MEI_long$YEAR, MEI_long$month))]

unique(eff$Sea_state_num) 
#DMB: 100 created come from NAs in the original column (double checked Sea_State) 
effkt <- eff %>%
  filter(Sea_state_num != 100)

# check
summary(effkt$Sea_state_num)

# create a column that defines all, double check to make sure it worked
effkt$Period <- ifelse(effkt$MEI <= -0.5, "LN",
                      ifelse(effkt$MEI >= 0.5, "EN", "Neutral"))


effkt$Date <- as.Date(effkt$datetime_hst1)


# summarize daily sea state
daily_sea_state <- effkt %>%
  group_by(Date) %>%
  summarize(avg_sea_state = mean(Sea_state_num))

# add month and year to match MEI
daily_sea_state <- daily_sea_state %>%
  mutate(
    year = year(Date),
    month = month(Date)
  )

#add MEI here too
daily_sea_state$MEI <- MEI_long$MEI[match(paste(daily_sea_state$year, daily_sea_state$month), paste(MEI_long$YEAR, MEI_long$month))]

# create a column that defines all, double check to make sure it worked
daily_sea_state$Period <- ifelse(daily_sea_state$MEI <= -0.5, "LN",
                       ifelse(daily_sea_state$MEI >= 0.5, "EN", "Neutral"))

# get summary table
summaryBy(avg_sea_state ~ Period, data = daily_sea_state, FUN = c(length, min, max, mean, median, sd, sum))

# run a Kruskal Wallis test 
kt <- kruskal.test(avg_sea_state ~ Period, data = daily_sea_state)
print(kt)
summary(kt)

# Post-Hoc
posthoc_results <- pairwise.wilcox.test(daily_sea_state$avg_sea_state, daily_sea_state$Period, p.adjust.method = "bonferroni")
print(posthoc_results)



#sea state by period instead
effortperiod_sea_state <- effkt %>%
  group_by(year = year(Date), month = month(Date)) %>%
  summarize(avg_sea_state = mean(Sea_state_num, na.rm = TRUE))
# add month and year to match MEI

#add MEI here too
effortperiod_sea_state$MEI <- MEI_long$MEI[match(paste(effortperiod_sea_state$year, effortperiod_sea_state$month), paste(MEI_long$YEAR, MEI_long$month))]

# create a column that defines all, double check to make sure it worked
effortperiod_sea_state$Period <- ifelse(effortperiod_sea_state$MEI <= -0.5, "LN",
                                 ifelse(effortperiod_sea_state$MEI >= 0.5, "EN", "Neutral"))

# get summary table
summaryBy(avg_sea_state ~ Period, data = effortperiod_sea_state, FUN = c(length, min, max, mean, median, sd, sum))

# run a Kruskal Wallis test 
kt <- kruskal.test(avg_sea_state ~ Period, data = effortperiod_sea_state)
print(kt)
summary(kt)

# Post-Hoc
posthoc_results <- pairwise.wilcox.test(daily_sea_state$avg_sea_state, daily_sea_state$Period, p.adjust.method = "bonferroni")
print(posthoc_results)
