## rangeuseratios_stats
## Summary and formal analysis of range use ratios of beaked whales ~ ENSO and season
## Author: Daniel M. Barrios, Michaela A. Kratofil
## Updated: 22 Sep 2024

# Load necessary libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(here)
library(mgcv)
library(gratia)
library(mgcv.helper)

## read in range use ratio dataframe from "calculate rangeuseratio" script ## -- ##
range_use_ratios_df <- read.csv("ZcTags_weekly_rangeuseratios_2024Aug07.csv")

## add monthly MEI values ## --------------------------------------------------- ##
#read in monthly MEI values, reshape into long format with year, month, and mei value columns
mei <- read.csv("MEI_values_csv.csv")
MEI_long <- reshape(mei, direction = "long", varying = list(names(mei)[2:13]), v.names = "mei", times = 1:12, timevar = "month", idvar = "Year")
MEI_long$month <- as.numeric(MEI_long$month)
range_use_ratios_df$MEI <- MEI_long$mei[match(paste(range_use_ratios_df$year, range_use_ratios_df$month), paste(MEI_long$YEAR, MEI_long$month))]

# create a column that defines all ENSO phases and seasons
range_use_ratios_df$Periodmeiwithflav <- ifelse(range_use_ratios_df$MEI <= -0.5, "LN",
                                                ifelse(range_use_ratios_df$MEI >= 0.5, "EN", "Neutral"))
range_use_ratios_df$season <- ifelse(range_use_ratios_df$month %in% c(11, 12, 1), "Fall",
                                     ifelse(range_use_ratios_df$month %in% c(2, 3, 4), "Winter",
                                            ifelse(range_use_ratios_df$month %in% c(5, 6, 7), "Spring",
                                                   ifelse(range_use_ratios_df$month %in% c(8, 9, 10), "Summer", NA))))

## summary stats ## ----------------------------------------------------------- ##
compute_stats <- function(data) {
  data %>%
    group_by(tag) %>%
    summarise(
      mean = mean(ratioweekly, na.rm = TRUE),
      median = median(ratioweekly, na.rm = TRUE),
      max = max(ratioweekly, na.rm = TRUE), 
      min= min(ratioweekly, na.rm = TRUE),
      sd = sd(ratioweekly, na.rm = TRUE),
      n_weekly_periods = n()
    )
}

compute_stats_season <- function(data) {
  data %>%
    group_by(tag, season) %>%
    summarise(
      mean = mean(ratioweekly, na.rm = TRUE),
      median = median(ratioweekly, na.rm = TRUE),
      max = max(ratioweekly, na.rm = TRUE), 
      min= min(ratioweekly, na.rm = TRUE),
      sd = sd(ratioweekly, na.rm = TRUE),
      n_weekly_periods = n()
    )
}

# range-use ratio function summaries
stats <- compute_stats(range_use_ratios_df)
statsszn <- compute_stats_season(range_use_ratios_df)

# get the number of weekly periods per season
range_use_ratios_df %>%
  group_by(season) %>%
  tally()

# write.csv(range_use_ratios_df, "range_use_ratios_JUN24.csv")
# write.csv(statsszn, "range_use_ratios_stats_szn_JUN24.csv")
# 
# write.csv(stats, "range_use_ratios_stats_JUN24.csv")

## summary plots ## ----------------------------------------------------------- ##
# make covariates ordered factors, and ratios numeric (not units)
range_use_ratios_df$Periodmeiwithflav <- factor(range_use_ratios_df$Periodmeiwithflav, levels = c("Neutral", "LN", "EN"))
range_use_ratios_df$season <- factor(range_use_ratios_df$season, levels = c("Spring", "Summer", "Fall", "Winter"))
range_use_ratios_df$ratioweekly <- as.numeric(range_use_ratios_df$ratioweekly)

# group the seasons into two categories, because few weeks for summer and winter
range_use_ratios_df$szn <- as.character(range_use_ratios_df$season)
range_use_ratios_df <- range_use_ratios_df %>%
  mutate(
    season2 = case_when(szn == "Summer" ~ "Fall",
                        szn == "Winter" ~ "Spring",
                        TRUE ~ szn)
  ) 
range_use_ratios_df$season2 <-  recode(range_use_ratios_df$season2, 
                                       Fall = "Summer/Fall",
                                       Spring = "Winter/Spring")

unique(range_use_ratios_df$season2)

# get the number of weeks per period
range_use_ratios_df %>%
  group_by(Periodmeiwithflav) %>%
  tally()

# get the number of weeks per season
range_use_ratios_df %>%
  group_by(season) %>%
  tally()

# boxplot: range use ratios by ENSO
ggplot(range_use_ratios_df, aes(x = Periodmeiwithflav, y = ratioweekly, fill = Periodmeiwithflav)) +
  geom_boxplot() +
  labs(title = "", x = "ENSO Phase", y = "Range-Use Ratio") +
  theme_minimal()

# boxplot: range use ratios by season
ggplot(range_use_ratios_df, aes(x = season, y = ratioweekly, fill = season)) +
  geom_boxplot() +
  labs(title = "", x = "Season", y = "Range-Use Ratio") +
  theme_minimal()

# boxplot: range use ratios by season (grouped)
ggplot(range_use_ratios_df, aes(x = season2, y = ratioweekly, fill = season2)) +
  geom_boxplot() +
  labs(title = "", x = "Season", y = "Range-Use Ratio") +
  theme_minimal()

# dot plot: range use ratios and MEI
ggplot(range_use_ratios_df, aes(x = MEI, y = ratioweekly)) +
  geom_point() +
  geom_smooth()

# dot plot: range use ratios for each tag 
ggplot(range_use_ratios_df, aes(x = week, y = ratioweekly)) +
  geom_point() +
  facet_wrap(~tag, scales = "free_x")

rur_means_enso <- range_use_ratios_df %>%
  group_by(Periodmeiwithflav) %>%
  summarize(mean_ratioweekly = mean(ratioweekly, na.rm = TRUE))

rur_means_season <- range_use_ratios_df %>%
  group_by(season) %>%
  summarize(mean_ratioweekly = mean(ratioweekly, na.rm = TRUE))

rur_means_season2 <- range_use_ratios_df %>%
  group_by(season2) %>%
  summarize(mean_ratioweekly = mean(ratioweekly, na.rm = TRUE))

## Kruskal-Wallis tests for univariate comparisons ## ----------------------- ##
kt_en <- kruskal.test(ratioweekly ~ Periodmeiwithflav, data = range_use_ratios_df)
print(kt_en)

kt_szn <- kruskal.test(ratioweekly ~ season2, data = range_use_ratios_df)
print(kt_szn)

## GAMMs: effect of MEI and season on weekly range-use ratios ## ------------- ##

# need to make tag a factor to add as a random effect 
range_use_ratios_df$tag_fac <- as.factor(range_use_ratios_df$tag)

# model range use ratios ~ MEI + season
rr_gamm <- mgcv::gam(ratioweekly ~ s(MEI, k = 5) + season2 + s(tag_fac, bs = "re"), 
                   data = range_use_ratios_df, family = betar(link = "logit"),
                   select = T,
                   method = "REML")

# review and check model 
summary(rr_gamm)
draw(rr_gamm, scales = "fixed")
gam.check(rr_gamm)