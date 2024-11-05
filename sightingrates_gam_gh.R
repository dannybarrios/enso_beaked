## sightingrates_gam.R
## Running Generalized Additive Models on sightings data for goose-beaked and Blainville's beaked whales
## Author: Daniel M. Barrios, Michaela A. Kratofil
## Updated: 17 Sep 2024

# load packages
library(mgcv)
library(ggplot2)
library(dplyr)
library(gratia)
library(doBy)
library(here)

#Read in csv (by sampling period- short months combined)
#effort with encounters removed unless with beaked whales

seff <- read.csv("hawaii_sighting_rates_by_consecutive_times_justsearching.csv")

# do exploratory plots of sightings and MEI, month/season, PDO, and NPGO
ggplot(data = seff, aes(x = goosebeaked)) +
  geom_histogram() 
ggplot(data = seff, aes(x = MEI, y = goosebeaked)) +
  geom_point() 
ggplot(data = seff, aes(x = pdo_val, y = goosebeaked)) +
  geom_point() 
ggplot(data = seff, aes(x = npgo, y = goosebeaked)) +
  geom_point() 
ggplot(data = seff, aes(x = month, y = goosebeaked)) +
  geom_point() 
ggplot(data = seff, aes(x = season, y = goosebeaked)) +
  geom_point() 

ggplot(data = seff, aes(x = Blainville)) +
  geom_histogram()
ggplot(data = seff, aes(x = MEI, y = Blainville)) +
  geom_point() 
ggplot(data = seff, aes(x = pdo_val, y = Blainville)) +
  geom_point() 
ggplot(data = seff, aes(x = npgo, y = Blainville)) +
  geom_point() 
ggplot(data = seff, aes(x = month, y = Blainville)) +
  geom_point() 
ggplot(data = seff, aes(x = season, y = Blainville)) +
  geom_point() 

# plot effort by month, then season, to determine whether the coarser representation
# of season (season, categorical) would be better than the continuous version (month)
ggplot(data = seff, aes(x = month, y = eff_hours)) +
  geom_point()
ggplot(data = seff, aes(x = season, y = eff_hours)) +
  geom_point()

#Correlation matrix to ensure that no variables are above a threshold of 0.7
correlation_matrix <- cor(seff[c("pdo_val", "MEI", "npgo", "month")])
print(correlation_matrix)

## fit generalized additive model for each species, predicting sightings (# per month-year)
## with smoothed MEI, NPGO, PDO, season, and an offset for effort (i.e., account for varying effort)

#Zc GAM
goosebeaked_gam_f_sighting <- gam(goosebeaked ~ s(MEI, k = 3) + season + s(npgo, k = 3) +
                            s(pdo_val, k = 3) + offset(log(eff_hours)), data = seff,
                          family=poisson(link = "log"),
                          select = T,
                          method = "REML")

summary(goosebeaked_gam_f_sighting)
gam.check(goosebeaked_gam_f_sighting)
draw(goosebeaked_gam_f_sighting, scales = "fixed")


#Md GAM
blv_gam_f_sighting <- gam(Blainville ~ s(MEI, k = 3) + s(npgo, k = 3) + season +
                            s(pdo_val, k = 3) + offset(log(eff_hours)), data = seff,
                          family=poisson(link = "log"),
                          select = T,
                          method = "REML")
summary(blv_gam_f_sighting)
gam.check(blv_gam_f_sighting)
draw(blv_gam_f_sighting, scales = "fixed")

#plotting
## plot partial effects for ziphius ## --------------------------------------- ##
sm <- smooth_estimates(goosebeaked_gam_f_sighting) %>%
  add_confint()
sm

# add partial residuals to the data
d <- seff %>%
  add_partial_residuals(goosebeaked_gam_f_sighting)

goosebeaked_smooth_MEI <- sm %>%
  ggplot() +
  geom_rug(aes(x = MEI),
           data = d,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = MEI),
              alpha = 0.2) +
  geom_point(aes(x = MEI, y = `s(MEI)`),
             data = d, cex = 1.5, colour = "steelblue3") +
  geom_line(aes(x = MEI, y = est), lwd = 1.2) +
  labs(y = "Partial effect", x = "MEI") +
  theme_classic() +
  theme(
    axis.title = element_text( size = 9),
    axis.text = element_text(size = 8, color = "black")
  )
goosebeaked_smooth_MEI

# make season a factor for display
seff$season <- factor(seff$season, levels = c("Winter", "Spring", "Summer", "Fall"))

ggplot(seff, aes(x = season, y = goosebeaked_sight_100hr)) +
  geom_boxplot() +
  labs(title = "",
       x = "", y = "Sighting Rate (per 100 hours)") +
  theme_minimal()

#plot pdo and npgo as well
goosebeaked_smooth_pdo_val <- sm %>%
  ggplot() +
  geom_rug(aes(x = pdo_val),
           data = d,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = pdo_val),
              alpha = 0.2) +
  geom_point(aes(x = pdo_val, y = `s(pdo_val)`),
             data = d, cex = 1.5, colour = "steelblue3") +
  geom_line(aes(x = pdo_val, y = est), lwd = 1.2) +
  labs(y = "Partial effect", x = "pdo_val") +
  theme_classic() +
  theme(
    axis.title = element_text( size = 9),
    axis.text = element_text(size = 8, color = "black")
  )
goosebeaked_smooth_pdo_val

goosebeaked_smooth_NPGO <- sm %>%
  ggplot() +
  geom_rug(aes(x = npgo),
           data = d,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = npgo),
              alpha = 0.2) +
  geom_point(aes(x = npgo, y = `s(npgo)`),
             data = d, cex = 1.5, colour = "steelblue3") +
  geom_line(aes(x = npgo, y = est), lwd = 1.2) +
  labs(y = "Partial effect", x = "npgo") +
  theme_classic() +
  theme(
    axis.title = element_text( size = 9),
    axis.text = element_text(size = 8, color = "black")
  )
goosebeaked_smooth_NPGO

# make plot of parametric term for season
goosebeaked_szn_term <- termplot(goosebeaked_gam_f_sighting, se = T, plot = F)$season

goosebeaked_par_szn <- ggplot(goosebeaked_szn_term, aes(x=x, y=y)) +
  geom_errorbar(aes(ymin = y-se, ymax = y+se)) +
  geom_point() +
  labs(y = "Partial effect", x = "Season") +
  theme_classic() +
  theme(
    axis.title = element_text( size = 9),
    axis.text = element_text(size = 8, color = "black")
  )

# plot each with annotations
plot_MEI_goosebeaked <- goosebeaked_smooth_MEI +
  annotate("text", x = Inf, y = Inf, label = "a", hjust = 1.1, vjust = 1.1, size = 5)
plot_pdo_goosebeaked <- goosebeaked_smooth_pdo_val +
  annotate("text", x = Inf, y = Inf, label = "b", hjust = 1.1, vjust = 1.1, size = 5)
plot_npgo_goosebeaked <- goosebeaked_smooth_NPGO +
  annotate("text", x = Inf, y = Inf, label = "c", hjust = 1.1, vjust = 1.1, size = 5) 
plot_szn_goosebeaked <- goosebeaked_par_szn +
  annotate("text", x = Inf, y = Inf, label = "d", hjust = 1.1, vjust = 1.1, size = 5)

# arrange plots
plot1 <- cowplot::plot_grid(plot_MEI_goosebeaked, plot_pdo_goosebeaked, plot_npgo_goosebeaked,plot_szn_goosebeaked, align = "h")

#save combined plot
ggsave("Zc_ENSO_SZN_PDO_NPGO_GAM_fig4_2024Sep22.jpg",plot1, 
       width = 169, height = 169, units = "mm", dpi = 600)

ggsave("Zc_ENSO_SZN_PDO_NPGO_GAM_fig4_2024Sep22.tiff",plot1, 
       width = 169, height = 169, units = "mm", dpi = 600)


## plot all partial effects for Mesoplodon ## --------------------------------------- ##

# evaluate the smooths 
sm <- smooth_estimates(blv_gam_f_sighting) %>%
  add_confint()
sm

# add partial residuals to the data
d <- seff %>%
  add_partial_residuals(blv_gam_f_sighting)

blv_smooth_MEI <- sm %>%
  ggplot() +
  geom_rug(aes(x = MEI),
           data = d,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = MEI),
              alpha = 0.2) +
  geom_point(aes(x = MEI, y = `s(MEI)`),
             data = d, cex = 1.5, colour = "steelblue3") +
  geom_line(aes(x = MEI, y = est), lwd = 1.2) +
  labs(y = "Partial effect", x = "MEI") +
  theme_classic() +
  theme(
    axis.title = element_text( size = 9),
    axis.text = element_text(size = 8, color = "black")
  )
blv_smooth_MEI

blv_smooth_pdo_val <- sm %>%
  ggplot() +
  geom_rug(aes(x = pdo_val),
           data = d,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = pdo_val),
              alpha = 0.2) +
  geom_point(aes(x = pdo_val, y = `s(pdo_val)`),
             data = d, cex = 1.5, colour = "steelblue3") +
  geom_line(aes(x = pdo_val, y = est), lwd = 1.2) +
  labs(y = "Partial effect", x = "pdo_val") +
  theme_classic() +
  theme(
    axis.title = element_text( size = 9),
    axis.text = element_text(size = 8, color = "black")
  )
blv_smooth_pdo_val

blv_smooth_NPGO <- sm %>%
  ggplot() +
  geom_rug(aes(x = npgo),
           data = d,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = npgo),
              alpha = 0.2) +
  geom_point(aes(x = npgo, y = `s(npgo)`),
             data = d, cex = 1.5, colour = "steelblue3") +
  geom_line(aes(x = npgo, y = est), lwd = 1.2) +
  labs(y = "Partial effect", x = "npgo") +
  theme_classic() +
  theme(
    axis.title = element_text( size = 9),
    axis.text = element_text(size = 8, color = "black")
  )
blv_smooth_NPGO

# make plot of parametric term for season
blv_szn_term <- termplot(blv_gam_f_sighting, se = T, plot = F)$season

blv_par_szn <- ggplot(blv_szn_term, aes(x=x, y=y)) +
  geom_errorbar(aes(ymin = y-se, ymax = y+se), width=.15, position=position_dodge(0.2)) +
  geom_point(position=position_dodge(0.2),  shape=16) +
  labs(y = "Partial effect", x = "Season") +
  theme_classic() +
  theme(
    axis.title = element_text( size = 9),
    axis.text = element_text(size = 8, color = "black")
  )
blv_par_szn

# plot each with annotations
plot_MEI_blv <- blv_smooth_MEI +
  annotate("text", x = Inf, y = Inf, label = "a", hjust = 1.1, vjust = 1.1, size = 5)

plot_pdo_blv <- blv_smooth_pdo_val +
  annotate("text", x = Inf, y = Inf, label = "b", hjust = 1.1, vjust = 1.1, size = 5)

plot_npgo_blv <- blv_smooth_NPGO +
  annotate("text", x = Inf, y = Inf, label = "c", hjust = 1.1, vjust = 1.1, size = 5)

plot_szn_blv <- blv_par_szn +
  annotate("text", x = Inf, y = Inf, label = "d", hjust = 1.1, vjust = 1.1, size = 5)

# arrange plots
plot2 <- cowplot::plot_grid(plot_MEI_blv, plot_pdo_blv, plot_npgo_blv,plot_szn_blv,align = "h")

#save combined plot
ggsave("Md_ENSO_SZN_PDO_NPGO_GAM_fig4_2024Sep22.jpg",plot2, 
       width = 169, height = 169, units = "mm", dpi = 600)

ggsave("Md_ENSO_SZN_PDO_NPGO_GAM_fig4_2024Sep22.tiff",plot2, 
       width = 169, height = 169, units = "mm", dpi = 600)
