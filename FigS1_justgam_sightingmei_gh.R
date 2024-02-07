## FIGS1_justgam_sightingmei_gh
## Running Generalized Additive Models on sightings data for Cuvier's and Blainville's beaked whales
## Author: Daniel M. Barrios & Michaela A. Kratofil
## Updated: 8 January 2024

## edited by Michaela Kratofil, 30 January 2024

#GAMs
library(mgcv)
library(ggplot2)
library(dplyr)
library(gratia)

#Read in csv (by sampling period- short months combined)
seff <- read.csv("beaked_whale_spatialuse-danny/enso_manuscript/hawaii_sighting_rates_by_consecutive_times.csv")

# do exploratory plot of sightings and MEI 
ggplot(data = seff, aes(x = Cuv_sight)) +
  geom_histogram()

ggplot(data = seff, aes(x = mei, y = Cuv_sight_100_hr)) +
  geom_point() 

ggplot(data = seff, aes(x = Blv_sight)) +
  geom_histogram()

ggplot(data = seff, aes(x = mei, y = Blv_sight_100_hr)) +
  geom_point() 

## fit generalized additive model for each species, predicting sightings (# per month-year)
## with a smoothed MEI term, and an offset for effort (i.e., account for varying effort)

cuv_gam_f_sighting <- gam(Cuv_sight ~ s(mei) + offset(log(eff_hours)), data = seff, family=poisson(),
                          method = "REML")
plot(cuv_gam_f_sighting, shade = T)

blv_gam_f_sighting <- gam(Blv_sight ~ s(mei) + offset(log(eff_hours)), data = seff, family=poisson(),
                          method = "REML")

plot(blv_gam_f_sighting)

#run gam check
gam.check(blv_gam_f_sighting)
gam.check(cuv_gam_f_sighting)

summary(blv_gam_f_sighting)
summary(cuv_gam_f_sighting)

## plot partial effects for ziphius ## --------------------------------------- ##

# evaluate the smooths 
sm <- smooth_estimates(cuv_gam_f_sighting) %>%
  add_confint()
sm

# add partial residuals to the data
d <- seff %>%
  add_partial_residuals(cuv_gam_f_sighting)

cuv_smooth <- sm %>%
  ggplot() +
  geom_rug(aes(x = mei),
           data = d,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = mei),
              alpha = 0.2) +
  geom_point(aes(x = mei, y = `s(mei)`),
             data = d, cex = 1.5, colour = "steelblue3") +
  geom_line(aes(x = mei, y = est), lwd = 1.2) +
  labs(y = "Partial effect", x = "MEI") +
  theme_classic() +
  theme(
    axis.title = element_text( size = 9),
    axis.text = element_text(size = 8, color = "black")
  )
cuv_smooth

ggsave("beaked_whale_spatialuse-danny/enso_manuscript/zc_gam_mei_partial_effects_plot.tiff", width = 81, height = 81, units = "mm", dpi = 300)


## do the same plot for Mesoplodon ## ---------------------------------------- ##

# evaluate the smooths 
md_sm <- smooth_estimates(blv_gam_f_sighting) %>%
  add_confint()
md_sm

# add partial residuals to the data
md_d <- seff %>%
  add_partial_residuals(blv_gam_f_sighting)

blv_smooth <- md_sm %>%
  ggplot() +
  geom_rug(aes(x = mei),
           data = md_d,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = mei),
              alpha = 0.2, data = md_sm) +
  geom_point(aes(x = mei, y = `s(mei)`),
             data = md_d, cex = 1.5, colour = "steelblue3") +
  geom_line(aes(x = mei, y = est), lwd = 1.2, data = md_sm) +
  labs(y = "Partial effect", x = "MEI") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8, color = "black")
  )
blv_smooth

ggsave("beaked_whale_spatialuse-danny/enso_manuscript/md_gam_mei_partial_effects_plot.tiff", width = 81, height = 81, units = "mm", dpi = 300)


## use ggpubr to combine the two ## ------------------------------------------ ##
library(ggpubr)

comb <- ggarrange(blv_smooth, cuv_smooth, nrow = 1, ncol = 2, labels = c("a","b"),
                  font.label = list(size = 10, face = "plain"), label.x = .9)
comb

# save it 
ggsave("beaked_whale_spatialuse-danny/enso_manuscript/combined_gam_mei_partial_effects_plots.tiff",
       width = 169, height = 81, units = "mm", dpi = 300)



