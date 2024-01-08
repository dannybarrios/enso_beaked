## FIGS1_justgam_sightingmei_gh
## Running Generalized Additive Models on sightings data for Cuvier's and Blainville's beaked whales
## Author: Daniel M. Barrios
## Updated: 8 January 2024

#GAMs
library(mgcv)
library(ggplot2)

#setwd

#Read in csv (by sampling period- short months combined)
seff <- read.csv("hawaii_sighting_rates_by_consecutive_times.csv")

#run gams with smoothed terms

cuv_gam_f_sighting <- gam(Cuv_sight ~ s(mei) + offset(log(eff_hours)),data = seff, family=poisson())
blv_gam_f_sighting <- gam(Blv_sight ~ s(mei) + offset(log(eff_hours)), data = seff, family=poisson())

#run gam check
gam.check(blv_gam_f_sighting)
gam.check(cuv_gam_f_sighting)

summary(blv_gam_f_sighting)
summary(cuv_gam_f_sighting)

#plotting the GAMs
library(sm)
library(mgcViz)
#cuviersplot
cuvgam <- getViz(cuv_gam_f_sighting)
o <- plot( sm(cuvgam, 1) )+ l_fitLine(colour = "red")+ l_rug(mapping = aes(x=x, y=y), alpha = 1) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) 
finalcuvpt <- o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = .5, alpha = 1, color="black") + theme_classic()+
  annotate("text", x =1.6, y = 4.5, label = "b",  size = 2, color = "black")+
  xlab("MEI value") +
  theme(
    axis.text = element_text(size = 5, color = "black"),
    axis.title=element_text(size=6, color="black")
)
finalcuvpt
#the dots are the partial residuals- should be somewhat evenly scattered for decent fit

#blainvillesplot
blvgam <- getViz(blv_gam_f_sighting)
o <- plot( sm(blvgam, 1) )+ l_fitLine(colour = "red")+ l_rug(mapping = aes(x=x, y=y), alpha = 1) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) 
finalblvpt <- o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = .5, alpha = 1, color="black") + theme_classic()+
  annotate("text", x =1.6, y = 4.5, label = "a",  size = 2, color = "black")+
  xlab("MEI value") +
  theme(
    axis.text = element_text(size = 5, color = "black"),
    axis.title=element_text(size=6, color="black")
  )
finalblvpt
#
jpeg(filename = "Md_gam_line_DC23.jpg", res=300)
finalblvpt
dev.off()

postscript(file = "Md_gam_line_DC23.eps", horizontal = FALSE, onefile = FALSE, paper = "special", width = 6, height = 6)
finalblvpt
dev.off()


jpeg(filename = "Zc_gam_line_DC23.jpg", res=300)
finalcuvpt
dev.off()


#check model fit qq plots
resid_cuv <- residuals(cuv_gam_f_sighting)
resid_blv <- residuals(blv_gam_f_sighting)

#shapiro test
shapiro.test(resid_cuv)
shapiro.test(resid_blv)

#compare qqplot line to norm
qqnorm(resid_cuv)
qqline(resid_cuv)

qqnorm(resid_blv)
qqline(resid_blv)
