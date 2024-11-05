## groupsize_comparisons
## Determining differences in group size for goose-beaked and Blainville's beaked whale between ENSO periods and season
## Author: Daniel M. Barrios
## Updated: Aug 15 2024

library(dplyr)
library(lubridate)
library(ggplot2)
library(doBy)
library(cowplot)
library(ggpubr)

#read in sightings dataset
sight <- read.csv("Allsightings_biasremoved_perbelow30hradj_oneffort_375m_HI_Island.csv")

#Remove Nov 2022 tag? Or add Nov 2022 effort to whole project?

#read in MEI values
MEI_long <- read.csv("MEI_values_csv_long.csv")
#add MEI values to sightings df
sight$MEI <- MEI_long$MEI[match(paste(sight$year, sight$month), paste(MEI_long$YEAR, MEI_long$month))]
#create ENSO labels from MEI
sight$Period_mei <- ifelse(sight$MEI <= -0.5, "LN",
                           ifelse(sight$MEI >= 0.5, "EN", "Neutral"))
#order ENSO periods as factor
sight$Period_mei <- factor(sight$Period_mei, levels = c("LN", "Neutral", "EN"),
                           labels = c("La Niña", "Neutral", "El Niño"))
# add an oceanographic season season column based on month and year
sight$season <- ifelse(sight$month %in% c(11, 12, 1), "Fall",
                       ifelse(sight$month %in% c(2, 3, 4), "Winter",
                              ifelse(sight$month %in% c(5, 6, 7), "Spring",
                                     ifelse(sight$month %in% c(8, 9, 10), "Summer", NA))))

#order seasons as a factor
sight <- sight %>%
  mutate(season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter")))

#create Md/Zc dataframes
Md <- filter(sight, Species=="Blainville's beaked whale")
Zc <- filter(sight, Species=="Cuvier's beaked whale")

#Group size summary for Md by ENSO period
#Plot ENSO vs. Md
unique(Md$Group.size..best.)
mdsize <- ggplot(Md, aes(x = Period_mei, y = Group.size..best.)) +
  geom_boxplot(color = "black", size = 0.25, outlier.size=.2) +  # Remove default outliers
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Group size") +
  xlab("") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 11.2), 
                     breaks = seq(0, 10, by = 2)) +
  theme(axis.text.x = element_text(size = 8, angle = 0, color = "black"),
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
  annotate("text", x = "El Niño", y =10.65 ,  label = "a", hjust=-5, size = 8)
mdsize

#ggsave("Md_mei_sightings_groupsize.jpeg", plot=last_plot(),
#       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

#ENSO summary for Md group size
Md_ENSO_sum <- summaryBy(Group.size..best. ~ Period_mei, data = Md, FUN = c(length, min, max, mean, median, sd))
Md_ENSO_sum
#write.csv(Zc_ENSO_sum, "Blainville_groupsize_summarybyENSO.csv", row.names = F)

#####Now do the same for Zc and ENSO
#Plot ENSO vs. Zc
unique(Zc$Group.size..best.)
zcsize <- ggplot(Zc, aes(x = Period_mei, y = Group.size..best.)) +
  geom_boxplot(color = "black", size = 0.25, outlier.size=.2) +  # Remove default outliers
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("") +
  xlab("") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7), 
                     breaks = seq(0, 7, by = 1)) +
  theme(axis.text.x = element_text(size = 8, angle = 0, color = "black"),
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
  annotate("text", x = "El Niño", y =6.8 ,  label = "b", hjust=-5, size = 8)
zcsize
#ggsave("Zc_mei_sightings_groupsize.jpeg", plot=last_plot(),
#       path=NULL, width = 12, height = 12, device="jpeg", dpi=600)

#ENSO summary for Zc group size
Zc_ENSO_sum <- summaryBy(Group.size..best. ~ Period_mei, data = Zc, FUN = c(length, min, max, mean, median, sd))
Zc_ENSO_sum
#write.csv(Zc_ENSO_sum, "goosebeaked_groupsize_summarybyENSO.csv", row.names = F)

#combine plots for Figure
comb <- ggarrange(mdsize, zcsize, nrow = 1, ncol = 2, labels = c("a","b"),
                  font.label = list(size = 10, face = "plain"), label.x = .9)
#ggsave("figS1_ENSO_group_size_JUN24.jpg",comb, 
#       width = 169, height = 81, units = "mm", dpi = 600)
#ggsave("figS1_ENSO_group_size_JUN24.tiff",comb, 
#       width = 169, height = 81, units = "mm", dpi = 600)

###################season

#Season vs Zc
zcsize <- ggplot(Zc, aes(x = season, y = Group.size..best.)) +
  geom_boxplot(color = "black", size = 0.25, outlier.size=.2) +  # Remove default outliers
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("") +
  xlab("") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 7), 
                     breaks = seq(0, 7, by = 1)) +
  theme(axis.text.x = element_text(size = 8, angle = 0, color = "black"),
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
  annotate("text", x = "Winter", y =6.8 ,  label = "b", hjust=-5, size = 8)
Zc_szn_sum <- summaryBy(Group.size..best. ~ Season, data = Zc, FUN = c(length, min, max, mean, median, sd))
Zc_szn_sum
#write.csv(Zc_szn_sum, "goosebeaked_groupsize_summarybyseason.csv", row.names = F)

#Season vs Md
mdsize <- ggplot(Md, aes(x = season, y = Group.size..best.)) +
  geom_boxplot(color = "black", size = 0.25, outlier.size=.2) +  # Remove default outliers
  theme(axis.text.x = element_text(angle = 45)) +
  ylab("Group size") +
  xlab("") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 11.2), 
                     breaks = seq(0, 10, by = 2)) +
  theme(axis.text.x = element_text(size = 8, angle = 0, color = "black"),
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
  annotate("text", x = "Winter", y =10.65 ,  label = "a", hjust=-5, size = 8)
Md_szn_sum <- summaryBy(Group.size..best. ~ Season, data = Md, FUN = c(length, min, max, mean, median, sd))
Md_szn_sum
#write.csv(Md_szn_sum, "Blainville_groupsize_summarybyseason.csv", row.names = F)

#Combine the two plots for Figure S2
comb <- ggarrange(mdsize, zcsize, nrow = 1, ncol = 2, labels = c("a","b"),
                  font.label = list(size = 10, face = "plain"), label.x = .9)

#ggsave("figS2_season_group_size_JUN24.jpg",comb, 
#       width = 169, height = 81, units = "mm", dpi = 600)

#ggsave("figS2_season_group_size_JUN24.tiff",comb, 
#       width = 169, height = 81, units = "mm", dpi = 600)


#Statistical tests

#Kruskal-Wallis test
#Md ENSO
kruskal_md <- kruskal.test(Group.size..best. ~ Period_mei, data = Md)
kruskal_md
#Md Season
kruskal_md_season <- kruskal.test(Group.size..best. ~ season, data = Md)
kruskal_md_season
#Zc ENSO
kruskal_zc <- kruskal.test(Group.size..best. ~ Period_mei, data = Zc)
kruskal_zc
#Zc Season
kruskal_zc_season <- kruskal.test(Group.size..best. ~ season, data = Zc)
kruskal_zc_season

#none significant per kw test  