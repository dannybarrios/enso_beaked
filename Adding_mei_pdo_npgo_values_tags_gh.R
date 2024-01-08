##Adding_mei_pdo_values_tags_gh.R
##Adding mei and pdo values to beaked whale tags

##Author: Daniel M. Barrios
## Updated: 8 January 2024
library(adehabitatLT)
library(dplyr)
library(lubridate)
library(ggplot2)
library(geosphere)

#set working directory (or whatever your preferred method is)
#read in csvs and filter gaps of 1+ days
cuv <- read.csv("Zctagbasefile")
cuv <- filter(cuv, !is.na(seg_id1))
blv <- read.csv("MdTagbasefile")
blv <- filter(blv, !is.na(seg_id1))

#adding MEI monthly MEI values
#read in monthly MEI values, reshape into long format with year, month, and mei value columns
mei <- read.csv("MEI_values_csv.csv")
MEI_long <- reshape(mei, direction = "long", varying = list(names(mei)[2:13]), v.names = "mei", times = 1:12, timevar = "month", idvar = "Year")
MEI_long$month <- as.numeric(MEI_long$month)

blv$MEI <- MEI_long$mei[match(paste(blv$year, blv$month), paste(MEI_long$Year, MEI_long$month))]
# create a column that defines all
blv$Periodmei <- ifelse(blv$MEI <= -0.5, "LN",
                        ifelse(blv$MEI >= 0.5, "EN", "Neutral"))

#do the same for Zc
cuv$MEI <- MEI_long$mei[match(paste(cuv$year, cuv$month), paste(MEI_long$Year, MEI_long$month))]
cuv$Periodmei <- ifelse(cuv$MEI <= -0.5, "LN",
                        ifelse(cuv$MEI >= 0.5, "EN", "Neutral"))


#PDO
#read in pre-made value and period csvs
cuv <- read.csv("ZcTags_4hr_use_seafloor_geomorph_oceangographic_enso_300m_var_MAY23_v1.csv")
blv <- read.csv("MdTags_4hr_use_seafloor_geomorph_oceangographic_var_enso_MAY23_v1.csv")

pdov <- read.csv("pdo_value_monthly.csv", header = TRUE, check.names = FALSE)
pdop <- read.csv("pdo_period.csv", header = TRUE, check.names = FALSE)

# reshape into long format with year, month, and pdo columns
pdo_long_v <- reshape(pdov, direction = "long", varying = list(names(pdov)[2:13]), v.names = "PDO_values", times = 1:12, timevar = "month")
pdo_long_p <- reshape(pdop, direction = "long", varying = list(names(pdop)[2:13]), v.names = "PDO_period", times = 1:12, timevar = "month")

#add pdo and pdo period to each dataset
blv$PDO <- pdo_long_v$PDO_values[match(paste(blv$year, blv$month), paste(pdo_long_v$Year, pdo_long_v$month))]
blv$PDO_period <- pdo_long_p$PDO_period[match(paste(blv$year, blv$month), paste(pdo_long_p$Year, pdo_long_p$month))]

cuv$PDO <- pdo_long_v$PDO_values[match(paste(cuv$year, cuv$month), paste(pdo_long_v$Year, pdo_long_v$month))]
cuv$PDO_period <- pdo_long_p$PDO_period[match(paste(cuv$year, cuv$month), paste(pdo_long_p$Year, pdo_long_p$month))]


#NPGO
#read in pre-made value and period csvs
cuv <- read.csv("ZcTags_4hr_use_seafloor_geomorph_oceangographic_enso_300m_var_MAY23_v1.csv")
blv <- read.csv("MdTags_4hr_use_seafloor_geomorph_oceangographic_var_enso_MAY23_v1.csv")

npgo <- read.csv("NPGO_values_monthly.csv", header = TRUE, check.names = FALSE)

#add npgo and npgo period to each dataset
blv$NPGO <- npgo$npgo_values[match(paste(blv$year, blv$month), paste(npgo$Year, npgo$month))]
blv$NPGO_period <- npgo$npgo_period[match(paste(blv$year, blv$month), paste(npgo$Year, npgo$month))]

cuv$NPGO <- npgo$NPGO_val[match(paste(cuv$year, cuv$month), paste(npgo$Year, npgo$month))]
cuv$npgo_period <- npgo$NPGO_val[match(paste(cuv$year, cuv$month), paste(npgo$Year, npgo$month))]


#write csvs
