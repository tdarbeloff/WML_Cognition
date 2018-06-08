#WML variables descriptives and transformations

library(tidyverse)
library(car)
library(ggplot2)
library(haven)
library(jtools)
library(stats)
library(ggpubr)
library(psych)
library(lm.beta)
library(investr)
library(MASS)
library(lmSupport)
library(caret)
library(broom)


d_wml <- read_csv("H:/Database/DBIS/Imaging/WhiteMatterHyperintensities.csv")
d_wml <- na.omit(d_wml)
d_tbv <- read_csv("H:/Database/DBIS/Imaging/FreeSurfer/FreeSurfer_aseg_SummaryMeasures.csv")


#Lets just look at a few variables we are interested in as of right now
d_wml$wholeBrainWMHvol_mm3 <- as.numeric(d_wml$wholeBrainWMHvol_mm3)
#NA's introduced by coercion?
hist(d_wml$wholeBrainWMHvol_mm3)
describe(d_wml$wholeBrainWMHvol_mm3)

d_wml$wholeBrainWMHvol_mm3_lg <- log(d_wml$wholeBrainWMHvol_mm3)
hist(d_wml$wholeBrainWMHvol_mm3_lg)
#log transformation turns this into a normal distribution

d_wml$PVWMHvol_mm3 <- as.numeric(d_wml$PVWMHvol_mm3)
hist(d_wml$PVWMHvol_mm3)
describe(d_wml$PVWMHvol_mm3)
d_wml$PVWMHvol_mm3_lg <- log(d_wml$PVWMHvol_mm3)
hist(d_wml$PVWMHvol_mm3_lg)
#again, log transformation turns this into a normal distribution

d_wml$DWMHvol_mm3 <- as.numeric(d_wml$DWMHvol_mm3)
hist(d_wml$DWMHvol_mm3)
describe(d_wml$DWMHvol_mm3)
#needs log transformation but min=0. Discuss with Max best way 
#to deal with this. I think my way sucks.

d_wml$wholeBrain_WMHnoc_total <- as.numeric(d_wml$wholeBrain_WMHnoc_total)
hist(d_wml$wholeBrain_WMHnoc_total)
d_wml$wholeBrain_WMHnoc_total_lg <- log(d_wml$wholeBrain_WMHnoc_total)
hist(d_wml$wholeBrain_WMHnoc_total_lg)
describe(d_wml$wholeBrain_WMHnoc_total)
#again, needed log transformation into normal distribution

hist(d_tbv$BrainSegVolNotVentSurf)
hist(d_tbv$TotalGrayVol)
hist(d_tbv$CerebralWhiteMatterVol)

d_BM <- inner_join(d_wml, d_tbv, by=c("ID"))
