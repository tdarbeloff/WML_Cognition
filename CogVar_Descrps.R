#Cognition and WML; Cognition variables- Descriptives and transformations

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

#Read in Datasets

#childhood variables
d_chld <- as.data.frame(read_sav("childhood.sav"))
d_chld <- d_chld %>%
  mutate(gsub("[[:space:]]", "", d_chld$subID),
         to_add = ifelse(nchar(d_chld$subID) == 3, "DMHDS0", "DMHDS"),
         ID = paste(to_add, subID, sep = ""))
d_chld <- d_chld[c(-1, -11, -12)]
d_chld <- d_chld[c(10, 1, 2, 3, 4, 5, 6, 7, 8, 9)]
d_chldNA <- na.omit(d_chld)                  


#Adult Variables
d_adlt <- as.data.frame(read_sav("Cog45_batch27.sav"))
d_adlt <- d_adlt %>%
  mutate(gsub("[[:space:]]", "", d_adlt$subID),
       to_add = ifelse(nchar(d_adlt$subID) == 3, "DMHDS0", "DMHDS"),
       ID = paste(to_add, subID, sep = ""))
d_adlt <- d_adlt[c(14, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]
d_adltNA <- na.omit(d_adlt)

#d_adlt <- d_adlt %>%
#  mutate(gsub("[[:space:]]", "", d_adlt$subID),
#         to_add = ifelse(nchar(d_adlt$subID) == 3, "DMHDS0", "DMHDS"),
#         ID = paste(to_add, subID, sep = "")) %>%
#  subset(d_adlt, select=c(ID, vci45a, pri45a, wmi45a, psi45a, fsiq45a, GpDomTim45,
#                          GpDif45, RAVLtot45, RAVLrec45, TrailB45))



#Education GWAS variable
d_gwas <- as.data.frame(read_sav("Dunedin_EA3_DWB180601.sav"))
d_gwas <- d_gwas %>%
  mutate(gsub("[[:space:]]", "", d_gwas$subID),
         to_add = ifelse(nchar(d_gwas$subID) == 3, "DMHDS0", "DMHDS"),
         ID = paste(to_add, subID, sep = ""))
d_gwas <- d_gwas[c(17, 2, 14)]
d_gwasNA <- na.omit(d_gwas)


#combine datasets by ID
d_1 <- inner_join(d_adltNA, d_chldNA, by=c("ID"))
dcog <- inner_join(d_1, d_gwasNA, by=c("ID"))



#Look over Cog variables and transform/z-score

hist(dcog$vci45a)
vci45_desc <- describe(dcog$vci45a)
#Verbal comprehension at age 45 is normally distributed

hist(dcog$pri45a)
pri45_desc <- describe(dcog$pri45a)
#perecptual reasoning age 45 is relatively normally distributed

hist(dcog$wmi45a)
wmi_desc <- describe(dcog$wmi45a)
#working memory index age 45 is relatively normally distributed

hist(dcog$psi45a)
psi_desc <- describe(dcog$psi45a)
#normal enough

hist(dcog$fsiq45a)
fsiq_desc <- describe(dcog$fsiq45a)
#normal enough

hist(dcog$GpDomTim45)
GpDom45_desc <- describe(dcog$GpDomTim45)
dcog$GpDomTim45_lg <- log(dcog$GpDomTim45)
boxplot(dcog$GpDomTim45_lg)
#log transformed, looks like there are some potential 
#outliers that may come up in future analyses

hist(dcog$GpDif45)
GpDif45_desc <- describe(dcog$GpDif45) 
boxplot(dcog$GpDif45)
#some clear outliers, unsure whether to transform or 
#deal with that at all right now...

hist(dcog$RAVLtot45)
RAVLtot45_desc <- describe(dcog$RAVLtot45)
#normal

hist(dcog$RAVLrec45)
RAVLrec45_desc <- describe(dcog$RAVLrec45)
#normal enough

hist(dcog$TrailB45)
TrailB45_desc <- describe(dcog$TrailB45)
dcog$TrailB45_sqrt <- sqrt(dcog$TrailB45)
boxplot(dcog$TrailB45_sqrt)
hist(dcog$TrailB45_sqrt)
#not sure if we want to stay with that transformation. Something to think about.

