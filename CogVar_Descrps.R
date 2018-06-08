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



#Look over Cog variables and transform
