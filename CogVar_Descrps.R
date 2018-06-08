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
d_chld <- as.data.frame(read_sav("childhood.sav"))
d_chld <- d_chld %>%
  mutate(gsub("[[:space:]]", "", d_chld$subID),
         to_add = ifelse(nchar(d_chld$subID) == 3, "DMHDS0", "DMHDS"),
         ID = paste(to_add, subID, sep = ""))
d_chld <- d_chld[c(-1, -11, -12)]
d_chld <- d_chld[c(10, 1, 2, 3, 4, 5, 6, 7, 8, 9)]
d_chldNA <- na.omit(d_chld)                  
