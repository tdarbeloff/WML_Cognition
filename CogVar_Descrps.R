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
#processing speed is normal enough

hist(dcog$fsiq45a)
fsiq_desc <- describe(dcog$fsiq45a)
#full scale IQ is normal enough

hist(dcog$GpDomTim45)
GpDom45_desc <- describe(dcog$GpDomTim45)
dcog$GpDomTim45_lg <- log(dcog$GpDomTim45)
boxplot(dcog$GpDomTim45_lg)
#log transformed, looks like there are some potential 
#outliers that may come up in future analyses
#Should discuss Peg Board dom hand

hist(dcog$GpDif45)
GpDif45_desc <- describe(dcog$GpDif45) 
boxplot(dcog$GpDif45)
#some clear outliers, unsure whether to transform or 
#deal with that at all right now...
#should discuss pegboard nondom hand.

hist(dcog$RAVLtot45)
RAVLtot45_desc <- describe(dcog$RAVLtot45)
#Rey Audit Verb Learning total correct is normal

hist(dcog$RAVLrec45)
RAVLrec45_desc <- describe(dcog$RAVLrec45)
#RAVL recall N correct is normal enough

hist(dcog$TrailB45)
TrailB45_desc <- describe(dcog$TrailB45)
dcog$TrailB45_sqrt <- sqrt(dcog$TrailB45)
boxplot(dcog$TrailB45_sqrt)
hist(dcog$TrailB45_sqrt)
Trail45_desc <- describe(dcog$TrailB45)
#not sure if we want to stay with that transformation. Something to think about.
#should discuss trails test B, timed in seconds

#that is it for adult cognitive variables for now






#Let's look at childhood variables

hist(dcog$ZCHBR3)
ZCHBR3_desc <- describe(dcog$ZCHBR3)
#I think this has already been z-scored? Not sure about transforming 
#this one. It does have a negative skew

hist(dcog$wviq7911)
wviq7_desc <- describe(dcog$wviq7911)
#mean verbal IQ in chldhood looks normal enoguh

hist(dcog$wpiq711)
wpiq7_desc <- describe(dcog$wpiq711)
boxplot(dcog$wpiq711)
#mean performance IQ is mostly normale with a slight negative skew

hist(dcog$wfsiq711)
wfsiq7_desc <- describe(dcog$wfsiq711)
#mean fullscale IQ looks normal enough

hist(dcog$RAVL13_tot)
RAVL13tot_desc <- describe(dcog$RAVL13_tot)
boxplot(dcog$RAVL13_tot)
#RAVL 4 trials summed at 13 doesnt look too bad, but
#there is definitely a negative skew. Not enough for me to transform right now

hist(dcog$RAVL13_recl)
RAVL13recl_desc <- describe(dcog$RAVL13_recl)
boxplot(dcog$RAVL13_recl)
#boxplot error?? Check with Max
#definitely negative skew. Did not transform it

hist(dcog$TrailsB13)
TrailsB13_desc <- describe(dcog$TrailsB13)
dcog$TrailsB13_lg <- log(dcog$TrailsB13)
hist(dcog$TrailsB13_lg)
boxplot(dcog$TrailsB13_lg)
#severe positive skew. log transformed into more normal shape
#although there is still slight positive skew

hist(dcog$GPdom13)
boxplot(dcog$GPdom13)
#normal shape with outliers--into positive skew

hist(dcog$GPdif13)
boxplot(dcog$GPdif13)
#mostly normal with slight negative skew due to probable outliers.

#last cognition variable is GWAS
hist(dcog$zrpgsEA3)
#looks really normal. I know nothing about using this data, so I will
#leave it alone for now.