#preliminary correlations
source("CogVar_Descrps.R")
source("WMLvar_Descrps.R")

#connect 2 datasets
d <- inner_join(d_BM, dcog, by=c("ID"))


#test a few childhood IQ measures, see if anything pops.
cor.test(d$wholeBrainWMHvol_mm3_lg, d$ZCHBR3)
cor.test(d$wholeBrainWMHvol_mm3_lg, d$wpiq711)
cor.test(d$wholeBrainWMHvol_mm3_lg, d$wviq7911)
cor.test(d$wholeBrainWMHvol_mm3_lg, d$wfsiq711)


#double check with adult measures
cor.test(d$wholeBrainWMHvol_mm3_lg, d$fsiq45a)
cor.test(d$PVWMHvol_mm3_lg, d$fsiq45a)

cor.test(d$wholeBrainWMHvol_mm3_lg, d$vci45a)
cor.test(d$wholeBrainWMHvol_mm3_lg, d$pri45a)
cor.test(d$wholeBrainWMHvol_mm3_lg, d$wmi45a)
cor.test(d$wholeBrainWMHvol_mm3_lg, d$psi45a)

#double check what other IQ (age38) correlation was.
d_IQ <- as.data.frame(read_sav("H:/Projects/Tracy/DBIS/RVC/RVC/Phase38_vars.sav"))
d_IQ <- subset(d_IQ, select=c(subID, WFSIQ38))                      
d_IQ <- d_IQ %>%
  mutate(gsub("[[:space:]]", "", d_IQ$subID),
         to_add = ifelse(nchar(d_IQ$subID) == 3, "DMHDS0", "DMHDS"),
         ID = paste(to_add, subID, sep = ""))
d_IQ <- na.omit(d_IQ)

d <- inner_join(d, d_IQ, by=c("ID"))


summary(lm(wholeBrainWMHvol_mm3_lg ~ WFSIQ38 + TotalGrayVol, d))
summary(lm(PVWMHvol_mm3_lg ~ WFSIQ38, d))
#ok, obviously I remembered that way wrong. I thought r^2 was .1 not .01
#boo

#ok. Last one, I promise. I shouldn't have peeked before scaling variables 
#and checking into covariates. Oh well.
summary(lm(wholeBrainWMHvol_mm3_lg ~ zrpgsEA3, d))
#ok, well that is something.
#lol.Probs a good stopping point.