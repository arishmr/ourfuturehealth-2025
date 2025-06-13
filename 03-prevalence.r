## Goal: Calculate prevalence of various Affective disorders in people with inflammatory conditions: overall, then by inflammatory condition, and finally stratified by sex and age brackets

casedata <- read.csv("data/cleandata_infl.csv")
controldata <- read.csv("data/cleandata_controls.csv")

library(dplyr)
library(epiR)


##############################
## OVERALL PREVALENCE
##############################

rm(list = ls())

casedata <- read.csv("data/cleandata_infl.csv")
controldata <- read.csv("data/cleandata_controls.csv")

## Calculate prevalence among "cases" i.e. people with autoimmune conditions
npop <- as.numeric(length(na.omit(casedata$affdisorder))) # total number of observations in the group
ncase <- as.numeric(length(na.omit(casedata$affdisorder[casedata$affdisorder == TRUE]))) # total number of people with the relevant outcome condition (in this case, any mood or anxiety disorder) observed in the group
tmp <- as.matrix(cbind(ncase, npop))
caseprev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100 # epi.conf calculates prevalence with 95% CIs
# add relevant labels to the output to correctly identify the prevalence estimates
caseprev$group <- rep("case")
caseprev$condition <- rep("Any")
caseprev$affdisorder <- rep("Any Affective Disorder")
# add number of observations to the output (just for reference)
caseprev$Ntotal <- npop
caseprev$Ncases <- ncase

## Calculate prevalence among "controls"
npop <- as.numeric(length(na.omit(controldata$affdisorder)))
ncase <- as.numeric(length(na.omit(controldata$affdisorder[controldata$affdisorder == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
controlprev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
controlprev$group <- rep("control")
controlprev$condition <- rep("None")
controlprev$affdisorder <- rep("Any Affective Disorder")
controlprev$Ntotal <- npop
controlprev$Ncases <- ncase

# put the prevalence estimates for both groups together - for this to work, all dataframes should have the same columns
output <- rbind(caseprev, controlprev)

write.csv(output, "results/prev_overall.csv", row.names = F)





##############################
## PREVALENCE OF SPECIFIC MOOD DISORDERS
##############################

rm(list = ls())

casedata <- read.csv("data/cleandata_infl.csv")
controldata <- read.csv("data/cleandata_controls.csv")


npop <- c(as.numeric(length(na.omit(casedata$affdisorder))),
          as.numeric(length(na.omit(casedata$affdisorder))),
          as.numeric(length(na.omit(casedata$affdisorder))))
ncase <- c(as.numeric(length(na.omit(casedata$depression[casedata$depression == TRUE]))),
           as.numeric(length(na.omit(casedata$bipolar[casedata$bipolar == TRUE]))),
           as.numeric(length(na.omit(casedata$anxiety[casedata$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev$group <- rep("case")
caseprev$condition <- rep("Any")
caseprev$affdisorder <- c("depression", "bipolar", "anxiety")
caseprev$Ntotal <- npop
caseprev$Ncases <- ncase

npop <- c(as.numeric(length(na.omit(controldata$affdisorder))),
          as.numeric(length(na.omit(controldata$affdisorder))),
          as.numeric(length(na.omit(controldata$affdisorder))))
ncase <- c(as.numeric(length(na.omit(controldata$depression[controldata$depression == TRUE]))),
           as.numeric(length(na.omit(controldata$bipolar[controldata$bipolar == TRUE]))),
           as.numeric(length(na.omit(controldata$anxiety[controldata$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
controlprev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
controlprev$group <- rep("control")
controlprev$condition <- rep("None")
controlprev$affdisorder <- c("depression", "bipolar", "anxiety")
controlprev$Ntotal <- npop
controlprev$Ncases <- ncase

prev_by_affdisorders <- rbind(caseprev, controlprev)

write.csv(prev_by_affdisorders, "results/prev_by_affdisorders.csv", row.names = F)






##############################
## OVERALL PREVALENCE BY EACH AUTOIMMUNE DISORDER: arthritis, psoriasis, ibd, graves, ms, lupus
##############################

rm(list = ls())

casedata <- read.csv("data/cleandata_infl.csv")
controldata <- read.csv("data/cleandata_controls.csv")

data <- subset(casedata, casedata$arthritis==TRUE)

  npop <- as.numeric(length(na.omit(data$affdisorder)))
  ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
  tmp <- as.matrix(cbind(ncase, npop))
  caseprev1 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
  caseprev1$group <- rep("case")
  caseprev1$condition <- rep("arthritis")
  caseprev1$affdisorder <- rep("Any Affective Disorder")
  caseprev1$Ntotal <- npop
  caseprev1$Ncases <- ncase
  
data <- subset(casedata, casedata$psoriasis==TRUE)
  
  npop <- as.numeric(length(na.omit(data$affdisorder)))
  ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
  tmp <- as.matrix(cbind(ncase, npop))
  caseprev2 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
  caseprev2$group <- rep("case")
  caseprev2$condition <- rep("psoriasis")
  caseprev2$affdisorder <- rep("Any Affective Disorder")
  caseprev2$Ntotal <- npop
  caseprev2$Ncases <- ncase

data <- subset(casedata, casedata$ibd==TRUE)
  
  npop <- as.numeric(length(na.omit(data$affdisorder)))
  ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
  tmp <- as.matrix(cbind(ncase, npop))
  caseprev3 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
  caseprev3$group <- rep("case")
  caseprev3$condition <- rep("ibd")
  caseprev3$affdisorder <- rep("Any Affective Disorder")
  caseprev3$Ntotal <- npop
  caseprev3$Ncases <- ncase
  
data <- subset(casedata, casedata$graves==TRUE)
  
  npop <- as.numeric(length(na.omit(data$affdisorder)))
  ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
  tmp <- as.matrix(cbind(ncase, npop))
  caseprev4 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
  caseprev4$group <- rep("case")
  caseprev4$condition <- rep("graves")
  caseprev4$affdisorder <- rep("Any Affective Disorder")
  caseprev4$Ntotal <- npop
  caseprev4$Ncases <- ncase
  
data <- subset(casedata, casedata$ms==TRUE)
  
  npop <- as.numeric(length(na.omit(data$affdisorder)))
  ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
  tmp <- as.matrix(cbind(ncase, npop))
  caseprev6 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
  caseprev6$group <- rep("case")
  caseprev6$condition <- rep("ms")
  caseprev6$affdisorder <- rep("Any Affective Disorder")
  caseprev6$Ntotal <- npop
  caseprev6$Ncases <- ncase
  
data <- subset(casedata, casedata$lupus==TRUE)
  
  npop <- as.numeric(length(na.omit(data$affdisorder)))
  ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
  tmp <- as.matrix(cbind(ncase, npop))
  caseprev7 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
  caseprev7$group <- rep("case")
  caseprev7$condition <- rep("lupus")
  caseprev7$affdisorder <- rep("Any Affective Disorder")
  caseprev7$Ntotal <- npop
  caseprev7$Ncases <- ncase
  
output <- rbind(caseprev1, caseprev2, caseprev3, caseprev4, caseprev6, caseprev7)

write.csv(output, "results/prev_by_autoimmune.csv", row.names = F)





##############################
## PREVALENCE OF SPECIFIC MOOD DISORDERS BY AUTOIMMUNE DISORDERS
##############################

rm(list = ls())

casedata <- read.csv("data/cleandata_infl.csv")
controldata <- read.csv("data/cleandata_controls.csv")


data <- subset(casedata, casedata$arthritis==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev1 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev1$group <- rep("case")
caseprev1$condition <- rep("arthritis")
caseprev1$affdisorder <- c("depression", "bipolar", "anxiety")
caseprev1$Ntotal <- npop
caseprev1$Ncases <- ncase

data <- subset(casedata, casedata$psoriasis==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev2 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev2$group <- rep("case")
caseprev2$condition <- rep("psoriasis")
caseprev2$affdisorder <- c("depression", "bipolar", "anxiety")
caseprev2$Ntotal <- npop
caseprev2$Ncases <- ncase

data <- subset(casedata, casedata$ibd==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev3 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev3$group <- rep("case")
caseprev3$condition <- rep("ibd")
caseprev3$affdisorder <- c("depression", "bipolar", "anxiety")
caseprev3$Ntotal <- npop
caseprev3$Ncases <- ncase

data <- subset(casedata, casedata$graves==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev4 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev4$group <- rep("case")
caseprev4$condition <- rep("graves")
caseprev4$affdisorder <- c("depression", "bipolar", "anxiety")
caseprev4$Ntotal <- npop
caseprev4$Ncases <- ncase

data <- subset(casedata, casedata$ms==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev6 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev6$group <- rep("case")
caseprev6$condition <- rep("ms")
caseprev6$affdisorder <- c("depression", "bipolar", "anxiety")
caseprev6$Ntotal <- npop
caseprev6$Ncases <- ncase

data <- subset(casedata, casedata$lupus==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev7 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev7$group <- rep("case")
caseprev7$condition <- rep("lupus")
caseprev7$affdisorder <- c("depression", "bipolar", "anxiety")
caseprev7$Ntotal <- npop
caseprev7$Ncases <- ncase

output <- rbind(caseprev1, caseprev2, caseprev3, caseprev4, caseprev6, caseprev7)

write.csv(output, "results/prev_by_affdisorders_autoimmune.csv", row.names = F)



##############################
## COMBINE ALL PREVALENCES INTO ONE OUTPUT
##############################
rm(list = ls())

prev_overall <- read.csv("results/prev_overall.csv")
prev_by_affdisorders <- read.csv("results/prev_by_affdisorders.csv")
prev_by_autoimmune <- read.csv("results/prev_by_autoimmune.csv")
prev_by_affdisorders_autoimmune <- read.csv("results/prev_by_affdisorders_autoimmune.csv")

output <- rbind(
  prev_overall, prev_by_autoimmune, prev_by_affdisorders, prev_by_affdisorders_autoimmune)
output <- output %>% arrange(affdisorder, desc(group), condition)
output <- output %>% relocate(affdisorder, .before = group)

write.csv(output, "results/allprev.csv", row.names = F)


