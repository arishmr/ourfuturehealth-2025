## Goal: Calculate prevalence of various Affective disorders in people with inflammatory conditions: overall, then by inflammatory condition, and finally stratified by sex and age brackets

library(dplyr)
library(epiR)

##############################
## OVERALL PREVALENCE BY SEX
##############################

rm(list = ls())

casedata <- read.csv("data/cleandata_infl.csv")
controldata <- read.csv("data/cleandata_controls.csv")

data <- subset(casedata, casedata$sex=="Male")

npop <- as.numeric(length(na.omit(data$affdisorder)))
ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
caseprev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev$group <- rep("case")
caseprev$condition <- rep("Any")
caseprev$affdisorder <- rep("Any Affective Disorder")
caseprev$Ntotal <- npop
caseprev$Ncases <- ncase

data <- subset(controldata, controldata$sex=="Male")

npop <- as.numeric(length(na.omit(data$affdisorder)))
ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
controlprev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
controlprev$group <- rep("control")
controlprev$condition <- rep("None")
controlprev$affdisorder <- rep("Any Affective Disorder")
controlprev$Ntotal <- npop
controlprev$Ncases <- ncase

mprev_overall <- rbind(caseprev, controlprev)
mprev_overall$sex <- rep("Male")

data <- subset(casedata, casedata$sex=="Female")

npop <- as.numeric(length(na.omit(data$affdisorder)))
ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
caseprev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev$group <- rep("case")
caseprev$condition <- rep("Any")
caseprev$affdisorder <- rep("Any Affective Disorder")
caseprev$Ntotal <- npop
caseprev$Ncases <- ncase

data <- subset(controldata, controldata$sex=="Female")

npop <- as.numeric(length(na.omit(data$affdisorder)))
ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
controlprev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
controlprev$group <- rep("control")
controlprev$condition <- rep("None")
controlprev$affdisorder <- rep("Any Affective Disorder")
controlprev$Ntotal <- npop
controlprev$Ncases <- ncase


fprev_overall <- rbind(caseprev, controlprev)
fprev_overall$sex <- rep("Female")

output <- rbind(mprev_overall, fprev_overall)

write.csv(output, "results/sexstr_prev_overall.csv", row.names = F)





##############################
## PREVALENCE OF SPECIFIC MOOD DISORDERS BY SEX
##############################

rm(list = ls())

casedata <- read.csv("data/cleandata_infl.csv")
controldata <- read.csv("data/cleandata_controls.csv")

data <- subset(casedata, casedata$sex=="Male")

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
mcaseprev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
mcaseprev$affdisorder <- c("depression", "bipolar", "anxiety")
mcaseprev$group <- rep("case")
mcaseprev$condition <- rep("Any")
mcaseprev$sex <- rep("Male")
mcaseprev$Ntotal <- npop
mcaseprev$Ncases <- ncase

data <- subset(controldata, controldata$sex=="Male")

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
mcontrolprev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
mcontrolprev$affdisorder <- c("depression", "bipolar", "anxiety")
mcontrolprev$group <- rep("control")
mcontrolprev$condition <- rep("None")
mcontrolprev$sex <- rep("Male")
mcontrolprev$Ntotal <- npop
mcontrolprev$Ncases <- ncase


data <- subset(casedata, casedata$sex=="Female")

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
fcaseprev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
fcaseprev$affdisorder <- c("depression", "bipolar", "anxiety")
fcaseprev$group <- rep("case")
fcaseprev$condition <- rep("Any")
fcaseprev$sex <- rep("Female")
fcaseprev$Ntotal <- npop
fcaseprev$Ncases <- ncase

data <- subset(controldata, controldata$sex=="Female")

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
fcontrolprev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
fcontrolprev$affdisorder <- c("depression", "bipolar", "anxiety")
fcontrolprev$group <- rep("control")
fcontrolprev$condition <- rep("None")
fcontrolprev$sex <- rep("Female")
fcontrolprev$Ntotal <- npop
fcontrolprev$Ncases <- ncase

output <- rbind(mcaseprev, mcontrolprev, fcaseprev, fcontrolprev)

write.csv(output, "results/sexstr_prev_by_affdisorders.csv", row.names = F)






##############################
## OVERALL PREVALENCE BY SEX AND BY EACH AUTOIMMUNE DISORDER: arthritis, psoriasis, ibd, graves, ms, lupus
##############################

rm(list = ls())

casedata <- read.csv("data/cleandata_infl.csv")
controldata <- read.csv("data/cleandata_controls.csv")

mdata <- subset(casedata, casedata$sex=="Male")

data <- subset(mdata, mdata$arthritis==TRUE)

npop <- as.numeric(length(na.omit(data$affdisorder)))
ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
caseprev1 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev1$affdisorder <- rep("Any Affective Disorder")
caseprev1$condition <- rep("arthritis")
caseprev1$group <- rep("case")
caseprev1$Ntotal <- npop
caseprev1$Ncases <- ncase

data <- subset(mdata, mdata$psoriasis==TRUE)

npop <- as.numeric(length(na.omit(data$affdisorder)))
ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
caseprev2 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev2$affdisorder <- rep("Any Affective Disorder")
caseprev2$condition <- rep("psoriasis")
caseprev2$group <- rep("case")
caseprev2$Ntotal <- npop
caseprev2$Ncases <- ncase

data <- subset(mdata, mdata$ibd==TRUE)

npop <- as.numeric(length(na.omit(data$affdisorder)))
ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
caseprev3 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev3$affdisorder <- rep("Any Affective Disorder")
caseprev3$condition <- rep("ibd")
caseprev3$group <- rep("case")
caseprev3$Ntotal <- npop
caseprev3$Ncases <- ncase

data <- subset(mdata, mdata$graves==TRUE)

npop <- as.numeric(length(na.omit(data$affdisorder)))
ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
caseprev4 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev4$affdisorder <- rep("Any Affective Disorder")
caseprev4$condition <- rep("graves")
caseprev4$group <- rep("case")
caseprev4$Ntotal <- npop
caseprev4$Ncases <- ncase

data <- subset(mdata, mdata$ms==TRUE)

npop <- as.numeric(length(na.omit(data$affdisorder)))
ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
caseprev6 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev6$affdisorder <- rep("Any Affective Disorder")
caseprev6$condition <- rep("ms")
caseprev6$group <- rep("case")
caseprev6$Ntotal <- npop
caseprev6$Ncases <- ncase

data <- subset(mdata, mdata$lupus==TRUE)

npop <- as.numeric(length(na.omit(data$affdisorder)))
ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
caseprev7 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev7$affdisorder <- rep("Any Affective Disorder")
caseprev7$condition <- rep("lupus")
caseprev7$group <- rep("case")
caseprev7$Ntotal <- npop
caseprev7$Ncases <- ncase

mcaseprev <- rbind(caseprev1, caseprev2, caseprev3, caseprev4, caseprev6, caseprev7)
mcaseprev$sex <- rep("Male")

fdata <- subset(casedata, casedata$sex=="Female")

data <- subset(fdata, fdata$arthritis==TRUE)

npop <- as.numeric(length(na.omit(data$affdisorder)))
ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
caseprev1 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev1$affdisorder <- rep("Any Affective Disorder")
caseprev1$condition <- rep("arthritis")
caseprev1$group <- rep("case")
caseprev1$Ntotal <- npop
caseprev1$Ncases <- ncase

data <- subset(fdata, fdata$psoriasis==TRUE)

npop <- as.numeric(length(na.omit(data$affdisorder)))
ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
caseprev2 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev2$affdisorder <- rep("Any Affective Disorder")
caseprev2$condition <- rep("psoriasis")
caseprev2$group <- rep("case")
caseprev2$Ntotal <- npop
caseprev2$Ncases <- ncase

data <- subset(fdata, fdata$ibd==TRUE)

npop <- as.numeric(length(na.omit(data$affdisorder)))
ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
caseprev3 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev3$affdisorder <- rep("Any Affective Disorder")
caseprev3$condition <- rep("ibd")
caseprev3$group <- rep("case")
caseprev3$Ntotal <- npop
caseprev3$Ncases <- ncase

data <- subset(fdata, fdata$graves==TRUE)

npop <- as.numeric(length(na.omit(data$affdisorder)))
ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
caseprev4 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev4$affdisorder <- rep("Any Affective Disorder")
caseprev4$condition <- rep("graves")
caseprev4$group <- rep("case")
caseprev4$Ntotal <- npop
caseprev4$Ncases <- ncase

data <- subset(fdata, fdata$ms==TRUE)

npop <- as.numeric(length(na.omit(data$affdisorder)))
ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
caseprev6 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev6$affdisorder <- rep("Any Affective Disorder")
caseprev6$condition <- rep("ms")
caseprev6$group <- rep("case")
caseprev6$Ntotal <- npop
caseprev6$Ncases <- ncase

data <- subset(fdata, fdata$lupus==TRUE)

npop <- as.numeric(length(na.omit(data$affdisorder)))
ncase <- as.numeric(length(na.omit(data$affdisorder[data$affdisorder == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
caseprev7 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev7$affdisorder <- rep("Any Affective Disorder")
caseprev7$condition <- rep("lupus")
caseprev7$group <- rep("case")
caseprev7$Ntotal <- npop
caseprev7$Ncases <- ncase

fcaseprev <- rbind(caseprev1, caseprev2, caseprev3, caseprev4, caseprev6, caseprev7)
fcaseprev$sex <- rep("Female")

output <- rbind(mcaseprev, fcaseprev)

write.csv(output, "results/sexstr_prev_by_autoimmune.csv", row.names = F)






##############################
## PREVALENCE OF SPECIFIC MOOD DISORDERS BY AUTOIMMUNE DISORDERS AND BY SEX
##############################

rm(list = ls())

casedata <- read.csv("data/cleandata_infl.csv")
controldata <- read.csv("data/cleandata_controls.csv")

mdata <- subset(casedata, casedata$sex=="Male")

data <- subset(mdata, mdata$arthritis==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev1 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev1$affdisorder <- c("depression", "bipolar", "anxiety")
caseprev1$condition <- rep("arthritis")
caseprev1$group <- rep("case")
caseprev1$Ntotal <- npop
caseprev1$Ncases <- ncase

data <- subset(mdata, mdata$psoriasis==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev2 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev2$affdisorder <- c("depression", "bipolar", "anxiety")
caseprev2$condition <- rep("psoriasis")
caseprev2$group <- rep("case")
caseprev2$Ntotal <- npop
caseprev2$Ncases <- ncase

data <- subset(mdata, mdata$ibd==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev3 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev3$affdisorder <- c("depression", "bipolar", "anxiety")
caseprev3$condition <- rep("ibd")
caseprev3$group <- rep("case")
caseprev3$Ntotal <- npop
caseprev3$Ncases <- ncase

data <- subset(mdata, mdata$graves==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev4 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev4$affdisorder <- c("depression", "bipolar", "anxiety")
caseprev4$condition <- rep("graves")
caseprev4$group <- rep("case")
caseprev4$Ntotal <- npop
caseprev4$Ncases <- ncase

data <- subset(mdata, mdata$ms==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev6 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev6$affdisorder <- c("depression", "bipolar", "anxiety")
caseprev6$condition <- rep("ms")
caseprev6$group <- rep("case")
caseprev6$Ntotal <- npop
caseprev6$Ncases <- ncase

data <- subset(mdata, mdata$lupus==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev7 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev7$affdisorder <- c("depression", "bipolar", "anxiety")
caseprev7$condition <- rep("lupus")
caseprev7$group <- rep("case")
caseprev7$Ntotal <- npop
caseprev7$Ncases <- ncase

mprev <- rbind(caseprev1, caseprev2, caseprev3, caseprev4, caseprev6, caseprev7)
mprev$sex <- rep("Male")



fdata <- subset(casedata, casedata$sex=="Female")

data <- subset(fdata, fdata$arthritis==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev1 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev1$affdisorder <- c("depression", "bipolar", "anxiety")
caseprev1$condition <- rep("arthritis")
caseprev1$group <- rep("case")
caseprev1$Ntotal <- npop
caseprev1$Ncases <- ncase

data <- subset(fdata, fdata$psoriasis==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev2 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev2$affdisorder <- c("depression", "bipolar", "anxiety")
caseprev2$condition <- rep("psoriasis")
caseprev2$group <- rep("case")
caseprev2$Ntotal <- npop
caseprev2$Ncases <- ncase

data <- subset(fdata, fdata$ibd==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev3 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev3$affdisorder <- c("depression", "bipolar", "anxiety")
caseprev3$condition <- rep("ibd")
caseprev3$group <- rep("case")
caseprev3$Ntotal <- npop
caseprev3$Ncases <- ncase

data <- subset(fdata, fdata$graves==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev4 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev4$affdisorder <- c("depression", "bipolar", "anxiety")
caseprev4$condition <- rep("graves")
caseprev4$group <- rep("case")
caseprev4$Ntotal <- npop
caseprev4$Ncases <- ncase

data <- subset(fdata, fdata$ms==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev6 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev6$affdisorder <- c("depression", "bipolar", "anxiety")
caseprev6$condition <- rep("ms")
caseprev6$group <- rep("case")
caseprev6$Ntotal <- npop
caseprev6$Ncases <- ncase

data <- subset(fdata, fdata$lupus==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$depression == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$bipolar == TRUE]))),
           as.numeric(length(na.omit(data$anxiety[data$anxiety == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev7 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev7$affdisorder <- c("depression", "bipolar", "anxiety")
caseprev7$condition <- rep("lupus")
caseprev7$group <- rep("case")
caseprev7$Ntotal <- npop
caseprev7$Ncases <- ncase

fprev <- rbind(caseprev1, caseprev2, caseprev3, caseprev4, caseprev6, caseprev7)
fprev$sex <- rep("Female")

output <- rbind(mprev, fprev)

write.csv(output, "results/sexstr_prev_by_affdisorders_autoimmune.csv", row.names = F)






##############################
## COMBINE ALL PREVALENCES INTO ONE OUTPUT
##############################
rm(list = ls())

prev_overall <- read.csv("results/sexstr_prev_overall.csv")
prev_by_affdisorders <- read.csv("results/sexstr_prev_by_affdisorders.csv")
prev_by_autoimmune <- read.csv("results/sexstr_prev_by_autoimmune.csv")
prev_by_affdisorders_autoimmune <- read.csv("results/sexstr_prev_by_affdisorders_autoimmune.csv")

output <- rbind(
  prev_overall, prev_by_autoimmune, prev_by_affdisorders, prev_by_affdisorders_autoimmune)
output <- output %>% arrange(affdisorder, desc(group), condition)
output <- output %>% relocate(affdisorder, .before = group)
output <- output %>% relocate(sex, .before = affdisorder)

write.csv(output, "results/sexstr_allprev.csv", row.names = F)


