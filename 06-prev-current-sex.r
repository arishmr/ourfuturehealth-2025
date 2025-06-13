## Goal: Calculate prevalence of various mood disorders in people with inflammatory conditions: overall, then by inflammatory condition, and finally stratified by sex and age brackets

#install.packages("epiR")
library(dplyr)
library(epiR)

##############################
## OVERALL PREVALENCE BY SEX
##############################

rm(list = ls())

casedata <- read.csv("data/cleandata_infl.csv")
controldata <- read.csv("data/cleandata_controls.csv")

data <- subset(casedata, casedata$sex=="Male")

npop <- as.numeric(length(na.omit(data$currdep)))
ncase <- as.numeric(length(na.omit(data$currdep[data$currdep == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
dcaseprev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
dcaseprev$group <- rep("case")
dcaseprev$condition <- rep("Any")
dcaseprev$affdisorder <- rep("currdepression")
dcaseprev$Ntotal <- npop
dcaseprev$Ncases <- ncase

npop <- as.numeric(length(na.omit(data$curranx)))
ncase <- as.numeric(length(na.omit(data$curranx[data$curranx == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
acaseprev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
acaseprev$group <- rep("case")
acaseprev$condition <- rep("Any")
acaseprev$affdisorder <- rep("curranxiety")
acaseprev$Ntotal <- npop
acaseprev$Ncases <- ncase

data <- subset(controldata, controldata$sex=="Male")

npop <- as.numeric(length(na.omit(data$currdep)))
ncase <- as.numeric(length(na.omit(data$currdep[data$currdep == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
dcontrolprev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
dcontrolprev$group <- rep("control")
dcontrolprev$condition <- rep("None")
dcontrolprev$affdisorder <- rep("currdepression")
dcontrolprev$Ntotal <- npop
dcontrolprev$Ncases <- ncase

npop <- as.numeric(length(na.omit(data$curranx)))
ncase <- as.numeric(length(na.omit(data$curranx[data$curranx == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
acontrolprev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
acontrolprev$group <- rep("control")
acontrolprev$condition <- rep("None")
acontrolprev$affdisorder <- rep("curranxiety")
acontrolprev$Ntotal <- npop
acontrolprev$Ncases <- ncase

mprev_overall <- rbind(dcaseprev, dcontrolprev, acaseprev, acontrolprev)
mprev_overall$sex <- rep("Male")

data <- subset(casedata, casedata$sex=="Female")

npop <- as.numeric(length(na.omit(data$currdep)))
ncase <- as.numeric(length(na.omit(data$currdep[data$currdep == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
dcaseprev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
dcaseprev$group <- rep("case")
dcaseprev$condition <- rep("Any")
dcaseprev$affdisorder <- rep("currdepression")
dcaseprev$Ntotal <- npop
dcaseprev$Ncases <- ncase

npop <- as.numeric(length(na.omit(data$curranx)))
ncase <- as.numeric(length(na.omit(data$curranx[data$curranx == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
acaseprev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
acaseprev$group <- rep("case")
acaseprev$condition <- rep("Any")
acaseprev$affdisorder <- rep("curranxiety")
acaseprev$Ntotal <- npop
acaseprev$Ncases <- ncase

data <- subset(controldata, controldata$sex=="Female")

npop <- as.numeric(length(na.omit(data$currdep)))
ncase <- as.numeric(length(na.omit(data$currdep[data$currdep == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
dcontrolprev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
dcontrolprev$group <- rep("control")
dcontrolprev$condition <- rep("None")
dcontrolprev$affdisorder <- rep("currdepression")
dcontrolprev$Ntotal <- npop
dcontrolprev$Ncases <- ncase

npop <- as.numeric(length(na.omit(data$curranx)))
ncase <- as.numeric(length(na.omit(data$curranx[data$curranx == TRUE])))
tmp <- as.matrix(cbind(ncase, npop))
acontrolprev <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
acontrolprev$group <- rep("control")
acontrolprev$condition <- rep("None")
acontrolprev$affdisorder <- rep("curranxiety")
acontrolprev$Ntotal <- npop
acontrolprev$Ncases <- ncase


fprev_overall <- rbind(dcaseprev, dcontrolprev, acaseprev, acontrolprev)
fprev_overall$sex <- rep("Female")

output <- rbind(mprev_overall, fprev_overall)

write.csv(output, "results/sexstr_currentprev_overall.csv", row.names = F)








##############################
## PREVALENCE OF SPECIFIC MOOD DISORDERS BY AUTOIMMUNE DISORDERS AND BY SEX
##############################

rm(list = ls())

casedata <- read.csv("data/cleandata_infl.csv")
controldata <- read.csv("data/cleandata_controls.csv")

mdata <- subset(casedata, casedata$sex=="Male")

data <- subset(mdata, mdata$arthritis==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$currdep == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$curranx == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev1 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev1$affdisorder <- c("currdepression", "curranxiety")
caseprev1$condition <- rep("arthritis")
caseprev1$group <- rep("case")
caseprev1$Ntotal <- npop
caseprev1$Ncases <- ncase

data <- subset(mdata, mdata$psoriasis==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$currdep == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$curranx == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev2 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev2$affdisorder <- c("currdepression", "curranxiety")
caseprev2$condition <- rep("psoriasis")
caseprev2$group <- rep("case")
caseprev2$Ntotal <- npop
caseprev2$Ncases <- ncase

data <- subset(mdata, mdata$ibd==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$currdep == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$curranx == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev3 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev3$affdisorder <- c("currdepression", "curranxiety")
caseprev3$condition <- rep("ibd")
caseprev3$group <- rep("case")
caseprev3$Ntotal <- npop
caseprev3$Ncases <- ncase

data <- subset(mdata, mdata$graves==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$currdep == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$curranx == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev4 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev4$affdisorder <- c("currdepression", "curranxiety")
caseprev4$condition <- rep("graves")
caseprev4$group <- rep("case")
caseprev4$Ntotal <- npop
caseprev4$Ncases <- ncase

data <- subset(mdata, mdata$ms==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$currdep == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$curranx == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev6 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev6$affdisorder <- c("currdepression", "curranxiety")
caseprev6$condition <- rep("ms")
caseprev6$group <- rep("case")
caseprev6$Ntotal <- npop
caseprev6$Ncases <- ncase

data <- subset(mdata, mdata$lupus==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$currdep == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$curranx == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev7 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev7$affdisorder <- c("currdepression", "curranxiety")
caseprev7$condition <- rep("lupus")
caseprev7$group <- rep("case")
caseprev7$Ntotal <- npop
caseprev7$Ncases <- ncase

mprev <- rbind(caseprev1, caseprev2, caseprev3, caseprev4, caseprev6, caseprev7)
mprev$sex <- rep("Male")



fdata <- subset(casedata, casedata$sex=="Female")

data <- subset(fdata, fdata$arthritis==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$currdep == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$curranx == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev1 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev1$affdisorder <- c("currdepression", "curranxiety")
caseprev1$condition <- rep("arthritis")
caseprev1$group <- rep("case")
caseprev1$Ntotal <- npop
caseprev1$Ncases <- ncase

data <- subset(fdata, fdata$psoriasis==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$currdep == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$curranx == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev2 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev2$affdisorder <- c("currdepression", "curranxiety")
caseprev2$condition <- rep("psoriasis")
caseprev2$group <- rep("case")
caseprev2$Ntotal <- npop
caseprev2$Ncases <- ncase

data <- subset(fdata, fdata$ibd==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$currdep == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$curranx == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev3 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev3$affdisorder <- c("currdepression", "curranxiety")
caseprev3$condition <- rep("ibd")
caseprev3$group <- rep("case")
caseprev3$Ntotal <- npop
caseprev3$Ncases <- ncase

data <- subset(fdata, fdata$graves==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$currdep == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$curranx == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev4 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev4$affdisorder <- c("currdepression", "curranxiety")
caseprev4$condition <- rep("graves")
caseprev4$group <- rep("case")
caseprev4$Ntotal <- npop
caseprev4$Ncases <- ncase

data <- subset(fdata, fdata$ms==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$currdep == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$curranx == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev6 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev6$affdisorder <- c("currdepression", "curranxiety")
caseprev6$condition <- rep("ms")
caseprev6$group <- rep("case")
caseprev6$Ntotal <- npop
caseprev6$Ncases <- ncase

data <- subset(fdata, fdata$lupus==TRUE)

npop <- c(as.numeric(length(na.omit(data$affdisorder))),
          as.numeric(length(na.omit(data$affdisorder))))
ncase <- c(as.numeric(length(na.omit(data$depression[data$currdep == TRUE]))),
           as.numeric(length(na.omit(data$bipolar[data$curranx == TRUE]))))
tmp <- as.matrix(cbind(ncase, npop))
caseprev7 <- epi.conf(tmp, ctype = "prevalence", method = "exact", conf.level = 0.95) * 100
caseprev7$affdisorder <- c("currdepression", "curranxiety")
caseprev7$condition <- rep("lupus")
caseprev7$group <- rep("case")
caseprev7$Ntotal <- npop
caseprev7$Ncases <- ncase

fprev <- rbind(caseprev1, caseprev2, caseprev3, caseprev4, caseprev6, caseprev7)
fprev$sex <- rep("Female")

output <- rbind(mprev, fprev)

write.csv(output, "results/sexstr_currentprev_by_affdisorders_autoimmune.csv", row.names = F)




##############################
## COMBINE ALL PREVALENCES INTO ONE OUTPUT
##############################
rm(list = ls())

prev_overall <- read.csv("results/sexstr_currentprev_overall.csv")
prev_by_affdisorders_autoimmune <- read.csv("results/sexstr_currentprev_by_affdisorders_autoimmune.csv")

output <- rbind(prev_overall, prev_by_affdisorders_autoimmune)
output <- output %>% arrange(affdisorder, desc(group), condition)
output <- output %>% relocate(affdisorder, .before = group)
output <- output %>% relocate(sex, .before = affdisorder)

write.csv(output, "results/current_sexstr_allprev.csv", row.names = F)