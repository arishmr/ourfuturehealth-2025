## GOAL: subgroups in the outputs of statistical analyses that have <10 participants must be redacted, as well as complementary groups in cases of sex-stratified analyses (e.g. if men in a group are <10, then sample size of females in that group must also be redacted)

## This is to comply with the Safe Outputs Policy for Our Future Health


data <- read.csv("results/allprev.csv")
data$Ntotal[data$Ntotal < 10] <- "Redacted"
data$Ncases[data$Ncases < 10] <- "Redacted"
write.csv(data, "results/allprev.csv", row.names = F)


data <- read.csv("results/sexstr_allprev.csv")
data$Ntotal[data$Ntotal < 10] <- "Redacted"
data$Ncases[data$Ncases < 10] <- "Redacted"
data$Ncases[data$affdisorder=="bipolar" & data$condition=="graves"] <- "Redacted"
data$Ncases[data$affdisorder=="bipolar" & data$condition=="lupus"] <- "Redacted"
data$Ncases[data$affdisorder=="bipolar" & data$condition=="ms"] <- "Redacted"
write.csv(data, "results/sexstr_allprev.csv", row.names = F)


data <- read.csv("results/current_allprev.csv")
data$Ntotal[data$Ntotal < 10] <- "Redacted"
data$Ncases[data$Ncases < 10] <- "Redacted"
write.csv(data, "results/current_allprev.csv", row.names = F)


data <- read.csv("results/current_sexstr_allprev.csv")
data$Ntotal[data$Ntotal < 10] <- "Redacted"
data$Ncases[data$Ncases < 10] <- "Redacted"
write.csv(data, "results/current_sexstr_allprev.csv", row.names = F)




