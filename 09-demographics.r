library(dplyr)
library(tidyr)

rm(list = ls())

##### CREATE COMBINED DATASET

casedata <- read.csv("data/cleandata_infl.csv")
controldata <- read.csv("data/cleandata_controls.csv")

casedata <- casedata %>% mutate(group = rep("case"))
controldata <- controldata %>% mutate(group = rep("control"))

alldata <- rbind(casedata, controldata)
alldata$group <- as.factor(alldata$group)

## Clean up coding of relevant sociodemographic covariates
alldata$ethnicity[alldata$ethnicity=="Prefer not to answer"] <- NA
alldata$sex[alldata$sex=="Prefer not to answer"] <- NA
alldata$sex[alldata$sex=="Intersex"] <- NA
alldata$income[alldata$income=="Prefer not to answer"] <- NA
alldata$income[alldata$income=="Do not know"] <- NA


## Convert relevant variables to factors
factorcols <- c(
  "sex", "ethnicity", "gender",
  "diag_auto",
  "diag_psych", "diag_psych_anx", "diag_psych_depr",
  "income", "fatherdiag", "fatherdiag_psych", "motherdiag", "motherdiag_psych",
  "group"
)
alldata[factorcols] <- lapply(alldata[factorcols], as.factor)
alldata$group <- relevel(alldata$group, ref = "control")
alldata$income <- relevel(alldata$income, ref = "Less than £18,000")
alldata$sex <- relevel(alldata$sex, ref = "Male")
alldata$ethnicity <- relevel(alldata$ethnicity, ref = "White")
rm(factorcols)

## Convert relevant variables to numeric
numcols <- c(
  "age",
  "phq01", "phq02", "phq03", "phq04", "phq05", "phq06", "phq07", "phq08", "phq09",
  "gad01", "gad02", "gad03", "gad04", "gad05", "gad06", "gad07",
  "PHQ", "GAD",
  "social"
)
alldata[numcols] <- lapply(alldata[numcols], as.numeric)
rm(numcols)

## Convert relevant variables to logical
logcols <- c(
  "depression", "bipolar", 'anxiety', 'affdisorder',
  'arthritis', 'psoriasis', 'ibd', 'graves', 'ms', 'lupus',
  'currdep', 'curranx', 'motherpsych', 'fatherpsych', "chronicpain"
)
alldata[logcols] <- lapply(alldata[logcols], as.logical)
rm(logcols)


## Reduce dataframe to variables relevant for logistic regression
alldata <- alldata %>% dplyr::select(
  "pid", "age", "sex", "ethnicity",
  "income", "motherpsych", "fatherpsych", "chronicpain", "social",
  "affdisorder", "depression", "bipolar", "anxiety", "currdep", "curranx", "PHQ", "GAD",
  "group"
)








##### Calculate summary statistics for age, sex, ethnicity, income, motherpsych, fatherpsych OVERALL

demoage <- alldata %>% select(age) %>%
  summarise_each(list(n = ~n(), 
                      q25 = ~quantile(., 0.25, na.rm = T), 
                      median = ~median(., na.rm = T), 
                      q75 = ~quantile(., 0.75, na.rm = T), 
                      mean = ~mean(., na.rm = T), 
                      sd = ~sd(., na.rm = T)))

demoPHQ <- alldata %>% select(PHQ) %>%
  summarise_each(list(n = ~n(), 
                      q25 = ~quantile(., 0.25, na.rm = T), 
                      median = ~median(., na.rm = T), 
                      q75 = ~quantile(., 0.75, na.rm = T), 
                      mean = ~mean(., na.rm = T), 
                      sd = ~sd(., na.rm = T)))

demoGAD <- alldata %>% select(GAD) %>%
  summarise_each(list(n = ~n(), 
                      q25 = ~quantile(., 0.25, na.rm = T), 
                      median = ~median(., na.rm = T), 
                      q75 = ~quantile(., 0.75, na.rm = T), 
                      mean = ~mean(., na.rm = T), 
                      sd = ~sd(., na.rm = T)))

demosex <- alldata %>% count(sex) %>% mutate(freq = n / sum(n))
demoethnicity <- alldata %>% count(ethnicity) %>% mutate(freq = n / sum(n))
demoincome <- alldata %>% count(income) %>% mutate(freq = n / sum(n))
demofatherpsych <- alldata %>% count(fatherpsych) %>% mutate(freq = n / sum(n))
demomotherpsych <- alldata %>% count(motherpsych) %>% mutate(freq = n / sum(n))

demoage$group <- "overall"
demoPHQ$group <- "overall"
demoGAD$group <- "overall"
demosex$group <- "overall"
demoethnicity$group <- "overall"
demoincome$group <- "overall"
demofatherpsych$group <- "overall"
demomotherpsych$group <- "overall"

##### Calculate summary statistics for age, sex, ethnicity, income, motherpsych, fatherpsych BY EACH GROUP

age_grouped <- alldata %>% group_by(group) %>% select(age) %>%
  summarise_each(list(n = ~n(), 
                      q25 = ~quantile(., 0.25, na.rm = T), 
                      median = ~median(., na.rm = T), 
                      q75 = ~quantile(., 0.75, na.rm = T), 
                      mean = ~mean(., na.rm = T), 
                      sd = ~sd(., na.rm = T)))

PHQ_grouped <- alldata %>% group_by(group) %>% select(PHQ) %>%
  summarise_each(list(n = ~n(), 
                      q25 = ~quantile(., 0.25, na.rm = T), 
                      median = ~median(., na.rm = T), 
                      q75 = ~quantile(., 0.75, na.rm = T), 
                      mean = ~mean(., na.rm = T), 
                      sd = ~sd(., na.rm = T)))

GAD_grouped <- alldata %>% group_by(group) %>% select(GAD) %>%
  summarise_each(list(n = ~n(), 
                      q25 = ~quantile(., 0.25, na.rm = T), 
                      median = ~median(., na.rm = T), 
                      q75 = ~quantile(., 0.75, na.rm = T), 
                      mean = ~mean(., na.rm = T), 
                      sd = ~sd(., na.rm = T)))

sex_grouped <- alldata %>% group_by(group) %>% count(sex) %>% mutate(freq = n / sum(n))
ethnicity_grouped <- alldata %>% group_by(group) %>% count(ethnicity) %>% mutate(freq = n / sum(n))
income_grouped <- alldata %>% group_by(group) %>% count(income) %>% mutate(freq = n / sum(n))
fatherpsych_grouped <- alldata %>% group_by(group) %>% count(fatherpsych) %>% mutate(freq = n / sum(n))
motherpsych_grouped <- alldata %>% group_by(group) %>% count(motherpsych) %>% mutate(freq = n / sum(n))



demoage <- rbind(demoage, age_grouped)
demosex <- rbind(demosex, sex_grouped)
demoethnicity <- rbind(demoethnicity, ethnicity_grouped)
demoincome <- rbind(demoincome, income_grouped)
demomotherpsych <- rbind(demomotherpsych, motherpsych_grouped)
demofatherpsych <- rbind(demofatherpsych, fatherpsych_grouped)
demoPHQ <- rbind(demoPHQ, PHQ_grouped)
demoGAD <- rbind(demoGAD, GAD_grouped)

### Add number of people who have median or quartile ages

n_q25 <- alldata %>% dplyr::filter(age==as.numeric(demoage[1,2])) %>% nrow()
n_median <- alldata %>% dplyr::filter(age==as.numeric(demoage[1,3])) %>% nrow()
n_q75 <- alldata %>% dplyr::filter(age==as.numeric(demoage[1,4])) %>% nrow()
noverall <- c(n_q25 = n_q25, n_median = n_median, n_q75 = n_q75)

n_q25 <- alldata %>% dplyr::filter(group==as.character(demoage[2,7])) %>% dplyr::filter(age==as.numeric(demoage[1,2])) %>% nrow()
n_median <- alldata %>% dplyr::filter(group==as.character(demoage[2,7])) %>% dplyr::filter(age==as.numeric(demoage[1,3])) %>% nrow()
n_q75 <- alldata %>% dplyr::filter(group==as.character(demoage[2,7])) %>% dplyr::filter(age==as.numeric(demoage[1,4])) %>% nrow()
ncontrol <- c(n_q25, n_median, n_q75)

n_q25 <- alldata %>% dplyr::filter(group==as.character(demoage[3,7])) %>% dplyr::filter(age==as.numeric(demoage[1,2])) %>% nrow()
n_median <- alldata %>% dplyr::filter(group==as.character(demoage[3,7])) %>% dplyr::filter(age==as.numeric(demoage[1,3])) %>% nrow()
n_q75 <- alldata %>% dplyr::filter(group==as.character(demoage[3,7])) %>% dplyr::filter(age==as.numeric(demoage[1,4])) %>% nrow()
ncase <- c(n_q25, n_median, n_q75)

numbers <- rbind(noverall, ncontrol, ncase)
demoage <- cbind(demoage, numbers)

write.csv(demoage, "results/age.csv", row.names = F)
write.csv(demosex, "results/sex.csv", row.names = F)
write.csv(demoethnicity, "results/ethnicity.csv", row.names = F)
write.csv(demoincome, "results/income.csv", row.names = F)
write.csv(demofatherpsych, "results/fatherpsych.csv", row.names = F)
write.csv(demomotherpsych, "results/motherpsych.csv", row.names = F)
write.csv(demoPHQ, "results/PHQ9.csv", row.names = F)
write.csv(demoGAD, "results/GAD7.csv", row.names = F)



#######################################
## TESTS OF SIGNIFICANT DIFFERENCE BETWEEN GROUPS
#######################################
summary(aov(age ~ group, data = alldata))
summary(aov(PHQ ~ group, data = alldata))
summary(aov(GAD ~ group, data = alldata))

chisq.test(alldata$sex, alldata$group)
chisq.test(alldata$ethnicity, alldata$group)
chisq.test(alldata$income, alldata$group)
chisq.test(alldata$fatherpsych, alldata$group)
chisq.test(alldata$motherpsych, alldata$group)