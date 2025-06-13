## Goal: Calculate Odds Ratios for experience of mood disorders (any + separately) for people with vs without autoimmune conditions
## adjust for age, sex, ethnicity
## then adjust for household income, mother psych history, father psych history, chronic pain status, and socialisation

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
  "affdisorder", "depression", "bipolar", "anxiety", "currdep", "curranx",
  "group"
)




##############################
## ODDS OF ANY AFFECTIVE DISORDER
##############################

output <- data.frame(
  group = character(0),
  outcome = character(0),
  model = character(0),
  n = numeric(0),
  OR = numeric(0),
  ci.min = numeric(0),
  ci.max = numeric(0),
  p.value = numeric(0),
  stringsAsFactors = F
)

regdf <- alldata

regdf <- gather(data = regdf,
                key = outcome,
                value = value,
                c(affdisorder, depression, bipolar, anxiety, currdep, curranx),
                factor_key = T)

splitdata <- regdf %>%
  group_by(outcome) %>%
  group_split

output <- lapply(splitdata, FUN = function (x) {
  fit1 <- glm(value ~ group,
              family = binomial(link="logit"),
              data = x)
  summary(fit1)
  estimates <- as.numeric(c(
	exp(coef(summary(fit1))[2]),		# Odds Ratio
	exp(confint.default(fit1))[2,],		# Confidence Intervals for OR
	coef(summary(fit1))[2,4]))		# p value
  group <- rownames(coef(summary(fit1)))[2]
  outcome <- rep(as.character(unique(x$outcome)))
  model <- paste("model1")
  n <- summary(fit1)$df[2]
  output[nrow(output)+1,] <- c(group, outcome, model, n, estimates)
  
  fit2 <- glm(value ~ group + age + sex + ethnicity,
              family = binomial(link="logit"),
              data = x)
  summary(fit2)
  estimates <- as.numeric(c(exp(coef(summary(fit2))[2]), exp(confint.default(fit2))[2,], coef(summary(fit2))[2,4]))
  group <- rownames(coef(summary(fit2)))[2]
  outcome <- rep(as.character(unique(x$outcome)))
  model <- paste("model2")
  n <- summary(fit2)$df[2]
  output[nrow(output)+1,] <- c(group, outcome, model, n, estimates)
  
  fit3 <- glm(value ~ group + age + sex + ethnicity + income + motherpsych + fatherpsych + chronicpain + social,
              family = binomial(link="logit"),
              data = x)
  summary(fit3)
  estimates <- as.numeric(c(exp(coef(summary(fit3))[2]), exp(confint.default(fit3))[2,], coef(summary(fit3))[2,4]))
  group <- rownames(coef(summary(fit3)))[2]
  outcome <- rep(as.character(unique(x$outcome)))
  model <- paste("model3")
  n <- summary(fit3)$df[2]
  output[nrow(output)+1,] <- c(group, outcome, model, n, estimates)
  
  output
})
output <- dplyr::bind_rows(output)
output$group <- gsub("group", "", output$group)

write.csv(output, "results/logitreg.csv", row.names = F)
