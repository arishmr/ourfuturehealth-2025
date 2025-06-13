rm(list = ls())
library(dplyr)

###############################################
## CASES
###############################################

## Some demographic data for participants is available in a separate file ("data_participant"), so first I must merge these two dataframes

participants <- read.csv("data/inflam/data_participant.csv")
questionnaire <- read.csv("data/inflam/data_questionnaire.csv")

alldata <- merge(participants, questionnaire, by = "pid")

colnames(alldata)

## Rename columns to more appropriate variable names
alldata <- alldata %>% rename(
  "sex2" = "demog_sex_2_1",
  "sex1" = "demog_sex_1_1",
  "ethn_orig" = "demog_ethnicity_1_1",
  "gender" = "demog_gender_2_1",
  "consent_year" = "consent_year",
  "birth_year" = "birth_year",
  "diag_auto" = "diag_auto_1_m",
  "diag_psych"= "diag_psych_1_m",
  "diag_psych_anx" = "diag_psych_anx_1_m",
  "diag_psych_depr" = "diag_psych_depr_1_m",
  "phq01" = "phq9_item1_interest_1_1",
  "phq02" = "phq9_item2_down_1_1",
  "phq03" = "phq9_item3_sleep_1_1",
  "phq04" = "phq9_item4_energy_1_1",
  "phq05" = "phq9_item5_appetite_1_1",
  "phq06" = "phq9_item6_bad_1_1",
  "phq07" = "phq9_item7_concentr_1_1",
  "phq08" = "phq9_item8_movement_1_1",
  "phq09" = "phq9_item9_harm_1_1",
  "gad01" = "gad7_item1_anx_1_1",
  "gad02" = "gad7_item2_worry_control_1_1",
  "gad03" = "gad7_item3_worry_amount_1_1",
  "gad04" = "gad7_item4_relax_1_1",
  "gad05" = "gad7_item5_restless_1_1",
  "gad06" = "gad7_item6_annoyed_1_1",
  "gad07" = "gad7_item7_afraid_1_1",
  "fatherdiag" = "father_diag_a_2_m",
  "fatherdiag_psych" = "father_diag_psych_1_m",
  "motherdiag" = "mother_diag_a_2_m",
  "motherdiag_psych" = "mother_diag_psych_1_m",
  "income" = "housing_income_1_1",
  "pain_chronic" = "health_pain_chronic_1_m",
  "social" = "lifestyle_social_visits_1_1"
)


## Recode ethnicity to:
# account for multiple selections (i.e. when the coded response contains "|") = "Multiple Selected"
# use collapsed categories (e.g. "White" or "Black")
alldata$ethnicity <- alldata$ethn_orig
alldata$ethnicity[grep("\\|", alldata$ethnicity)] <- "Multiple Options Selected"
alldata$ethnicity[alldata$ethnicity == 'Any other Asian/Asian British background'] <- "Other"
alldata$ethnicity[alldata$ethnicity == 'Any other Black / African / Caribbean background'] <- "Other"
alldata$ethnicity[alldata$ethnicity == 'Any other mixed multiple ethnic background'] <- "Other"
alldata$ethnicity[alldata$ethnicity == 'Asian or Asian British – Bangladeshi'] <- "South Asian"
alldata$ethnicity[alldata$ethnicity == 'Asian or Asian British – Indian'] <- "South Asian"
alldata$ethnicity[alldata$ethnicity == 'Asian or Asian British – Pakistani'] <- "South Asian"
alldata$ethnicity[alldata$ethnicity == 'Mixed – White and Asian'] <- "Mixed or Multiple Heritage"
alldata$ethnicity[alldata$ethnicity == 'Mixed – White and Black African'] <- "Mixed or Multiple Heritage"
alldata$ethnicity[alldata$ethnicity == 'Mixed – White and Black Caribbean'] <- "Mixed or Multiple Heritage"
alldata$ethnicity[alldata$ethnicity == 'Any other white background'] <- "White"
alldata$ethnicity[alldata$ethnicity == 'White – English / Welsh / Scottish / Northern Irish / British'] <- "White"
alldata$ethnicity[alldata$ethnicity == 'White – Gypsy or Irish Traveller'] <- "White"
alldata$ethnicity[alldata$ethnicity == 'White – Irish'] <- "White"
alldata$ethnicity[alldata$ethnicity == 'White – Polish'] <- "White"
alldata$ethnicity[alldata$ethnicity == 'Black or Black British – African'] <- "Black"
alldata$ethnicity[alldata$ethnicity == 'Black or Black British – Caribbean'] <- "Black"
alldata$ethnicity[alldata$ethnicity == 'Prefer not to answer'] <- NA

## Combine sex data from two variables (pilot vs full cohort) into a single sex column
alldata$sex2[alldata$sex2 == ""] <- NA
alldata$sex1[alldata$sex1 == ""] <- NA
alldata$sex <- coalesce(alldata$sex2, alldata$sex1)
alldata <- alldata %>% relocate(sex, .after = pid)

## Convert relevant variables to factors
factorcols <- c(
  "sex", "ethnicity", "gender",
  "diag_auto",
  "diag_psych", "diag_psych_anx", "diag_psych_depr",
  "income", "fatherdiag", "fatherdiag_psych", "motherdiag", "motherdiag_psych"
  )
alldata[factorcols] <- lapply(alldata[factorcols], as.factor)
rm(factorcols)

## Recode PHQ-9 and GAD-7 and loneliness variables from characters to relevant numeric code and then convert to numeric variables
alldata[alldata == "Not at all"] <- 0
alldata[alldata == "Several days"] <- 1
alldata[alldata == "More than half the days"] <- 2
alldata[alldata == "Nearly every day"] <- 3

alldata$social[alldata$social == "Do not know"] <- NA
alldata$social[alldata$social == "Prefer not to answer"] <- NA
alldata$social[alldata$social == "No friends/family outside household"] <- 0
alldata$social[alldata$social == "Never or almost never"] <- 0
alldata$social[alldata$social == "Once every few month"] <- 1
alldata$social[alldata$social == "About once a month"] <- 2
alldata$social[alldata$social == "About once a week"] <- 3
alldata$social[alldata$social == "2-4 times a week"] <- 4
alldata$social[alldata$social == "Almost daily"] <- 5

numcols <- c(
  "phq01", "phq02", "phq03", "phq04", "phq05", "phq06", "phq07", "phq08", "phq09",
  "gad01", "gad02", "gad03", "gad04", "gad05", "gad06", "gad07",
  "social"
)
alldata[numcols] <- lapply(alldata[numcols], as.numeric)
rm(numcols)

## Convert consent_year and birth_year variables to R-friendly year format
alldata$consent_year <- format(as.Date(strptime(alldata$consent_year, format = "%Y")), "%Y")
alldata$consent_year <- as.numeric(alldata$consent_year)
alldata$birth_year <- format(as.Date(strptime(alldata$birth_year, format = "%Y")), "%Y")
alldata$birth_year <- as.numeric(alldata$birth_year)

## Calculate age at point of consent from consent year and birth year
alldata$age <- alldata$consent_year - alldata$birth_year
alldata <- alldata %>% relocate(age, .after = pid) # bringing age to front of dataframe
#alldata <- alldata[,c(1,ncol(alldata),2:(ncol(alldata)-1))] 


## Create new columns for mood disorders
# TRUE if the variable diag_psych contains the strings "Anxiety", "Bipolar disorder", or "Depression"
alldata$depression <- grepl("Depression", alldata$diag_psych, ignore.case = T)
alldata$bipolar <- grepl("Bipolar disorder", alldata$diag_psych, ignore.case = T)
alldata$anxiety <- grepl("Anxiety", alldata$diag_psych, ignore.case = T)
alldata$affdisorder <- ifelse(alldata$depression == F & 
                                 alldata$bipolar == F & 
                                 alldata$anxiety == F,
                               FALSE, TRUE)

## Create new columns for autoimmune disorders
# TRUE if diag_auto contains the strings "Rheumatoid arthritis", "Lupus", "Inflammatory Bowel Disease (IBD)", "Multiple Sclerosis (MS)", "Graves' disease", or "Psoriasis"
alldata$arthritis <- grepl("Rheumatoid arthritis", alldata$diag_auto, ignore.case = T)
alldata$psoriasis <- grepl("Psoriasis", alldata$diag_auto, ignore.case = T)
alldata$ibd <- grepl("Inflammatory Bowel Disease", alldata$diag_auto, ignore.case = T)
alldata$graves <- grepl("Graves", alldata$diag_auto, ignore.case = T)
alldata$ms <- grepl("Multiple Sclerosis", alldata$diag_auto, ignore.case = T)
alldata$lupus <- grepl("Lupus", alldata$diag_auto, ignore.case = T)


## Create new columns for current depression/anxiety
# PHQ (sum of phq scores), GAD (sum of gad7 scores), currdep (PHQ > 9), curranx (GAD > 7)
alldata$PHQ <- rowSums(alldata[,c("phq01", "phq02", "phq03", "phq04", "phq05", "phq06", "phq07", "phq08", "phq09")])
alldata$GAD <- rowSums(alldata[,c("gad01", "gad02", "gad03", "gad04", "gad05", "gad06", "gad07")])

alldata$currdep <- ifelse(alldata$PHQ > 9, TRUE, FALSE)
alldata$curranx <- ifelse(alldata$GAD > 7, TRUE, FALSE)

## Create new columns for family psychiatric history
## TRUE if mother/father diag_psych contains "Depression", "Bipolar", "Anxiety"
alldata$motherpsych <- ifelse(
    grepl("Depression", alldata$motherdiag_psych, ignore.case = T) |
    grepl("Bipolar disorder", alldata$motherdiag_psych, ignore.case = T) | 
    grepl("Anxiety", alldata$motherdiag_psych, ignore.case = T),
  TRUE, FALSE
)

alldata$fatherpsych <- ifelse(
  grepl("Depression", alldata$fatherdiag_psych, ignore.case = T) |
    grepl("Bipolar disorder", alldata$fatherdiag_psych, ignore.case = T) | 
    grepl("Anxiety", alldata$fatherdiag_psych, ignore.case = T),
  TRUE, FALSE
)

## Create new column for chronic pain
alldata$pain_chronic[alldata$pain_chronic == "Prefer not to answer"] <- NA
alldata$chronicpain <- ifelse(
  grepl("None of the above", alldata$pain_chronic, ignore.case = T),
  FALSE, TRUE
)

## ensure blank cells are recoded to NA
alldata[alldata == ""] <- NA


write.csv(alldata, "data/cleandata_infl.csv", row.names = F)




## repeat cleaning procedures for dataset of people without autoimmune conditions ("controls")
###############################################
## CONTROLS
###############################################
rm(list = ls())

participants <- read.csv("data/controls/data_participant.csv")
questionnaire <- read.csv("data/controls/data_questionnaire.csv")

alldata <- merge(participants, questionnaire, by = "pid")

colnames(alldata)

## Rename columns
alldata <- alldata %>% rename(
  "sex2" = "demog_sex_2_1",
  "sex1" = "demog_sex_1_1",
  "ethn_orig" = "demog_ethnicity_1_1",
  "gender" = "demog_gender_2_1",
  "consent_year" = "consent_year",
  "birth_year" = "birth_year",
  "diag_auto" = "diag_auto_1_m",
  "diag_psych"= "diag_psych_1_m",
  "diag_psych_anx" = "diag_psych_anx_1_m",
  "diag_psych_depr" = "diag_psych_depr_1_m",
  "phq01" = "phq9_item1_interest_1_1",
  "phq02" = "phq9_item2_down_1_1",
  "phq03" = "phq9_item3_sleep_1_1",
  "phq04" = "phq9_item4_energy_1_1",
  "phq05" = "phq9_item5_appetite_1_1",
  "phq06" = "phq9_item6_bad_1_1",
  "phq07" = "phq9_item7_concentr_1_1",
  "phq08" = "phq9_item8_movement_1_1",
  "phq09" = "phq9_item9_harm_1_1",
  "gad01" = "gad7_item1_anx_1_1",
  "gad02" = "gad7_item2_worry_control_1_1",
  "gad03" = "gad7_item3_worry_amount_1_1",
  "gad04" = "gad7_item4_relax_1_1",
  "gad05" = "gad7_item5_restless_1_1",
  "gad06" = "gad7_item6_annoyed_1_1",
  "gad07" = "gad7_item7_afraid_1_1",
  "fatherdiag" = "father_diag_a_2_m",
  "fatherdiag_psych" = "father_diag_psych_1_m",
  "motherdiag" = "mother_diag_a_2_m",
  "motherdiag_psych" = "mother_diag_psych_1_m",
  "income" = "housing_income_1_1",
  "pain_chronic" = "health_pain_chronic_1_m",
  "social" = "lifestyle_social_visits_1_1"
)

## Recode ethnicity to:
# account for multiple selections (i.e. when the coded response contains "|") = "Multiple Selected"
# use collapsed categories (i.e. "White")
alldata$ethnicity <- alldata$ethn_orig
alldata$ethnicity[grep("\\|", alldata$ethnicity)] <- "Multiple Options Selected"
alldata$ethnicity[alldata$ethnicity == 'Any other Asian/Asian British background'] <- "Other"
alldata$ethnicity[alldata$ethnicity == 'Any other Black / African / Caribbean background'] <- "Other"
alldata$ethnicity[alldata$ethnicity == 'Any other mixed multiple ethnic background'] <- "Other"
alldata$ethnicity[alldata$ethnicity == 'Asian or Asian British – Bangladeshi'] <- "South Asian"
alldata$ethnicity[alldata$ethnicity == 'Asian or Asian British – Indian'] <- "South Asian"
alldata$ethnicity[alldata$ethnicity == 'Asian or Asian British – Pakistani'] <- "South Asian"
alldata$ethnicity[alldata$ethnicity == 'Mixed – White and Asian'] <- "Mixed or Multiple Heritage"
alldata$ethnicity[alldata$ethnicity == 'Mixed – White and Black African'] <- "Mixed or Multiple Heritage"
alldata$ethnicity[alldata$ethnicity == 'Mixed – White and Black Caribbean'] <- "Mixed or Multiple Heritage"
alldata$ethnicity[alldata$ethnicity == 'Any other white background'] <- "White"
alldata$ethnicity[alldata$ethnicity == 'White – English / Welsh / Scottish / Northern Irish / British'] <- "White"
alldata$ethnicity[alldata$ethnicity == 'White – Gypsy or Irish Traveller'] <- "White"
alldata$ethnicity[alldata$ethnicity == 'White – Irish'] <- "White"
alldata$ethnicity[alldata$ethnicity == 'White – Polish'] <- "White"
alldata$ethnicity[alldata$ethnicity == 'Black or Black British – African'] <- "Black"
alldata$ethnicity[alldata$ethnicity == 'Black or Black British – Caribbean'] <- "Black"
alldata$ethnicity[alldata$ethnicity == 'Prefer not to answer'] <- NA

## Combine sex data from two variables (pilot vs full cohort) into a single sex column
alldata$sex2[alldata$sex2 == ""] <- NA
alldata$sex1[alldata$sex1 == ""] <- NA
alldata$sex <- coalesce(alldata$sex2, alldata$sex1)
alldata <- alldata %>% relocate(sex, .after = pid)

## Convert variables to factors
factorcols <- c(
  "sex", "ethnicity", "gender",
  "diag_auto",
  "diag_psych", "diag_psych_anx", "diag_psych_depr",
  "income", "fatherdiag", "fatherdiag_psych", "motherdiag", "motherdiag_psych"
)
alldata[factorcols] <- lapply(alldata[factorcols], as.factor)
rm(factorcols)

## Recode PHQ-9 and GAD-7 variables and then convert to numeric
alldata[alldata == "Not at all"] <- 0
alldata[alldata == "Several days"] <- 1
alldata[alldata == "More than half the days"] <- 2
alldata[alldata == "Nearly every day"] <- 3

alldata$social[alldata$social == "Do not know"] <- NA
alldata$social[alldata$social == "Prefer not to answer"] <- NA
alldata$social[alldata$social == "No friends/family outside household"] <- 0
alldata$social[alldata$social == "Never or almost never"] <- 0
alldata$social[alldata$social == "Once every few month"] <- 1
alldata$social[alldata$social == "About once a month"] <- 2
alldata$social[alldata$social == "About once a week"] <- 3
alldata$social[alldata$social == "2-4 times a week"] <- 4
alldata$social[alldata$social == "Almost daily"] <- 5

numcols <- c(
  "phq01", "phq02", "phq03", "phq04", "phq05", "phq06", "phq07", "phq08", "phq09",
  "gad01", "gad02", "gad03", "gad04", "gad05", "gad06", "gad07",
  "social"
)
alldata[numcols] <- lapply(alldata[numcols], as.numeric)
rm(numcols)

## Convert consent_year and birth_year variables to R-friendly year format
alldata$consent_year <- format(as.Date(strptime(alldata$consent_year, format = "%Y")), "%Y")
alldata$consent_year <- as.numeric(alldata$consent_year)
alldata$birth_year <- format(as.Date(strptime(alldata$birth_year, format = "%Y")), "%Y")
alldata$birth_year <- as.numeric(alldata$birth_year)

## Calculate age at point of consent from consent year and birth year
alldata$age <- alldata$consent_year - alldata$birth_year
alldata <- alldata %>% relocate(age, .after = pid) # bringing age to front of dataframe
#alldata <- alldata[,c(1,ncol(alldata),2:(ncol(alldata)-1))] 


## Create new columns for mood disorders
# TRUE if diag_psych contains the strings "Anxiety", "Bipolar disorder", or "Depression"
alldata$depression <- grepl("Depression", alldata$diag_psych, ignore.case = T)
alldata$bipolar <- grepl("Bipolar disorder", alldata$diag_psych, ignore.case = T)
alldata$anxiety <- grepl("Anxiety", alldata$diag_psych, ignore.case = T)
alldata$affdisorder <- ifelse(alldata$depression == F & 
                                 alldata$bipolar == F & 
                                 alldata$anxiety == F,
                               FALSE, TRUE)

## Create new columns for autoimmune disorders
# TRUE if diag_auto contains the strings "Rheumatoid arthritis", "Lupus", "Inflammatory Bowel Disease (IBD)", "Multiple Sclerosis (MS)", "Graves' disease", or "Psoriasis"
alldata$arthritis <- grepl("Rheumatoid arthritis", alldata$diag_auto, ignore.case = T)
alldata$psoriasis <- grepl("Psoriasis", alldata$diag_auto, ignore.case = T)
alldata$ibd <- grepl("Inflammatory Bowel Disease", alldata$diag_auto, ignore.case = T)
alldata$graves <- grepl("Graves", alldata$diag_auto, ignore.case = T)
alldata$ms <- grepl("Multiple Sclerosis", alldata$diag_auto, ignore.case = T)
alldata$lupus <- grepl("Lupus", alldata$diag_auto, ignore.case = T)


## Create new columns for current depression/anxiety
# PHQ (sum of phq scores), GAD (sum of gad7 scores), currdep (PHQ > 9), curranx (GAD > 7)
alldata$PHQ <- rowSums(alldata[,c("phq01", "phq02", "phq03", "phq04", "phq05", "phq06", "phq07", "phq08", "phq09")])
alldata$GAD <- rowSums(alldata[,c("gad01", "gad02", "gad03", "gad04", "gad05", "gad06", "gad07")])

alldata$currdep <- ifelse(alldata$PHQ > 9, TRUE, FALSE)
alldata$curranx <- ifelse(alldata$GAD > 7, TRUE, FALSE)

## Create new columns for family psychiatric history
## TRUE if mother/father diag_psych contains "Depression", "Bipolar", "Anxiety"
alldata$motherpsych <- ifelse(
  #grepl("Mental Health", alldata$motherdiag, ignore.case = T) |
  grepl("Depression", alldata$motherdiag_psych, ignore.case = T) |
    grepl("Bipolar disorder", alldata$motherdiag_psych, ignore.case = T) | 
    grepl("Anxiety", alldata$motherdiag_psych, ignore.case = T),
  TRUE, FALSE
)

alldata$fatherpsych <- ifelse(
  #grepl("Mental Health", alldata$fatherdiag, ignore.case = T) |
  grepl("Depression", alldata$fatherdiag_psych, ignore.case = T) |
    grepl("Bipolar disorder", alldata$fatherdiag_psych, ignore.case = T) | 
    grepl("Anxiety", alldata$fatherdiag_psych, ignore.case = T),
  TRUE, FALSE
)

## Create new column for chronic pain
alldata$pain_chronic[alldata$pain_chronic == "Prefer not to answer"] <- NA
alldata$chronicpain <- ifelse(
  grepl("None of the above", alldata$pain_chronic, ignore.case = T),
  FALSE, TRUE
)


alldata[alldata == ""] <- NA


write.csv(alldata, "data/cleandata_controls.csv", row.names = F)
