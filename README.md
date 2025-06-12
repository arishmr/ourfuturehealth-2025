# Affective disorders and chronic inflammatory conditions: analysis of 1.5 million participants in Our Future Health
Analysis code for an Our Future Health cohort study exploring the prevalence of affective disorders among people with autoimmune conditions.

### PROJECT SUMMARY
The goal of this study was to determine the prevalence of lifetime diagnoses of affective disorders (depression, anxiety, and bipolar) in people with autoimmune conditions, compared to people without autoimmune conditions, in the Our Future Health dataset.
We also wanted to quantify the risk of experiencing an affective disorder in people with autoimmune conditions (compared to people without autoimmune conditions) while controlling for relevant sociodemographic characteristics.

### ASSOCIATED PUBLICATION
This analysis code was used for the study described in the following article:
Mudra Rakshasa-Loots, Swiffen, Steyn, Marwick, and Smith. (2025). "Affective disorders and chronic inflammatory conditions: analysis of 1.5 million participants in Our Future Health". BMJ Mental Health, [DOI: 10.1136/bmjment-2025-301706](https://doi.org/10.1136/bmjment-2025-301706).

### CONTACT
For any questions about the code, please contact the lead investigator: Dr Arish Mudra Rakshasa-Loots ([arish.mrl@ed.ac.uk](mailto:arish.mrl@ed.ac.uk)).

### DATA DICTIONARY
A full description of variables used in the analysis can be found in the Our Future Health data dictionary, at this link: https://research.ourfuturehealth.org.uk/data-and-cohort/

### GENERAL APPROACH
To calculate prevalence estimates, we used the [`epi.conf`](https://rdrr.io/cran/epiR/man/epi.conf.html) function. This function requires a matrix of the number of cases (i.e. people with the relevant psychiatric outcome) in the sample and the total sample size. To calculate Odds Ratios, we ran logistic regression models iteratively for each outcome adjusted for the relevant covariates.

### DESCRIPTION OF SCRIPTS
Scripts are described below in the order in which they are run, though they are fairly independent (except `01-cleaning` is always run first).

**01-cleaning.R** cleans the raw data imported from Our Future Health into a format that is usable for the intended analyses (in brief, this involves merging variables from two dataframes that were originally provided separately, renaming columns, recoding variables to appropriate categories, assigning variables to be factors or numeric, and deriving new variables such as age or specific diagnoses from available data).

**02-startup-bash.sh** is the bash code that is used to create relevant directories within the DNAnexus platform for analysis of this data.

**03-prevalence.R** involves the main approach to calculating prevalence estimates of the outcomes of interest (lifetime diagnoses of various affective disorders).

**04-prevalence-sex.R** repeats these analyses separately for males and females.

**05-prev-current.R** repeats these analyses for current depression and current anxiety as the outcomes.

**06-prev-current-sex.R** repeats these analyses for current depression and current anxiety separately for males and females.

**07-redaction.R** redacts the number of participants where these were <10, as per the Safe Outputs Policy for OFH.

**08-logitreg.R** runs logistic regression models iteratively on various subsets of the data to calculate Odds Ratios for relevant outcomes.

**09-demographics.R** summarises key demographic information for the participants included in the study.

**10-ending-bash.sh** is the bash code that is used to upload the outputs of the analyses to the DNAnexus platform for storage.
