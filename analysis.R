rm (list = ls())
# Read the data
data <- read.csv("data/09.06_COMBINED DATASET REDUCED.csv", header = T, stringsAsFactors = F, skipNul = TRUE)

data$tot_personyrs <- as.numeric(data$tot_personyrs)
data[(is.na(data$tot_personyrs)),]$tot_personyrs <- 
  data[(is.na(data$tot_personyrs)),]$mean_followup * data[(is.na(data$tot_personyrs)),]$n_baseline
data[(is.na(data$mean_followup)),]$mean_followup <- 
  data[(is.na(data$mean_followup)),]$tot_personyrs / data[(is.na(data$mean_followup)),]$n_baseline

# Read all the functions
source("all-functions.R")

# Identify unique outcomes
uoutcome <- data.frame(outcome = as.character(unique(data$outcome)))


## ALL CAUSE MORTALITY

# Overall All Cause Mortality
acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[1], paexposure = "LTPA", overall1 = 1)
acmfdata <- formatData(acmdata, kcases = T)
metaAnalysis(acmfdata, ptitle = "All Cause Mortality - LTPA - Total Population", covMethed = T)

# All Cause Mortality - Males
acmmdata <- getDiseaseSpecificData(data, uoutcome$outcome[1], paexposure = "LTPA", gender = 1)
acmmfdata <- formatData(acmmdata, kcases = T)
# Remove all rows with missing total persons for RR and OR, and missing person years for HR
# acmmfdata1 <- subset(acmmfdata, !(effect_measure == "rr" & (!is.na(totalpersons) || totalpersons == 0)))
metaAnalysis(acmmfdata, ptitle = "All Cause Mortality - LTPA - Males")

# All Cause Mortality - Females
acmfdata <- getDiseaseSpecificData(data, uoutcome$outcome[1], paexposure = "LTPA", gender = 2)
acmffdata <- formatData(acmfdata, kcases = T)
metaAnalysis(acmffdata, ptitle = "All Cause Mortality - LTPA - Females") 

## ALL CAUSE MORTALITY - TPA

# Overall All Cause Mortality
acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[1], paexposure = "TPA", overall1 = 1)
acmfdata <- formatData(acmdata, kcases = T)
metaAnalysis(acmfdata, ptitle = "All Cause Mortality - TPA - Total Population")

# All Cause Mortality - Males
acmmdata <- getDiseaseSpecificData(data, uoutcome$outcome[1], paexposure = "TPA", gender = 1)
acmmfdata <- formatData(acmmdata, kcases = T)
metaAnalysis(acmmfdata, ptitle = "All Cause Mortality - TPA - Males")

# All Cause Mortality - Females
acmfdata <- getDiseaseSpecificData(data, uoutcome$outcome[1], paexposure = "TPA", gender = 2)
acmffdata <- formatData(acmfdata, kcases = T)
metaAnalysis(acmffdata, ptitle = "All Cause Mortality - TPA - Females")

## STROKE

# Overall Stroke
strokedata <- getDiseaseSpecificData(data, "stroke", paexposure = "LTPA", overall1 = 1)
strokefdata <- formatData(strokedata, kcases = T)
# Remove all rows with missing RR and Dose
strokefdata <- subset(strokefdata, !is.na(rr) & !is.na(dose))
metaAnalysis(strokefdata, ptitle = "Stroke - LTPA - Total Population")

# Stroke Male
strokemdata <- getDiseaseSpecificData(data, "stroke", paexposure = "LTPA", gender = 1)
strokemfdata <- formatData(strokemdata, kcases = T)
# Remove all rows with missing RR and Dose
strokemfdata <- subset(strokemfdata, !is.na(rr) & !is.na(dose))
metaAnalysis(strokemfdata, ptitle = "Stroke - LTPA - Males")

# Stroke Female
strokefdata <- getDiseaseSpecificData(data, "stroke", paexposure = "LTPA", gender = 2)
strokeffdata <- formatData(strokefdata, kcases = T)
# Remove all rows with missing RR and Dose
strokeffdata <- subset(strokeffdata, !is.na(rr) & !is.na(dose))
metaAnalysis(strokeffdata, ptitle = "Stroke - LTPA - Females", covMethed = T)


## STROKE - TPA

# Overall Stroke
strokedata <- getDiseaseSpecificData(data, "stroke", paexposure = "TPA", overall1 = 1)
strokefdata <- formatData(strokedata, kcases = T)
# Remove all rows with missing RR and Dose
strokefdata <- subset(strokefdata, !is.na(rr) & !is.na(dose))
strokefdata[strokefdata$logrr == 0,]$se <- strokefdata[strokefdata$logrr == 0,]$lci <- strokefdata[strokefdata$logrr == 0,]$uci <- 0
metaAnalysis(strokefdata, ptitle = "Stroke - TPA - Total Population")

# Stroke Male
strokemdata <- getDiseaseSpecificData(data, "stroke", paexposure = "TPA", gender = 1)
strokemfdata <- formatData(strokemdata, kcases = T)
# Remove all rows with missing RR and Dose
strokemfdata <- subset(strokemfdata, !is.na(rr) & !is.na(dose))
metaAnalysis(strokemfdata, ptitle = "Stroke - TPA - Males")

# Stroke Female
strokefdata <- getDiseaseSpecificData(data, "stroke", paexposure = "TPA", gender = 2)
strokeffdata <- formatData(strokefdata, kcases = T)
# Remove all rows with missing RR and Dose
strokeffdata <- subset(strokeffdata, !is.na(rr) & !is.na(dose))
strokeffdata[strokeffdata$logrr == 0,]$se <- strokeffdata[strokeffdata$logrr == 0,]$lci <- strokeffdata[strokeffdata$logrr == 0,]$uci <- 0
metaAnalysis(strokeffdata, ptitle = "Stroke - TPA - Females")

## CHD

# Overall CHD
chddata <- getDiseaseSpecificData(data, "CHD", paexposure = "LTPA", overall1 = 1)
chdfdata <- formatData(chddata, kcases = T)
# Remove all rows with missing RR and Dose
chdfdata <- subset(chdfdata, !is.na(rr) & !is.na(dose))
metaAnalysis(chdfdata, ptitle = "CHD - LTPA - Total Population")

# CHD MALE
chdmdata <- getDiseaseSpecificData(data, "CHD", paexposure = "LTPA", gender = 1)
chdmfdata <- formatData(chdmdata, kcases = T)
# Remove all rows with missing RR and Dose
chdmfdata <- subset(chdmfdata, !is.na(rr) & !is.na(dose))
metaAnalysis(chdmfdata, ptitle = "CHD - LTPA - Males")

# CHD FEMALE
chdfdata <- getDiseaseSpecificData(data, "CHD", paexposure = "LTPA", gender = 2)
chdffdata <- formatData(chdfdata, kcases = T)
# Remove all rows with missing RR and Dose
chdffdata <- subset(chdffdata, !is.na(rr) & !is.na(dose))
metaAnalysis(chdffdata, ptitle = "CHD - LTPA - Females", covMethed = T)

## CVD

# Overall CvD
cvddata <- getDiseaseSpecificData(data, "CVD", paexposure = "LTPA", overall1 = 1)
cvdfdata <- formatData(cvddata, kcases = T)
# Remove all rows with missing RR and Dose
cvdfdata <- subset(cvdfdata, !is.na(rr) & !is.na(dose))
metaAnalysis(cvdfdata, ptitle = "CVD - LTPA - Total Population")

# CVD MALE
cvdmdata <- getDiseaseSpecificData(data, "CVD", paexposure = "LTPA", gender = 1)
cvdmfdata <- formatData(cvdmdata, kcases = T)
# Remove all rows with missing RR and Dose
cvdmfdata <- subset(cvdmfdata, !is.na(rr) & !is.na(dose))
metaAnalysis(cvdmfdata, ptitle = "CVD - LTPA - Males")

# CVD FEMALE
cvdfdata <- getDiseaseSpecificData(data, "CVD", paexposure = "LTPA", gender = 2)
cvdffdata <- formatData(cvdfdata, kcases = T)
# Remove all rows with missing RR and Dose
cvdffdata <- subset(cvdffdata, !is.na(rr) & !is.na(dose))
metaAnalysis(cvdffdata, ptitle = "CVD - LTPA - Females", covMethed = T)
