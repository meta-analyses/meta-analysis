rm (list = ls())
# Read the data
data <- read.csv("../meta-analysis/data/09.06_COMBINED DATASET REDUCED.csv", header = T, stringsAsFactors = F, skipNul = TRUE)

data$tot_personyrs <- as.numeric(data$tot_personyrs)
data[(is.na(data$tot_personyrs)),]$tot_personyrs <- 
  data[(is.na(data$tot_personyrs)),]$mean_followup * data[(is.na(data$tot_personyrs)),]$n_baseline
data[(is.na(data$mean_followup)),]$mean_followup <- 
  data[(is.na(data$mean_followup)),]$tot_personyrs / data[(is.na(data$mean_followup)),]$n_baseline

data$outcome <- trimws(data$outcome)

# Read all the functions
source("../meta-analysis/all-functions.R")

# Identify unique outcomes
uoutcome <- data.frame(outcome = as.character(unique(data$outcome)))
uoutcome$outcome <- as.character(uoutcome$outcome)

i = 1
cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", gender = 1)
#acmdata <- subset(acmdata, outcome_type == "mortality")
acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = T)

plot_male_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T, minQuantile = 0, maxQuantile = 0.75, lout = 100, lby = NULL))
colnames(plot_male_data) <- c("dose","RR", "lb", "ub")


i = 1
cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", gender = 2)
#acmdata <- subset(acmdata, outcome_type == "mortality")
acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = T)

plot_female_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T, minQuantile = 0, maxQuantile = 0.75))
colnames(plot_female_data) <- c("dose","RR", "lb", "ub")



i = 1
cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", overall1 = 1)
#acmdata <- subset(acmdata, outcome_type == "mortality")
acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = T)

plot_overall_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T, minQuantile = 0, maxQuantile = 0.7, lout = 100))
colnames(plot_overall_data) <- c("dose","RR", "lb", "ub")


