# Remove all elements from the memory
rm (list = ls())

local_pa_domain_subgroup <- "LTPA"

# Read the data
raw_data <- read.csv("data/20171129_MASTER_PA_Dose_Metananalysis_Data_Extraction.csv", header = T, stringsAsFactors = F, skipNul = TRUE)

raw_data$tot_personyrs <- as.numeric(raw_data$tot_personyrs)
#raw_data[is.na(raw_data$tot_personyrs),]$tot_personyrs <- 0
raw_data$mean_followup <- as.numeric(raw_data$mean_followup)
raw_data$n_baseline <- as.numeric(raw_data$n_baseline)
#raw_data[is.na(raw_data$n_baseline),]$n_baseline <- 0

raw_data[(is.na(raw_data$tot_personyrs)),]$tot_personyrs <- 
  raw_data[(is.na(raw_data$tot_personyrs)),]$mean_followup * raw_data[(is.na(raw_data$tot_personyrs)),]$n_baseline
raw_data[(is.na(raw_data$mean_followup)),]$mean_followup <- 
  raw_data[(is.na(raw_data$mean_followup)),]$tot_personyrs / raw_data[(is.na(raw_data$mean_followup)),]$n_baseline

raw_data$outcome <- trimws(raw_data$outcome)

raw_data$effect_measure <- trimws(raw_data$effect_measure)

raw_data$outcome <- trimws(raw_data$outcome)
raw_data$pa_domain_subgroup <- trimws(raw_data$pa_domain_subgroup)
raw_data$overall <- trimws(raw_data$overall)
raw_data$sex_subgroups <- trimws(raw_data$sex_subgroups)

# Read all the functions
source("all-functions.R")

# Identify unique outcomes
uoutcome <- data.frame(outcome = as.character(unique(raw_data$outcome)))
uoutcome$outcome <- as.character(uoutcome$outcome)
# Sort
uoutcome$outcome <- sort(uoutcome$outcome)
# Remove the blank outcome
uoutcome <- dplyr::filter(uoutcome, outcome != "")


raw_data$totalpersons <- as.numeric(raw_data$totalpersons)
raw_data$personyrs <- as.numeric(raw_data$personyrs)

raw_data[raw_data$effect_measure == "RR",]$effect_measure <- "RR"
raw_data[raw_data$effect_measure == "HR",]$effect_measure <- "hr"

raw_data$type <- ""

raw_data[raw_data$effect_measure == "or",]$type <- "ir"
raw_data[raw_data$effect_measure == "RR",]$type <- "ir"
raw_data[raw_data$effect_measure == "hr",]$type <- "ci"
raw_data$type <- as.character(raw_data$type)

## RENAME columns

raw_data$cases <- as.numeric(raw_data$cases)

raw_data$dose <- raw_data$Final.Harmonised.exposure..MMET.hrs.wk.
raw_data$Final.Harmonised.exposure..MMET.hrs.wk. <- NULL
raw_data$RR <- raw_data$effect

raw_data <- subset(raw_data, select = c(ref_number, Study, Author, outcome, outcome_type, pa_domain_subgroup, overall, sex_subgroups, effect_measure, type, n_baseline, totalpersons, tot_personyrs, personyrs, 
                                        mean_followup, dose, RR, effect, uci_effect, lci_effect, tot_cases, cases, objective_calibration))


## Populate missing totalpersons and personyrs

for (i in unique(raw_data$ref_number)){
  
  raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$totalpersons <-
    round(raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$cases /
            sum(raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$cases)  *
            raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$n_baseline)
  
  
  raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$personyrs <-
    round(raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$cases /
            sum(raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$cases)  *
            raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$tot_personyrs)
}

#raw_data[raw_data$overall != 1 & raw_data$sex_subgroups == 1,]$ref_number <- paste0(raw_data[raw_data$overall != 1 & raw_data$sex_subgroups == 1,]$ref_number, "-1")
#raw_data[raw_data$overall != 1 & raw_data$sex_subgroups == 2,]$ref_number <- paste0(raw_data[raw_data$overall != 1 & raw_data$sex_subgroups == 2,]$ref_number, "-2")



