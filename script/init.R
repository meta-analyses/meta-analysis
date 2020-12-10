# Remove all elements from the memory
rm (list = ls())
require(tidyverse)

local_pa_domain_subgroup <- "LTPA" 

# Read the data
raw_data <- read.csv("data/R_variables_final (1).csv", header = T, stringsAsFactors = F, skipNul = TRUE)

raw_data$mean_followup <- as.numeric(raw_data$mean_followup)
raw_data$n_baseline <- as.numeric(raw_data$n_baseline)
#raw_data[is.na(raw_data$n_baseline),]$n_baseline <- 0

raw_data[(is.na(raw_data$tot_personyrs)),]$tot_personyrs <- 
  raw_data[(is.na(raw_data$tot_personyrs)),]$mean_followup * raw_data[(is.na(raw_data$tot_personyrs)),]$n_baseline
raw_data[(is.na(raw_data$mean_followup)),]$mean_followup <- 
  raw_data[(is.na(raw_data$mean_followup)),]$tot_personyrs / raw_data[(is.na(raw_data$mean_followup)),]$n_baseline

raw_data$outcome <- trimws(raw_data$outcome)
raw_data$pa_domain_subgroup <- trimws(raw_data$pa_domain_subgroup) 
raw_data$overall <- trimws(raw_data$overall) 
raw_data$sex_subgroups <- trimws(raw_data$sex_subgroups)

raw_data$totalpersons <- as.numeric(raw_data$n_per_category)
raw_data$personyrs <- as.numeric(raw_data$person_years_per_category)

raw_data[raw_data$effect_measure == "RR",]$effect_measure <- "rr"
raw_data[raw_data$effect_measure == "HR",]$effect_measure <- "hr"

raw_data$type <- ""

#raw_data[raw_data$effect_measure == "or",]$type <- "ir" #no odds ratio
raw_data[raw_data$effect_measure == "rr",]$type <- "ir"
raw_data[raw_data$effect_measure == "hr",]$type <- "ci"
raw_data$type <- as.character(raw_data$type)


## RENAME columns

raw_data <- raw_data %>% rename(effect = most_adj_effect, cases = cases_per_category,
                                uci_effect = most_adj_uci, lci_effect = most_adj_lci) %>% 
  mutate(cases = as.numeric(cases), uci_effect = as.numeric(uci_effect), lci_effect = as.numeric(lci_effect))



# ## FOR SENSITIVITY ANALYSIS
#raw_data$dose <- raw_data$Final.Harmonised.exposure..MMET.hrs.wk...FOR.SENSITIVITY.ANALYSIS


## FOR THE CURRENT ASSUMPTIONS
raw_data$dose <- round(raw_data$m_met_h_wk, 2)

#raw_data$Final.Harmonised.exposure..MMET.hrs.wk. <- NULL
raw_data$RR <- raw_data$effect

raw_data <- subset(raw_data, select = c(ref_number, first_author, outcome, outcome_type, sex_subgroups, type, n_baseline, totalpersons, tot_personyrs, personyrs,
                                        mean_followup, dose, RR, effect, effect_measure, lci_effect, uci_effect, cases, overall, pa_domain_subgroup))


## Populate missing totalpersons and personyrs

for (i in unique(raw_data$ref_number)){
  
  raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & !(is.na(raw_data$personyrs)) & !(is.na(raw_data$tot_personyrs)),]$totalpersons <-
    round(raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & !(is.na(raw_data$personyrs)) & !(is.na(raw_data$tot_personyrs)),]$personyrs /
            sum(raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & !(is.na(raw_data$personyrs)) & !(is.na(raw_data$tot_personyrs)),]$tot_personyrs)  *
            raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & !(is.na(raw_data$personyrs)) & !(is.na(raw_data$tot_personyrs)),]$n_baseline)
  
  
  raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$personyrs <-
    round(raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$cases /
            sum(raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$cases)  *
            raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$tot_personyrs)
}

# Replace 'Heart failure' with 'heart failure'
#raw_data[raw_data$outcome == 'Heart failure',]$outcome <- 'heart failure'

# Replace "CVD" with "Cardiovascular Disease"
#raw_data[raw_data$outcome == "CVD",]$outcome <- "Cardiovascular Disease"

# Replace "CHD" with "Coronary Heart Disease"
#raw_data[raw_data$outcome == "CHD",]$outcome <- "Coronary Heart Disease"

# 
raw_data$outcome <- stringi::stri_trans_totitle(raw_data$outcome, opts_brkiter = stringi::stri_opts_brkiter(type = "sentence"))

# Identify unique outcomes
uoutcome <- data.frame(outcome = as.character(unique(raw_data$outcome)))
uoutcome$outcome <- as.character(uoutcome$outcome)

uoutcome$outcome <- stringi::stri_trans_totitle(uoutcome$outcome, opts_brkiter = stringi::stri_opts_brkiter(type = "sentence"))

# Sort
uoutcome$outcome <- sort(uoutcome$outcome)

# Remove the blank outcome
uoutcome <- dplyr::filter(uoutcome, outcome != "")

# All-cause mortality
# Cardiovascular diseases
# Coronary heart disease
# Stroke
# Breast cancer
# Colon cancer
# Endometrial cancer
# Lung cancer
# Total cancer
#uoutcome$outcome <- uoutcome[c(1, 3, 5, 9, 2, 4, 6, 8, 10, 7),]

source("script/all-functions.R")
