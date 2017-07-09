rm (list = ls())

total_population <- T
male_population <- T
female_population <- T
local_pa_domain_subgroup <- "TPA"

# Read the data
raw_data <- read.csv("data/20170626_MASTER_PA_Dose_Metananalysis_Data_Extraction.csv", header = T, stringsAsFactors = F, skipNul = TRUE)

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

#Remove empty outcome
#nv <- uoutcome[!apply(is.na(uoutcome) | uoutcome == "", 1, all),]
#uoutcome <- data.frame(outcome = nv)

raw_data$totalpersons <- as.numeric(raw_data$totalpersons)
raw_data$personyrs <- as.numeric(raw_data$personyrs)

raw_data[raw_data$effect_measure == "RR",]$effect_measure <- "rr"
raw_data[raw_data$effect_measure == "HR",]$effect_measure <- "hr"

raw_data$type <- ""

raw_data[raw_data$effect_measure == "or",]$type <- "ir"
raw_data[raw_data$effect_measure == "rr",]$type <- "ir"
raw_data[raw_data$effect_measure == "hr",]$type <- "ci"
raw_data$type <- as.character(raw_data$type)


## RENAME columns

raw_data$cases <- as.numeric(raw_data$cases)

raw_data$dose <- raw_data$Final.Harmonised.exposure..MMET.hrs.wk.
raw_data$Final.Harmonised.exposure..MMET.hrs.wk. <- NULL
raw_data$rr <- raw_data$effect

raw_data <- subset(raw_data, select = c(ref_number, outcome, pa_domain_subgroup, overall, sex_subgroups, effect_measure, type, n_baseline, totalpersons, tot_personyrs, personyrs, 
                                   mean_followup, dose, rr, effect, uci_effect, lci_effect, cases))


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


if (total_population){
  for (i in 1:nrow(uoutcome)){
    i <- 6
    cat("Total Population - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmfdata <- subset(raw_data, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup & (overall == 1 | sex_subgroups == 3))
    if (nrow(acmfdata) > 0){

      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      # Subset to selected columns
      acmfdata <- subset(acmfdata, select = c(id, ref_number, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))
      
      if (local_pa_domain_subgroup == "TPA" && (i == 4 || i == 10)){
        acmfdata[acmfdata$logrr == 0,]$se <- acmfdata[acmfdata$logrr == 0,]$uci_effect <- acmfdata[acmfdata$logrr == 0,]$lci_effect <- 0
      }
      
      if (nrow(acmfdata) > 0){
        metaAnalysis(acmfdata, ptitle = paste0( uoutcome$outcome[i] , " (", local_pa_domain_subgroup,") ", " - Total Population"), covMethed = T, minQuantile = 0, maxQuantile = 0.75)
      }
    }
  }
  
}

if(male_population){
  for (i in 1:nrow(uoutcome)){
    #i <- 11
    cat("Male Population - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmfdata <- subset(raw_data, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup & sex_subgroups == 1)
    if (nrow(acmfdata) > 0){
      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      # Remove where both dose and response are null
      acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
      
      acmfdata <- subset(acmfdata, select = c(id, ref_number, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))
      
      if (local_pa_domain_subgroup == "TPA" && (i == 11) )# Remove duplicate rows for ref_number 73
        acmfdata <- acmfdata[!duplicated(acmfdata[c("id", "dose")]),]
      
      if (nrow(acmfdata) > 0){
        metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i] ,  " (", local_pa_domain_subgroup,") ", " - Male Population"), covMethed = T, minQuantile = 0, maxQuantile = 0.75)
      }
    }
  }
}

if(female_population){
  for (i in 1:nrow(uoutcome)){
    i <- 6
    cat("Female Population - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmfdata <- subset(raw_data, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup & sex_subgroups == 2)
    if (nrow(acmfdata) > 0){
      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      acmfdata <- subset(acmfdata, select = c(id, ref_number, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))
      
      if (local_pa_domain_subgroup == "TPA" && (i == 4 || i == 10))
        acmfdata[acmfdata$logrr == 0,]$se <- acmfdata[acmfdata$logrr == 0,]$uci_effect <- acmfdata[acmfdata$logrr == 0,]$lci_effect <- 0
      
      if (nrow(acmfdata) > 0){
        metaAnalysis(acmfdata, ptitle = paste0( uoutcome$outcome[i] ,  " (", local_pa_domain_subgroup,") ", " - Female Population"), covMethed = T, minQuantile = 0, maxQuantile = 0.75)
      }
    }
  }
}