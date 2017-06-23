rm (list = ls())

total_population <- F
male_population <- T
female_population <- F

# Read the data
raw_data <- read.csv("data/20170621_MASTER_PA_Dose_Metananalysis_Data_Extraction.csv", header = T, stringsAsFactors = F, skipNul = TRUE)
#raw_data[is.na(raw_data)] <- ""
#raw_data <- raw_data[!apply(raw_data == "", 1, all),]

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


brd <- raw_data

raw_data <- subset(brd, select = c(ref_number, outcome, pa_domain_subgroup, overall, sex_subgroups, effect_measure, type, totalpersons, personyrs, 
                                   mean_followup, dose, rr, effect, uci_effect, lci_effect, cases))


#effect_measure, totalpersons, personyrs, mean_followup)

#raw_data <- brd

if (total_population){
  for (i in 1:nrow(uoutcome)){
    # i = 6
    
    cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmfdata <- subset(raw_data, outcome == uoutcome$outcome[i] & pa_domain_subgroup == "LTPA" & (overall == 1 | sex_subgroups == 3))
    if (nrow(acmfdata) > 0){

      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      # Subset to selected columns
      acmfdata <- subset(acmfdata, select = c(id, ref_number, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))
      
      if (uoutcome$outcome[i] == 'breast cancer'){ #1 4 5 6 7 9 10 11 12 13 14 15 16 17 19 20
        
        # Remove a study with just one exposure
        acmfdata <- subset(acmfdata, !id %in% c(14))
        
        # Set standard error to zero to the first exposure
        # id == 15, rr 1.05, effect 1.05, uci_effect 1.34, lci_effect 0.82, cases == 210, 
        acmfdata[acmfdata$id == 15 & acmfdata$rr == 1.05 & acmfdata$cases == 210,]$se <- 0
        # acmfdata[acmfdata$id == 15 & acmfdata$rr == 1.05 & acmfdata$effect == 1.05 & acmfdata$uci_effect == 1.34 & acmfdata$lci_effect == 0.82
        #          & acmfdata$cases == 210,]$se <- 0
        
        # # -----------------------
        # # 9, 14, 15
        # acmfdata <- subset(b, id %in% c(1, 4, 5, 6, 7, 10:13, 16, 17, 19, 20))
        
      }
      
      if (nrow(acmfdata) > 0){
        metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i] , " (LTPA) ", " - Total Population"), covMethed = T, minQuantile = 0, maxQuantile = 0.75)
      }
    }
  }
  
}

if(male_population){
  for (i in 1:nrow(uoutcome)){
    # i <- 6
    cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmfdata <- subset(raw_data, outcome == uoutcome$outcome[i] & pa_domain_subgroup == "LTPA" & sex_subgroups == 1)
    if (nrow(acmfdata) > 0){
      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      # Remove where both dose and response are null
      acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
      
      acmfdata <- subset(acmfdata, select = c(id, ref_number, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))
      b <- acmfdata
      
      if (nrow(acmfdata) > 0){
        metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i] ,  " (LTPA) ", " - Male Population"), covMethed = T, minQuantile = 0, maxQuantile = 0.75)
      }
    }
  }
}

if(female_population){
  for (i in 1:nrow(uoutcome)){
    # i = 3
    cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmfdata <- subset(raw_data, outcome == uoutcome$outcome[i] & pa_domain_subgroup == "LTPA" & sex_subgroups == 2)
    if (nrow(acmfdata) > 0){
      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      acmfdata <- subset(acmfdata, select = c(id, ref_number, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, uci_effect, lci_effect, se))
      b <- acmfdata
      
      if (uoutcome$outcome[i] == 'breast cancer'){ 
        
        # Problematic ids: 17, 18
        # Remove id 17 as it has only one exposure
        acmfdata <- subset(acmfdata, !id %in% 17)
        # Set se to zero with rr 1, other than the first rr
        acmfdata[acmfdata$id == 18 & acmfdata$rr == 1.05 & acmfdata$cases == 210,]$se <- 0
      }
      
      if (nrow(acmfdata) > 0){
        metaAnalysis(acmfdata, ptitle = paste0( uoutcome$outcome[i] ,  " (LTPA) ", " - Female Population"), covMethed = T, minQuantile = 0, maxQuantile = 0.75)
      }
    }
  }
}