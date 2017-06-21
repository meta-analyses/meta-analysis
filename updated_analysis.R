rm (list = ls())

total_population <- F
male_population <- F
female_population <- T

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
  #i = 1
  
  cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
  acmfdata <- subset(raw_data, outcome == uoutcome$outcome[i] & pa_domain_subgroup == "LTPA" & (overall == 1 | sex_subgroups == 3))
  if (nrow(acmfdata) > 0){
    #acmfdata$dose <- acmfdata$Final.Harmonised.exposure..MMET.hrs.wk.
    #acmfdata$Final.Harmonised.exposure..MMET.hrs.wk. <- NULL
    
    #acmfdata$rr <- acmfdata$effect
    
    acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
    
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    
    
    
    #cat(unique(acmfdata$id))
    
    b <- acmfdata
    
    acmfdata <- subset(b, select = c(id, ref_number, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, se))
    
    #metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i] , " LTPA - Total Population"), covMethed = T)
    
    #acmfdata <- b
    
    if (i == 11)
      acmfdata <- subset(acmfdata, !id %in% 1)
    if (i == 1){ #1 4 5 6 7 9 10 11 12 13 14 15 16 17 19 20
      # 9, 14, 15
      acmfdata <- subset(b, id %in% c(1, 4, 5, 6, 7, 10:13, 16, 17, 19, 20))# 5 6 7 9 10 11 12 13 14 15 16 17 19 20))
      #metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i] , " LTPA - Total Population"), covMethed = T)
    }
    
    if (i == 7){#1 2 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
      # 2, 18
      acmfdata <- subset(b, id %in% c(1, 4:17, 19, 20))
      #metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i] , " LTPA - Total Population"), covMethed = T)
      
    }
    
    if (nrow(acmfdata) > 0){
      metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i] , " LTPA - Total Population"), covMethed = T)
    }
  }
}
  
}

if(male_population){
for (i in 1:nrow(uoutcome)){
  cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
  acmfdata <- subset(raw_data, outcome == uoutcome$outcome[i] & pa_domain_subgroup == "LTPA" & sex_subgroups == 1)
  if (nrow(acmfdata) > 0){
    #acmfdata$dose <- acmfdata$Final.Harmonised.exposure..MMET.hrs.wk.
    #acmfdata$Final.Harmonised.exposure..MMET.hrs.wk. <- NULL
    
    #acmfdata$rr <- acmfdata$effect
    
    acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
    
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    
    
    
    cat(unique(acmfdata$id))
    
    b <- acmfdata
    
    acmfdata <- subset(b, select = c(id, ref_number, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, se))
    
    if (i == 4){#1 3 5 6
      # 1
      acmfdata <- subset(b, !id %in% c(1))
      metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i] , " LTPA - Total Population"))
      
    }
    
    if (i == 6){#1 3 5 6
      # 1
      acmfdata <- subset(b, !id %in% c(1))
      #metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i] , " LTPA - Total Population"), covMethed = T)
      
    }
    
    if (i == 7){#1:10
      # 7, 9, 10
      acmfdata <- subset(b, id %in% c(1:6, 8))
      #metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i] , " LTPA - Total Population"))
      
    }
    
    
    #metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i] , " LTPA - Male Population"), covMethed = T)
    
    if (nrow(acmfdata) > 0){
      metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i] , " LTPA - Male Population"), covMethed = T)
    }
  }
}
}



if(female_population){
for (i in 1:nrow(uoutcome)){
  #i = 11
  cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
  acmfdata <- subset(raw_data, outcome == uoutcome$outcome[i] & pa_domain_subgroup == "LTPA" & sex_subgroups == 2)
  if (nrow(acmfdata) > 0){
    #acmfdata$dose <- acmfdata$Final.Harmonised.exposure..MMET.hrs.wk.
    #acmfdata$Final.Harmonised.exposure..MMET.hrs.wk. <- NULL
    
    #acmfdata$rr <- acmfdata$effect
    
    acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
    
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    
    
    
    #cat(unique(acmfdata$id))
    
    b <- acmfdata
    
    acmfdata <- subset(b, select = c(id, ref_number, effect_measure, type, totalpersons, personyrs, dose, rr, logrr, cases, se))
    
    if (i == 1){ #1 4 5 6 7 9 10 11 12 13 14 15 16 17 19 20
      # 11, 17
      acmfdata <- subset(b, id %in% c(1, 4:7, 9, 10, 12:16, 12, 20))
      #metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i] , " LTPA - Total Population"), covMethed = T)
    }
    
    if (i == 11){ #1 2 3 4 6 8 9
      # 1
      acmfdata <- subset(b, id %in% c(2:4, 6, 8, 9))
      #metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i] , " LTPA - Total Population"), covMethed = T)
    }
    

    if (nrow(acmfdata) > 0){
      metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i] , " LTPA - Female Population"), covMethed = T)
    }
  }
}
}