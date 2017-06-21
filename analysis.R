#rm (list = ls())
# Read the data
raw_data <- read.csv("data/09.06_COMBINED DATASET REDUCED.csv", header = T, stringsAsFactors = F, skipNul = TRUE)

raw_data$tot_personyrs <- as.numeric(raw_data$tot_personyrs)
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

#fdata <- NULL
fdata <- basic_df[c(), ]

for(k in unique(td$ref_number)){
  raw_sub_data <- subset(raw_data, ref_number == k)

  for (i in 1:nrow(uoutcome)){
    # i= 6
    cat("ref: ", k,  "Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmdata <- getDiseaseSpecificData(raw_sub_data, uoutcome$outcome[i], paexposure = "LTPA", overall1 = 1)
    #acmdata <- subset(acmdata, outcome_type == "mortality")
    acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = F)
    if (nrow(acmfdata) > 0){
      fdata <- rbind(fdata, acmfdata)
    }else{
      acmfdata <- formatData(acmdata, infertotalpersons = F)
      if (nrow(acmfdata) > 0){
        fdata <- rbind(fdata, acmfdata)
      }
    }
    
    
    acmdata <- getDiseaseSpecificData(raw_sub_data, uoutcome$outcome[i], paexposure = "LTPA", gender = 1)
    #acmdata <- subset(acmdata, outcome_type == "mortality")
    acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = F)
    if (nrow(acmfdata) > 0){
      fdata <- rbind(fdata, acmfdata)
    }else{
      acmfdata <- formatData(acmdata, infertotalpersons = F)
      if (nrow(acmfdata) > 0){
        fdata <- rbind(fdata, acmfdata)
      }
    }
    
    
    acmdata <- getDiseaseSpecificData(raw_sub_data, uoutcome$outcome[i], paexposure = "LTPA", gender = 2)
    #acmdata <- subset(acmdata, outcome_type == "mortality")
    acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = F)
    
    if (nrow(acmfdata) > 0){
      fdata <- rbind(fdata, acmfdata)
    }else{
      acmfdata <- formatData(acmdata, infertotalpersons = F)
      if (nrow(acmfdata) > 0){
        fdata <- rbind(fdata, acmfdata)
      }
    }
    
    
    acmdata <- getDiseaseSpecificData(raw_sub_data, uoutcome$outcome[i], paexposure = "LTPA", gender = 3)
    #acmdata <- subset(acmdata, outcome_type == "mortality")
    acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = F)
    
    if (nrow(acmfdata) > 0){
      fdata <- rbind(fdata, acmfdata)
    }else{
      acmfdata <- formatData(acmdata, infertotalpersons = F)
      if (nrow(acmfdata) > 0){
        fdata <- rbind(fdata, acmfdata)
      }
    }
    
  
    acmdata <- getDiseaseSpecificData(raw_sub_data, uoutcome$outcome[i], paexposure = "TPA", overall1 = 1)
    if (nrow(acmdata) > 0){
      acmfdata <- formatData(acmdata, kcases = T) 
      if (nrow(acmfdata) > 0){
        fdata <- rbind(fdata, acmfdata)
      }else{
        acmfdata <- formatData(acmdata, infertotalpersons = F)
        if (nrow(acmfdata) > 0){
          fdata <- rbind(fdata, acmfdata)
        }
      }
    }
    
    acmdata <- getDiseaseSpecificData(raw_sub_data, uoutcome$outcome[i], paexposure = "TPA", gender = 1)
    acmfdata <- formatData(acmdata, kcases = T) 
    if (nrow(acmfdata) > 0){
      fdata <- rbind(fdata, acmfdata)
    }else{
      acmfdata <- formatData(acmdata, infertotalpersons = F)
      if (nrow(acmfdata) > 0){
        fdata <- rbind(fdata, acmfdata)
      }
    }
  
    
    acmdata <- getDiseaseSpecificData(raw_sub_data, uoutcome$outcome[i], paexposure = "TPA", gender = 2)
    acmfdata <- formatData(acmdata, kcases = T) 
    if (nrow(acmfdata) > 0){
      fdata <- rbind(fdata, acmfdata)
    }else{
      acmfdata <- formatData(acmdata, infertotalpersons = F)
      if (nrow(acmfdata) > 0){
        fdata <- rbind(fdata, acmfdata)
      }
    }
    
    acmdata <- getDiseaseSpecificData(raw_sub_data, uoutcome$outcome[i], paexposure = "TPA", gender = 3)
    acmfdata <- formatData(acmdata, kcases = T) 
    if (nrow(acmfdata) > 0){
      fdata <- rbind(fdata, acmfdata)
    }else{
      acmfdata <- formatData(acmdata, infertotalpersons = F)
      if (nrow(acmfdata) > 0){
        fdata <- rbind(fdata, acmfdata)
      }
    }
    # # Remove all cases where both rr and dose are null
    # acmfdata <- subset(acmfdata, !is.na(effect) & !is.na(`Harmonised exposure (MMET-hrs/wk)`))
    # # Remove when totalperson is not available for hr, and personsyears for rr/or
    # acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
    #                                  (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    # metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i], " LTPA - Total Population"), covMethed = T)
  }
  
  fdata <- dplyr::filter(fdata,  !is.na(ref_number))
  fdata <- fdata[!duplicated(fdata), ]
  
  
}


## sort by ref_number
fdata <- fdata[order(fdata$ref_number),] 


for (i in 1:nrow(uoutcome)){
  i = 3
  # cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
  acmdata <- getDiseaseSpecificData(raw_data, uoutcome$outcome[i], paexposure = "LTPA", overall1 = 1)
  # cat(nrow(acmdata), "\n")
  # acmdata <- acmdata[!duplicated(acmdata$ref_number),]
  if (nrow(acmdata) > 0){
    acmfdata <- formatData(acmdata, kcases = T)  
    # Remove all cases where both rr and dose are null
    acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    #if (i %in% c(3))
    #  acmfdata[acmfdata$logrr == 0,]$se <- acmfdata[acmfdata$logrr == 0,]$lci <- acmfdata[acmfdata$logrr == 0,]$uci <- 0
    if (nrow(acmfdata) > 0){
      #cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
      metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i], " LTPA - Male Population"))
    }else{
      cat("LTPA - Male Population - Outcome: ", uoutcome$outcome[i], " and rows ", nrow(acmdata), "\n")
    }
  }else{
    cat("LTPA - Male Population - Outcome: ", uoutcome$outcome[i], " and rows ", nrow(acmdata), "\n")
  }
}

for (i in 1:nrow(uoutcome)){
  #cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
  acmdata <- getDiseaseSpecificData(raw_data, uoutcome$outcome[i], paexposure = "LTPA", gender = 2)
  # acmdata <- acmdata[!duplicated(acmdata$ref_number),]
  acmfdata <- formatData(acmdata, kcases = T)
  # Remove all cases where both rr and dose are null
  acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
  # Remove when totalperson is not available for hr, and personsyears for rr/or
  acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                   (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
  if (nrow(acmfdata) > 0)
    metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i], " LTPA - Female Population"), covMethed = T)
  else
    cat("LTPA - Female Population - Outcome: ", uoutcome$outcome[i], " and rows ", nrow(acmdata), "\n")
  
}

for (i in 1:nrow(uoutcome)){
  if (!i %in% c(5, 6)){
  cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
  acmdata <- getDiseaseSpecificData(raw_data, uoutcome$outcome[i], paexposure = "TPA", overall1 = 1)
  acmfdata <- formatData(acmdata, kcases = T)
  # Remove all cases where both rr and dose are null
  acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
  # Remove when totalperson is not available for hr, and personsyears for rr/or
  acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                   (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
  if (i %in% c(5, 6))
    acmfdata[acmfdata$logrr == 0,]$se <- acmfdata[acmfdata$logrr == 0,]$lci <- acmfdata[acmfdata$logrr == 0,]$uci <- 0
  
  if (nrow(acmfdata) > 0){
    #cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i], " TPA - Total Population"), covMethed = T)
  }
  else
    cat("TPA - Total Population - Outcome: ", uoutcome$outcome[i], " and rows ", nrow(acmdata), "\n")
  }
  
}


for (i in 1:nrow(uoutcome)){
  #cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
  # i = 7
  acmdata <- getDiseaseSpecificData(raw_data, uoutcome$outcome[i], paexposure = "TPA", gender = 1)
  if (nrow(acmdata) > 0){
    acmfdata <- formatData(acmdata, kcases = F)
    # Remove all cases where both rr and dose are null
    acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    if (nrow(acmfdata) > 0)
      metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i], " TPA - Male Population"), covMethed = T)
    else
      cat("TPA - Male Population - Outcome: ", uoutcome$outcome[i], " and rows ", nrow(acmdata), "\n")
  }else{
    cat("TPA - Male Population - Outcome: ", uoutcome$outcome[i], " and rows ", nrow(acmdata), "\n")
  }
}

for (i in 1:nrow(uoutcome)){
  #i = 10
  acmdata <- getDiseaseSpecificData(raw_data, uoutcome$outcome[i], paexposure = "TPA", gender = 2)
  if (nrow(acmdata) > 0){
    acmfdata <- formatData(acmdata, kcases = T)
    # Remove all cases where both rr and dose are null
    acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    if (i %in% c(5, 6))
      acmfdata[acmfdata$logrr == 0,]$se <- acmfdata[acmfdata$logrr == 0,]$lci <- acmfdata[acmfdata$logrr == 0,]$uci <- 0
    
    if (nrow(acmfdata) > 0){
      #cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
      metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i], " TPA - Female Population"), covMethed = T)
    }else
      cat("TPA - Female Population - Outcome: ", uoutcome$outcome[i], " and rows ", nrow(acmdata), "\n")
  }else
    cat("TPA - Female Population - Outcome: ", uoutcome$outcome[i], " and rows ", nrow(acmdata), "\n")
}