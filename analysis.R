rm (list = ls())
# Read the data
data <- read.csv("data/09.06_COMBINED DATASET REDUCED.csv", header = T, stringsAsFactors = F, skipNul = TRUE)

data$tot_personyrs <- as.numeric(data$tot_personyrs)
data[(is.na(data$tot_personyrs)),]$tot_personyrs <- 
  data[(is.na(data$tot_personyrs)),]$mean_followup * data[(is.na(data$tot_personyrs)),]$n_baseline
data[(is.na(data$mean_followup)),]$mean_followup <- 
  data[(is.na(data$mean_followup)),]$tot_personyrs / data[(is.na(data$mean_followup)),]$n_baseline

data$outcome <- trimws(data$outcome)

# Read all the functions
source("all-functions.R")

# Identify unique outcomes
uoutcome <- data.frame(outcome = as.character(unique(data$outcome)))
uoutcome$outcome <- as.character(uoutcome$outcome)

for (i in 1:nrow(uoutcome)){
  i = 1
  cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
  acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", overall1 = 1)
  #acmdata <- subset(acmdata, outcome_type == "mortality")
  acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = T)
  # Remove all cases where both rr and dose are null
  acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
  # Remove when totalperson is not available for hr, and personsyears for rr/or
  acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                   (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
  metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i], " LTPA - Total Population"), covMethed = T)
}

for (i in 1:nrow(uoutcome)){
#   i = 3
  # cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
  acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", gender = 1)
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
  acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", gender = 2)
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
  acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "TPA", overall1 = 1)
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
  acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "TPA", gender = 1)
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
  acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "TPA", gender = 2)
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