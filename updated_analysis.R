#rm (list = ls())
# Read the data
raw_data <- read.csv("//me-filer1/home$/aa797/Downloads/20170621_MASTER_PA_Dose_Metananalysis_Data_Extraction.csv", header = T, stringsAsFactors = F, skipNul = TRUE)
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

raw_data$totalpersons <- as.numeric(raw_data$totalpersons)

raw_data[raw_data$effect_measure == "RR",]$effect_measure <- "rr"
raw_data[raw_data$effect_measure == "HR",]$effect_measure <- "hr"

raw_data$type <- ""

raw_data[raw_data$effect_measure == "or",]$type <- "ir"
raw_data[raw_data$effect_measure == "rr",]$type <- "ir"
raw_data[raw_data$effect_measure == "hr",]$type <- "ci"
raw_data$type <- as.character(raw_data$type)

fdata <- raw_data

#fdata <- NULL
fdata <- fdata[c(), ]

for (i in 1:nrow(uoutcome)){
  #   i = 3
  # cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
  acmdata <- subset(raw_data, outcome == "total cancer" & pa_domain_subgroup == "LTPA" & overall == 1)
  acmfdata <- acmdata
  acmfdata$rr <- acmfdata$Final.Harmonised.exposure..MMET.hrs.wk.
  acmfdata$Final.Harmonised.exposure..MMET.hrs.wk. <- NULL
  
  acmfdata$rr <- acmfdata$effect
  acmfdata$id <- acmfdata$ref_number
  
  # cat(nrow(acmdata), "\n")
  # acmdata <- acmdata[!duplicated(acmdata$ref_number),]
  if (nrow(acmdata) > 0){
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    #if (i %in% c(3))
    #  acmfdata[acmfdata$logrr == 0,]$se <- acmfdata[acmfdata$logrr == 0,]$lci <- acmfdata[acmfdata$logrr == 0,]$uci <- 0
    if (nrow(acmfdata) > 0){
      #cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
      metaAnalysis(acmfdata, ptitle = paste( "total cancer", " LTPA - Total Population"))
    }else{
      cat("LTPA - Male Population - Outcome: ", uoutcome$outcome[i], " and rows ", nrow(acmdata), "\n")
    }
  }else{
    cat("LTPA - Male Population - Outcome: ", uoutcome$outcome[i], " and rows ", nrow(acmdata), "\n")
  }
}