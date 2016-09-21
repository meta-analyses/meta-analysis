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

# all/male population - stroke remove 70
# CHD remove 38

i = 5
cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "TPA", gender = 2)
#acmdata <- subset(acmdata, outcome_type == "mortality")
acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = T)
acmfdata[acmfdata$logrr == 0,]$se <- acmfdata[acmfdata$logrr == 0,]$lci <- acmfdata[acmfdata$logrr == 0,]$uci <- 0

plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T, minQuantile = 0, maxQuantile = 0.85, lout = 1000))
colnames(plot_data) <- c("dose","RR", "lb", "ub")

removeNA <- F

# Update RR from the lookup table
for (i in 1:nrow(acmfdata)){
  val <- subset(plot_data, round(dose, 1) <= (acmfdata$dose[i] + 0.05) & round(dose, 1) >= (acmfdata$dose[i] - 0.05))
  if (nrow(val) > 0){
    acmfdata$rr[i] <- val$RR[1]
    if (removeNA){
      if (!is.na(acmfdata$lci[i]))
        acmfdata$lci[i] <- val$lb[1]
      if (!is.na(acmfdata$lci[i]))
        acmfdata$uci[i] <- val$ub[1]
    }else{
      acmfdata$lci[i] <- val$lb[1]
      acmfdata$uci[i] <- val$ub[1]
    }
  }
}

(sum_tp <- sum(acmfdata$totalpersons * acmfdata$rr, na.rm = T) )

acmfdata_ls <- acmfdata

#Replace lower dose with 8.75
acmfdata_ls[acmfdata_ls$dose < 8.75,]$dose <- 8.75

local_var <- acmfdata_ls

val <- subset(plot_data, round(dose, 1) <= (8.75 + 0.05) & round(dose, 1) >= (8.75 - 0.05))

if (nrow(val) > 0)
  acmfdata_ls[acmfdata_ls$dose == 8.75,]$rr <- val$RR[1]

sum_ls_tp <- sum(acmfdata$totalpersons * acmfdata_ls$rr, na.rm = T)

(pert_ls <- ((sum_tp - sum_ls_tp) / sum_tp) * 100)

acmfdata_ls <- local_var

if (nrow(val) > 0){
  
  if (removeNA){
    acmfdata_ls[acmfdata_ls$dose == 8.75 & !is.na(acmfdata_ls$uci),]$uci <- val$ub[1]
  }else{
    acmfdata_ls[acmfdata_ls$dose == 8.75,]$uci <- val$ub[1]
  }
  
}

sum_ls_lower_tp <- sum(acmfdata$totalpersons * acmfdata_ls$uci, na.rm = T)

sum_tp <- sum(acmfdata$totalpersons * acmfdata$uci, na.rm = T)

(pert_ls_lower <- ((sum_tp - sum_ls_lower_tp) / sum_tp) * 100)

acmfdata_ls <- local_var

if (nrow(val) > 0){
  if (removeNA){
    acmfdata_ls[acmfdata_ls$dose == 8.75 & !is.na(acmfdata_ls$lci),]$lci <- val$lb[1]
  }else{
    acmfdata_ls[acmfdata_ls$dose == 8.75,]$lci <- val$lb[1]
  }
}

sum_ls_upper_tp <- sum(acmfdata$totalpersons * acmfdata_ls$lci, na.rm = T)

sum_tp <- sum(acmfdata$totalpersons * acmfdata$lci, na.rm = T)

pert_ls_upper <- ((sum_tp - sum_ls_upper_tp) / sum_tp) * 100

lower_guideline_value <<- paste0(round(pert_ls, 2) , "% (95% CI: ", round(pert_ls_lower, 2), " - ",  round(pert_ls_upper, 2), ")" )


# -------------------------------- higher confidence --------------------------------- #

acmfdata_hs <- acmfdata

#Replace higher dose with 17.5
acmfdata_hs[acmfdata_hs$dose < 17.5,]$dose <- 17.5

local_var <- acmfdata_hs

val <- subset(plot_data, round(dose, 1) <= (17.5 + 0.05) & round(dose, 1) >= (17.5 - 0.05))
if (nrow(val) == 0)
  val <- subset(plot_data, round(dose, 1) <= (17.5 + 0.1) & round(dose, 1) >= (17.5 - 0.1))

if (nrow(val) > 0)
  acmfdata_hs[acmfdata_hs$dose == 17.5,]$rr <- val$RR[1]

(sum_hs_tp <- sum(acmfdata$totalpersons * acmfdata_hs$rr, na.rm = T))

(sum_tp <- sum(acmfdata$totalpersons * acmfdata$rr, na.rm = T) )

(pert_hs <- ((sum_tp - sum_hs_tp) / sum_tp) * 100)

acmfdata_hs <- local_var

if (nrow(val) > 0){
  if (removeNA){
    acmfdata_hs[acmfdata_hs$dose == 17.5 & !is.na(acmfdata_ls$uci),]$uci <- val$ub[1]
  }else{
    acmfdata_hs[acmfdata_hs$dose == 17.5,]$uci <- val$ub[1]
  }
}

(sum_hs_lower_tp <- sum(acmfdata$totalpersons * acmfdata_hs$uci, na.rm = T))

(sum_tp <- sum(acmfdata$totalpersons * acmfdata$uci, na.rm = T))

(pert_hs_lower <- ((sum_tp - sum_hs_lower_tp) / sum_tp) * 100)

acmfdata_hs <- local_var

if (nrow(val) > 0){
  if (removeNA){
    acmfdata_hs[acmfdata_hs$dose == 17.5 & !is.na(acmfdata_ls$uci),]$lci <- val$lb[1]
  }else{
    acmfdata_hs[acmfdata_hs$dose == 17.5,]$lci <- val$lb[1]
  }
}

(sum_hs_upper_tp <- sum(acmfdata$totalpersons * acmfdata_hs$lci, na.rm = T))

(sum_tp <- sum(acmfdata$totalpersons * acmfdata$lci, na.rm = T))

(pert_hs_upper <- ((sum_tp - sum_hs_upper_tp) / sum_tp) * 100)

upper_guideline_value <<- paste0(round(pert_hs, 2) , "% (95% CI: ", round(pert_hs_lower, 2), " - ",  round(pert_hs_upper, 2), ")" )

cat(lower_guideline_value, " - ", upper_guideline_value, "\n")
