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


i = 1
cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", overall1 = 1)
#acmdata <- subset(acmdata, outcome_type == "mortality")
acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = T)

plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T, minQuantile = 0, maxQuantile = 0.75, lout = 1000))
colnames(plot_data) <- c("dose","RR", "lb", "ub")

removeNA <- F

# Update RR from the lookup table

for (i in 1:nrow(acmfdata)){
  # local_dose <- acmfdata$dose[i]
  # local_rr <- acmfdata$rr[i]
  # local_lci <- acmfdata$lci[i]
  # local_uci <- acmfdata$uci[i]
  
  val <- subset(plot_data, round(dose, 1) <= (acmfdata$dose[i] + 0.05) & round(dose, 1) >= (acmfdata$dose[i] - 0.05))
  cat(acmfdata$dose[i], " nrow ", nrow(val), "\n")
  #cat(str(val))
  if (nrow(val) > 0){
    # cat(acmfdata$rr[i], " with ", val$RR[1], "\n" ,
    #     acmfdata$lci[i], " with ", val$lb[1], "\n" ,
    #     acmfdata$uci[i], " with ", val$ub[1], "\n" 
    #     
    #     )
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

sum_tp <- sum(acmfdata$totalpersons * acmfdata$rr, na.rm = T) 

acmfdata_ls <- acmfdata

#Replace lower dose with 8.75
acmfdata_ls[acmfdata_ls$dose < 8.75,]$dose <- 8.75

local_var <- acmfdata_ls

val <- subset(plot_data, round(dose, 1) <= (8.75 + 0.05) & round(dose, 1) >= (8.75 - 0.05))
cat(str(val))
if (nrow(val) > 0)
  acmfdata_ls[acmfdata_ls$dose == 8.75,]$rr <- val$RR[1]

sum_ls_tp <- sum(acmfdata$totalpersons * acmfdata_ls$rr, na.rm = T)

(pert_ls <- ((sum_tp - sum_ls_tp) / sum_tp) * 100)

acmfdata_ls <- local_var

if (nrow(val) > 0){
  
  if (removeNA){
    cat(str(acmfdata_ls[acmfdata_ls$dose == 8.75 & !is.na(acmfdata_ls$uci),]$uci))
    cat(val$ub[1], "\n")
    acmfdata_ls[acmfdata_ls$dose == 8.75 & !is.na(acmfdata_ls$uci),]$uci <- val$ub[1]
  }else{
    cat(str(acmfdata_ls[acmfdata_ls$dose == 8.75,]$uci))
    cat(val$ub[1], "\n")
    acmfdata_ls[acmfdata_ls$dose == 8.75,]$uci <- val$ub[1]
  }
  
}

(sum_ls_lower_tp <- sum(acmfdata$totalpersons * acmfdata_ls$uci, na.rm = T))

(sum_tp <- sum(acmfdata$totalpersons * acmfdata$uci, na.rm = T)) 

(pert_ls_lower <- ((sum_tp - sum_ls_lower_tp) / sum_tp) * 100)

acmfdata_ls <- local_var

if (nrow(val) > 0){
  if (removeNA){
    cat(str(acmfdata_ls[acmfdata_ls$dose == 8.75 & !is.na(acmfdata_ls$lci),]$lci))
    cat(val$lb[1], "\n")
    acmfdata_ls[acmfdata_ls$dose == 8.75 & !is.na(acmfdata_ls$uci),]$lci <- val$lb[1]
  }else{
    cat(str(acmfdata_ls[acmfdata_ls$dose == 8.75,]$lci))
    cat(val$lb[1], "\n")
    acmfdata_ls[acmfdata_ls$dose == 8.75,]$lci <- val$lb[1]
    
  }
}

(sum(acmfdata_ls$lci, na.rm = T))

(sum(acmfdata$lci, na.rm = T))

(sum_ls_upper_tp <- sum(acmfdata$totalpersons * acmfdata_ls$lci, na.rm = T))

(sum_tp <- sum(acmfdata$totalpersons * acmfdata$lci, na.rm = T))

(pert_ls_upper <- ((sum_tp - sum_ls_upper_tp) / sum_tp) * 100)

cat(pert_ls, " ", pert_ls_lower,  " ", pert_ls_upper ,  "\n")

(lower_guideline_value <<- paste(round(pert_ls, 2) , "(", round(pert_ls_lower, 2), " - ",  round(pert_ls_upper, 2), ")" ))

acmfdata_hs <- acmfdata

#Replace lower dose with 8.75
acmfdata_hs[acmfdata_hs$dose < 17.5,]$dose <- 17.5

local_var <- acmfdata_hs

val <- subset(plot_data, round(dose, 1) <= (17.5 + 0.05) & round(dose, 1) >= (17.5 - 0.05))

cat(nrow(val))

if (nrow(val) > 0){
  cat(str(acmfdata_ls[acmfdata_ls$dose == 17.5,]$rr))
  cat(val$RR[1], "\n")
  acmfdata_hs[acmfdata_hs$dose == 17.5,]$rr <- val$RR[1]
}

sum_hs_tp <- sum(acmfdata$totalpersons * acmfdata_hs$rr, na.rm = T)

sum_tp <- sum(acmfdata$totalpersons * acmfdata$rr, na.rm = T) 

(pert_hs <- ((sum_tp - sum_hs_tp) / sum_tp) * 100)

acmfdata_hs <- local_var

if (nrow(val) > 0){
  if (removeNA){
    cat(str(acmfdata_hs[acmfdata_hs$dose == 17.5 & !is.na(acmfdata_ls$uci),]$uci))
    cat(val$ub[1], "\n")
    acmfdata_hs[acmfdata_hs$dose == 17.5 & !is.na(acmfdata_ls$uci),]$uci <- val$ub[1]
  }else{
    cat(str(acmfdata_hs[acmfdata_hs$dose == 17.5,]$uci))
    cat(val$ub[1], "\n")
    acmfdata_hs[acmfdata_hs$dose == 17.5,]$uci <- val$ub[1]
  }
}

(sum_hs_lower_tp <- sum(acmfdata$totalpersons * acmfdata_hs$uci, na.rm = T))

(sum_tp <- sum(acmfdata$totalpersons * acmfdata$uci, na.rm = T)) 

(pert_hs_lower <- ((sum_tp - sum_hs_lower_tp) / sum_tp) * 100)

acmfdata_hs <- local_var

if (nrow(val) > 0){
  if (removeNA){
    cat(str(acmfdata_hs[acmfdata_hs$dose == 17.5 & !is.na(acmfdata_ls$lci),]$lci))
    cat(val$lb[1], "\n")
    acmfdata_hs[acmfdata_hs$dose == 17.5 & !is.na(acmfdata_ls$uci),]$lci <- val$lb[1]
  }else{
    cat(str(acmfdata_hs[acmfdata_hs$dose == 17.5,]$lci))
    cat(val$lb[1], "\n")
    acmfdata_hs[acmfdata_hs$dose == 17.5,]$lci <- val$lb[1]
  }
}

(sum_hs_upper_tp <- sum(acmfdata$totalpersons * acmfdata_hs$lci, na.rm = T))

(sum_tp <- sum(acmfdata$totalpersons * acmfdata$lci, na.rm = T))

(pert_hs_upper <- ((sum_tp - sum_hs_upper_tp) / sum_tp) * 100)

cat(pert_hs, " ", pert_hs_lower,  " ", pert_hs_upper ,  "\n")
