rm (list = ls())
# Read the data
data <- read.csv("data/09.06_COMBINED DATASET REDUCED.csv", header = T, stringsAsFactors = F, skipNul = TRUE)

data$tot_personyrs <- as.numeric(data$tot_personyrs)
data[(is.na(data$tot_personyrs)),]$tot_personyrs <- 
  data[(is.na(data$tot_personyrs)),]$mean_followup * data[(is.na(data$tot_personyrs)),]$n_baseline
data[(is.na(data$mean_followup)),]$mean_followup <- 
  data[(is.na(data$mean_followup)),]$tot_personyrs / data[(is.na(data$mean_followup)),]$n_baseline

data$outcome <- trimws(data$outcome)

# Replace 'Heart failure' with 'heart failure'
data[data$outcome == 'Heart failure',]$outcome <- 'heart failure'

# Replace "Alzheimer's Disease" with "Alzheimer's disease"
data[data$outcome == "Alzheimer's Disease",]$outcome <- "Alzheimer's disease"

# Replace "CVD" with "Cardiovascular Disease"
data[data$outcome == "CVD",]$outcome <- "Cardiovascular Disease"

# Replace "CHD" with "Coronary Heart Disease"
data[data$outcome == "CHD",]$outcome <- "Coronary Heart Disease"

# Read all the functions
source("all-functions.R")

# Identify unique outcomes
uoutcome <- data.frame(outcome = as.character(unique(data$outcome)))
uoutcome$outcome <- as.character(uoutcome$outcome)

df <- NULL
for (i in 1:nrow(uoutcome)){
  # i = 1
  cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
  acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", overall1 = 1, out_type = "all")
  #acmdata <- subset(acmdata, outcome_type == "mortality")
  acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = T)
  # Remove all cases where both rr and dose are null
  acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
  # Remove when totalperson is not available for hr, and personsyears for rr/or
  acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                   (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
  plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T, minQuantile = 0, maxQuantile = 0.75 , lout = 500))
  colnames(plot_data) <- c("dose","RR", "lb", "ub")
  temp_df <- data.frame(dose=plot_data$dose, RR=plot_data$RR, outcome = stringi::stri_trans_totitle(uoutcome$outcome[i]), col = i) #col=col_pal[i])
  df <- rbind(df,temp_df)
}

ggplot(df,aes(x=dose,y=RR,group=col,colour = outcome)) + geom_line() + labs(title="Overall Population - Outcome Type (All)")

df <- NULL
for (i in 1:nrow(uoutcome)){
  # i = 1
  #cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
  acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", overall1 = 1, out_type = "mortality")
  #acmdata <- subset(acmdata, outcome_type == "mortality")
  acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = T)
  # Remove all cases where both rr and dose are null
  acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
  # Remove when totalperson is not available for hr, and personsyears for rr/or
  acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                   (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
  plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T, minQuantile = 0, maxQuantile = 0.75 , lout = 500))
  if (nrow(plot_data) > 0){
    colnames(plot_data) <- c("dose","RR", "lb", "ub")
    temp_df <- data.frame(dose=plot_data$dose, RR=plot_data$RR, outcome = stringi::stri_trans_totitle(uoutcome$outcome[i]), col = i) #col=col_pal[i])
    df <- rbind(df,temp_df)
  }else{
    cat("Data Not Found: Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
  }
}
ggplot(df,aes(x=dose,y=RR,group=col,colour = outcome)) + geom_line() + labs(title="Overall Population - Outcome Type (Mortality)")


df <- NULL
for (i in 1:nrow(uoutcome)){
  # i = 1
  #cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
  acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", overall1 = 1, out_type = "incidence")
  #acmdata <- subset(acmdata, outcome_type == "mortality")
  acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = T)
  # Remove all cases where both rr and dose are null
  acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
  # Remove when totalperson is not available for hr, and personsyears for rr/or
  acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                   (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
  plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T, minQuantile = 0, maxQuantile = 0.75 , lout = 500))
  if (nrow(plot_data) > 0){
    colnames(plot_data) <- c("dose","RR", "lb", "ub")
    temp_df <- data.frame(dose=plot_data$dose, RR=plot_data$RR, outcome = stringi::stri_trans_totitle(uoutcome$outcome[i]), col = i) #col=col_pal[i])
    df <- rbind(df,temp_df)
  }else{
    cat("Data Not Found: Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
  }
}
ggplot(df,aes(x=dose,y=RR,group=col,colour = outcome)) + geom_line() + labs(title="Overall Population - Outcome Type (Incidence)")
