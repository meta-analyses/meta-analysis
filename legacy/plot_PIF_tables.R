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
# Remove mental outcomes
uoutcome <- data.frame(outcome = uoutcome[-c(4, 8, 12, 13),])

outcome_type <- c("all", 
                  "mortality",
                  "incidence")

df <- NULL
for (j in 1:length(outcome_type)){
  for (i in 1:nrow(uoutcome)){
    # i = 1
    # cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", overall1 = 1, out_type = outcome_type[j])
    #acmdata <- subset(acmdata, outcome_type == "mortality")
    acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = T)
    # Remove all cases where both rr and dose are null
    acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T, minQuantile = 0, maxQuantile = 0.75 , lout = 1000))
    
    if (nrow(plot_data) > 0){
      colnames(plot_data) <- c("dose","RR", "lb", "ub")
      
      if (nrow(acmfdata) > 0){
        
        m <- getPIF(acmfdata, plot_data)
        
        if (nrow(m) > 0){
          
          temp_df <- data.frame(outcome = m[1,1], outcome_type = m[1,2], lower_guideline = m[1,3],
                                lower_CFI = paste("(",m[1,4], ",", m[1,5], ")"), higher_guideline = m[1,6],
                                higher_CFI = paste("(",m[1,7], ",", m[1,8], ")"))
          
          df <- rbind(df,temp_df)
        }
      }
      
    }else{
      cat("Data Not Found: Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    }
  }
}

write.csv(df, file = "total-population-PIFs.csv", row.names = F)


df <- NULL
for (j in 1:length(outcome_type)){
  for (i in 1:nrow(uoutcome)){
    # i = 1
    # cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", gender = 1, out_type = outcome_type[j])
    #acmdata <- subset(acmdata, outcome_type == "mortality")
    acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = T)
    # Remove all cases where both rr and dose are null
    acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T, minQuantile = 0, maxQuantile = 0.75 , lout = 1000))
    
    if (nrow(plot_data) > 0){
      colnames(plot_data) <- c("dose","RR", "lb", "ub")
      
      if (nrow(acmfdata) > 0){
        
        m <- getPIF(acmfdata, plot_data)
        
        if (nrow(m) > 0){
          
          temp_df <- data.frame(outcome = m[1,1], outcome_type = m[1,2], lower_guideline = m[1,3],
                                lower_CFI = paste("(", m[1,4], ",", m[1,5], ")"), higher_guideline = m[1,6],
                                higher_CFI = paste("(",m[1,7], ",", m[1,8], ")"))
          
          df <- rbind(df,temp_df)
        }
      }
      
    }else{
      cat("Data Not Found: Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    }
  }
}

write.csv(df, file = "male-population-PIFs.csv", row.names = F)

df <- NULL
for (j in 1:length(outcome_type)){
  for (i in 1:nrow(uoutcome)){
    # i = 1
    # cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", gender = 2, out_type = outcome_type[j])
    #acmdata <- subset(acmdata, outcome_type == "mortality")
    acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = T)
    # Remove all cases where both rr and dose are null
    acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T, minQuantile = 0, maxQuantile = 0.75 , lout = 1000))
    
    if (nrow(plot_data) > 0){
      colnames(plot_data) <- c("dose","RR", "lb", "ub")
      
      if (nrow(acmfdata) > 0){
        
        m <- getPIF(acmfdata, plot_data)
        
        if (nrow(m) > 0){
          
          temp_df <- data.frame(outcome = m[1,1], outcome_type = m[1,2], lower_guideline = m[1,3],
                                lower_CFI = paste("(",m[1,4], ",", m[1,5], ")"), higher_guideline = m[1,6],
                                higher_CFI = paste("(",m[1,7], ",", m[1,8], ")"))
          
          df <- rbind(df,temp_df)
        }
      }
      
    }else{
      cat("Data Not Found: Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    }
  }
}

write.csv(df, file = "female-population-PIFs.csv", row.names = F)

