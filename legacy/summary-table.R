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

summary_table <- data.frame(exposure = character(),
                            outcome = character(),
                            overall = numeric(),
                            gender = numeric(),
                            sample_size = numeric(),
                            total_population = character(),
                            stringsAsFactors = FALSE)
index <- 1
for (i in 1:nrow(uoutcome)){
  
  paexpg = c("LTPA", "TPA")
  ov <- 1
  for(paexp in paexpg){
    gg <- c(0, 1, 2)
    for (g in gg){
      # cat(g, "\n")
      # g <- 1
      # cat("Unprocessed - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
      # if (is.null(g)){
      if (g == 0){
        acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = paexp, overall1 = 1)
        # cat("overall")
      }else{
        acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = paexp, gender = g)
      }
      #acmdata <- subset(acmdata, outcome_type == "mortality")
      acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = T)
      
      # Remove all cases where both rr and dose are null
      acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      if (uoutcome$outcome[i] == 'stroke' && paexp == "TPA" && g == 0){
        # Remove study # 70 from stroke
        acmfdata <- subset(acmfdata, !ref_number %in% c(70))
      }
      
      if(uoutcome$outcome[i] == 'CHD' && paexp == "TPA" && g == 0){
        # Remove study # 38 from stroke
        acmfdata <- subset(acmfdata, !ref_number %in% c(70))
      }
      
      if(uoutcome$outcome[i] == 'CHD' && paexp == "TPA" && g == 2){
        # Remove study # 38 from stroke
        acmfdata <- subset(acmfdata, !ref_number %in% c(38))
      }
      
      # cat("Studies ", unique(acmfdata$ref_number), "\n")
      
      if (i %in% c(5, 6))
        acmfdata[acmfdata$logrr == 0,]$se <- acmfdata[acmfdata$logrr == 0,]$lci <- acmfdata[acmfdata$logrr == 0,]$uci <- 0
      
      if (i == 5 && paexp == "TPA" && g == 2){
        plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T))
      }else{
        plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T, minQuantile = 0, maxQuantile = 0.85))
      }
      
      summary_table[index, 1] <- paexp
      summary_table[index, 2] <- uoutcome$outcome[i]
      summary_table[index, 3] <- ifelse(g == 0, 1, 0)
      summary_table[index, 4] <- ifelse(g == 0, 0, g)
      
      if (nrow(plot_data) > 0){
        # cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
        colnames(plot_data) <- c("dose","RR", "lb", "ub")
        
        summary_table[index, 5] <- length(unique(acmfdata$id))
        summary_table[index, 6] <- formatC(round(sum(acmfdata$totalpersons, na.rm = T)), 
                                           format = "f", big.mark = ",", drop0trailing = TRUE)
        
      }else{
        # cat("(NOT) Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
        
        summary_table[index, 5] <- 0
        summary_table[index, 6] <- 0
      }
      index <- index + 1
    }
  }
}
