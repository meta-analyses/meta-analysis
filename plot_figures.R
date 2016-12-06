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

# col_pal <- brewer.pal(nrow(uoutcome), "Set1")#topo.colors(nrow(uoutcome))
df <- NULL
for (i in 1:nrow(uoutcome)){
  # i = 1
  cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
  acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", overall1 = 1)
  #acmdata <- subset(acmdata, outcome_type == "mortality")
  acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = T)
  # Remove all cases where both rr and dose are null
  acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
  # Remove when totalperson is not available for hr, and personsyears for rr/or
  acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                   (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
  #metaAnalysis(acmfdata, ptitle = paste( uoutcome$outcome[i], " LTPA - Total Population"), covMethed = T)
  
  plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T, minQuantile = 0, maxQuantile = 0.75 , lout = 500))
  colnames(plot_data) <- c("dose","RR", "lb", "ub")
  
  
  temp_df <- data.frame(dose=plot_data$dose, RR=plot_data$RR, col = i) #col=col_pal[i])
  df <- rbind(df,temp_df)
  
  # if (i == 1)
  #   plot(plot_data$dose, plot_data$RR, ylim = c(0,1), xlab = "Dose", ylab = "Relative Risk", col = col_pal[i], cex = 0.05)
  # else
  #   points(plot_data$dose, plot_data$RR, col = col_pal[i], cex = 0.05)
}

ggplot(df,aes(x=dose,y=RR,group=col,colour=factor(col))) + geom_line()

