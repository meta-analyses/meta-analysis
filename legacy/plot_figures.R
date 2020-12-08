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

backup_outcome <- uoutcome

# outcome_type <- c("all", 
#                   "mortality",
#                   "incidence")

outcome_type <- c("all")

for (j in 1:length(outcome_type)){
  df <- NULL
  for (i in 1:nrow(uoutcome)){
    i = 8
    # j = 1
    #     cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", overall1 = 1, out_type = outcome_type[j])
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
      df <- data.frame(dose=plot_data$dose, RR=plot_data$RR, lb = plot_data$lb, ub = plot_data$ub, outcome = stringi::stri_trans_totitle(uoutcome$outcome[i]), col = i) #col=col_pal[i])
      #df <- rbind(df,temp_df)
      
      grph <- ggplot(df,aes(x=dose,y=RR,group=col,colour = outcome)) + geom_line(size = 1) + 
        geom_ribbon(data = df, aes(ymin=lb,ymax=ub),alpha=.3, colour=NA) +
        xlab("Relative Risk") +
        ylab("Marginal MET hours per week") +
        xlim(0, 80) +
        ylim(0, 1) +
        labs(title=paste0("Overall Population - ", df$outcome, " - ", "Outcome Type (", stringi::stri_trans_totitle(outcome_type[j]), ") \n"
                          
                          , "Number of samples: ",  length(unique(acmfdata$id)) , 
                          " & Number of people: " , formatC(round(sum(acmfdata$totalpersons, na.rm = T)), 
                                                            format = "f", big.mark = ",", drop0trailing = TRUE)))
      
      
      ggsave(grph, file=paste0("total-population-",as.character(df$outcome[1]), "-", outcome_type[j], ".png"), dpi = 300)
      #b)	10 diseases * 2  genders (for all but endometrial and breast cancer) * LTPA  * (combined incidence + mortality) =  This would be 18 figures but I suggest we show the two genders on the same chart= 10 figures
      #c)	10 diseases * LTPA  * outcome type (incidence or mortality)= this would be 20 figures but I suggest we show the incidence and mortality on the same chart= 10 figures.
      
      
      
    }else{
      cat("Data Not Found: Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    }
    
  }
  
  # grph <- ggplot(df,aes(x=dose,y=RR,group=col,colour = outcome)) + geom_line() + 
  #   labs(title=paste0("Overall Population - Outcome Type (", stringi::stri_trans_totitle(outcome_type[j]), ")"))
  # 
  # ggsave(grph, file=paste0("total-population-",outcome_type[j], ".png"), dpi = 300)
}

## FOR BOTH GENDERS

outcome_type <- c("all")

for (j in 1:length(outcome_type)){
  df <- NULL
  for (i in c(7, 8)){#1:nrow(uoutcome)){
    acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", gender = 2, out_type = outcome_type[j])
    acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = T)
    acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T, minQuantile = 0, maxQuantile = 0.75 , lout = 500))
    if (nrow(plot_data) > 0){
      colnames(plot_data) <- c("dose","RR", "lb", "ub")
      
      # df1 <- data.frame(gender = 1, dose=plot_data$dose, RR=plot_data$RR, lb = plot_data$lb, ub = plot_data$ub, outcome = stringi::stri_trans_totitle(uoutcome$outcome[i]), col = i) #col=col_pal[i])
      # 
      # acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", gender = 2, out_type = outcome_type[j])
      # acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = T)
      # acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
      # acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
      #                                  (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      # plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T, minQuantile = 0, maxQuantile = 0.75 , lout = 500))
      # 
      # 
      # if (nrow(plot_data) > 0){
      #   
      #   colnames(plot_data) <- c("dose","RR", "lb", "ub")
        
        # df2 <- data.frame(gender = 2, dose=plot_data$dose, RR=plot_data$RR, lb = plot_data$lb, ub = plot_data$ub, outcome = stringi::stri_trans_totitle(uoutcome$outcome[i]), col = i) #col=col_pal[i])      colnames(plot_data) <- c("dose","RR", "lb", "ub")
        # df <- rbind(df1,df2)
        
        df <- df1
        
        
        grph <- ggplot(df,aes(x=dose,y=RR,group=gender,colour = factor(gender))) + geom_line(size = 1) + 
          xlab("Relative Risk") +
          ylab("Marginal MET hours per week") +
          xlim(0, 80) +
          ylim(0, 1) +
          scale_color_manual(name="Gender", labels = c("Males", "Females"), values = c("blue", "red")) +
          labs(title=paste0("Both Gender - ", df$outcome, " - ", "Outcome Type (", stringi::stri_trans_totitle(outcome_type[j]), ")"))
        #print(grph)
        ggsave(grph, file=paste0("female-",uoutcome$outcome[i], "-", outcome_type[j], ".png"), dpi = 300)
      # }
    }else{
      cat("Data Not Found: Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    }
  }
  # grph <- ggplot(df,aes(x=dose,y=RR,group=col,colour = outcome)) + geom_line() + 
  #   labs(title=paste0("Overall Population - Outcome Type (", stringi::stri_trans_totitle(outcome_type[j]), ")"))
  # 
  # ggsave(grph, file=paste0("total-population-",outcome_type[j], ".png"), dpi = 300)
}


outcome_type <- c("all")

for (j in 1:length(outcome_type)){
  df <- NULL
  for (i in 1:nrow(uoutcome)){
    i = 8
    # j = 1
    #     cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", overall1 = 1, out_type = outcome_type[j])
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
      df <- data.frame(dose=plot_data$dose, RR=plot_data$RR, lb = plot_data$lb, ub = plot_data$ub, outcome = stringi::stri_trans_totitle(uoutcome$outcome[i]), col = i) #col=col_pal[i])
      #df <- rbind(df,temp_df)
      
      grph <- ggplot(df,aes(x=dose,y=RR,group=col,colour = outcome)) + geom_line(size = 1) + 
        geom_ribbon(data = df, aes(ymin=lb,ymax=ub),alpha=.3, colour=NA) +
        xlab("Relative Risk") +
        ylab("Marginal MET hours per week") +
        xlim(0, 80) +
        ylim(0, 1) +
        labs(title=paste0("Overall Population - ", df$outcome, " - ", "Outcome Type (", stringi::stri_trans_totitle(outcome_type[j]), ") \n"
                          
                          , "Number of samples: ",  length(unique(acmfdata$id)) , 
                          " & Number of people: " , formatC(round(sum(acmfdata$totalpersons, na.rm = T)), 
                                                            format = "f", big.mark = ",", drop0trailing = TRUE)))
      
      
      ggsave(grph, file=paste0("total-population-",as.character(df$outcome[1]), "-", outcome_type[j], ".png"), dpi = 300)
      #b)	10 diseases * 2  genders (for all but endometrial and breast cancer) * LTPA  * (combined incidence + mortality) =  This would be 18 figures but I suggest we show the two genders on the same chart= 10 figures
      #c)	10 diseases * LTPA  * outcome type (incidence or mortality)= this would be 20 figures but I suggest we show the incidence and mortality on the same chart= 10 figures.
      
      
      
    }else{
      cat("Data Not Found: Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    }
    
  }
  
  # grph <- ggplot(df,aes(x=dose,y=RR,group=col,colour = outcome)) + geom_line() + 
  #   labs(title=paste0("Overall Population - Outcome Type (", stringi::stri_trans_totitle(outcome_type[j]), ")"))
  # 
  # ggsave(grph, file=paste0("total-population-",outcome_type[j], ".png"), dpi = 300)
}



## --

## FOR ALL OUTCOMES EXCEPT CANCERS
uoutcome <- data.frame(outcome = uoutcome[-c(3, 7, 8, 9, 10),])

outcome_type <- c("all")

for (j in 1:length(outcome_type)){
  df <- NULL
  for (i in 1:nrow(uoutcome)){
    # i = 1
    # j = 1
    #     cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", overall1 = 1, out_type = outcome_type[j])
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
      temp_df <- data.frame(dose=plot_data$dose, RR=plot_data$RR, lb = plot_data$lb, ub = plot_data$ub, outcome = stringi::stri_trans_totitle(uoutcome$outcome[i]), col = i) #col=col_pal[i])
      df <- rbind(df,temp_df)
      # print(grph)    
      #a)	10 diseases LTPA * (combined incidence + mortality) * total population =10 figures
      #b)	10 diseases * 2  genders (for all but endometrial and breast cancer) * LTPA  * (combined incidence + mortality) =  This would be 18 figures but I suggest we show the two genders on the same chart= 10 figures
      #c)	10 diseases * LTPA  * outcome type (incidence or mortality)= this would be 20 figures but I suggest we show the incidence and mortality on the same chart= 10 figures.
      
    }else{
      cat("Data Not Found: Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    }
    
  }
  
  grph <- ggplot(df,aes(x=dose,y=RR,group=col,colour = outcome)) + geom_line(size = 1) + 
    #geom_point(aes(shape = factor(col)), show.legend = F, size = 1) +
    #geom_ribbon(data = df, aes(ymin=lb,ymax=ub),alpha=.07, colour=NA) +
    labs(title=paste0("Overall Population - Outcome Type (", stringi::stri_trans_totitle(outcome_type[j]), ")")) +
    ylim(0, 1) +
    xlim(0, 80) +
    ylab("Relative Risk") +
    xlab("Marginal MET hours per week")
  print(grph)
  
  ggsave(grph, file=paste0("total-population-all-but-cancers.png"), dpi = 300)
  
  # grph <- ggplot(df,aes(x=dose,y=RR,group=col,colour = outcome)) + geom_line() + 
  #   labs(title=paste0("Overall Population - Outcome Type (", stringi::stri_trans_totitle(outcome_type[j]), ")"))
  # 
  # ggsave(grph, file=paste0("total-population-",outcome_type[j], ".png"), dpi = 300)
}


# NOW FOR THE CANCERS

## FOR ALL OUTCOMES EXCEPT CANCERS
uoutcome <- backup_outcome

# Remove all but cancers from the list
uoutcome <- data.frame(outcome = uoutcome[c(3, 7, 8, 9, 10),])

outcome_type <- c("all")

for (j in 1:length(outcome_type)){
  df <- NULL
  for (i in 1:nrow(uoutcome)){
    acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", overall1 = 1, out_type = outcome_type[j])
    acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = T)
    acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T, minQuantile = 0, maxQuantile = 0.75 , lout = 500))
    if (nrow(plot_data) > 0){
      colnames(plot_data) <- c("dose","RR", "lb", "ub")
      temp_df <- data.frame(dose=plot_data$dose, RR=plot_data$RR, lb = plot_data$lb, ub = plot_data$ub, outcome = stringi::stri_trans_totitle(uoutcome$outcome[i]), col = i) #col=col_pal[i])
      df <- rbind(df,temp_df)
    }else{
      cat("Data Not Found: Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    }
  }
  
  grph <- ggplot(df,aes(x=dose,y=RR,group=col,colour = outcome)) + geom_line(size = 1) + 
    #geom_point(aes(shape = factor(col)), show.legend = F, size = 1) +
    #geom_ribbon(data = df, aes(ymin=lb,ymax=ub),alpha=.07, colour=NA) +
    labs(title=paste0("Overall Population - Outcome Type (", stringi::stri_trans_totitle(outcome_type[j]), ")")) +
    ylim(0, 1) +
    xlim(0, 80) +
    ylab("Relative Risk") +
    xlab("Marginal MET hours per week")
  print(grph)
  
  ggsave(grph, file=paste0("total-population-cancers.png"), dpi = 300)
  
  # grph <- ggplot(df,aes(x=dose,y=RR,group=col,colour = outcome)) + geom_line() + 
  #   labs(title=paste0("Overall Population - Outcome Type (", stringi::stri_trans_totitle(outcome_type[j]), ")"))
  # 
  # ggsave(grph, file=paste0("total-population-",outcome_type[j], ".png"), dpi = 300)
}


# ---

for (j in 1:length(outcome_type)){
  df <- NULL
  for (i in 1:nrow(uoutcome)){
    # i = 1
    #cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", gender = 1, out_type = outcome_type[j])
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
      df <- data.frame(dose=plot_data$dose, RR=plot_data$RR, outcome = stringi::stri_trans_totitle(uoutcome$outcome[i]), col = i) #col=col_pal[i])
      
      grph <- ggplot(df,aes(x=dose,y=RR,group=col,colour = outcome)) + geom_line() + 
        geom_ribbon(data = df, aes(ymin=lb,ymax=ub),alpha=0.3) +
        labs(title=paste0("Overall Population - ", df$outcome, " - ", "Outcome Type (", stringi::stri_trans_totitle(outcome_type[j]), ")"))
      
      ggsave(grph, file=paste0("total-population-",as.character(df$outcome[1]), "-", outcome_type[j], ".png"), dpi = 300)
      
      #df <- rbind(df,temp_df)
    }else{
      cat("Data Not Found: Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    }
  }
  
  grph <- ggplot(df,aes(x=dose,y=RR,group=interaction(col, gender),colour = outcome)) + geom_line() + 
    labs(title=paste0("Male Population - Outcome Type (", stringi::stri_trans_totitle(outcome_type[j]), ")"))
  
  ggsave(grph, file=paste0("male-population-",outcome_type[j], ".png"))
  
  
}


for (j in 1:length(outcome_type)){
  #j = 3
  df <- NULL
  for (i in 1:nrow(uoutcome)){
    #i = 12
    #cat("Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmdata <- getDiseaseSpecificData(data, uoutcome$outcome[i], paexposure = "LTPA", gender = 2, out_type = outcome_type[j])
    #acmdata <- subset(acmdata, outcome_type == "mortality")
    acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = T)
    # Remove all cases where both rr and dose are null
    acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T, minQuantile = 0, maxQuantile = 0.75))# , lout = 500))
    if (nrow(plot_data) > 0){
      colnames(plot_data) <- c("dose","RR", "lb", "ub")
      temp_df <- data.frame(dose=plot_data$dose, RR=plot_data$RR, outcome = stringi::stri_trans_totitle(uoutcome$outcome[i]), col = i) #col=col_pal[i])
      df <- rbind(df,temp_df)
    }else{
      cat("Data Not Found: Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    }
  }
  if (!is.null(df)){
    grph <- ggplot(df,aes(x=dose,y=RR,group=col,colour = outcome)) + geom_line() + 
      labs(title=paste0("Female Population - Outcome Type (", stringi::stri_trans_totitle(outcome_type[j]), ")"))
    #print(grph)
    ggsave(grph, file=paste0("female-population-",outcome_type[j], ".png"))
  }
  
  
}