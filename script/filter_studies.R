library(tidyverse)

source("script/init.R")

### Remove studies with less than 40k for all-cause, or 10k for the rest outcomes
#raw_data <- subset(raw_data, (outcome == "all-cause mortality" & n_baseline >= 40000 ) |
                        #    (outcome != "all-cause mortality" & n_baseline >= 10000 ))

raw_data_tp_ltpa <- subset(raw_data, pa_domain_subgroup == "LTPA")
for (i in 1:nrow(uoutcome)){
  dat <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i])
  for (j in 1:length(unique(dat$ref_number))){
    dat1 <- subset(dat, ref_number == (unique(dat$ref_number))[j]) 
    uoverall <- unique(dat1$overall)
    usexgroups <- unique(dat1$sex_subgroups)
    if (length(usexgroups) > 2 && length(uoverall) > 1){
      # Remove gender specific from total_population
      raw_data_tp_ltpa <- subset(raw_data_tp_ltpa, (ref_number == unique(dat1$ref_number) & !sex_subgroups %in% c(1,2)) | (ref_number != unique(dat1$ref_number))) 
      # cat(uoutcome$outcome[i], " - ", unique(dat1$ref_number), " - ", unique(dat1$overall), " - ", unique(dat1$sex_subgroups), "\n")
    }
    
  }
}

raw_data_gsp_ltpa <- subset(raw_data, pa_domain_subgroup == "LTPA" & (sex_subgroups %in% c(1,2)))

for (i in 1:nrow(uoutcome)){
  if (!uoutcome$outcome[i] %in% c('Breast cancer','Endometrial cancer')){
    dat <- subset(raw_data_gsp_ltpa, outcome == uoutcome$outcome[i])
    uid <- unique(dat$ref_number)
    for (j in 1:length(uid)){
      dat1 <- subset(dat, ref_number == uid[j]) 
      usexgroups <- unique(dat1$sex_subgroups)
      if (length(usexgroups) == 1){
        # Remove single gender specific studies
        raw_data_gsp_ltpa <- subset(raw_data_gsp_ltpa, (ref_number != uid[j])) 
        # cat("gsp ",uoutcome$outcome[i], " - ", unique(dat1$ref_number), " - ", unique(dat1$overall), " - ", unique(dat1$sex_subgroups), "\n")
      }
    }
  }
}


## Create ref_number for men and women subgroups
## for total population
raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 1,]$ref_number <- paste(raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 1,]$ref_number, "-1")
raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 2,]$ref_number <- paste(raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 2,]$ref_number, "-2")

raw_data_tp_ltpa <- plyr::arrange(raw_data_tp_ltpa, outcome)


raw_data_gsp_ltpa[raw_data_gsp_ltpa$sex_subgroups == 1,]$ref_number <- paste0(raw_data_gsp_ltpa[raw_data_gsp_ltpa$sex_subgroups == 1,]$ref_number, "-1")
raw_data_gsp_ltpa[raw_data_gsp_ltpa$sex_subgroups == 2,]$ref_number <- paste0(raw_data_gsp_ltpa[raw_data_gsp_ltpa$sex_subgroups == 2,]$ref_number, "-2")

raw_data_gsp_ltpa <- plyr::arrange(raw_data_gsp_ltpa, outcome)
