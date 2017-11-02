library(tidyverse)

source("init.R")
raw_data_tp_ltpa <- subset(raw_data, pa_domain_subgroup == "LTPA")
for (i in 1:nrow(uoutcome)){
  #i <- 5
  dat <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i])
  for (j in 1:length(unique(dat$ref_number))){
    dat1 <- subset(dat, ref_number == (unique(dat$ref_number))[j]) 
    uoverall <- unique(dat1$overall)
    usexgroups <- unique(dat1$sex_subgroups)
    if (length(usexgroups) > 2 && length(uoverall) > 1){
      # Remove gender specific from total_population
      raw_data_tp_ltpa <- subset(raw_data_tp_ltpa, (ref_number == unique(dat1$ref_number) & !sex_subgroups %in% c(1,2)) | (ref_number != unique(dat1$ref_number))) 
    }
    
  }
}

raw_data_gsp_ltpa <- subset(raw_data, pa_domain_subgroup == "LTPA" & (sex_subgroups %in% c(1,2)))

for (i in 1:nrow(uoutcome)){
  if (!i %in% c(2,6)){
    dat <- subset(raw_data_gsp_ltpa, outcome == uoutcome$outcome[i])
    for (j in 1:length(unique(dat$ref_number))){
      dat1 <- subset(dat, ref_number == (unique(dat$ref_number))[j]) 
      usexgroups <- unique(dat1$sex_subgroups)
      if (length(usexgroups) == 1){
        # Remove single gender specific studies
        raw_data_gsp_ltpa <- subset(raw_data_gsp_ltpa,(ref_number != unique(dat1$ref_number))) 
      }
    }
  }
}


## Create ref_number for men and women subgroups
## for total population
raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 1,]$ref_number <- paste(raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 1,]$ref_number, "-1")
raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 2,]$ref_number <- paste(raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 2,]$ref_number, "-2")

raw_data_tp_ltpa <- plyr::arrange(raw_data_tp_ltpa, outcome)


raw_data_gsp_ltpa[raw_data_gsp_ltpa$sex_subgroups == 1,]$ref_number <- paste(raw_data_gsp_ltpa[raw_data_gsp_ltpa$sex_subgroups == 1,]$ref_number, "-1")
raw_data_gsp_ltpa[raw_data_gsp_ltpa$sex_subgroups == 2,]$ref_number <- paste(raw_data_gsp_ltpa[raw_data_gsp_ltpa$sex_subgroups == 2,]$ref_number, "-2")

raw_data_gsp_ltpa <- plyr::arrange(raw_data_gsp_ltpa, outcome)
