source("script/init.R")


raw_data_tp_ltpa <- subset(raw_data, pa_domain_subgroup == "LTPA" & overall == 1)

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
      }
    }
  }
}


## Create ref_number for men and women subgroups
## for total population
if (nrow(raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 1,]) > 0)
  raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 1,]$ref_number <- paste(raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 1,]$ref_number, "-1")
if (nrow(raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 2,]) > 0)
raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 2,]$ref_number <- paste(raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 2,]$ref_number, "-2")

# Create ID column
raw_data_tp_ltpa$id <- as.integer(as.factor(raw_data_tp_ltpa$ref_number))

raw_data_tp_ltpa <- plyr::arrange(raw_data_tp_ltpa, outcome)


raw_data_gsp_ltpa[raw_data_gsp_ltpa$sex_subgroups == 1,]$ref_number <- paste0(raw_data_gsp_ltpa[raw_data_gsp_ltpa$sex_subgroups == 1,]$ref_number, "-1")
raw_data_gsp_ltpa[raw_data_gsp_ltpa$sex_subgroups == 2,]$ref_number <- paste0(raw_data_gsp_ltpa[raw_data_gsp_ltpa$sex_subgroups == 2,]$ref_number, "-2")

# Create ID column
raw_data_gsp_ltpa$id <- as.integer(as.factor(raw_data_gsp_ltpa$ref_number))

raw_data_gsp_ltpa <- plyr::arrange(raw_data_gsp_ltpa, outcome)
