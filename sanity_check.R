## Read raw data and run following tests:
# sum of totalpersons should be of order with n_baseline
# 
# sum of tot_personyrs should be of order with person years
# 
# same for cases
# 
# Referrant category should have the lowest dose
# if greater than 10, flag it up
# if greater than 80, flag it up

source("filter_studies.R")

for (i in 1:nrow(uoutcome)){
  acmfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup)
  acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
  
  # Remove when totalperson is not available for hr, and personsyears for rr/or
  acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                   (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
  
  
  for (j in unique(acmfdata$id)){
    d <- subset(acmfdata, id == j)
    
    total_pyears <- sum(d$personyrs, na.rm = T)
    diff <- 100 - (total_pyears /  unique(d$tot_personyrs) * 100)
    if (!is.na(diff) && diff > 10){
      cat("PYRS ", uoutcome$outcome[i], " ", diff, " ", unique(d$ref_number), "\n")
    }
    
  }
}



for (i in 1:nrow(uoutcome)){
  acmfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup)
  acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
  
  # Remove when totalperson is not available for hr, and personsyears for rr/or
  acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                   (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
  
  
  for (j in unique(acmfdata$id)){
    d <- subset(acmfdata, id == j)
    
    total_persons <- sum(d$totalpersons, na.rm = T)
    diff <- 100 - (total_persons /  unique(d$n_baseline) * 100)
    if (!is.na(diff) && diff > 10){
      cat("TPRS ", uoutcome$outcome[i], " ", diff, " ", unique(d$ref_number), "\n")
    }
  }
}


for (i in 1:nrow(uoutcome)){
  acmfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup)
  acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
  
  # Remove when totalperson is not available for hr, and personsyears for rr/or
  acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                   (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
  
  
  for (j in unique(acmfdata$id)){
    d <- subset(acmfdata, id == j)
    
    total_cases <- sum(d$cases, na.rm = T)
    diff <- 100 - (total_cases /  unique(d$tot_cases) * 100)
    if (!is.na(diff) && diff > 10){
      cat("Cases ", uoutcome$outcome[i], " ", diff, " ", unique(d$ref_number), "\n")
    }
  }
}



for (i in 1:nrow(uoutcome)){
  acmfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup)
  acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)

  # Remove when totalperson is not available for hr, and personsyears for rr/or
  acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) |
                                   (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
  acmfdata$ref_number <- sapply(strsplit(acmfdata$ref_number," "), `[`, 1)
  cat(length(unique(acmfdata$id)), " ", length(unique(acmfdata$Study)), "\n")
  td <- acmfdata %>% select(ref_number, Study) %>% distinct()
  #
  td <- td %>% group_by(Study) %>% filter( n() > 1 )
  if (nrow(td) > 0){
    td$outcome <- uoutcome$outcome[i]
    td$population <- "total population"

    write.table(td, "test.csv", sep = ",", col.names = F, row.names = F, append = T)
  }
  
  # i <- 1
  acmfdata <- subset(raw_data_gsp_ltpa, outcome == uoutcome$outcome[i] & 
                       pa_domain_subgroup == local_pa_domain_subgroup & 
                       sex_subgroups == 1)
  
  acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
  acmfdata[acmfdata$ref_number == "18 -1",]$Study <- "EPIC-Spain"
  if (!is.null(acmfdata) && nrow(acmfdata) > 0){
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    
    acmfdata$ref_number <- sapply(strsplit(acmfdata$ref_number," "), `[`, 1)
    
    cat(uoutcome$outcome[i], " " , length(unique(acmfdata$id)), " ", length(unique(acmfdata$Study)), "\n")
    td <- acmfdata %>% select(ref_number, Study) %>% distinct()
    
    td <- td %>% group_by(Study) %>% filter( n() > 1 )
    if (nrow(td) > 0){
      cat("male population for ", uoutcome$outcome[i], "\n")
      td$outcome <- uoutcome$outcome[i]
      td$population <- "male population"
    }
  }
  
  acmfdata <- subset(raw_data_gsp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == local_pa_domain_subgroup & sex_subgroups == 2)
  acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
  if (!is.null(acmfdata) && nrow(acmfdata) > 0){
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) |
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    acmfdata$ref_number <- sapply(strsplit(acmfdata$ref_number," "), `[`, 1)
    cat(length(unique(acmfdata$id)), " ", length(unique(acmfdata$Study)), "\n")
    td <- acmfdata %>% select(ref_number, Study) %>% distinct()

    td <- td %>% group_by(Study) %>% filter( n() > 1 )
    if (nrow(td) > 0){
      td$outcome <- uoutcome$outcome[i]
      td$population <- "female population"
      write.table(td, "test.csv", sep = ",", col.names = F, row.names = F, append = T)
    }
  }
}


