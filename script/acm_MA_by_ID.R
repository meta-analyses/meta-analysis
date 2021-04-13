rm (list = ls())
library(tidyverse)

acm <- readr::read_csv('data/csv/all-cause mortality.csv')

source("script/all-functions.R")

# Set fixed last knot to 75th of person years
local_last_knot <- 0.75

# Get last knot based on 75% of person years
last_knot <- get_last_knot(acm, dose_pert = local_last_knot, personyrs_pert = local_last_knot)
last_knot <- last_knot[2]

res <- list()

rr_conf_df <- NULL

for (uid in unique(acm$id)){ 
  acm_by_id <- filter(acm, id == uid)
  res[[uid]] <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(res[[uid]][[1]], res[[uid]][[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "all-cause mortality", id = uid, lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/acm_RR_by_id.csv")