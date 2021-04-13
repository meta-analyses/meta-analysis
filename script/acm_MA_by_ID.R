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
  
  # By default run the analysis with Hamling method to approximate covariance
  res[[uid]] <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(res[[uid]]) || is.na(res[[uid]])) {
    res[[uid]] <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(res[[uid]]) || is.na(res[[uid]])) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      res[[uid]] <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(res[[uid]])) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
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