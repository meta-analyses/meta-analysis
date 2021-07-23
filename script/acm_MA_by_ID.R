rm (list = ls())
library(tidyverse)

acm <- readr::read_csv('data/csv/main-All-cause mortality-Fatal and non-fatal.csv')

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "All-cause mortality", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                          highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/All-cause mortality_RR_by_id.csv")



######################################################################
rm (list = ls())
library(tidyverse)

acm <- readr::read_csv('data/csv/main-All-cause cancer-Fatal and non-fatal.csv')

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "All-cause cancer", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/All-cause cancer_RR_by_id.csv")

######################################################################
rm (list = ls())
library(tidyverse)

acm <- readr::read_csv('data/csv/main-All-cause cvd-Fatal and non-fatal.csv')

source("script/all-functions.R")

# Set fixed last knot to 75th of person years
local_last_knot <- 0.75

# Get last knot based on 75% of person years
last_knot <- get_last_knot(acm, dose_pert = local_last_knot, personyrs_pert = local_last_knot)
last_knot <- last_knot[2]

res <- list()

rr_conf_df <- NULL

for (uid in unique(acm$id)){ 
  
  # uid <- 27
  acm_by_id <- filter(acm, id == uid)
  
  print(paste('uid is ', uid))
  
  # By default run the analysis with Hamling method to approximate covariance
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  rr_conf <- data.frame(outcome = "All-cause cvd", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
}

write_csv(rr_conf_df, "data/csv/All-cause cvd_RR_by_id.csv")

######################################################################
rm (list = ls())
library(tidyverse)

acm <- readr::read_csv('data/csv/main-All-cause dementia-Fatal and non-fatal.csv')

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "All-cause dementia", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/All-cause dementia_RR_by_id.csv")

######################################################################
rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Bladder cancer-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Bladder cancer", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Bladder cancer_RR_by_id.csv")


###################################################################
rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Alzheimer's disease-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Alzheimer's disease", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Alzheimer's disease_RR_by_id.csv")

###################################################################
rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Breast cancer-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Breast cancer", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Breast cancer_RR_by_id.csv")


###################################################################
rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Colon cancer-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Colon cancer", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Colon cancer_RR_by_id.csv")


###################################################################

rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Coronary heart disease-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Coronary heart disease", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Coronary heart disease_RR_by_id.csv")


###################################################################

rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Depression-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Depression", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Depression_RR_by_id.csv")


###################################################################

rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Major depression-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Major depression", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Majordepression_RR_by_id.csv")


###################################################################




rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Depressive symptoms-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Depressive symptoms", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Depressive symptoms_RR_by_id.csv")


###################################################################

rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Endometrial cancer-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Endometrial cancer", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Endometrial cancer_RR_by_id.csv")


###################################################################

rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Esophageal cancer-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Esopahgeal cancer", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Esopahgeal cancer_RR_by_id.csv")


###################################################################

###################################################################

rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Gastric cardia cancer-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Gastric cardia cancer", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Gastric cardia cancer_RR_by_id.csv")

###################################################################

rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Head and neck cancer-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Head and neck cancer", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Head and neck cancer_RR_by_id.csv")

###################################################################

rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Heart failure-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Heart failure", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Heart failure_RR_by_id.csv")

###################################################################

rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Kidney cancer-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Kidney cancer", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Kidney cancer_RR_by_id.csv")

##########################################################
rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Lung cancer-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Lung cancer", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Lung cancer_RR_by_id.csv")


##########################################################
rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Liver cancer-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Liver cancer", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Liver cancer_RR_by_id.csv")





##########################################################
rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Myeloid leukemia-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Myeloid leukemia", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Myeloid leukemia_RR_by_id.csv")

##########################################################
rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Myeloma-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Myeloma", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Myeloma_RR_by_id.csv")


##########################################################
rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Parkinson's disease-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Parkinson's disease", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Parkinson's disease_RR_by_id.csv")


##########################################################
rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Prostate cancer-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Prostate cancer", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Prostate cancer_RR_by_id.csv")

##########################################################
rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Rectum cancer-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Rectum cancer", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Rectum cancer_RR_by_id.csv")

##########################################################
rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Stroke-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Stroke", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Stroke_RR_by_id.csv")

##########################################################
rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-Vascular dementia-Fatal and non-fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "Vascular dementia", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/Vascular dementia_RR_by_id.csv")

##########################################################
rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-All-cause cvd-Fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "All cause cvd mortality", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/All cause cvd mortality_RR_by_id.csv")

##########################################################
rm (list = ls())
library(tidyverse)

acm <- readr::read_csv("data/csv/main-All-cause cancer-Fatal.csv")

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
  result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  
  # If it fails, use the default by Greenland and Longnecker (gl)
  if (is.null(result_ma) || is.na(result_ma)) {
    result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
  }
  
  # If this too fails, increase last_knot by 5% until it converges
  if (is.null(result_ma) || is.na(result_ma)) {
    for (nq in seq(from = local_last_knot, to = 1, by = 0.01)) {
      print(nq)
      last_knot <- get_last_knot(acm_by_id, dose_pert = nq, personyrs_pert = nq)
      q <- quantile(acm_by_id$dose, prob = last_knot[2])
      result_ma <- metaAnalysis(acm_by_id, ptitle = "", returnval = TRUE, covMethed = FALSE, minQuantile = 0, maxQuantile = last_knot[2], lout = 1000)
      if (!is.null(result_ma)) {
        last_quintile <- gsub("%", "", names(q)) %>%
          as.numeric() %>%
          round(1)
        last_knot_title <- paste0(last_quintile, "% dose (using ", (nq * 100), "% person years)")
        break
      }
    }
  }
  
  # Save results as data frame
  dataset2 <- data.frame(cbind(result_ma[[1]], result_ma[[2]]))
  
  # Assign names
  colnames(dataset2) <- c("dose", "RR", "lb", "ub")
  
  rr_df <- get_ma_table(dataset2, "RR")
  lb_df <- get_ma_table(dataset2, "lb")
  ub_df <- get_ma_table(dataset2, "ub")
  
  
  rr_conf <- data.frame(outcome = "All cause cancer mortality", id = uid, ref_number = unique(acm_by_id$ref_number), lowest_guideline = rr_df[1],
                        lowest_CFI = paste0("(",lb_df[1], " - ", ub_df[1], ")"), mid_guideline = rr_df[2],
                        mid_CFI = paste0("(", lb_df[2], " - ", ub_df[2], ")"), mid_lb=lb_df[2], mid_ub=ub_df[2],
                        highest_guideline = rr_df[3],
                        highest_CFI = paste0("(", lb_df[3], " - ", ub_df[3], ")"))
  
  rr_conf_df <- rbind(rr_conf, rr_conf_df)
  
  
}

write_csv(rr_conf_df, "data/csv/All cause cancer mortality_RR_by_id.csv")