rm (list = ls())
library(MASS)
library(tidyverse)
library(janitor)
set.seed(101)

acm <- readr::read_csv('data/csv/all-cause mortality.csv')

my_data <- acm$dose
my_data[my_data == 0] <- 0.5


fit <- fitdistr(my_data, densfun="log-normal")  # we assume my_data ~ Normal(?,?)
fit

hist(my_data, pch=20, breaks=25, prob=TRUE, main="")
curve(dlnorm(x, fit$estimate[1], fit$estimate[2]), col="red", lwd=2, add=T)

# For the study with 3 levels of exposure:
#   
# 0
# 0-6 MET.hrs/week
# 6+ MET.hrs/week

three_exposures <- data.frame(dose = c("0", "0 - 6", "6+"), sum_total_persons = c(

acm %>% filter(dose == 0) %>% dplyr::select(totalpersons) %>% colSums(na.rm = T),

acm %>% filter(dose <= 6) %>% dplyr::select(totalpersons) %>% colSums(na.rm = T),

acm %>% filter(dose > 6) %>% dplyr::select(totalpersons) %>% colSums(na.rm = T)))

write_csv(three_exposures, "data/csv/acm_sum_total_persons_three_exp.csv")

# For the study with 4 levels of exposure:
#   
# 0-5 met.hrs/week
# 5-9 met.hrs/week
# 9-20 met.hrs/week
# 20+ met.hrs/week

four_exposures <- data.frame(dose = c("0 - 5", "5 - 9", "9 - 20", "20+"), sum_total_persons = c(

acm %>% filter(dose == 0 & dose <= 5) %>% dplyr::select(totalpersons) %>% colSums(na.rm = T),

acm %>% filter(dose > 5 & dose <= 9) %>% dplyr::select(totalpersons) %>% colSums(na.rm = T),

acm %>% filter(dose > 9 & dose <= 20) %>% dplyr::select(totalpersons) %>% colSums(na.rm = T),

acm %>% filter(dose > 20) %>% dplyr::select(totalpersons) %>% colSums(na.rm = T)))

write_csv(four_exposures, "data/csv/acm_sum_total_persons_four_exp.csv")



