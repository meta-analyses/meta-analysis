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

overall_sum_total_persons <- sum(acm$totalpersons, na.rm = T)

# For the study with 3 levels of exposure:
#   
# 0
# 0-6 MET.hrs/week
# 6+ MET.hrs/week

three_exposures <- acm %>% mutate(dose_range = case_when(dose == 0 ~ "0",
                                                         dose <= 6 ~ "0 - 6",
                                                         dose > 6 ~ "6+")) %>% 
  group_by(dose_range) %>%
  summarise(sum_total_persons = sum(totalpersons, na.rm = T), 'percentage (%)' = round(sum_total_persons / overall_sum_total_persons * 100, 2))

write_csv(three_exposures, "data/csv/acm_sum_total_persons_three_exp.csv")

# For the study with 4 levels of exposure:
#   
# 0-5 met.hrs/week
# 5-9 met.hrs/week
# 9-20 met.hrs/week
# 20+ met.hrs/week

four_exposures <- acm %>% mutate(dose_range = case_when(dose <= 5 ~ "0 - 5",
                                                        dose > 5 & dose <= 9 ~ "5 - 9",
                                                        dose > 9 & dose <= 20 ~ "9 - 20",
                                                        dose > 20 ~ "20+")) %>% 
  group_by(dose_range) %>%
  summarise(sum_total_persons = sum(totalpersons, na.rm = T), 'percentage (%)' = round(sum_total_persons / overall_sum_total_persons * 100, 2))
  
write_csv(four_exposures, "data/csv/acm_sum_total_persons_four_exp.csv")



