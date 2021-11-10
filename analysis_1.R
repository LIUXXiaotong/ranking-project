library("tidyverse")
library("afex")

dat <- read_csv("study_1_df.csv") 


with(dat, table(if_there_are_errors, error_type, useNA = "ifany"))

with(dat, table(between_subject_condition, classify_all_ranks, useNA = "ifany"))

mtab <- dat %>% 
  group_by(within_subject_condition, between_subject_condition) %>% 
  count(classify_all_ranks) %>% 
  mutate(prop = n / sum(n))

mtab %>% 
  pivot_wider(
    id_cols = c(between_subject_condition, within_subject_condition), 
    names_from = classify_all_ranks, 
    values_from = prop)


dat %>% 
  filter(classify_all_ranks != "logical") %>% 
  group_by(within_subject_condition, between_subject_condition) %>% 
  count(classify_all_ranks) %>% 
  mutate(prop = n / sum(n)) %>% 
  pivot_wider(
    id_cols = c(between_subject_condition, within_subject_condition), 
    names_from = classify_all_ranks, 
    values_from = prop)
