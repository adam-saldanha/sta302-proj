library(haven)
library(dplyr)

setwd("/Users/ellacharles/Downloads/sta302")

exam <- read_xpt("bloodpressure.xpt")
day1 <- read_xpt("day1diet.xpt")
day2 <- read_xpt("day2diet.xpt")
demo <- read_xpt("demographics.xpt")

# 1. Total protein and carbs per person, per day
day1_person <- day1 %>%
  filter(DR1DRSTZ == 1) %>%                  # reliable recalls only
  group_by(SEQN) %>%
  summarise(
    protein_d1 = sum(DR1IPROT, na.rm = TRUE),
    carbs_d1   = sum(DR1ICARB, na.rm = TRUE),
    .groups = "drop"
  )

day2_person <- day2 %>%
  filter(DR2DRSTZ == 1) %>%
  group_by(SEQN) %>%
  summarise(
    protein_d2 = sum(DR2IPROT, na.rm = TRUE),
    carbs_d2   = sum(DR2ICARB, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Combine the two days (full_join keeps people who have only one day)
diet <- full_join(day1_person, day2_person, by = "SEQN") %>%
  mutate(
    protein = rowMeans(cbind(protein_d1, protein_d2), na.rm = TRUE),
    carbs   = rowMeans(cbind(carbs_d1, carbs_d2), na.rm = TRUE),
    protein = ifelse(is.nan(protein), NA, protein),
    carbs   = ifelse(is.nan(carbs), NA, carbs)
  )

# 3. Load demographics and merge everything on SEQN
demo <- read_xpt("demographics.xpt") 

dat <- demo %>%
  left_join(exam, by = "SEQN") %>%
  left_join(diet, by = "SEQN")

dat_clean <- dat %>%
  select(SEQN, RIAGENDR, RIDAGEYR, DMDEDUC2, INDFMPIR, BPXOSY1, BPXOSY2, 
         BPXOSY3, BPXODI1, BPXODI2, BPXODI3, RIDRETH3, protein, carbs)

dat_clean <- dat_clean %>%
  mutate(
    sbp = rowMeans(cbind(BPXOSY1, BPXOSY2, BPXOSY3), na.rm = TRUE),
    dbp = rowMeans(cbind(BPXODI1, BPXODI2, BPXODI3), na.rm = TRUE),
    sbp = ifelse(is.nan(sbp), NA, sbp),
    dbp = ifelse(is.nan(dbp), NA, dbp)
  ) %>%
  select(-any_of(c("BPXOSY1", "BPXOSY2", "BPXOSY3",
                   "BPXODI1", "BPXODI2", "BPXODI3")))

dat_final <- dat_clean %>%
  filter(RIDAGEYR >= 20, RIDAGEYR <= 65) %>%
  na.omit()