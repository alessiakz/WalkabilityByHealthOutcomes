#setting root directory
here::i_am("code/01_tables.R")

#making dataframe from the saved data object
data_clean <- readRDS(
  file = here::here("output/data_clean.rds")
)

#loading libraries
library(tidyverse)
library(gt)
library(gtsummary)

#making table 1

### setting better labels
attr(data_clean$meanNatWalkInd,'label') <- 
  'Walkability Index'
attr(data_clean$Arthritis,'label') <- 
  'Prevalence of Arthritis (%)'
attr(data_clean$Cancer,'label') <- 
  'Prevalence of Cancer (Excluding Skin Cancers) (%)'
attr(data_clean$KidneyDisease,'label') <- 
  'Prevalence of Chronic Kidney Disease (%)'
attr(data_clean$CHD,'label') <- 
  'Prevalence of Coronary Heart Disease (%)'
attr(data_clean$Asthma,'label') <- 
  'Prevalence of Asthma (%)'
attr(data_clean$Diabetes,'label') <- 
  'Prevalence of Diagnosed Diabetes (%)'
attr(data_clean$HBP,'label') <- 
  'Prevalence of High Blood Pressure (%)'
attr(data_clean$Cholesterol,'label') <- 
  'Prevalence of High Cholesterol Among Those Who Have Been Screened in the Past 5 Years (%)'
attr(data_clean$MentHealth,'label') <- 
  'Prevalence of Poor Mental health For >= 14 Days (%)'
attr(data_clean$PhysHealth,'label') <- 
  'Prevalence of Poor Physical health For >= 14 Days (%)'
attr(data_clean$Stroke,'label') <- 
  'Prevalence of Strokes (%)'

#making table 1
tab1 <- data_clean %>% 
  select(meanNatWalkInd, Arthritis, Cancer, KidneyDisease, CHD, Asthma,
         Diabetes, HBP, Cholesterol, MentHealth, PhysHealth, Stroke) %>% 
  tbl_summary() %>% 
  modify_header(label = "**Variable**")

tab1

tab2 <- data_20 %>%
  select(CBSA_Name, meanNatWalkInd, Arthritis, Cancer, KidneyDisease, CHD, Asthma,
         Diabetes, HBP, Cholesterol, MentHealth, PhysHealth, Stroke) %>% 
  arrange(CBSA_Name) %>% 
  gt(rowname_col = "CBSA_Name") %>% 
  tab_header(
    title = md("**Walkability Index and Prevalence of Health Outcomes (%) Among Top 20 Cities by Population**")) %>% 
  tab_stubhead(label = "Core-Based Statistical Area (CBSA)"
  ) %>% 
  fmt_number(
    columns = c(meanNatWalkInd, Arthritis, Cancer, KidneyDisease, CHD, Asthma, Diabetes, HBP, Cholesterol, MentHealth, PhysHealth, Stroke),
    rows = everything(),
    decimals = 2,
    n_sigfig = NULL,
  ) %>% 
  cols_label(
    meanNatWalkInd = "Walkability Index",
    KidneyDisease = "Kidney Disease",
    CHD = "Coronary Heart Disease",
    HBP = "High Blood Pressure",
    Cholesterol = "High Cholesterol",
    MentHealth = "Poor Mental Health",
    PhysHealth = "Poor Physical Health"
  ) %>% 
  cols_align(
    align = "center",
    columns = everything()
  ) %>% 
  cols_align_decimal() %>% 
  cols_width(
    CBSA_Name ~ px(150),
    everything() ~ px(85)
  )

tab2

saveRDS(
  tab1,
  file = here::here("output/table_1.rds")
)


saveRDS(
  tab2,
  file = here::here("output/table_2.rds")
)

