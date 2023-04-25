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
  'Prevalence of High Cholesterol (%)'
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
  modify_header(label = "**Variable**") %>% 
  as_gt()

tab1

tab15 <- data_clean %>% 
  select(State) %>% 
  tbl_summary() %>% 
  as_gt()

tab15

#limiting to top 20 cities
data_20 <- data_clean %>% 
  filter(rank(desc(CBSA_POP))<=20)

# tab 2 includes: meanNatWalkInd, Arthritis, Asthma, Cancer
tab2 <- data_20 %>%
  select(CBSA_Name, meanNatWalkInd, Arthritis, Asthma, Cancer) %>% 
  arrange(CBSA_Name) %>% 
  gt(rowname_col = "CBSA_Name") %>% 
  tab_header(
    title = md("**Walkability Index and Prevalence of Arthritis, Asthma, and Cancer (%) Among Top 20 Cities by Population**")) %>% 
  tab_stubhead(label = "Core-Based Statistical Area (CBSA)"
  ) %>% 
  fmt_number(
    columns = c(meanNatWalkInd, Arthritis, Asthma, Cancer),
    rows = everything(),
    decimals = 2,
    n_sigfig = NULL,
  ) %>% 
  cols_label(
    meanNatWalkInd = "Walkability Index") %>% 
  cols_align(
    align = "center",
    columns = everything()
  ) %>% 
  cols_align_decimal() #%>% 
#cols_width(
#  CBSA_Name ~ px(150),
#  everything() ~ px(85)
#)

tab2

# tab 3 includes: meanNatwalkInd, KidneyDisease, Diabetes

tab3 <- data_20 %>%
  select(CBSA_Name, meanNatWalkInd, KidneyDisease, Diabetes) %>% 
  arrange(CBSA_Name) %>% 
  gt(rowname_col = "CBSA_Name") %>% 
  tab_header(
    title = md("**Walkability Index and Prevalence of Kidney Disease and Diabetes (%) Among Top 20 Cities by Population**")) %>% 
  tab_stubhead(label = "Core-Based Statistical Area (CBSA)"
  ) %>% 
  fmt_number(
    columns = c(meanNatWalkInd, KidneyDisease, Diabetes),
    rows = everything(),
    decimals = 2,
    n_sigfig = NULL,
  ) %>% 
  cols_label(
    meanNatWalkInd = "Walkability Index",
    KidneyDisease = "Kidney Disease"
  ) %>% 
  cols_align(
    align = "center",
    columns = everything()
  ) %>% 
  cols_align_decimal() #%>% 
#cols_width(
#  CBSA_Name ~ px(150),
#  everything() ~ px(85)
#)

tab3

# tab 4 includes: meanNatWalkInd, CHD, Stroke
tab4 <- data_20 %>%
  select(CBSA_Name, meanNatWalkInd, CHD, Stroke) %>% 
  arrange(CBSA_Name) %>% 
  gt(rowname_col = "CBSA_Name") %>% 
  tab_header(
    title = md("**Walkability Index and Prevalence of Coronary Heart Disease and Stroke (%) Among Top 20 Cities by Population**")) %>% 
  tab_stubhead(label = "Core-Based Statistical Area (CBSA)"
  ) %>% 
  fmt_number(
    columns = c(meanNatWalkInd, CHD, Stroke),
    rows = everything(),
    decimals = 2,
    n_sigfig = NULL,
  ) %>% 
  cols_label(
    meanNatWalkInd = "Walkability Index",
    CHD = "Coronary Heart Disease"
  ) %>% 
  cols_align(
    align = "center",
    columns = everything()
  ) %>% 
  cols_align_decimal() #%>% 
#cols_width(
#  CBSA_Name ~ px(150),
#  everything() ~ px(85)
#)

tab4

# tab 4 includes: meanNatWalkInd, HBP, Cholesterol

tab5 <- data_20 %>%
  select(CBSA_Name, meanNatWalkInd, HBP, Cholesterol) %>% 
  arrange(CBSA_Name) %>% 
  gt(rowname_col = "CBSA_Name") %>% 
  tab_header(
    title = md("**Walkability Index and Prevalence of High Blood Pressure and Cholesterol (%) Among Top 20 Cities by Population**")) %>% 
  tab_stubhead(label = "Core-Based Statistical Area (CBSA)"
  ) %>% 
  fmt_number(
    columns = c(meanNatWalkInd, HBP, Cholesterol),
    rows = everything(),
    decimals = 2,
    n_sigfig = NULL,
  ) %>% 
  cols_label(
    meanNatWalkInd = "Walkability Index",
    HBP = "High Blood Pressure",
    Cholesterol = "High Cholesterol",
  ) %>% 
  cols_align(
    align = "center",
    columns = everything()
  ) %>% 
  cols_align_decimal() #%>% 
#cols_width(
#  CBSA_Name ~ px(150),
#  everything() ~ px(85)
#)

tab5

# tab 5 includes: meanNatWalkInd, MentHealth, Phys Health

tab6 <- data_20 %>%
  select(CBSA_Name, meanNatWalkInd, MentHealth, PhysHealth) %>% 
  arrange(CBSA_Name) %>% 
  gt(rowname_col = "CBSA_Name") %>% 
  tab_header(
    title = md("**Walkability Index, Poor Mental Health and Physical Health Among Top 20 Cities by Population**")) %>% 
  tab_stubhead(label = "Core-Based Statistical Area (CBSA)"
  ) %>% 
  fmt_number(
    columns = c(meanNatWalkInd, MentHealth, PhysHealth),
    rows = everything(),
    decimals = 2,
    n_sigfig = NULL,
  ) %>% 
  cols_label(
    meanNatWalkInd = "Walkability Index",
    MentHealth = "Poor Mental Health",
    PhysHealth = "Poor Physical Health"
  ) %>% 
  cols_align(
    align = "center",
    columns = everything()
  ) %>% 
  cols_align_decimal() #%>% 
#cols_width(
#  CBSA_Name ~ px(150),
#  everything() ~ px(85)
#)

tab6

#identifying top 5 cities with lowest and higest rates of chronic disease (avg)
rowwise_mean <- function(ds_subset) {
  Reduce(`+`, ds_subset) / ncol(ds_subset)
}
data_clean$avgchrodis<- rowwise_mean(data_clean [, c("Asthma", "Arthritis","Cancer", 
                                                     "KidneyDisease", "Diabetes", "CHD", "Stroke", "HBP", "Cholesterol", "MentHealth", "PhysHealth")])

data_high5<- data_clean %>% 
  filter(rank(desc(avgchrodis))<=5)

data_low5<- data_clean %>% 
  filter(rank((avgchrodis))<=5)

#making highest 5
highest5 <- data_high5 %>% 
  select(CBSA_Name, avgchrodis) %>% 
  arrange(avgchrodis) %>% 
  gt(rowname_col = "CBSA_Name") %>% 
  tab_header(
    title = md("**Top 5 Metroplitan Areas with the Highest Average Age-Adjusted Prevalence (%) for Chronic Disease**")) %>% 
  tab_stubhead(label = "Core-Based Statistical Area (CBSA)"
  ) %>% 
  fmt_number(
    columns = c(avgchrodis),
    rows = everything(),
    decimals = 2,
    n_sigfig = NULL,
  ) %>% 
  cols_label(
    avgchrodis = "Average Age-Ajusted Prevalence of Chronic Disease") %>% 
  cols_align(
    align = "center",
    columns = everything()
  ) %>% 
  cols_align_decimal()

highest5 

#making lowest 5
lowest5 <- data_low5 %>% 
  select(CBSA_Name, avgchrodis) %>% 
  arrange(avgchrodis) %>% 
  gt(rowname_col = "CBSA_Name") %>% 
  tab_header(
    title = md("**Top 5 Metroplitan Areas with the Lowest Average Age-Adjusted Prevalence (%) for Chronic Disease**")) %>% 
  tab_stubhead(label = "Core-Based Statistical Area (CBSA)"
  ) %>% 
  fmt_number(
    columns = c(avgchrodis),
    rows = everything(),
    decimals = 2,
    n_sigfig = NULL,
  ) %>% 
  cols_label(
    avgchrodis = "Average Age-Ajusted Prevalence of Chronic Disease") %>% 
  cols_align(
    align = "center",
    columns = everything()
  ) %>% 
  cols_align_decimal()

lowest5

#saving tables as RDS
saveRDS(
  tab1,
  file = here::here("output/table_1.rds")
)

saveRDS(
  tab2,
  file = here::here("output/table_2.rds")
)

saveRDS(
  tab3,
  file = here::here("output/table_3.rds")
)

saveRDS(
  tab4,
  file = here::here("output/table_4.rds")
)

saveRDS(
  tab5,
  file = here::here("output/table_5.rds")
)

saveRDS(
  tab6,
  file = here::here("output/table_6.rds")
)

saveRDS(
  highest5,
  file = here::here("output/highest5.rds")
)

saveRDS(
  lowest5,
  file = here::here("output/lowest5.rds")
)




