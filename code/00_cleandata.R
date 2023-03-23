here::i_am("code/00_cleandata.R") #identifies root directory location
CDC_path <- here::here("raw_data/500_Cities__Local_Data_for_Better_Health__2019_release.csv")
EPA_path <- here::here("raw_data/EPA_SmartLocationDatabase_V3_Jan_2021_Final.csv")
cdc_raw <- read.csv(CDC_path, header = TRUE)
epa_raw <- read.csv(EPA_path, header = TRUE)

library(tidyverse)

#finding all unique city names in CDC and EPA data
unique(cdc_raw$CityName)
unique(epa_raw$CSA_Name)

#Cleaning CDC Data
### selecting variables of interest
### filtering: removing missing values, setting to be only at City level, and only have health outcome measures
### grouping by city and measure

cdc_clean <- cdc_raw %>%
  filter(!is.na(Data_Value), GeographicLevel == "City", Category == "Health Outcomes") %>%
  select(StateDesc, CityName, Category, Measure, Data_Value_Unit, Data_Value_Type, 
         Data_Value, Low_Confidence_Limit, High_Confidence_Limit, CityFIPS) %>% 
  group_by(CityName, Measure)

### renaming columns
colnames(cdc_clean) <- c("State", "City", "Category", "Measure", "Data_Value_Unit", "Data_Value_Type", 
                            "Data_Value", "Lower", "Upper", "CityFIPS")

#Cleaning EPA Data
epa_filtered <- epa_raw %>% 
  filter(!is.na(CSA)) %>% 
  select(STATEFP, COUNTYFP,BLKGRPCE, CSA, CSA_Name, CBSA, CBSA_Name, CBSA_POP, NatWalkInd)

epa_meannatwalkind <- epa_filtered %>% 
  group_by(CBSA) %>% 
  summarise_at(vars(NatWalkInd), list(meanNatWalkInd = mean))

### adding mean nat walk ind to filtered data
epa_clean <- epa_filtered %>% 
  left_join(epa_meannatwalkind, by ='CBSA') %>% 
  select (STATEFP, COUNTYFP, CSA, CSA_Name, CBSA, CBSA_Name, CBSA_POP, meanNatWalkInd) %>% 
  distinct(CBSA, .keep_all=TRUE)

#Merging CDC and EPA data

#saving clean data
saveRDS(
  data_clean,
  file = here::here("output/data_clean.rds"))
  
  
  