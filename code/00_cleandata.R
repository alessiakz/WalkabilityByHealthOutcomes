here::i_am("code/00_cleandata.R") #identifies root directory location
CDC_path <- here::here("raw_data/500_Cities__Local_Data_for_Better_Health__2019_release.csv")
EPA_path <- here::here("raw_data/EPA_SmartLocationDatabase_V3_Jan_2021_Final.csv")
cdc_raw <- read.csv(CDC_path, header = TRUE)
epa_raw <- read.csv(EPA_path, header = TRUE)

#Loading Libraries
library(tidyverse)
library(stringr) 

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
epa_split <- epa_filtered %>% 
  left_join(epa_meannatwalkind, by ='CBSA') %>% 
  select (STATEFP, COUNTYFP, CSA, CSA_Name, CBSA, CBSA_Name, CBSA_POP, meanNatWalkInd) %>%
  mutate(City = sub(",.*", "", CBSA_Name)) %>%
  distinct(CBSA, .keep_all=TRUE) 
  mutate(City2 = str_split(City, "-"))

city_split <- str_split(epa_split$City, "-")

city_split_clean <- data.frame(matrix(unlist(city_split_dummy), byrow = TRUE))

#Merging CDC and EPA data
sort(epa_clean$STATEFP)

## adding cbsa to CDC
cdc_clean_cbsa <- cdc_clean %>% 
  mutate(CBSA = case_when(
    City == "Dallas" ~ 19100,
    City == "Fort Worth" ~ 19100,
    City == "Arlington" ~ 19100,
    City == "Midland" ~ 33260,
    City == "Houston" ~ 26420,
    City == "Sugar Land" ~ 26420,
    City == "Corpus Christi" ~ 18580,
    City == "Lubbock" ~ 31180,
    City == "Odessa" ~ 36220,
    City == "Amarillo" ~ 11100,
    City == "McAllen" ~ 32580,
    City == "Edinburg" ~ 32580,
    City == "Mission" ~ 32580,
    City == "San Antonio" ~ 41700,
    City == "Brownsville" ~ 15180,
    City == "El Paso" ~ 21340,
    City == "Tyler" ~ 46340,
    City == "Virginia Beach" ~ 47260,
    City == "Norfolk" ~ 47260,
    City == "Newport News" ~ 47260,
    City == "Washington" ~ 47900,
    City == "Alexandria" ~ 47900,
    City == "Birmingham" ~ 13820,
    City == "Hoover" ~ 13820,
    City == "Columbus" & State == "Georgia" ~ 17980,
    City == "Mobile" ~ 33660,
    City == "Huntsville" ~ 26620,
    City == "Montgomery" ~ 33860,
    City == "Phoenix" ~ 38060,
    City == "Mesa" ~ 38060,
    City == "Chandler" ~ 38060,
    City == "Tucson" ~ 46060,
    City == "Little Rock" ~ 30780,
    City == "Memphis" ~ 23820,
    City == "Santa Rosa" ~ 42220,
    City == "San Francisco" ~ 41860,
    City == "Oakland" ~ 41860,
    City == "Berkeley" ~ 41860,
    City == "Oxnard" ~ 37100,
    City == "Thousand Oaks" ~ 37100, 
    City == "San Buenaventura (Ventura)" ~ 37100,
    City == "Riverside" ~ 40140,
    City == "San Bernadino" ~ 40140,
    City == "Ontario" ~ 40140,
    City == "Los Angeles" ~ 31080,
    City == "Long Beach" ~ 31080,
    City == "Anaheim" ~ 30180,
    City == "San Jose" ~ 41940,
    City == "Sunnyvale" ~ 41940,
    City == "Santa Clara" ~ 41940,
    City == "Vallejo" ~ 46700,
    City == "Napa" ~ 34900,
    City == "Sacramento" ~ 40900,
    City == "Roseville" ~ 40900,
    City == "Folsom" ~ 40900,
    City == "Modesto" ~ 33700,
    City == "Merced" ~ 32900,
    City == "Fresno" ~ 23420,
    City == "Stockton" ~ 44700,
    City == "Redding" ~ 39820,
  ))

library("writexl")
write_xlsx(epa_clean,"C:\\Users\\Alessia\\Desktop\\epa_clean.xlsx")

#saving clean data
saveRDS(
  data_clean,
  file = here::here("output/data_clean.rds"))
  
  
  