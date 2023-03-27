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
  filter(!is.na(Data_Value), GeographicLevel == "City", Category == "Health Outcomes", DataValueTypeID == "AgeAdjPrv") %>%
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

epa_joined <- epa_filtered %>%
  select(CBSA, CBSA_Name, CBSA_POP) %>% 
  left_join(epa_meannatwalkind, by = 'CBSA') %>% 
  distinct(CBSA, .keep_all=TRUE)

#archived code
### adding mean nat walk ind to filtered data
#epa_split <- epa_filtered %>% 
#  left_join(epa_meannatwalkind, by ='CBSA') %>% 
#  select (STATEFP, COUNTYFP, CSA, CSA_Name, CBSA, CBSA_Name, CBSA_POP, meanNatWalkInd) %>%
#  mutate(City = sub(",.*", "", CBSA_Name)) %>%
#  distinct(CBSA, .keep_all=TRUE) %>%
#  mutate(City1 = substring(City, 1, regexpr("-", City) - 1)) %>% 
#  mutate(City2 = sub(".*-(.*)-.*", "\\1", City)) %>% 
#  mutate(City3 = sub("^.*-", "", City))
#  mutate(City2 = str_split(City, "-"))

#both <- left_join(x=cdc_clean,
#                   y=epa_split,
#                   by.x="City",
#                   by.y=c("City1","City2","City3"))

#both_clean <- both %>% 
#  filter(!is.na(meanNatWalkInd))


#epa_filtered_2 <- epa_filtered %>% 
#  filter(CSA_Name == "Atlanta--Athens-Clarke County--Sandy Springs, GA-AL")

#city_split <- str_split(epa_split$City, "-")

#city_split_clean <- data.frame(matrix(unlist(city_split_dummy), byrow = TRUE))

## adding cbsa to CDC for matching to EPA
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
    City == "Ontario" & State == "California" ~ 40140,
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
    City == "Denver" ~ 19740,
    City == "Aurora" & State == "Colorado" ~ 19740,
    City == "Greeley" ~ 24540,
    City == "Boulder" ~ 14500,
    City == "Pueblo" ~ 39380,
    City == "New Haven" ~ 35300,
    City == "Hartford" ~ 25540,
    City == "Bridgeport" ~ 14860,
    City == "Worcester" ~ 49340,
    City == "Philadelphia" ~ 37980,
    City == "Camden" ~ 37980,
    City == "Wilmington" & State == "Delaware" ~ 37980,
    City == "Orlando" ~ 36740,
    City == "Cape Coral" ~ 15980,
    City == "Miami" ~ 33100,
    City == "Fort Lauderdale" ~ 33100,
    City == "Pompano Beach" ~ 33100,
    City == "Gainesville" ~ 23540,
    City == "Lakeland" ~ 29460,
    City == "Port St. Lucie" ~ 38940,
    City == "Jacksonville" & State == "Florida" ~ 27260,
    City == "Deltona" ~ 19660,
    City == "Atlanta" ~ 12060,
    City == "Sandy Springs" ~ 12060,
    City == "Macon" ~ 31420,
    City == "Warner Robins" ~ 47580,
    City == "Savannah" ~ 42340,
    City == "Athens" ~ 12020,
    City == "Chattanooga" ~ 16860,
    City == "Boise City" ~ 14260,
    City == "Chicago" ~ 16980,
    City == "Naperville" ~ 16980,
    City == "Elgin" ~ 16980,
    City == "Rockford" ~ 40420,
    City == "St. louis" ~ 41180,
    City == "Davenport" ~ 19340,
    City == "Bloomington" & State == "Illinois" ~ 14010,
    City == "Springfield" & State == "Illinois" ~ 44100,
    City == "Indianapolis" ~ 26900,
    City == "Carmel" ~ 26900,
    City == "Louisville" ~ 31140,
    City == "Bloomington" & State == "Indiana" ~ 14020,
    City == "Fort Wayne" & State == "Indiana" ~ 23060,
    City == "South Bend" ~ 43780,
    City == "Lafayette" ~ 29200,
    City == "Cincinnati" ~ 17140,
    City == "Muncie" ~ 34620,
    City == "Decatur" ~ 19540,
    City == "Omaha" ~ 36540,
    City == "Des Moines" ~ 19780,
    City == "Cedar Rapids" ~ 16300,
    City == "Iowa City" ~ 26980,
    City == "Kansas City" ~ 28140,
    City == "Wichita" ~ 48620,
    City == "Lawrence" & State == "Kansas" ~ 29940,
    City == "St. Joseph" ~ 41140,
    City == "Lexington" ~ 30460,
    City == "New Orleans" ~ 35380,
    City == "Lafayette" & State == "Louisiana" ~ 29180,
    City == "Lake Charles" ~ 29340,
    City == "Shreveport" ~ 43340,
    City == "Portland" & State == "Maine" ~ 38860,
    City == "Baltimore" ~ 12580,
    City == "Boston" ~ 14460,
    City == "Cambridge" ~ 14460,
    City == "Newton" ~ 14460,
    City == "Providence" ~ 39300,
    City == "Warwick" ~ 39300,
    City == "Detroit" ~ 19820,
    City == "Warren" ~ 19820,
    City == "Dearborn" ~ 19820,
    City == "Flint" ~ 22420,
    City == "Kalamazoo" ~ 29020,
    City == "Grand Rapids" ~ 24340,
    City == "Ann Arbor" ~ 11460,
    City == "Minneapolis" ~ 33460,
    City == "St. Paul" ~ 33460,
    City == "Bloomington" & State == "Minnesota" ~ 33460,
    City == "Rochester" & State == "Minnesota" ~ 40340,
    City == "Fargo" ~ 22020,
    City == "Jackson" ~ 27140,
    City == "Columbia" & State == "Missouri" ~ 17860,
    City == "Lincoln" ~ 30700,
    City == "Las Vegas" ~ 29820,
    City == "Henderson" ~ 29820,
    City == "Reno" ~ 39900,
    City == "Manchester" ~ 31700,
    City == "Nashua" ~ 31700,
    City == "New York" ~ 35620,
    City == "Newark" ~ 35620,
    City == "Jersey City" ~ 35620,
    City == "Trenton" ~ 45940,
    City == "Albuquerque" ~ 10740,
    City == "Las Cruces" ~ 29740,
    City == "Santa Fe" ~ 42140,
    City == "Buffalo" ~ 15380,
    City == "Rochester" & State == "New York" ~ 15380,
    City == "Albany" & State == "New York" ~ 10580,
    City == "Schenectady" ~ 10580,
    City == "Syracuse" ~ 45060,
    City == "Winston-Salem" ~ 49180,
    City == "Charlotte" ~ 16740,
    City == "Concord" & State == "North Carolina" ~ 16740,
    City == "Gastonia" ~ 16740,
    City == "Greensboro" ~ 24660,
    City == "High Point" ~ 24660,
    City == "Fayetteville" & State == "North Carolina" ~ 22180,
    City == "Greenville" ~ 24780,
    City == "Asheville" ~ 11700,
    City == "Durham" ~ 20500,
    City == "Raleigh" ~ 39580,
    City == "Cary" ~ 39580,
    City == "Akron" ~ 10420,
    City == "Canton" ~ 15940,
    City == "Youngstown" ~ 49660,
    City == "Columbus" & State == "Ohio" ~ 18140,
    City == "Dayton" ~ 19430,
    City == "Toledo" ~ 45780,
    City == "Cleveland" ~ 17460,
    City == "Tulsa" ~ 46140,
    City == "Oklahoma City" ~ 36420,
    City == "Salem" ~ 41420,
    City == "Portland" & State == "Oregon" ~ 38900,
    City == "Vancouver" ~ 38900,
    City == "Hillsboro" ~ 38900,
    City == "Medford" ~ 32780,
    City == "Bend" ~ 13460,
    City == "Pittsburgh" ~ 38300,
    City == "Reading" ~ 39740,
    City == "Erie" ~ 21500,
    City == "Columbia" & State == "South Carolina" ~ 17900,
    City == "Rapid City" ~ 39660,
    City == "Nashville" ~ 34980,
    City == "Murfreesboro" ~ 34980,
    City == "Knoxville" ~ 28940,
    City == "Provo" ~ 39340,
    City == "Orem" ~ 39340,
    City == "Salt Lake CIty" ~ 41620,
    City == "Ogden" ~ 36260,
    City == "Burlington" ~ 15540,
    City == "Seattle" ~ 42660,
    City == "Tacoma" ~ 42660,
    City == "Bellevue" ~ 42660,
    City == "Spokane" ~ 44060,
    City == "Spokane Valley" ~ 44060,
    City == "Kennewick" ~ 28420,
    City == "Charleston" & State == "West Virginia" ~ 16620,
    City == "Green Bay" ~ 24580,
    City == "Racine" ~ 39540,
    City == "Appleton" ~ 11540,
    City == "Milwaukee" ~ 33340,
    City == "Waukesha" ~ 33340,
    City == "Madison" ~ 31540
  )) %>% 
  filter(!is.na(CBSA))

both <- cdc_clean_cbsa %>% 
  left_join(epa_joined,
            by="CBSA")

measures_wide <- both %>%
  group_by(CBSA, Measure) %>%
  pivot_wider(names_from = Measure, values_from = Data_Value)

colnames(measures_wide) <- c("State", "City", "Category", "Data_Value_Unit", "Data_Value_Type", 
                             "Lower", "Upper", "CityFIPS", "CBSA", "CBSA_Name", "CBSA_POP", 
                             "MeanNatWalkInd", "TeethLost", "Arthritis", "Cancer", "KidneyDisease",
                             "PulmonaryDisease", "CHD", "Asthma", "Diabetes", "HBP", "Cholesterol", "MentHealth",
                             "PhysHealth", "Stroke")


measures <- measures_wide %>%
  select(State, City, CBSA, CBSA_Name, MeanNatWalkInd, TeethLost, Arthritis, Cancer, KidneyDisease,
         PulmonaryDisease, CHD, Asthma, Diabetes, HBP, Cholesterol, MentHealth, PhysHealth, Stroke) %>% 
  group_by(CBSA) %>%
  summarise(across(c(TeethLost, Arthritis, Cancer, KidneyDisease, PulmonaryDisease, CHD, Asthma,
                     Diabetes, HBP, Cholesterol, MentHealth, PhysHealth, Stroke), mean, na.rm = TRUE))

data_epa_measures <- epa_joined %>% 
  inner_join(measures,
             by="CBSA")

cdc_geo <- cdc_clean_cbsa %>%
  ungroup() %>% 
  select(State, City, CBSA) %>% 
  distinct(CBSA, .keep_all = TRUE)

data_all <- data_epa_measures %>% 
  inner_join(cdc_geo,
             by="CBSA")

data_clean <- data_all %>%
  select(State, City, CBSA, CBSA_Name, CBSA_POP, meanNatWalkInd, TeethLost, Arthritis, Cancer, KidneyDisease, PulmonaryDisease, CHD, Asthma,
         Diabetes, HBP, Cholesterol, MentHealth, PhysHealth, Stroke)


#saving clean data
saveRDS(
  data_clean,
  file = here::here("output/data_clean.rds"))


