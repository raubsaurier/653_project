rm(list=ls())
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(countrycode)

# Kim's wd <- "C:/Users/hochsted/Documents/653_project/data_files/"
# Irena's wd: wd  <- "~/repos/653_project/data_files/"
## load the data 

# ------------------
## TOTAL DATASET:
# ------------------
totalData <- data.table(read.csv(paste0(wd,"totalData.csv"), stringsAsFactors = FALSE))
## filter only the diseases that we want: 
totalData <- totalData[cause_name%in%c("Malaria", "Yellow fever", "Encephalitis" ,
                                     "Dengue", "Zika virus")]

# ------------------
## temperature +urban + refugee data 
# ------------------
temp_data <- data.table(read.csv(paste0(wd,"1991_2016_temp_data.csv"), stringsAsFactors = FALSE))

refugee_data <- data.table(read.csv(paste0(wd,"unhcr_popstats_export_persons_of_concern_all_data.csv"), stringsAsFactors = FALSE))

## urban data 
urban_data <- data.table(read.csv(paste0(wd,"wb_urbanization/percent_of_pop_living_in_urban_area.csv"), stringsAsFactors = FALSE))

## don't need the first two columns 
urban_data <- urban_data[,-c(1:3)]

colnames(urban_data)[1] <- c("iso3")
colnames(urban_data)[2:31] <- seq(1990, 2019, 1)

urban_data <- melt(urban_data, id.vars = c("iso3"), variable.name = "year",
                     value.name = "urban_perc")


urban_data$urban_perc <- as.numeric(urban_data$urban_perc)
urban_data$year <- as.character(urban_data$year)
urban_data$year <- as.numeric(urban_data$year)


## clean the refugeee data 
refugee_data <- refugee_data[-c(1:2),]
colnames(refugee_data) <- as.character(refugee_data[1, ])
refugee_data <- refugee_data[-1,]
colnames(refugee_data)[1] <- "year"
refugee_data$year <- as.numeric(refugee_data$year)
refugee_data <- refugee_data[year>=1990]

colnames(refugee_data)[2] <- "country"


refugee_data <- melt(refugee_data, id.vars = c("year", "country", "Origin"), variable.name = "refugee_type",
                     value.name = "num_persons")

setnames(refugee_data, "Origin", "refugee_origin_country")

refugee_data$iso3 <- countrycode(refugee_data$country, "country.name", "iso3c")

refugee_data <- refugee_data[country=="Central African Rep.", iso3:="CAF"]

refugee_subset <- refugee_data[,c("year", "iso3", "refugee_origin_country", "refugee_type","num_persons")
                               , with=FALSE]

## refugee and urban data 
WDI_data <- merge(refugee_subset, urban_data, by=c("year", "iso3"))

totalData <- merge(totalData, WDI_data, by=c("year", "iso3"), allow.cartesian=TRUE)

# ------------------
## GDP data 
# ------------------
gdpData <- data.table(read.csv(paste0(wd,"Final_GDP_Data.csv"), stringsAsFactors = FALSE))
gdpData <- gdpData[,-c(1, 66:68)]
colnames(gdpData)[1:4] <- c("country", "iso3", "gdp_ind_name", "gdp_ind_code")
colnames(gdpData)[5:64] <- seq(1960,2019,1)

gdpData <- melt(gdpData, id.vars = c("gdp_ind_name", "gdp_ind_code", "country", "iso3"),
                variable.name = "year",
                     value.name = "gdp_measurement")

gdpData$year <- as.numeric(as.character(gdpData$year))

gdpData <- gdpData[year>=1990]

totalData <- merge(gdpData, totalData, by=c("year", "iso3"))

totalData$country.x <- NULL
totalData$country.y <- NULL


# ------------------
## disease files downloaded seperately:
# ------------------
disease_data1 <- data.table(read.csv(paste0(wd,"IHME-GBD_2017_DATA-1066c200-1.csv"), stringsAsFactors = FALSE))
disease_data2 <- data.table(read.csv(paste0(wd,"IHME-GBD_2017_DATA-1066c200-2.csv"), stringsAsFactors = FALSE))

disease_data <- rbind(disease_data1, disease_data2)

# ------------------
temp_data <- data.table(read.csv(paste0(wd,""), stringsAsFactors = FALSE))
# ------------------


# ------------------
# Read in EPI data.
# ------------------
epi_data <- data.table(read.csv(paste0(wd,"EPIlong.csv"), stringsAsFactors = FALSE))

# Remove spaces in EPI country names.
epi_data$iso3 <- countrycode(epi_data$Country, "country.name", "iso3c")


# Rename year variables, select variables of interest.
setnames(epi_data, "Year", "year")
epi_data <- epi_data[,c("year", "iso3", "EPI", "EPInormalized"), with=FALSE]

totalData <- merge(totalData, epi_data, by=c("year", "iso3"))



## rename so that data processing is easier 
names(temp_data) <- c("temperature", "year", "stat", "country", "iso3")
setnames(disease_data, c("val", "upper", "lower"), c("disease_value", "disease_upperCI", "disease_lowerCI"))


## add ISO code to the disease dataset (3 letter abbreviation of country name): 
disease_data$iso3 <- countrycode(disease_data$location_name, 'country.name', 'iso3c')
## the ISO3 for temp data is messed up so just replace it with standardized ones 
temp_data$iso3 <- countrycode(temp_data$country, 'country.name', 'iso3c')

## North Korea has no observations so we'll remove it for now 
currentTemp <- temp_data[!is.na(iso3)]

## change from character to numeric
currentTemp$temperature <- as.numeric(currentTemp$temperature)

## since the disease data is at the annual level, for now: average the temp data by year: 
## can undo this if we decide to do different time measurement
currentTemp <- currentTemp[,list(mean_temp=mean(na.omit(temperature))), 
                                 by=c("year", "country", "iso3")]

## merge datasets 
totalData <- merge(currentTemp, disease_data, by=c("iso3", "year"))



# Load UN data sets.
health_ex <- data.table(read.csv(paste0(wd,"UN_HealthExpenditure.csv"),
                                 stringsAsFactors = FALSE, skip = 1))

health_p <- data.table(read.csv(paste0(wd,"UN_HealthPersonnel.csv"),
                                 stringsAsFactors = FALSE, skip = 1))

education <- data.table(read.csv(paste0(wd,"UN_Education.csv"),
                               stringsAsFactors = FALSE,
                      skip = 1))

# Take the space out of country names.
countries <- gsub(" ", "", unique(totalData$country), fixed = TRUE)

# Clean UN data sets.
# Take space out of country names.
# Select relevant variables.
# Rename countries if they are different than Irena's data.
# Filter to only include countries that are in Inena's data. 
education$Area <- gsub(" ", "", education$X, fixed = TRUE)

education_reduced <- education %>%
  select(Area, Year, Series, Value) %>%
  mutate(Area = ifelse(Area == "Czechia", "CzechRepublic", Area)) %>%
  mutate(Area = ifelse(Area == "SaintLucia", "St.Lucia", Area)) %>%
  mutate(Area = ifelse(Area == "Serbia", "RepublicofSerbia", Area)) %>%
  mutate(Area = ifelse(Area == "Korea", "RepublicofKorea", Area)) %>%
  mutate(Area = ifelse(Area == "UnitedStates", "UnitedStatesofAmerica", Area)) %>%
  mutate(Area = ifelse(Area == "TimorLeste", "Timor-Leste", Area)) %>%
  mutate(Area = ifelse(Area == "Venezuela", "Venezuela(Boliv.Rep.of)", Area)) %>%
  mutate(Area = ifelse(Area == "CaboVerde", "CapeVerde", Area)) %>%
  mutate(Area = ifelse(Area == "SyrianArabRepublic", "Syria", Area)) %>%
  filter(Area %in% countries)

health_ex$Area <- gsub(" ", "", health_ex$X, fixed = TRUE)

health_ex_reduced <- health_ex %>%
  select(Area, Year, Series, Value) %>%
  mutate(Area = ifelse(Area == "Czechia", "CzechRepublic", Area)) %>%
  mutate(Area = ifelse(Area == "SaintLucia", "St.Lucia", Area)) %>%
  mutate(Area = ifelse(Area == "Serbia", "RepublicofSerbia", Area)) %>%
  mutate(Area = ifelse(Area == "Korea", "RepublicofKorea", Area)) %>%
  mutate(Area = ifelse(Area == "UnitedStates", "UnitedStatesofAmerica", Area)) %>%
  mutate(Area = ifelse(Area == "TimorLeste", "Timor-Leste", Area)) %>%
  mutate(Area = ifelse(Area == "Venezuela", "Venezuela(Boliv.Rep.of)", Area)) %>%
  mutate(Area = ifelse(Area == "CaboVerde", "CapeVerde", Area)) %>%
  mutate(Area = ifelse(Area == "SyrianArabRepublic", "Syria", Area)) %>%
  filter(Area %in% countries)

health_p$Area <- gsub(" ", "", health_p$X, fixed = TRUE)

health_p_reduced <- health_p %>%
  select(Area, Year, Series, Value) %>%
  mutate(Area = ifelse(Area == "Czechia", "CzechRepublic", Area)) %>%
  mutate(Area = ifelse(Area == "SaintLucia", "St.Lucia", Area)) %>%
  mutate(Area = ifelse(Area == "Serbia", "RepublicofSerbia", Area)) %>%
  mutate(Area = ifelse(Area == "Korea", "RepublicofKorea", Area)) %>%
  mutate(Area = ifelse(Area == "UnitedStates", "UnitedStatesofAmerica", Area)) %>%
  mutate(Area = ifelse(Area == "TimorLeste", "Timor-Leste", Area)) %>%
  mutate(Area = ifelse(Area == "Venezuela", "Venezuela(Boliv.Rep.of)", Area)) %>%
  mutate(Area = ifelse(Area == "CaboVerde", "CapeVerde", Area)) %>%
  mutate(Area = ifelse(Area == "SyrianArabRepublic", "Syria", Area)) %>%
  filter(Area %in% countries)

# Convert UN data sets to wide data sets. 
education_wide <- education_reduced %>%
  spread(Series, Value)

education_wide$gross_enroll_primaryF <- education_wide$`Gross enrollment ratio - Primary (female)`
education_wide$gross_enroll_primaryM <- education_wide$`Gross enrollement ratio - Primary (male)`
education_wide$gross_enroll_secondaryF <- education_wide$`Gross enrollment ratio - Secondary (female)`
education_wide$gross_enroll_secondaryM <- education_wide$`Gross enrollment ratio - Secondary (male)`
education_wide$gross_enroll_tertiaryF <- education_wide$`Gross enrollment ratio - Tertiary (female)`
education_wide$gross_enroll_tertiaryM <- education_wide$`Gross enrollment ratio - Tertiary (male)`
education_wide$primary_thousands <- education_wide$`Students enrolled in primary education (thousands)`
education_wide$secondary_thousands <- education_wide$`Students enrolled in secondary education (thousands)`
education_wide$tertiary_thousands <- education_wide$`Students enrolled in tertiary education (thousands)`

education_wide_reduced <- education_wide %>%
  select(Area, Year, gross_enroll_primaryF, gross_enroll_primaryM,
         gross_enroll_secondaryF, gross_enroll_secondaryM,
         gross_enroll_tertiaryF, gross_enroll_tertiaryM,
         primary_thousands, secondary_thousands, tertiary_thousands)

health_ex_wide <- health_ex_reduced %>%
  spread(Series, Value)

health_ex_wide$health_ex_percentGDP <- health_ex_wide$`Current health expenditure (% of GDP)`
health_ex_wide$health_ex_percentGov <- health_ex_wide$`Domestic general government health expenditure (% of total government expenditure)`

health_ex_wide_reduced <- health_ex_wide %>%
  select(Area, Year, health_ex_percentGDP, health_ex_percentGov)

health_p_wide <- health_p_reduced %>%
  spread(Series, Value)

health_p_wide$pharma_n <- health_p_wide$`Health personnel: Pharmacists (number)`
health_p_wide$pharma_per1000 <- health_p_wide$`Health personnel: Pharmacists (per 1000 population)`
health_p_wide$phys_n <- health_p_wide$`Health personnel: Physicians (number)`
health_p_wide$phys_per1000 <- health_p_wide$`Health personnel: Physicians (per 1000 population)`

health_p_wide_reduced <- health_p_wide %>%
  select(Area, Year, pharma_n, pharma_per1000, phys_n, phys_per1000)

UN_data <- education_wide_reduced %>%
  full_join(health_ex_wide_reduced, by = c("Area", "Year")) %>%
  full_join(health_p_wide_reduced, by = c("Area", "Year")) %>%
  rename("country" = Area,
         "year" = Year)

# Take the space out of country names.
totalData$country <- gsub(" ", "", unique(totalData$country), fixed = TRUE)

# Full join the UN data with the total data from Irena.
totalData_UN <- full_join(totalData, UN_data, by = c("country", "year")) %>%
  filter(year >= 2000)

## output dataset 
write.csv(totalData_UN, paste0(wd, "totalData.csv"), row.names=FALSE)



