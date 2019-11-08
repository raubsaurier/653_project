rm(list=ls())
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(countrycode)

wd <- "~/repos/653_project/data_files/"
## load the data 
temp_data <- data.table(read.csv(paste0(wd,"1991_2016_temp_data.csv"), stringsAsFactors = FALSE))

## disease files downloaded seperately:
disease_data1 <- data.table(read.csv(paste0(wd,"IHME-GBD_2017_DATA-1066c200-1.csv"), stringsAsFactors = FALSE))
disease_data2 <- data.table(read.csv(paste0(wd,"IHME-GBD_2017_DATA-1066c200-2.csv"), stringsAsFactors = FALSE))

disease_data <- rbind(disease_data1, disease_data2)

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

health_ex_wide <- health_ex_reduced %>%
  spread(Series, Value)

health_p_wide <- health_p_reduced %>%
  spread(Series, Value)

UN_data <- education_wide %>%
  full_join(health_ex_wide, by = c("Area", "Year")) %>%
  full_join(health_p_wide, by = c("Area", "Year")) %>%
  rename("country" = Area,
         "year" = Year)

# Take the space out of country names.
totalData$country <- gsub(" ", "", unique(totalData$country), fixed = TRUE)

# Full join the UN data with the total data from Irena.
totalData_UN <- full_join(totalData, UN_data, by = c("country", "year"))

## output dataset 
write.csv(totalData_UN, paste0(wd, "totalData.csv"), row.names=FALSE)



