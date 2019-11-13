rm(list=ls())
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(countrycode)
library(reshape2)

# Kim's wd <- "C:/Users/hochsted/Documents/653_project/data_files/"
# Irena's wd: wd  <- "~/repos/653_project/data_files/"
# Madeline's wd: wd <- "/Users/madelineabbott/Desktop/biostat653_project/653_project-master/data_files/"
## load the data 

###----------------------------
## temp and disease data 
##---------------------------
disease_data1 <- data.table(read.csv(paste0(wd,"IHME-GBD_2017_DATA-1066c200-1.csv"), stringsAsFactors = FALSE))
disease_data2 <- data.table(read.csv(paste0(wd,"IHME-GBD_2017_DATA-1066c200-2.csv"), stringsAsFactors = FALSE))

disease_data <- rbind(disease_data1, disease_data2)

# temp data 
temp_data <- data.table(read.csv(paste0(wd,"1991_2016_temp_data.csv"), stringsAsFactors = FALSE))
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
totalData <- totalData[cause_name%in%c("Malaria", "Yellow fever", "Encephalitis" ,
                                       "Dengue", "Zika virus")&measure_name=="Incidence"&metric_name=="Rate"]

# ------------------
## urban + refugee data 
# ------------------

refugee_data <- data.table(read.csv(paste0(wd,"unhcr_popstats_export_persons_of_concern_all_data.csv"), stringsAsFactors = FALSE))

## urban data 
urban_data <- data.table(read.csv(paste0(wd,"wb_urbanization/percent_of_pop_living_in_urban_area.csv"), stringsAsFactors = FALSE))

## don't need the first two columns 
urban_data <- urban_data[,-c(1:3)]

colnames(urban_data)[1] <- c("iso3")
colnames(urban_data)[2:31] <- seq(1990, 2019, 1)

urban_data <- melt(urban_data, id.vars = c("iso3"), variable.name = "year",
                   value.name = "urban_perc")
urban_data  <- urban_data[!is.na(iso3)]

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


setnames(refugee_data, "Origin", "refugee_origin_country")

refugee_data <- melt(refugee_data, id.vars = c("year", "country", "refugee_origin_country"), variable.name = "refugee_type",
                     value.name = "num_persons")


refugee_data$iso3 <- countrycode(refugee_data$country, "country.name", "iso3c")


refugee_data <- refugee_data[country=="Central African Rep.", iso3:="CAF"]
refugee_data  <- refugee_data[!is.na(iso3)]


refugee_subset <- refugee_data[,c("year", "iso3", "refugee_origin_country", "refugee_type","num_persons")
                               , with=FALSE]

refugee_subset$num_persons <- as.numeric(refugee_subset$num_persons)
refugee_subset  <- refugee_subset [,list(num_persons=sum(na.omit(num_persons))), by=c("refugee_type",
                                                                                      "iso3","year")]


## refugee and urban data 
WDI_data <- merge(refugee_subset, urban_data, by=c("year", "iso3"))

## merge to total dataset 
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

#totalData$country.x <- NULL
#totalData$country.y <- NULL

# ------------------
# Read in EPI data.
# ------------------
epi_data <- data.table(read.csv(paste0(wd,"EPIlong.csv"), stringsAsFactors = FALSE))

# Remove spaces in EPI country names.
epi_data$iso3 <- countrycode(epi_data$Country, "country.name", "iso3c")


# Rename year variables, select variables of interest.
setnames(epi_data, "Year", "year")
epi_data <- epi_data[,c("year", "iso3", "EPI", "EPInormalized"), with=FALSE]

totalData <- merge(totalData, epi_data, all.x=TRUE, by=c("year", "iso3"))


#---------------------------
# Load UN data sets.
#----------------------------
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
# Filter to only include countries that are in Irena's data. 
education$iso3 <- countrycode(education$X, "country.name", "iso3c")
education_reduced <- education[!is.na(iso3)]

health_p$iso3 <- countrycode(health_p$X, "country.name", "iso3c")
health_reduced <- health_p[!is.na(iso3)]

health_ex$iso3 <- countrycode(health_ex$X, "country.name", "iso3c")
health_reduced2 <- health_ex[!is.na(iso3)]

setnames(health_reduced, c("Series", "Value","Year"), c("health_p_measure", "health_p_value","year"))
health_reduced <- health_reduced[,c("health_p_measure", "health_p_value","year", "iso3"),with=FALSE]
health_reduced <- health_reduced[health_p_measure%in%c("Health personnel: Physicians (number)",
                                                       "Health personnel: Physicians (per 1000 population)",
                                                       "Health personnel: Pharmacists (number)",
                                                       "Health personnel: Pharmacists (per 1000 population)"
)]

setnames(health_reduced2, c("Series", "Value","Year"), c("health_exp_measure", "health_exp_value",                                                         "year"))
health_reduced2 <- health_reduced2[,c("health_exp_measure", "health_exp_value","year", "iso3"),with=FALSE]

setnames(education_reduced, c("Series", "Value","Year"), c("edu_measure", "edu_value","year"))
education_reduced <- education_reduced[,c("edu_measure", "edu_value","year", "iso3"),with=FALSE]


health_data <- merge(health_reduced, health_reduced2, by=c("year", "iso3"))
health_edu_data <- merge(health_data, education_reduced,by=c("year", "iso3"),allow.cartesian=TRUE)

totalData <- merge(health_edu_data, totalData, all.y=TRUE, by=c("year", "iso3"), allow.cartesian=TRUE)





# ---------------------------
# Join Precipitation Data
# ---------------------------

# Read in current totalData
#totalData <- read.csv(paste0(wd, "totalData.csv"))
# Remove duplicate country name columns
totalData <- totalData %>%
  mutate(country = country.x) %>%
  select(-c(country.x, country.y))
# Take the space out of country names.
totalData$country <- gsub(" ", "", totalData$country, fixed = TRUE)
# Standardize country names (note: not spaces between multi-word names)
totalData$country <- gsub("Egypt,ArabRep.", "Egypt", totalData$country, fixed = TRUE)
totalData$country <- gsub("Korea,Rep.", "Korea", totalData$country, fixed = TRUE)
totalData$country <- gsub("SyrianArabRepublic", "Syria", totalData$country, fixed = TRUE)
totalData$country <- gsub("Venezuela,RB", "Venezuela", totalData$country, fixed = TRUE)
totalData$country <- gsub("Yemen,Rep.", "Yemen", totalData$country, fixed = TRUE)
totalData$country <- gsub("KyrgyzRepublic", "Kyrgyzstan", totalData$country, fixed = TRUE)

# Read in precipitation data
precipitation <- read.csv(paste(wd, "UNdata_Precipitation.csv", sep = ""))
precipitation <- precipitation[,c(1, 2, 3)]
colnames(precipitation) <- c("country", "year", "precip")
precipitation <- precipitation %>%
  dplyr::mutate(year = as.numeric(as.character(year))) %>%
  dplyr::mutate(country = gsub(" ", "", country, fixed = TRUE))
# Standardize country names
precipitation$country <- gsub("Czechia", "Czech Republic", precipitation$country, fixed = TRUE)
precipitation$country <- gsub("SyrianArabRepublic", "Syria", precipitation$country, fixed = TRUE)

# Left join precip onto totalData
totalData <- dplyr::left_join(totalData, precipitation, by = c("country", "year")) #totalData


# ---------------------------
# Join Happiness Data
# ---------------------------

# Read in happiness data
happiness <- read.csv(paste0(wd, "online-data-chapter-2-whr-2017.csv"))

# Select a few variables of interest
happiness <- happiness %>%
  select(c("country", "year", "gini.of.household.income.reported.in.Gallup..by.wp5.year",
           "Healthy.life.expectancy.at.birth", "Log.GDP.per.capita",
           "Life.Ladder"))
colnames(happiness) <- c("country", "year", "gini_of_household_income", "healthy_life_exp_at_birth", "log_GDP_per_capita", "life_ladder")

# Left-join happiness data onto totalData
totalData <- dplyr::left_join(totalData, happiness, by = c("country", "year"))


# ---------------------------
# Join Wastewater Tx Data
# ---------------------------

# Read in wastewater treatment data
wastewater <- read.csv(paste0(wd, "Pop_Connected_Wastewater_Treatment.csv"), header = T)

# Clean up and convert to long format
wastewater <- wastewater[c(2:nrow(wastewater)), c(seq(from = 1, to = ncol(wastewater), by = 2))]
colnames(wastewater) <- c("country", gsub("X", "", colnames(wastewater)[2:ncol(wastewater)]))
wastewater_long <- melt(wastewater, id.vars = c("country"))
colnames(wastewater_long) <- c("country", "year", "wastewater")
wastewater_long <- wastewater_long %>%
  mutate(wastewater = as.numeric(wastewater)) %>%
  mutate(year = as.numeric(as.character(year)))

totalData <- dplyr::left_join(totalData, wastewater_long, by = c("country", "year"))

# -----------------------------------
# Add in tropical country indicator
# -----------------------------------

# Identify countries as tropical or not (1 = tropical, 0 = not)

tropics <- read.csv(paste0(wd, "countries_list.csv"))
totalData <- dplyr::left_join(totalData, tropics, by = c("country"))

# -----------------------------------
# Add in country area
# -----------------------------------

# because precipitation data is measured in terms of total volume of water in a 
#  given country, here we adjust for total area of each country by measuring
#  precipitation in volume/area

# precip_per_km2 <- precip / area_km2

land_area <- read.csv(paste0(wd, "land_area.csv"))
colnames(land_area) <- c("country", "iso3", "area_km2")
land_area <- land_area %>%
  select(c(iso3, area_km2))

totalData <- dplyr::left_join(totalData, land_area, by = c("iso3"))

# calculate precip/area variable
totalData <- totalData %>%
  mutate(precip_per_km2 = precip / area_km2)


# -----------------------------
# Save new version of totalData
# -----------------------------

#write.csv(totalData, paste0(wd, "totalData.csv"), row.names=FALSE)


