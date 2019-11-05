rm(list=ls())
library(data.table)
library(dplyr)
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

## output dataset 
write.csv(totalData, paste0(wd, "totalData.csv"), row.names=FALSE)



