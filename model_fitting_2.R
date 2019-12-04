
# Model fitting

library(dplyr)
library(lme4)
library(ggplot2)
library(cowplot)
library(tidyr)
library(nlme)

# Read in data
wd <- "/Users/madelineabbott/Desktop/biostat653_project/653_project-master/data_files/" # Madeline's wd
totalData <- read.csv(paste0(wd, "totalData.csv"))

# --------------------------------------
# Select years and variables of interest
# --------------------------------------

# Filter to only include years 2006-2016
totalData_0616 <- totalData %>%
  filter(year >= 2006)

# Select our (current) variables of interest (response & covars)
cur_vars <- c("year", "iso3", "country",
              "cause_name", "disease_value", # correspond to disease and incidence rate
              "tropical", "healthy_life_exp_at_birth", "EPInormalized",
              "gdp_measurement", "urban_perc", "precip_per_km2",
              "refugee_type", "num_persons", "mean_temp")
# currently only considering total refugee population
totalData_0616 <- totalData_0616 %>%
  select(cur_vars) %>%
  filter(refugee_type %in% c("Total Population")) %>%
  mutate(total_refugee_pop = num_persons) %>%
  select(-c("refugee_type", "num_persons"))
# remove duplicated rows
totalData_0616 <- totalData_0616[!duplicated(totalData_0616), ]


# Center and standardize variables
gdp_mean <- mean(totalData_0616$gdp_measurement, na.rm = T) # gdp
gdp_sd <- sd(totalData_0616$gdp_measurement, na.rm = T)


#setdiff(totalData_0616$country, total_pop$country)

# also standardize refugee population as per country total population
total_pop <- read.csv(paste0(wd, "total_population_final-1.csv")) %>%
  select(c("location_name", 'year_id', "val"))
colnames(total_pop) <- c("country", "year", "total_pop")
totalData_0616 <- left_join(totalData_0616, total_pop, by = c("year", "country"))

refug_perc <- totalData_0616$total_refugee_pop / totalData_0616$total_pop

totalData_0616 <- totalData_0616 %>%
  mutate(log_gdp_meas = log(gdp_measurement)) %>%
  mutate(std_gdp_meas = (gdp_measurement - gdp_mean)/gdp_sd) %>%
  mutate(std_log_gdp_meas = (log_gdp_meas - mean(log_gdp_meas, na.rm = T))/sd(log_gdp_meas, na.rm = T)) %>%
  mutate(refugee_perc = refug_perc)

# fill in missing tropical status for south sudan
totalData_0616[which(totalData_0616$country == "SouthSudan"),]$tropical <- 1


#totalData_0616 <- read.csv("~/Desktop/model_data.csv")

complete_0616 <- totalData_0616 %>%
  filter(!is.na(std_gdp_meas)) %>%
  filter(!is.na(tropical)) %>%
  filter(!is.na(urban_perc)) %>%
  filter(!is.na(refugee_perc))

# for calculating the lil term added to log (1% of min non-zero disease incidence value for each disease)
complete_0616_2 <- complete_0616 %>% filter(disease_value > 0) %>% group_by(cause_name) %>% summarise(min_val = .01*min(disease_value ))
min_dengue <- complete_0616_2$min_val[1]
min_malaria <- complete_0616_2$min_val[3]
min_yellow_fever <- complete_0616_2$min_val[4]
min_zika <- complete_0616_2$min_val[5]


# -------------------------------------------------------------------
# EDA: Plot disease trends vs. covariates to inform model selection
# -------------------------------------------------------------------

# First, convert to wide format for display purposes
complete_wide <- spread(complete_0616, cause_name, disease_value)
# Then, calculate log values for dz
complete_wide <- complete_wide %>% 
  mutate(log_dengue = log(Dengue + min_dengue)) %>% 
  mutate(log_encephalitis = log(Encephalitis + 0.001)) %>% 
  mutate(log_malaria = log(Malaria + min_malaria)) %>% 
  mutate(log_yellow_fever = log(`Yellow fever` + min_yellow_fever)) %>% 
  mutate(log_zika = log(`Zika virus` + min_zika))





## Dependent variable: time

# Dengue
plt_dengue <- ggplot(aes(x = year, y = log_dengue, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "Year", y = "log(Incidence Rate)", title = "Dengue") +
  theme_bw() + theme(legend.position = "none")
# Encephalitis
plt_encephalitis <- ggplot(aes(x = year, y = log_encephalitis, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "Year", y = "log(Incidence Rate)", title = "Encephalitis") +
  theme_bw() + theme(legend.position = "none")
# Malaria
plt_malaria <- ggplot(aes(x = year, y = log_malaria, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "Year", y = "log(Incidence Rate)", title = "Malaria") +
  theme_bw() + theme(legend.position = "none")
# Yellow fever
plt_yellow_fever <- ggplot(aes(x = year, y = log_yellow_fever, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "Year", y = "log(Incidence Rate)", title = "Yellow Fever") +
  theme_bw() + theme(legend.position = "none")
# Zika virus
plt_zika <- ggplot(aes(x = year, y = log_zika, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "Year", y = "log(Incidence Rate)", title = "Zika Virus") +
  theme_bw() + theme(legend.position = "none")

plot_grid(plt_dengue, plt_malaria, 
          plt_yellow_fever, plt_zika, nrow = 3)




## Dependent variable: std_gdp_meas

# Dengue
plt_dengue <- ggplot(aes(x = std_gdp_meas, y = log_dengue, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "GDP (standardized)", y = "log(Incidence Rate)", title = "Dengue") +
  theme_bw() + theme(legend.position = "none")
# Encephalitis
plt_encephalitis <- ggplot(aes(x = std_gdp_meas, y = log_encephalitis, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "GDP (standardized)", y = "log(Incidence Rate)", title = "Encephalitis") +
  theme_bw() + theme(legend.position = "none")
# Malaria
plt_malaria <- ggplot(aes(x = std_gdp_meas, y = log_malaria, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "GDP (standardized)", y = "log(Incidence Rate)", title = "Malaria") +
  theme_bw() + theme(legend.position = "none")
# Yellow fever
plt_yellow_fever <- ggplot(aes(x = std_gdp_meas, y = log_yellow_fever, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "GDP (standardized)", y = "log(Incidence Rate)", title = "Yellow Fever") +
  theme_bw() + theme(legend.position = "none")
# Zika virus
plt_zika <- ggplot(aes(x = std_gdp_meas, y = log_zika, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "GDP (standardized)", y = "log(Incidence Rate)", title = "Zika Virus") +
  theme_bw() + theme(legend.position = "none")

plot_grid(plt_dengue, plt_encephalitis, plt_malaria, 
          plt_yellow_fever, plt_zika, nrow = 3)



# Dengue
plt_dengue <- ggplot(aes(x = std_log_gdp_meas, y = log_dengue, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "log(GDP) (standardized)", y = "log(Incidence Rate)", title = "Dengue") +
  theme_bw() + theme(legend.position = "none")
# Malaria
plt_malaria <- ggplot(aes(x = std_log_gdp_meas, y = log_malaria, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "log(GD) (standardized)", y = "log(Incidence Rate)", title = "Malaria") +
  theme_bw() + theme(legend.position = "none")
# Yellow fever
plt_yellow_fever <- ggplot(aes(x = std_log_gdp_meas, y = log_yellow_fever, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "log(GDP) (standardized)", y = "log(Incidence Rate)", title = "Yellow Fever") +
  theme_bw() + theme(legend.position = "none")
# Zika virus
plt_zika <- ggplot(aes(x = std_log_gdp_meas, y = log_zika, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "log(GDP) (standardized)", y = "log(Incidence Rate)", title = "Zika Virus") +
  theme_bw() + theme(legend.position = "none")

plot_grid(plt_dengue, plt_malaria, 
          plt_yellow_fever, plt_zika, nrow = 2)



## Dependent variable: urban_perc

# Dengue
plt_dengue <- ggplot(aes(x = urban_perc, y = log_dengue, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "% Urban", y = "log(Incidence Rate)", title = "Dengue") +
  theme_bw() + theme(legend.position = "none")
# Encephalitis
plt_encephalitis <- ggplot(aes(x = urban_perc, y = log_encephalitis, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "% Urban", y = "log(Incidence Rate)", title = "Encephalitis") +
  theme_bw() + theme(legend.position = "none")
# Malaria
plt_malaria <- ggplot(aes(x = urban_perc, y = log_malaria, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "% Urban", y = "log(Incidence Rate)", title = "Malaria") +
  theme_bw() + theme(legend.position = "none")
# Yellow fever
plt_yellow_fever <- ggplot(aes(x = urban_perc, y = log_yellow_fever, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "% Urban", y = "log(Incidence Rate)", title = "Yellow Fever") +
  theme_bw() + theme(legend.position = "none")
# Zika virus
plt_zika <- ggplot(aes(x = urban_perc, y = log_zika, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "% Urban", y = "log(Incidence Rate)", title = "Zika Virus") +
  theme_bw() + theme(legend.position = "none")

plot_grid(plt_dengue, plt_encephalitis, plt_malaria, 
          plt_yellow_fever, plt_zika, nrow = 3)


# Dengue
plt_dengue <- ggplot(aes(x = log(urban_perc), y = log_dengue, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "log(% Urban)", y = "log(Incidence Rate)", title = "Dengue") +
  theme_bw() + theme(legend.position = "none")
# Encephalitis
plt_encephalitis <- ggplot(aes(x = log(urban_perc), y = log_encephalitis, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "log(% Urban)", y = "log(Incidence Rate)", title = "Encephalitis") +
  theme_bw() + theme(legend.position = "none")
# Malaria
plt_malaria <- ggplot(aes(x = log(urban_perc), y = log_malaria, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "log(% Urban)", y = "log(Incidence Rate)", title = "Malaria") +
  theme_bw() + theme(legend.position = "none")
# Yellow fever
plt_yellow_fever <- ggplot(aes(x = log(urban_perc), y = log_yellow_fever, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "log(% Urban)", y = "log(Incidence Rate)", title = "Yellow Fever") +
  theme_bw() + theme(legend.position = "none")
# Zika virus
plt_zika <- ggplot(aes(x = log(urban_perc), y = log_zika, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "log(% Urban)", y = "log(Incidence Rate)", title = "Zika Virus") +
  theme_bw() + theme(legend.position = "none")

plot_grid(plt_dengue, plt_encephalitis, plt_malaria, 
          plt_yellow_fever, plt_zika, nrow = 3)





## Dependent variable: refugee_perc

# Dengue
plt_dengue <- ggplot(aes(x = refugee_perc, y = log_dengue, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "Prop. Refugee", y = "log(Incidence Rate)", title = "Dengue") +
  theme_bw() + theme(legend.position = "none")
# Encephalitis
plt_encephalitis <- ggplot(aes(x = refugee_perc, y = log_encephalitis, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "Prop. Refugee", y = "log(Incidence Rate)", title = "Encephalitis") +
  theme_bw() + theme(legend.position = "none")
# Malaria
plt_malaria <- ggplot(aes(x = refugee_perc, y = log_malaria, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "Prop. Refugee", y = "log(Incidence Rate)", title = "Malaria") +
  theme_bw() + theme(legend.position = "none")
# Yellow fever
plt_yellow_fever <- ggplot(aes(x = refugee_perc, y = log_yellow_fever, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "Prop. Refugee", y = "log(Incidence Rate)", title = "Yellow Fever") +
  theme_bw() + theme(legend.position = "none")
# Zika virus
plt_zika <- ggplot(aes(x = refugee_perc, y = log_zika, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "Prop. Refugee", y = "log(Incidence Rate)", title = "Zika Virus") +
  theme_bw() + theme(legend.position = "none")

plot_grid(plt_dengue, plt_encephalitis, plt_malaria, 
          plt_yellow_fever, plt_zika, nrow = 3)

# Dengue
plt_dengue <- ggplot(aes(x = log(refugee_perc + 0.001), y = log_dengue, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "Prop. Refugee", y = "log(Incidence Rate)", title = "Dengue") +
  theme_bw() + theme(legend.position = "none")
# Encephalitis
plt_encephalitis <- ggplot(aes(x = log(refugee_perc + 0.001), y = log_encephalitis, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "Prop. Refugee", y = "log(Incidence Rate)", title = "Encephalitis") +
  theme_bw() + theme(legend.position = "none")
# Malaria
plt_malaria <- ggplot(aes(x = log(refugee_perc + 0.001), y = log_malaria, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "Prop. Refugee", y = "log(Incidence Rate)", title = "Malaria") +
  theme_bw() + theme(legend.position = "none")
# Yellow fever
plt_yellow_fever <- ggplot(aes(x = log(refugee_perc + 0.001), y = log_yellow_fever, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "Prop. Refugee", y = "log(Incidence Rate)", title = "Yellow Fever") +
  theme_bw() + theme(legend.position = "none")
# Zika virus
plt_zika <- ggplot(aes(x = log(refugee_perc + 0.001), y = log_zika, group = country), data = complete_wide) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "Prop. Refugee", y = "log(Incidence Rate)", title = "Zika Virus") +
  theme_bw() + theme(legend.position = "none")

plot_grid(plt_dengue, plt_encephalitis, plt_malaria, 
          plt_yellow_fever, plt_zika, nrow = 3)




## Dependent variable: tropical

# Dengue
plt_dengue <- ggplot(aes(x = year, y = log_dengue, group = country), data = complete_wide) +
  geom_line(aes(color = tropical)) + geom_point(aes(color = tropical)) + 
  labs(x = "Year", y = "log(Incidence Rate)", title = "Dengue") +
  theme_bw() + theme(legend.position = "none")
# Encephalitis
plt_encephalitis <- ggplot(aes(x = year, y = log_encephalitis, group = country), data = complete_wide) +
  geom_line(aes(color = tropical)) + geom_point(aes(color = tropical)) + 
  labs(x = "Year", y = "log(Incidence Rate)", title = "Encephalitis") +
  theme_bw() + theme(legend.position = "none")
# Malaria
plt_malaria <- ggplot(aes(x = year, y = log_malaria, group = country), data = complete_wide) +
  geom_line(aes(color = tropical)) + geom_point(aes(color = tropical)) + 
  labs(x = "Year", y = "log(Incidence Rate)", title = "Malaria") +
  theme_bw() + theme(legend.position = "none")
# Yellow fever
plt_yellow_fever <- ggplot(aes(x = year, y = log_yellow_fever, group = country), data = complete_wide) +
  geom_line(aes(color = tropical)) + geom_point(aes(color = tropical)) + 
  labs(x = "Year", y = "log(Incidence Rate)", title = "Yellow Fever") +
  theme_bw() + theme(legend.position = "none")
# Zika virus
plt_zika <- ggplot(aes(x = year, y = log_zika, group = country), data = complete_wide) +
  geom_line(aes(color = tropical)) + geom_point(aes(color = tropical)) + 
  labs(x = "Year", y = "log(Incidence Rate)", title = "Zika Virus") +
  theme_bw() + theme(legend.position = "none")

plot_grid(plt_dengue, plt_encephalitis, plt_malaria, 
          plt_yellow_fever, plt_zika, nrow = 3)


# --------------------------------------
# Outcome: Malaria
# --------------------------------------


# filter to include only malaria cases
df_malaria <- complete_0616 %>%
  filter(cause_name == "Malaria") %>%
  mutate(time = as.numeric(as.factor(year))) %>%
  mutate(log_disease_value = log(disease_value + min_malaria)) %>% 
  mutate(time_gt2010 = ifelse(year > 2010, time, 0))

# fit model with random slope and random intercept
mod_malaria_randIntSlope_compSymm <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + time,
            random = ~ 1 + time|country,
            correlation = corCompSymm(),
            data = df_malaria)
mod_malaria_randIntSlope_AR1 <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + time,
                                         random = ~ 1 + time|country,
                                         correlation =  corAR1(),
                                         data = df_malaria,
                                          control = lmeControl(maxIter = 100, msMaxIter = 100))
mod_malaria_randIntSlope_compSymm_catT <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + as.factor(time),
                                         random = ~ 1 + time|country,
                                         correlation = corCompSymm(),
                                         data = df_malaria)
mod_malaria_randIntSlope_AR1_catT <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + as.factor(time),
                                    random = ~ 1 + time|country,
                                    correlation =  corAR1(),
                                    data = df_malaria,
                                    control = lmeControl(maxIter = 150, msMaxIter = 150))
mod_malaria_randIntSlope_compSymm_splineT <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + time + time_gt2010,
                                              random = ~ 1 + time|country,
                                              correlation = corCompSymm(),
                                              data = df_malaria)
mod_malaria_randIntSlope_AR1_splineT <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + time + time_gt2010,
                                         random = ~ 1 + time|country,
                                         correlation =  corAR1(),
                                         data = df_malaria,
                                         control = lmeControl(maxIter = 150, msMaxIter = 150))


# fit model with random intercept
mod_malaria_randInt_compSymm <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + time,
                              random = ~ 1|country,
                              correlation = corCompSymm(),
                              data = df_malaria)
mod_malaria_randInt_AR1 <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + time,
                                    random = ~ 1|country,
                                    correlation = corAR1(),
                                    data = df_malaria)
mod_malaria_randInt_compSymm_catT <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + as.factor(time),
                                    random = ~ 1|country,
                                    correlation = corCompSymm(),
                                    data = df_malaria)
mod_malaria_randInt_AR1_catT <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + as.factor(time),
                               random = ~ 1|country,
                               correlation = corAR1(),
                               data = df_malaria)
mod_malaria_randInt_compSymm_splineT <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + time + time_gt2010,
                                         random = ~ 1|country,
                                         correlation = corCompSymm(),
                                         data = df_malaria)
mod_malaria_randInt_AR1_splineT <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + time + time_gt2010,
                                    random = ~ 1|country,
                                    correlation = corAR1(),
                                    data = df_malaria)

# Testing if we want random slope or just random intercept is good enough:
# For CS var covar
anova(mod_malaria_randIntSlope_compSymm, mod_malaria_randInt_compSymm) # time continuous
# based on AIC and LRT, chose random slope
# For AR1 var covar
anova(mod_malaria_randIntSlope_AR1, mod_malaria_randInt_AR1) # time continuous
# based on AIC and LRT, want random slope

# Do we want CS or AR1 var covar structure? Let time be continuous/categorical/spline to increase flexibility of our model
anova(mod_malaria_randIntSlope_compSymm_catT, mod_malaria_randInt_compSymm_catT) # time as a factor
anova(mod_malaria_randIntSlope_AR1_catT, mod_malaria_randInt_AR1_catT) # time as a factor
anova(mod_malaria_randIntSlope_AR1_splineT, mod_malaria_randInt_AR1_splineT) # spline for time with knot at 2010
# Comparing covariance strucutres, based on AIC we pick AR1


# BLUPs for our fav model
df_malaria$blup <- fitted(mod_malaria_randIntSlope_AR1_splineT)

# ugly plots: Plot BLUPS and TRUE log disease values over time
malaria_p_blup <- ggplot(aes(x = time, y = blup, group = country), data = df_malaria) +
          geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
          labs(x = "Year", y = "log(Incidence Rate)", title = "Malaria") +
          theme_bw() + theme(legend.position = "none")
malaria_p_true <- ggplot(aes(x = time, y = log_disease_value, group = country), data = df_malaria) +
      geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
      labs(x = "Year", y = "log(Incidence Rate)", title = "Malaria") +
      theme_bw() + theme(legend.position = "none")

plot_grid(malaria_p_true, malaria_p_blup)

# pick a country and plot
my_country <- unique(totalData_0616$country)[4]
ggplot(aes(x = time, group = country), data = df_malaria %>% filter(country %in% my_country)) +
  geom_line(aes(y = blup, color = country), linetype = "dashed") +
      geom_point(aes(y = blup, color = country, shape = "1"), size = 3) + 
  geom_line(aes(y = log_disease_value, color = country)) +
      geom_point(aes(y = log_disease_value, color = country, shape = "2"), size = 3) + 
  labs(x = "Year", y = "log(Incidence Rate)", title = "Malaria") +
  theme_bw() + theme(legend.position = "none")



# --------------------------------------
# Outcome: Dengue
# --------------------------------------

# filter to include only dengue cases
df_dengue <- complete_0616 %>%
  filter(cause_name == "Dengue") %>%
  mutate(time = as.numeric(as.factor(year))) %>% 
  mutate(log_disease_value = log(disease_value + min_dengue)) %>% 
  mutate(time_gt2010 = ifelse(year > 2010, time, 0))

# fit model with random slope and random intercept
mod_dengue_randIntSlope_compSymm <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + time,
                                        random = ~ 1 + time|country,
                                        correlation = corCompSymm(),
                                        data = df_dengue)
mod_dengue_randIntSlope_AR1 <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + time,
                                   random = ~ 1 + time|country,
                                   correlation =  corAR1(),
                                   data = df_dengue,
                                   control = lmeControl(maxIter = 100, msMaxIter = 100))
mod_dengue_randIntSlope_compSymm_catT <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + as.factor(time),
                                             random = ~ 1 + time|country,
                                             correlation = corCompSymm(),
                                             data = df_dengue)
mod_dengue_randIntSlope_AR1_catT <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + as.factor(time),
                                        random = ~ 1 + time|country,
                                        correlation =  corAR1(),
                                        data = df_dengue,
                                        control = lmeControl(maxIter = 100, msMaxIter = 100))
mod_dengue_randIntSlope_compSymm_splineT <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + time + time_gt2010,
                                                random = ~ 1 + time|country,
                                                correlation = corCompSymm(),
                                                data = df_dengue)
mod_dengue_randIntSlope_AR1_splineT <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + time + time_gt2010,
                                           random = ~ 1 + time|country,
                                           correlation =  corAR1(),
                                           data = df_dengue,
                                           control = lmeControl(maxIter = 100, msMaxIter = 100))


# fit model with random intercept
mod_dengue_randInt_compSymm <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + time,
                                   random = ~ 1|country,
                                   correlation = corCompSymm(),
                                   data = df_dengue)
mod_dengue_randInt_AR1 <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + time,
                              random = ~ 1|country,
                              correlation = corAR1(),
                              data = df_dengue,
                              control = lmeControl(maxIter = 100, msMaxIter = 100))
mod_dengue_randInt_compSymm_catT <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + as.factor(time),
                                        random = ~ 1|country,
                                        correlation = corCompSymm(),
                                        data = df_dengue)
mod_dengue_randInt_AR1_catT <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + as.factor(time),
                                   random = ~ 1|country,
                                   correlation = corAR1(),
                                   data = df_dengue,
                                   control = lmeControl(maxIter = 100, msMaxIter = 100))
mod_dengue_randInt_compSymm_splineT <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + time + time_gt2010,
                                           random = ~ 1|country,
                                           correlation = corCompSymm(),
                                           data = df_dengue)
mod_dengue_randInt_AR1_splineT <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + time + time_gt2010,
                                      random = ~ 1|country,
                                      correlation = corAR1(),
                                      data = df_dengue,
                                      control = lmeControl(maxIter = 100, msMaxIter = 100))

# Testing if we want ranomd slope or just random intercept is good enough:
# For CS var covar
anova(mod_dengue_randIntSlope_compSymm, mod_dengue_randInt_compSymm) # time continuous
# based on AIC and LRT, chose random slope
# For AR1 var covar
anova(mod_dengue_randIntSlope_AR1, mod_dengue_randInt_AR1) # time continuous
# based on AIC and LRT, want random slope

# Do we want CS or AR1 var covar structure? Let time be continuous/categorical/spline to increase flexibility of our model
anova(mod_dengue_randIntSlope_compSymm_catT, mod_dengue_randInt_compSymm_catT) # time as a factor
anova(mod_dengue_randIntSlope_AR1_catT, mod_dengue_randInt_AR1_catT) # time as a factor
anova(mod_dengue_randIntSlope_AR1_splineT, mod_dengue_randInt_AR1_splineT) # spline for time with knot at 2010
# Comparing covariance strucutres, based on AIC we pick AR1


# BLUPs for our fav model
df_dengue$blup <- fitted(mod_dengue_randIntSlope_AR1_splineT)



# --------------------------------------
# Outcome: Zika
# --------------------------------------

# IMPORTANT: because zika incidence is zero for all countries 2010 and earlier,
#             we only consider 2011 and onward in these models for zika (this also means
#             that we don't consider a spline)

# filter to include only zika cases
df_zika <- complete_0616 %>%
  filter(cause_name == "Zika virus") %>%
  mutate(time = as.numeric(as.factor(year))) %>% 
  mutate(log_disease_value = log(disease_value + min_zika)) %>% 
  mutate(time_gt2010 = ifelse(year > 2010, time, 0))


# fit model with random slope and random intercept
mod_zika_randIntSlope_compSymm <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + time,
                                      random = ~ 1 + time|country,
                                      correlation = corCompSymm(),
                                      data = df_zika %>% filter(year > 2010),
                                      control = lmeControl(maxIter = 100, msMaxIter = 100))
mod_zika_randIntSlope_AR1 <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + time,
                                 random = ~ 1 + time|country,
                                 correlation =  corAR1(),
                                 data = df_zika %>% filter(year > 2010),
                                 control = list(opt = "ucminf"))
mod_zika_randIntSlope_compSymm_catT <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + as.factor(time),
                                           random = ~ 1 + time|country,
                                           correlation = corCompSymm(),
                                           data = df_zika %>% filter(year > 2010))
mod_zika_randIntSlope_AR1_catT <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + as.factor(time),
                                      random = ~ 1 + time|country,
                                      correlation =  corAR1(),
                                      data = df_zika %>% filter(year > 2010),
                                      control = list(opt = "ucminf"))


# fit model with random intercept
mod_zika_randInt_compSymm <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + time,
                                 random = ~ 1|country,
                                 correlation = corCompSymm(),
                                 data = df_zika %>% filter(year > 2010))
mod_zika_randInt_AR1 <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + time,
                            random = ~ 1|country,
                            correlation = corAR1(),
                            data = df_zika %>% filter(year > 2010))
mod_zika_randInt_compSymm_catT <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + as.factor(time),
                                      random = ~ 1|country,
                                      correlation = corCompSymm(),
                                      data = df_zika %>% filter(year > 2010))
mod_zika_randInt_AR1_catT <- lme(fixed = log_disease_value ~ tropical + std_log_gdp_meas + urban_perc + refugee_perc + mean_temp + as.factor(time),
                                 random = ~ 1|country,
                                 correlation = corAR1(),
                                 data = df_zika %>% filter(year > 2010))


# Testing if we want ranomd slope or just random intercept is good enough:
# For CS var covar
anova(mod_zika_randIntSlope_compSymm, mod_zika_randInt_compSymm) # time continuous
# based on AIC and LRT, chose random slope
# For AR1 var covar
anova(mod_zika_randIntSlope_AR1, mod_zika_randInt_AR1) # time continuous
# based on AIC and LRT, want random slope

# Do we want CS or AR1 var covar structure? Let time be continuous/categorical/spline to increase flexibility of our model
anova(mod_zika_randIntSlope_compSymm_catT, mod_zika_randInt_compSymm_catT) # time as a factor
anova(mod_zika_randIntSlope_AR1_catT, mod_zika_randInt_AR1_catT) # time as a factor
# Comparing covariance strucutres, based on AIC we pick AR1


# BLUPs for our fav model
df_zika <- df_zika %>%
  filter(year > 2010)
df_zika$blup <- fitted(mod_zika_randIntSlope_AR1_catT)











