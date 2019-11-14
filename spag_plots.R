
# Spaghetti Plots


# useful packages
library(dplyr)
library(ggplot2)
library(cowplot)
library(reshape2)
library(kableExtra)


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
              "refugee_type", "num_persons")
# currently only considering total refugee population
totalData_0616 <- totalData_0616 %>%
  select(cur_vars) %>%
  filter(refugee_type %in% c("Total Population")) %>%
  mutate(total_refugee_pop = num_persons) %>%
  select(-c("refugee_type", "num_persons"))
# remove duplicated rows
totalData_0616 <- totalData_0616[!duplicated(totalData_0616), ]

# --------------------------------------
# Preview Data
# --------------------------------------

# TO DO:
# (1) Show the first five subjects’ data (the actual data).
# (2) List the variable names, say which one is the response, which ones are covariates.

# First, convert to wide format for display purposes
totalData_wide <- spread(totalData_0616, cause_name, disease_value)

preview <- totalData_wide %>%
  filter(country %in% unique(totalData_wide$country)[1:5]) %>% # select 5 countries
  arrange(country)

#write.csv(preview, paste0(wd, "data_preview.csv"))



# --------------------------------------
# Spaghetti Plots
# --------------------------------------

# For continous outcomes, use spaghetti plots to visualize 20 individuals’ responses over time;
# For non-continuous outcomes, plot 10 individuals’ responses over time in 10 separate panels.

# First, select 20 countries for plotting purposes
my_countries <- unique(totalData_0616$country)[1:20]

totalData_wide20 <- totalData_wide %>%
  filter(country %in% my_countries)

# For each outcome of dengue, encephalitis, malaria, yellow fever, and zika virus,  (incidence rate)

my_colors <- c("#000075", "#3cb44b", "#4363d8", "#42d4f4",
               "#469990", "#f58231", "#e6beff", "#9A6324", "#fffac8", "#800000", 
               "#f032e6", "#bfef45", "#fabebe", "#aaffc3", "#808000", "#911eb4", 
               "#ffd8b1", "#e6194B", "#a9a9a9", "#ffe119", "#ffffff")

# Dengue
plt_dengue <- ggplot(aes(x = year, y = Dengue, group = country), data = totalData_wide20) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "Year", y = "Incidence Rate", title = "Dengue") +
  theme_bw() + theme(legend.position = "none") + scale_color_manual(values = my_colors)

# Encephalitis
plt_encephalitis <- ggplot(aes(x = year, y = Encephalitis, group = country), data = totalData_wide20) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "Year", y = "Incidence Rate", title = "Encephalitis") +
  theme_bw() + theme(legend.position = "none") + scale_color_manual(values = my_colors)

# Malaria
plt_malaria <- ggplot(aes(x = year, y = Malaria, group = country), data = totalData_wide20) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "Year", y = "Incidence Rate", title = "Malaria") +
  theme_bw() + theme(legend.position = "none") + scale_color_manual(values = my_colors)

# Yellow fever
plt_yellow_fever <- ggplot(aes(x = year, y = `Yellow fever`, group = country), data = totalData_wide20) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "Year", y = "Incidence Rate", title = "Yellow Fever") +
  theme_bw() + theme(legend.position = "none") + scale_color_manual(values = my_colors)

# Zika virus
plt_zika <- ggplot(aes(x = year, y = `Zika virus`, group = country), data = totalData_wide20) +
  geom_line(aes(color = country)) + geom_point(aes(color = country)) + 
  labs(x = "Year", y = "Incidence Rate", title = "Zika Virus") + guides(color=guide_legend(ncol=2)) +
  theme_bw() + scale_color_manual(values = my_colors) # + theme(legend.position = "none")

plot_grid(plt_dengue, plt_encephalitis, plt_malaria, 
                    plt_yellow_fever, plt_zika, nrow = 3)









