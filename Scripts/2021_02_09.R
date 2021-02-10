#Tidy Tuesday 2021-02-09
#Wealth Inequality in America- Wealth and income over time
#Created by Jamie Kerlin
#Created on 2021-02-09

### Load libraries #######
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(viridis)
library(hrbrthemes)

#### Load data #########
data <- tidytuesdayR::tt_load('2021-02-09')
income_dist <- data$income_distribution 

### Clean data ######
#I want to only select the rows I will use- year, race, income_mean, income_mean_moe
income_dist <- income_dist %>%
  select(year, race, income_mean, income_mean_moe)

#Now I want to combine the race groups for the alone + alone or in combination
income_dist <- income_dist %>%
  mutate(race = recode(race, 'All Races' = "all races",
                       'Black Alone' = "black",
                       'White Alone' = "white",
                       'Asian Alone' = "asian",
                       'Hispanic (Any Race)' = "hispanic",
                       'Black Alone or in Combination' = "black",
                       'White Alone, Not Hispanic' = "white",
                       'Asian Alone or in Combination' = "asian"))

#Now need to combine the income distributions for now-matching rows
combined_dist <- income_dist %>%
  unique()

### Plotting #####
ggplot(data = combined_dist, mapping = aes(x = year,
                                           y = income_mean,
                                           color = race)) +
  geom_line(size = 1.3) +
  scale_color_viridis_d() +
  labs(title = "Racial Inequality in the U.S.", 
       subtitle = "Mean Income (USD) Trends from 1967-2019",
       x = "Year",
       y = "Mean Income (USD)",
       color = "Race") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
