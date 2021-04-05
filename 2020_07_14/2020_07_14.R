# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: 

install.packages("tidytuesdayR")
install.packages("janitor")

# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load(2020, week = 29)

astronauts <- tuesdata$astronauts

# Or read in the data manually

library(tidyverse)
library(janitor)

astronauts <- clean_names() %>% 
  filter(!is.na(number)) %>%  # remove last row (all values are NA)
  mutate(
    sex = if_else(sex == "M", "male", "female"),
    military_civilian = if_else(military_civilian == "Mil", "military", "civilian"))
#© 2020 GitHub, Inc.

astronauts_grouped <- astronauts %>% 
  select(year_of_mission, year_of_birth, nationality, sex) %>%
  mutate(age = year_of_mission - year_of_birth)
  
astronauts_continent <- astronauts_grouped %>% 
  mutate(continent = recode(nationality, 'U.K.' = "Europe", 'Brazil' = "S. America", 'U.S.' = "N. America", 'U.S.S.R/Russia' = "Europe", 'Mongolia' = "Asia", 'Romania' = "Europe", 'France' = "Europe", 'Czechoslovakia' = "Europe", 'Poland' = "Europe", 'Germany' = "Europe", 'Bulgaria' = "Europe", 'Hungry' = "Europe", 'Vietnam' = "Asia", 'Cuba' = "N. America", 'India' = "Asia", 'Canada' = "N. America", 'Saudi Arabia' = "Asia", 'Netherland' = "Europe", 'Mexico' = "N. America", 'Syria' = "Asia", 'Afghanistan' = "Asia", 'Japan' = "Asia", "Austria" = "Europe", 'Belgium' = "Europe", 'Switzerland' = "Europe", 'Italy' = "Europe", 'Australia' = "Australia", 'U.S.S.R/Ukraine' = "Europe", 'Spain' = "Europe", 'Slovakia' = "Europe", 'Republic of South Africa' = "Africa", 'Israel' = "Asia", 'China' = "Asia", 'Sweden' = "Europe", 'Malysia' = "Asia", 'Korea' = "Asia", 'U.K./U.S.' = 'N. America/Europe', 'Denmark' = "Europe", 'Kazakhstan' = "Asia", 'UAE' = "Asia"))


ggplot(data = astronauts_continent) + 
  geom_point(mapping = aes(x = year_of_mission, y = age, color = sex, shape = continent)) +
  scale_color_discrete("sex")+
  labs(x = "Mission Year", y = "Age", color = "Sex", shape = "Continent")

ggplot(data = astronauts_continent, mapping = aes(x = sex)) +
  geom_bar(mapping = aes(fill = continent))
