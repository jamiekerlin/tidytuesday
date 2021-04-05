### Tidy Tuesday 2021-03-09 ####################################
# Bechdel Test
# Created by Jamie Kerlin
# Created on 2021-03-09
###############################################################

### Load libraries ############################################
library(tidyverse)
library(tidytuesdayR)
library(here)
library(maps)
library(mapdata)
library(mapproj)
library(gridExtra)

### Load data #################################################
tuesdata <- tidytuesdayR::tt_load('2021-03-09')
raw_bechdel <- tuesdata$raw_bechdel
movie_bechdel <- tuesdata$movies
world <- map_data("world")

### Clean data ###############################################
movies <- movie_bechdel %>% 
  select(c(year, country, genre, awards, language, binary)) %>% #select columns
  filter(complete.cases(.)) %>% #filter for complete cases
  separate(country, c("country1", #separate country column into multiple
           "country2", 
           "country3",
           "country4",
           "country5",
           "country6",
           "country7",
           "country8"),
           extra = "drop", #fix issues with not all having same # of countries
           fill = "right",
           sep = ",") %>% #separate by commas
  pivot_longer(2:9, #pivot country columns longer
               values_to = "country") %>% #country values to new column
  filter(complete.cases(.)) %>% #filter by complete cases again
  mutate(country = gsub(" ", "", country)) %>% #issue with spaces so take out all 
  mutate(country = recode(country, "WestGermany" = "Germany")) %>%
  group_by(country) %>% #group by year and country
  count(binary) %>% #count Pass/fail by year and country
  pivot_wider(names_from = binary, #pivot wider 
              values_from = n) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>% #make NAs 0s for binary
  group_by(country) %>% #group by country
  mutate(difference = (PASS - FAIL)) %>% #new column of difference in movies
  mutate(pos = difference >= 0) #setting column that lists true for positive, false for negative

movies_noUS <- movies %>% #separate US because way higher difference
  filter(country != "USA")

movies_US <- movies %>% #create new one with just US data
  filter(country == "USA")

### Join data sets to get world map dataset #######################
world_countries <- world %>%
  rename("country" = region) #rename for matching column names 

world_bechdel <- full_join(world_countries, movies_noUS)

#now add just the US data to create US map 
usa <- world_countries %>%
  filter(country == "USA")#filter for US
usa_bechdel <- full_join(usa, movies_US) #join data sets

### Create world map #############################################
ggplot() + 
  geom_polygon(data = world_bechdel, #get data for world map
               aes(x = long, #create world map
                   y = lat, 
                   group = group,
                   fill = difference), #create region colors
               color = "black") + #region outlines
  coord_map(projection = "mercator", #set projection type
            xlim = c(-180, 180)) +
  scale_fill_viridis_c() + #change color
  theme_minimal() + #change theme 
  #change title, subtitle, and fill titles
  labs(title = "Differences between number of movies passing or failing the Bechdel Test",
       subtitle = "Separated by country between the years 1970 and 2013,
with positive values indicating a higher number of passing movies
       
     
        
        ",
     
       fill = "Difference between passing and failing movies") +
  theme(legend.position = c(0.3, 1.1), #change legend position and direction
        legend.direction = "horizontal") +
  ggsave(here("Output", "2021_03_09", "2021_03_09_worldmap.png"))

### Create USA Map ########################################################
ggplot() + 
  geom_polygon(data = usa_bechdel, #get data for world map
               aes(x = long, #create world map
                   y = lat, 
                   group = group,
                   fill = difference)) + #fill by differences
  coord_map(projection = "mercator", #set projection type
            xlim = c(-180, -50)) + #set x lim to focus on USA
  scale_fill_viridis_c(begin = 0) + #change color theme
  theme_minimal() + #change theme
  theme(legend.position = c(0.3, 1.1),  #est legend position and direction
        legend.direction = "horizontal") +
  #setting title, subtitle, and fill title
  labs(title = "Differences between number of movies passing or failing the Bechdel Test",
       subtitle = "In the USA between the years 1970 and 2013,
with positive values indicating a higher number of passing movies
       
     
        
        ",
       
       fill = "Difference between passing and failing movies") +
  ggsave(here("Output", "2021_03_09", "2021_03_09_usamap.png"))

