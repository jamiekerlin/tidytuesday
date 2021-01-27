#Plastics 2021-01-26
#Created by Jamie Kerlin

install.packages("tidytuesdayR")
library(tidytuesdayR)
library(tidyverse)
library(fs)
install.packages("purrr")
library(purrr)
library(dplyr)

#Load data

data <- tidytuesdayR::tt_load('2021-01-26')
plastics <- data$plastics


#Separating into years

plastics2019 <- plastics %>% filter(year == '2019')
plastics2020 <- plastics %>% filter(year == "2020")

#Remove year & empty column

plastics2019 <- plastics2019 %>% select(!year) %>% select(!empty)
plastics2020 <- plastics2020 %>% select(!year) %>% select(!empty)

#Pivot_longer

plastics2019_longer <- plastics2019 %>% 
  pivot_longer(3:9, names_to = "recycling codes") %>%
  filter(value > 0)

plastics2020_longer <- plastics2020 %>%
  pivot_longer(3:9, names_to = "recycling_codes") %>%
  filter(value > 0)

#Separate into parent company source and grand total

plastics2019_company <- plastics2019_longer %>%
  select(country, parent_company, grand_total)

plastics2020_company <- plastics2020_longer %>%
  select(country, parent_company, grand_total)

#Remove duplicate rows

plastics2019_company <- plastics2019_company %>%
  unique()

plastics2020_company <- plastics2020_company %>%
  unique()

#Look at levels of parent_company column

levels(factor(plastics2019_company$parent_company))

#Find large companies of US- PepsiCo, Nestle, Coca-Cola

bigcompanies <- c("PepsiCo", "Nestle", "The Coca-Cola Company", 
                                "Pepsico", "Pepsi", "Coca-Cola",
                                "Nestlé")

plastics2019_bigcompany <- plastics2019_company %>%
  filter(parent_company %in% bigcompanies)

plastics2020_bigcompany <- plastics2020_company %>%
  filter(parent_company %in% bigcompanies)

plastics2019_bigcompany <- plastics2019_bigcompany %>%
  filter(country != "EMPTY")

plastics2020_bigcompany <- plastics2020_bigcompany %>%
  filter(country != "EMPTY")

#I want to make something that shows larger logos of the big companies
#in areas where they have the most plastic waste that was found on world map

library(ggplot2)
install.packages("ggmap")
library(ggmap)
library(maps)
install.packages("mapdata")
library(mapdata)

world <- map_data("world")

worldmap <- ggplot() + geom_polygon(data = world, aes(x = long, y= lat, 
                                          group = group)) + coord_fixed(1.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        axis.line = element_line(colour = "white"), legend.position = "none", 
        axis.ticks = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank())
worldmap

#Here I got the dataset country_centroids and working off this

country_coords <- country_centroids %>%
  select(c(name_long, Longitude, Latitude)) %>%
  rename('country' = "name_long")

#Join new dataset to other one

coords2019joined <- inner_join(plastics2019_bigcompany, country_coords, 
                               by = "country")

coords2020joined <- inner_join(plastics2020_bigcompany, country_coords, 
                               by = "country")
#Rename coca cola

coords2019joined <- coords2019joined %>%
  mutate(parent_company = recode(parent_company, 'The Coca-Cola Company' = "Coca Cola",
                          'PepsiCo' = "Pepsi"))

coords2020joined <- coords2020joined %>%
  mutate(parent_company = recode(parent_company, 'The Coca-Cola Company' = "Coca Cola",
                                 'PepsiCo' = "Pepsi"))

#See what it looks like with companies separated
coords2019pepsico <- coords2019joined %>%
  filter(parent_company == "PepsiCo")

#Ok now going to try to create map

#PEPSICO

world <- map_data("world")

pepsimap2019 <- ggplot() + geom_polygon(data = world, aes(x = long, y= lat, 
                                                      group = group)) + coord_fixed(1.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        axis.line = element_line(colour = "white"), legend.position = "none", 
        axis.ticks = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  ggtitle("2019 Plastic Pollution from Pepsi Company") +
  geom_point(data = coords2019pepsico, aes(x = Longitude, y = Latitude,
                                           color = "blue",
                                          size = grand_total))
pepsimap2019
              
                                           
#THE COCA-COLA COMPANY

coords2019cocacola <- coords2019joined %>%
  filter(parent_company == "The Coca-Cola Company")

#Ok now going to try to create map


world <- map_data("world")

cocacolamap2019 <- ggplot() + geom_polygon(data = world, aes(x = long, y= lat, 
                                                          group = group)) + coord_fixed(1.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        axis.line = element_line(colour = "white"), legend.position = "none", 
        axis.ticks = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  ggtitle("2019 Plastic Pollution from The Coca-Cola Company") +
  geom_point(data = coords2019cocacola, aes(x = Longitude, y = Latitude,
                                           color = "blue",
                                           size = grand_total))
cocacolamap2019


#NESTLE

coords2019nestle <- coords2019joined %>%
  filter(parent_company == "Nestlé")

#Ok now going to try to create map


world <- map_data("world")

nestlemap2019 <- ggplot() + geom_polygon(data = world, aes(x = long, y= lat, 
                                                             group = group)) + coord_fixed(1.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        axis.line = element_line(colour = "white"), legend.position = "none", 
        axis.ticks = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  ggtitle("2019 Plastic Pollution from Nestlé") +
  geom_point(data = coords2019nestle, aes(x = Longitude, y = Latitude,
                                            color = "blue",
                                            size = grand_total))
nestlemap2019


#Maybe try to overlay each map??

install.packages("gganimate")
library(gganimate)


all2019 <- ggplot() + geom_polygon(data = world, aes(x = long, y= lat, group = group)) + coord_fixed(1.3) +
  geom_point(data = coords2019joined, aes(x = Longitude, y = Latitude,
                                          color = parent_company,
                                          size = grand_total))+
  scale_size(range = c(2,8))+
  guides(color = guide_legend(title = "Company"))+
  guides(size = guide_legend(title = "Plastic Count")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        axis.line = element_line(colour = "white"), 
        axis.ticks = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  ggtitle("2019 Plastic Pollution from Large U.S. Companies")
  
all2019



                       
