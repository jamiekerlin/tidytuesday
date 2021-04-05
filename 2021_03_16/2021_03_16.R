### Tidy Tuesday 2021-03-16 ####################################
# Gaming
# Created by Jamie Kerlin
# Created on 2021-03-22
###############################################################

### Load libraries ############################################
library(tidyverse)
library(tidytuesdayR)
library(here)
library(gganimate)
library(PNWColors)


### Load data #################################################
tuesdata <- tidytuesdayR::tt_load #load data from tidytuesday
games <- tuesdata$games #save data set to environment

### Clean data ################################################
games <- games %>%
  filter(year == "2021") %>% #filter by 2021
  mutate(avg_peak_percent = (avg/peak) * 100) %>% #recalculate so it is numeric
  select(!c(year, month, avg_peak_perc)) #deselect columns not needed

games1 <- games %>%
  filter(avg > 30000.0) %>% #filter for highest average simultaneous players
  pivot_longer(2:5, #pivot longer by all popularity variables
               names_to = "variables",
               values_to = "values")
  
games2 <- games3 %>% 
  group_by(variables) %>% #group by variables
  mutate(std_values = (values - mean(values))/sd(values)) #standardized values for heatmap


### Create graph ##############################################
pnw1 <- rev(pnw_palette(6)) #create color palette object

ggplot(games2, mapping = aes(x = variables, #x axis is variables
                             y = gamename, #y axis is videogame names
                             fill = std_values)) + #filling by standardized values
  geom_tile() + #geom tile for ggplot heatmap
  scale_fill_gradientn(colors = pnw1) + #add previously saved palette to plot
  labs(x = "Variables", #label x, y, legend, title, and caption
       y = "Name of videogame",
       fill = "Standardized Values",
       title = "2021 Popularity of Videogames on Steam",
       caption = "Data from SteamCharts | Code at github.com/jamiekerlin") +
  theme_light() + #set theme
  scale_y_discrete(limits=rev) + #reverse y axis order
  scale_x_discrete(labels = c("Average", "Percent Average Peak", "Gain", "Peak")) +
#rename variables
  ggsave(here("Output",
              "2021_03_16",
              "2021_03_16_heatmap.png"))

