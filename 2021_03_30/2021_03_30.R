### Tidy Tuesday 2021-03-30 ####################################
# UN Votes
# Created by Jamie Kerlin
# Created on 2021-04-03
###############################################################

### Load libraries ############################################
library(tidyverse)
library(tidytuesdayR)
library(here)
library(PNWColors)
library(ggraph)
library(igraph)
library(RColorBrewer) 


### Load data #################################################
tuesdata <- tidytuesdayR::tt_load('2021-03-30') 
data <- tuesdata$allShades #save data to the environment

### Clean data ###############################################
data_clean <- data %>%
  select(!c(url, imgSrc, imgAlt, name, specific)) %>% #deselect rows I won't use
  filter(grepl("Foundation", product)) #filter for only foundation products

data_compare <- data_clean %>% #choose 4 different types of brands to compare
  filter(brand == "MAC" | brand == "Maybelline" | brand == "Gucci" | brand == "FENTY BEAUTY by Rihanna") 

### Plot data ################################################
ggplot(data = data_compare,
       mapping = aes(x = lightness, fill = brand)) + #set x axis and fill color
  geom_bar() + #geom bar plot
  facet_wrap(~brand) + #facet wrap by brand 
  guides(fill = FALSE) + #remove legend
  theme_classic() + #set theme
  labs(x = "Lightness", #label x axis, y axis, title, and caption
       y = "Count",
       title = "Amount of foundation products available in shade ranges",
       caption = "Tidy Tuesday | Data from The Pudding") +
  ggsave(here("Output",
              "2021_03_30",
              "2021_03_30_plot.png"))
    