### Tidy Tuesday 2021-04-06 ###################################
# Deforestation
# Created by Jamie Kerlin
# Created on 2021-04-05
###############################################################

### Load libraries ############################################
library(tidyverse)
library(tidytuesdayR)
library(ggstream)
library(PNWColors)
library(ggthemr)
library(here)

### Load data #################################################
tuesdata <- tidytuesdayR::tt_load('2021-04-06') #load data from tidytuesday
brazil <- tuesdata$brazil_loss #save data set to environment

### Clean data ################################################
brazil_data <- brazil %>%
  pivot_longer(cols = commercial_crops:small_scale_clearing, #pivot table longer
               names_to = "type", #name new name and value columns
               values_to = "loss") %>%
  #rename types of drivers for proper capitalization in plot legend
  mutate(type = recode(type, "commercial_crops" = "Commercial crops",
                             "fire" = "Fire",
                              "flooding_due_to_dams" = "Flooding due to dams",
                              "mining" = "Mining",
                              "natural_disturbances" = "Natural disturbances",
                              "other_infrastructure" = "Other infrastructure",
                              "pasture" = "Pasture",
                              "roads" = "Roads",
                              "selective_logging" = "Selective logging",
                              "small_scale_clearing" = "Small scale clearing",
                              "tree_plantations_including_palm" = "Tree plantations including palm")) 

### Plot #####################################################
ggthemr('light') #set theme using ggthemr package

plot <- ggplot(brazil_data, aes(year, loss, fill = type)) + #set aesthetics
  geom_stream() + #using geom stream graph
  scale_fill_manual(values = pnw_palette("Sailboat", 11)) + #set color palette for 11 colors
  labs(x = "Year", #change labels for x and y axes, title, and caption
       y = "Forest loss",
       title = "Drivers of Brazilian Deforestation from 2001 to 2013",
       caption = "Tidy Tuesday | Data: Our World in Data | Code: github.com/jamiekerlin") + 
  guides(fill = guide_legend(title = "Drivers")) + #change title for fill legend
  ggsave(here("Output", #save plot to output folder as png
              "2021_04_06",
              "2021_04_06_plot.png"))
