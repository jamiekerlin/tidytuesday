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
library(ggstream)


### Load data #################################################
tuesdata <- tidytuesdayR::tt_load('2021-03-16')
games <- tuesdata$games

### Clean data ################################################
games$month <- factor(games$month, levels=c("December", "November",
                                            "October", "September", "August",
                                            "July", "June", "May",
                                            "April", "March", 
                                            "February", "January"))

### Create graph ##############################################
ggplot(data = games,
       mapping = aes(x = month, 
                     y = gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  transition_states(
    year)


ggplot(data = games,
       mapping = aes(x = year, 
                     y = peak,
                     fill = month)) + 
  geom_stream()


ggplot(games, aes(year, gain, fill = month)) +
  geom_stream()









