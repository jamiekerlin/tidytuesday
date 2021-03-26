### Tidy Tuesday 2021-03-23 ####################################
# UN Votes
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
tuesdata <- tidytuesdayR::tt_load('2021-03-23')
unvotes <- tuesdata$unvotes
rollcall <- tuesdata$roll_calls

ggplot(unvotes, aes(year, gain, fill = month)) +
  geom_stream()

