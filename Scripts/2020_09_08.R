##Tidy Tuesday 9-8-2020
##Created by Jamie Kerlin

#load packages
library(tidytuesdayR)
library(tidyverse)
library(gganimate)
library(ggplot2)
library(transformr)
library(gifski)
library(here)
library(forcats)

#clear environment
rm(list=ls())


#load data for this week
data <- tidytuesdayR::tt_load('2020-09-08')
friends <- data$friends
friends_info <- data$friends_info
friends_emotions <- data$friends_emotions

#join two data sets
friends_joined <- left_join(friends, friends_emotions, by = c("season", "episode", "scene", "utterance"))

#filter by the main characters
main_characters <- c('Phoebe Buffay', 'Rachel Green', 'Joey Tribbiani', 'Chandler Bing', 'Monica Geller', 'Ross Geller')
characters <- filter(friends_joined, speaker %in% main_characters)

#take out na's
characters <- na.omit(characters)

#rename utterances as list
characters_data <- characters %>%
  mutate(utterance_number = 1:n())

#select columns that I want to use for each dataset
characters_data <- characters_data %>%
  select(speaker, utterance_number, emotion, season)

#count emotions
#character_data <- characters_data %>%
  #count(speaker, season, emotion)

#I want to show happiness levels of characters over time so filter by these emotions
main_emotions <- c("Joyful", "Peaceful", "Neutral", "Sad", "Mad")
characters_data <- filter(characters_data, emotion %in% main_emotions)

#recode emotions as ranking of happiness
character_data <- characters_data %>%
  mutate(emotion = recode(emotion, 'Joyful' = 10, 'Peaceful' = 7.5, 'Neutral' = 5, 'Sad' = 2.5, 'Mad' = 0))

#find average happiness level of characters
friends_avg <- character_data %>%
  group_by(speaker, season) %>%
  summarize(avg_happiness = mean(emotion))


#graphing
ggplot(friends_avg, mapping = aes(x = season, y = avg_happiness, color = speaker)) +
  geom_line(size = 1) +
  labs(title = 'Average Happiness by Season', x = 'Season', y = 'Average Happiness (on a scale of 1 to 10)', color = "Character") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggsave("Output/friends.png")

