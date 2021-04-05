avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')
scene_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/scene_description.csv')

library(tidyverse)
library(vegan)
library(gganimate)
library(ggplot2)
library(transformr)
library(gifski)
library(here)
library(qdap)


#select character, character_words, and imdb_rating
avatar_character <- avatar %>%
  select("book_num", "chapter_num", "character", "character_words", "imdb_rating")

#Gather years into one column, exclude NA 
avatar_long <- avatar_character %>% 
  na.exclude() %>%
  unite("book_chapter", book_num, chapter_num, sep = "_", remove = TRUE) %>%
  select("book_chapter", "character", "imdb_rating") %>%
  filter(character %in% c("Katara", "Sokka", "Aang", "Zuko")) %>%
  left_join(character, filter())
  
  #mutate(n = 1) %>%
  #mutate(temp = ifelse(n == 0, 1, 0)) %>%
  #group_by(character, imdb_rating, book_chapter) %>%
  #summarize(count = sum(temp))


ggplot(data = avatar_long) + geom_point(mapping = aes(x = character, y = book_chapter, color = imdb_rating)) +
  ggtitle("Character Line Count vs. IMDb rating per Episode")

