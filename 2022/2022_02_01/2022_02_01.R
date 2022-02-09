### Tidy Tuesday 2-1-2022 ##################################################
# Tidy tuesday looking at dog breeds
# Jamie Kerlin
# Created on 2022_02_01
############################################################################

### Load libraries #########################################################
library(tidyverse)
library(here)
library(tidytuesdayR)
library(janitor)
library(ggstream)
library(streamgraph)

### Load data ##############################################################
breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

breed_traits %>% write_csv(here("2022/2022_02_01/breed_traits.csv"))

breed_traits <- clean_names(breed_traits)
breed_rank_all <- clean_names(breed_rank_all)
breed_traits$breed <- breed_rank_all$breed

### Work with breed trait data #############################################
breeds_long <- breed_traits %>%
  pivot_longer(cols = c(2:7, 10:17),
                names_to = "trait",
                values_to = "score") %>%
  group_by(breed, coat_type, coat_length) %>%
  mutate(total_score = sum(score))

### Join data ##############################################################
breed_rank <- right_join(traits_long, breed_rank_all) 

### Work with joined data ##################################################
breed_rank1 <- breed_rank %>%
  group_by(breed, trait, score) %>%
  pivot_longer(cols = x2013_rank:x2020_rank,
               names_to = "year", 
               values_to = "rank") %>%
  mutate(average_rank = mean(rank), 
         percent_score = (total_score/70) * 100)

breed_rank2 <- breed_rank1 %>%
  pivot_wider(names_from = trait,
              values_from = score) %>%
  select(c(breed, percent_score, average_rank)) %>%
  unique() %>%
  na.omit()

breed_p80 <- breed_rank2 %>%
  group_by(breed) %>%
  filter(percent_score >= 80) %>%
  mutate(prank = 1)

breed_p70 <- breed_rank2 %>%
  group_by(breed) %>%
  filter(percent_score >= 70 & percent_score < 80) %>%
  mutate(prank = 2)

breed_p60 <- breed_rank2 %>%
  group_by(breed) %>%
  filter(percent_score >= 60 & percent_score < 70) %>%
  mutate(prank = 3)

breed_p50 <- breed_rank2 %>%
  group_by(breed) %>%
  filter(percent_score >= 50 & percent_score < 60) %>%
  mutate(prank = 4)

breed_p40 <- breed_rank2 %>%
  group_by(breed) %>%
  filter(percent_score >= 40 & percent_score < 50) %>%
  mutate(prank = 5)

breed_p30 <- breed_rank2 %>%
  group_by(breed) %>%
  filter(percent_score >= 30 & percent_score < 40) %>%
  mutate(prank = 6)

breed_p20 <- breed_rank2 %>%
  group_by(breed) %>%
  filter(percent_score >= 20 & percent_score < 30) %>%
  mutate(prank = 7)

breed_p10 <- breed_rank2 %>%
  group_by(breed) %>%
  filter(percent_score >= 10 & percent_score < 20) %>%
  mutate(prank = 8)

breed_p0 <- breed_rank2 %>%
  group_by(breed) %>%
  filter(percent_score < 10) %>%
  mutate(prank = 9)


combined <- rbind(breed_p80, breed_p70, breed_p60, breed_p50, breed_p40, breed_p30, breed_p20, breed_p10, breed_p0)

breed_r1 <- combined %>%
  group_by(breed) %>%
  filter(average_rank <= 20) %>%
  mutate(rank_group = 1)

breed_r2 <- combined %>%
  group_by(breed) %>%
  filter(average_rank <= 40 & average_rank > 20) %>%
  mutate(rank_group = 2)

breed_r3 <- combined %>%
  group_by(breed) %>%
  filter(average_rank <= 60 & average_rank > 40) %>%
  mutate(rank_group = 3)

breed_r4 <- combined %>%
  group_by(breed) %>%
  filter(average_rank <= 80 & average_rank > 60) %>%
  mutate(rank_group = 4)

breed_r5 <- combined %>%
  group_by(breed) %>%
  filter(average_rank <= 100 & average_rank > 80) %>%
  mutate(rank_group = 5)

breed_r6 <- combined %>%
  group_by(breed) %>%
  filter(average_rank <= 120 & average_rank > 100) %>%
  mutate(rank_group = 6)

breed_r7 <- combined %>%
  group_by(breed) %>%
  filter(average_rank <= 140 & average_rank > 120) %>%
  mutate(rank_group = 7)

breed_r8 <- combined %>%
  group_by(breed) %>%
  filter(average_rank <= 160 & average_rank > 140) %>%
  mutate(rank_group = 8)

breed_r9 <- combined %>%
  group_by(breed) %>%
  filter(average_rank <= 180 & average_rank > 160) %>%
  mutate(rank_group = 9)

breed_r10 <- combined %>%
  group_by(breed) %>%
  filter(average_rank <= 200 & average_rank > 180) %>%
  mutate(rank_group = 10)

combined2 <- rbind(breed_r1, breed_r2, breed_r3, breed_r4, breed_r5, breed_r6, breed_r7, breed_r8, breed_r9, breed_r10) %>%
  mutate(difference = rank_group - prank)

combined3 <- left_join(combined2, breed_traits) %>%
  mutate(difference = rank_group - prank) 

combined3$barking_level <- as.factor(combined3$barking_level)
combined3$affectionate_with_family <- as.factor(combined3$affectionate_with_family)

combined4 <- left_join(combined3, breed_rank_all) %>%
  rename("2013" = x2013_rank, "2014" = x2014_rank, "2015" = x2015_rank, 
                       "2016" = x2016_rank, "2017" = x2017_rank, "2018" = x2018_rank,
                       "2019" = x2019_rank, "2020" = x2020_rank) %>%
  pivot_longer(cols = "2013":"2020",
               names_to = "year",
               values_to = "rank")

combined4$year <- as.factor(combined4$year)


ggplot(combined4, mapping = aes(x = year, y = difference, fill = barking_level)) +
  geom_stream()

    