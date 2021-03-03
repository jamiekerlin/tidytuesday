### Tidy Tuesday 2021-03-02 ####################################
# Superbowl ads
# Created by Jamie Kerlin
# Created on 2021-03-02
###############################################################

### Load libraries ############################################
library(tidyverse)
library(tidytuesdayR)
library(here)
library(PNWColors)
library(gganimate)
library(gifski)
library(magick)


### Load data #################################################
tuesdata <- tidytuesdayR::tt_load('2021-03-02')
ads <- tuesdata$youtube

### Clean data ################################################
ads <- ads %>%
  select(year, brand, funny, patriotic, celebrity,
         danger, animals, use_sex, view_count) #select only columns I want
ads_long <- ads %>% #pivot longer by what they used
  pivot_longer(funny:use_sex,
               names_to = "uses",
               values_to = "values")
ads_long <- ads_long %>% #take out all FALSE rows
  filter(values == TRUE) %>%
  filter(brand == "Bud Light" | #filter for drink brands
         brand == "Budweiser" |
           brand == "Coca-Cola" |
           brand == "Pepsi")
ads_long <- ads_long %>%
  mutate(uses = recode(uses, 'animals' = "Animals", #rename so looks better in plot
                       "celebrity" = 'Celebrity',
                       "danger" = 'Danger',
                       "funny" = 'Humor',
                       "patriotic" = 'Patriotism',
                       "use_sex" = 'Sexuality'))


### Plot data #################################################
ads_long %>%
  ggplot(mapping = aes(x = uses, y = view_count, fill = uses)) + #map aesthetics
  geom_bar(stat = "identity") + #set bar to identity of view counts
  geom_jitter(color ="#26A0CE", #add jitter plot and change color to purple
                          alpha =0.5) +
  facet_wrap(~brand, ncol = 1, scales = "free") + #facet wrap by brand
  coord_flip() + #flip coordinates
  theme_classic() +  #change theme to classic
  scale_fill_manual(values = pnw_palette(1)[c(2, 3, 4, 5, 6, 7)]) + #fill colors
  labs(y = "View Count", #change x, y & fill labels & title, and subtitle
       x = "Characteristics Included",
       fill = "Characteristics",
       title = "View Counts of Superbowl Commercials from 2000-2020",
       subtitle = "By Brand and Commercial Characteristic") 
ggsave(here("Output", "2021_03_02", "superbowl_ads.png"))

  