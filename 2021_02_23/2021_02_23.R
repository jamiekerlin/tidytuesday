### Tidy Tuesday 2021-02-23 ####################################
# Employed Status from U.S. Bureau of Labor Statistics 
# Created by Jamie Kerlin
# Created on 2021-02-23
###############################################################

### Load libraries ############################################
library(tidyverse)
library(tidytuesdayR)
library(here)
library(PNWColors)


### Load data #################################################
tuesdata <- tidytuesdayR::tt_load('2021-02-23')
employed <- tuesdata$employed

### Filter for data ##########################################
#I am interested in seeing trendsin 
#Farming, fishing, and forestry occupations over the years

employed_women <- employed %>%
  filter(minor_occupation == "Farming, fishing, and forestry occupations") %>%
  filter(race_gender == "Women") %>%
  filter(complete.cases(.)) %>% #take out rows that are incomplete
  select(!c(industry, major_occupation)) %>% #deselect columns 
  group_by(year) %>% #group by minor occupation and year
  summarise(women = sum(employ_n)) #sum of women each year

#same thing with total employees
employed_total <- employed %>%
  filter(minor_occupation == "Farming, fishing, and forestry occupations") %>%
  filter(race_gender == "TOTAL") %>%
  filter(complete.cases(.)) %>% #take out rows that are incomplete
  select(!c(industry, major_occupation)) %>% #deselect columns 
  group_by(year) %>% #group by minor occupation and year
  summarise(total = sum(employ_n)) #sum of each year

#now find for each race as well
employed_white <- employed %>%
  filter(minor_occupation == "Farming, fishing, and forestry occupations") %>%
  filter(race_gender == "White") %>%
  filter(complete.cases(.)) %>% #take out rows that are incomplete
  select(!c(industry, major_occupation)) %>% #deselect columns 
  group_by(year) %>% #group by minor occupation and year
  summarise(white = sum(employ_n)) #sum of each year

employed_black <- employed %>%
  filter(minor_occupation == "Farming, fishing, and forestry occupations") %>%
  filter(race_gender == "Black or African American") %>%
  filter(complete.cases(.)) %>% #take out rows that are incomplete
  select(!c(industry, major_occupation)) %>% #deselect columns 
  group_by(year) %>% #group by minor occupation and year
  summarise(black = sum(employ_n)) #sum of each year

employed_asian <- employed %>%
  filter(minor_occupation == "Farming, fishing, and forestry occupations") %>%
  filter(race_gender == "Asian") %>%
  filter(complete.cases(.)) %>% #take out rows that are incomplete
  select(!c(industry, major_occupation)) %>% #deselect columns 
  group_by(year) %>% #group by minor occupation and year
  summarise(asian = sum(employ_n)) #sum of each year

### Join data frames #########################################
employed_total <- left_join(employed_total, employed_women)
employed_total <- left_join(employed_total, employed_black)
employed_total <- left_join(employed_total, employed_white)
employed_total <- left_join(employed_total, employed_asian)

### Pivot longer ############################################
employed_total <- employed_total %>%
  pivot_longer(women:asian,
               names_to = "race_gender",
               values_to = "count")

### Rearrange columns so numeric columns together ##########
employed_total <- employed_total %>%
  relocate(where(is.numeric), 
           .after = where(is.character)) 

### Calculate percent of total each group ###################
employed_total <- employed_total %>%
  group_by(year, race_gender) %>%
  mutate(percent_of_total = ((count/total)*100))

### Filter out white people ################################
#Percent of white people is really high and hard to see
#trends for other groups 
employed_total <- employed_total %>%
  filter(race_gender != "white")

### Plotting ################################################
ggplot(data = employed_total, 
       mapping = aes(x = year, 
                     y = percent_of_total, 
                     color = race_gender)) + #set mapping
  geom_line(size = 1.2) + #change line size
  labs(title = "Percent of Total Employees in Farming, Fishing, and Forestry Occupations",
       subtitle = "Percent of Women, Black, and Asian Employees from 2015-2020",
       caption = "Data from the U.S. Bureau of Labor Statistics",
       x = "Year",
       y = "Percent of Total Employees",
       color = "Race/Sex") + #adding title, subtitle, caption, axis/legend titles
  scale_color_manual(values = pnw_palette(5)[c(5, 4, 2)]) + #setting color of lines
  theme(plot.background = element_rect(fill ="#f0f0e4"), #fill background
        panel.background = element_rect(fill = "#AFA690"), #fill panel
        legend.background = element_rect(fill = "#f0f0e4"), #fill legend background
        panel.grid.major = element_blank(), #take out major grid
        panel.grid.minor = element_line(color = "#f0f0e4", size = 0.1, linetype = "dotted"),
        #change minor gridline aesthetics
        plot.title = element_text(color = "#4a3a3b"), #change color of text
        plot.subtitle = element_text(color = "#4a3a3b"),
        plot.caption = element_text(color = "#4a3a3b", size = 12),
        axis.title = element_text(color = "#4a3a3b", size = 14),
        axis.text = element_text(color = "#4a3a3b", size = 10),
        legend.key = element_rect(fill = "#f0f0e4")) + #fill in legend key background
  ggsave(here("Output", "2021_02_23", "2021_02_23.png")) #save
  
