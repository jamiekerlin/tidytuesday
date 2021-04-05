#HBCU Enrollment 2021-02-02
#Created by Jamie Kerlin

library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(scico)

#Load data
data <- tidytuesdayR::tt_load('2021-02-02')
hbcu_all <- data$hbcu_all
hbcu_black <- data$hbcu_black


#Rename columns
hbcu_all <- hbcu_all %>%
  rename("total_enrollment" = 'Total enrollment', "males" = 'Males',
         "females" = 'Females', "four_year" = '4-year', "two_year" = '2-year',
         "total_public" = 'Total - Public', "four_year_public" = '4-year - Public',
         "two_year_public" = '2-year - Public', "total_private" = 'Total - Private',
         "four_year_private" = '4-year - Private', "two_year_private" = '2-year - Private',
         "year" = 'Year')

#Create columns with percent of group enrollment for total enrollment
hbcu_percentenrollment <- hbcu_all %>%
  mutate(percent_male = (males/total_enrollment) * 100, 
         percent_female = (females/total_enrollment) * 100, 
         percent_two_year_all = (two_year/total_enrollment) * 100, 
         percent_four_year_all = (four_year/total_enrollment) * 100,
         percent_public_all = (total_public/total_enrollment) * 100, 
         percent_private_all = (total_private/total_enrollment) * 100)

#Create columns with percent change of male/female enrollment of total 
#enrollment over years
hbcu_percentchange <- hbcu_percentenrollment %>%
  mutate(pct_change_female = (percent_female - lag(percent_female)),
         pct_change_male = (percent_male - lag(percent_male)))

#Plot
#Following blog post steps by Otho Mantegazza 
#https://otho.netlify.app/

#3 different plots... 
#Plot 1: shows female enrollment in tens of thousands and arrows show
#change in percent of female enrollment of TOTAL enrollment between years
#Plot 2: shows female enrollment in tens of thousands and arrows show
#change in percent of female enrollment between years
#Plot 3: shows percent of female enrollment of total enrollment and arrows
#show percent change of female enrollment of total enrollment between years
#I shared Plot 2 to twitter

################################################################
#Plot of female enrollment and percent change

hbcu_percentchange <- hbcu_percentchange %>%
  mutate(females_10000 = females/10000)

#Change color palette & center divergent palette
lim <- 
  hbcu_percentchange$pct_change_female %>% 
  range() %>% 
  abs() %>% 
  max()

#Setting theme
theme_set(
  theme_minimal() +
    theme(text = element_text(family = "Arial Narrow",
                              colour = "grey40",
                              size = 11),
          axis.title = element_text(size = 14),
          plot.title = element_text(colour = "grey20",
                                    face = "bold",
                                    size = 18),
          plot.subtitle = element_text(face = "bold",
                                       size = 12),
          aspect.ratio = .6,   
          plot.margin = margin(t = 10, r = 15, b = 0, l = 10,
                               unit = "mm"))
)


#Set basic aesthetic mapping

female_enrollment_plot <-hbcu_percentchange %>%
  mutate(yend = females_10000 + (pct_change_female)) %>%
  ggplot(aes(x = year, y = females_10000)) +
  geom_segment(aes(yend = yend, #Add first geometric objects
                   xend = ..x..,
                   colour = pct_change_female),
               size = 2, arrow = arrow(length = unit(3, "mm"),
                                       type = "closed")) +
  geom_point(colour = "grey40", size = 2) +
  #Add text to specify percent changes
  geom_text(nudge_y = 1, aes(y = case_when(pct_change_female > 0 ~ yend + .5,
                                                        TRUE ~ yend - 2.5),
                                          label = pct_change_female %>%
                                            round(digits = 1) %>% paste0("%"),
                                          colour = pct_change_female),
            angle = 90,
            size = 3.5) +
  scale_colour_scico(palette = "roma",
                     direction = 1,
                     limits = c(-lim, lim) * 
                       max(abs(hbcu_percentchange$pct_change_female)),
                     guide = FALSE) +
  #Add labels/title
  labs(title = "Female Enrollment in HBCUs",
       subtitle = str_wrap("Between 1976 and 2015, with total HBCU enrollment
       (in tens of thousands) and percent change compared to the previous year of data."),
       y = "Females Enrolled (Tens of thousands)",
       x = "Year")

female_enrollment_plot
############################################################

#Plot of female enrollment and percent change
#OF JUST WOMEN ENROLLMENT

hbcu_percentchange <- hbcu_percentchange %>%
  mutate(females_10000 = females/10000)

hbcu_percentchange <- hbcu_percentchange %>%
  mutate(pct_change = (females_10000/lag(females_10000) - 1) * 100)

#Change color palette & center divergent palette
lim <- 
  hbcu_percentchange$pct_change %>% 
  range() %>% 
  abs() %>% 
  max()

#Setting theme
theme_set(
  theme_minimal() +
    theme(text = element_text(family = "Arial Narrow",
                              colour = "grey40",
                              size = 11),
          axis.title = element_text(size = 14),
          plot.title = element_text(colour = "grey20",
                                    face = "bold",
                                    size = 18),
          plot.subtitle = element_text(face = "bold",
                                       size = 12),
          aspect.ratio = .6,   
          plot.margin = margin(t = 10, r = 15, b = 0, l = 10,
                               unit = "mm"))
)


#Set basic aesthetic mapping

female_enrollment_plot <-hbcu_percentchange %>%
  mutate(yend = females_10000 + (pct_change)) %>%
  ggplot(aes(x = year, y = females_10000)) +
  geom_segment(aes(yend = yend, #Add first geometric objects
                   xend = ..x..,
                   colour = pct_change),
               size = 2, arrow = arrow(length = unit(3, "mm"),
                                       type = "closed")) +
  geom_point(colour = "grey40", size = 2) +
  #Add text to specify percent changes
  geom_text(nudge_y = 1, aes(y = case_when(pct_change > 0 ~ yend + .5,
                                           TRUE ~ yend - 2.5),
                             label = pct_change %>%
                               round(digits = 1) %>% paste0("%"),
                             colour = pct_change),
            angle = 90,
            size = 3.5) +
  scale_colour_scico(palette = "roma",
                     direction = 1,
                     limits = c(-lim, lim) * 
                       max(abs(hbcu_percentchange$pct_change)),
                     guide = FALSE) +
  #Add labels/title
  labs(title = "Female Enrollment in HBCUs",
       subtitle = str_wrap("Between 1976 and 2015, with total HBCU enrollment
       (in tens of thousands) and percent change compared to the previous year of data."),
       y = "Females Enrolled (Tens of thousands)",
       x = "Year")

female_enrollment_plot

############################################################
#Plot of percent of total enrollment and percent change 

#Change color palette & center divergent palette
lim <- 
  hbcu_percentchange$pct_change_female %>% 
  range() %>% 
  abs() %>% 
  max()

#Setting theme
theme_set(
  theme_minimal() +
    theme(text = element_text(family = "Arial Narrow",
                              colour = "grey40",
                              size = 11),
          axis.title = element_text(size = 14),
          plot.title = element_text(colour = "grey20",
                                    face = "bold",
                                    size = 18),
          plot.subtitle = element_text(face = "bold",
                                       size = 12),
          aspect.ratio = .6,   
          plot.margin = margin(t = 10, r = 15, b = 0, l = 10,
                               unit = "mm"))
)


#Set basic aesthetic mapping

pct_change_plot <-hbcu_percentchange %>%
  mutate(yend = percent_female + (pct_change_female)) %>%
  ggplot(aes(x = year, y = percent_female)) +
  geom_segment(aes(yend = yend, #Add first geometric objects
                   xend = ..x..,
                   colour = pct_change_female),
               size = 2, arrow = arrow(length = unit(1.5, "mm"),
                                       type = "closed")) +
  geom_point(colour = "grey40", size = 2) +
#Add text to specify percent changes
  geom_text(nudge_x = .5, nudge_y = .5, aes(y = case_when(pct_change_female > 0 ~ yend + .5,
                              TRUE ~ yend - 1.2),
                label = pct_change_female %>%
                  round(digits = 1) %>% paste0("%"),
                colour = pct_change_female),
            angle = 90,
            size = 3.5) +
  scale_colour_scico(palette = "roma",
                     direction = 1,
                     limits = c(-lim, lim) * 
                       max(abs(hbcu_percentchange$pct_change_female)),
                     guide = FALSE) +
#Add labels/title
  labs(title = "Female Enrollment in HBCUs",
       subtitle = str_wrap("Between 1976 and 2015, with percent of total 
                           HBCU enrollment and percent change compared to the 
                           previous year of data."),
       y = "Female Percentage of Total Enrollment",
       x = "Year")

pct_change_plot

