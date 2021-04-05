#Download csv file on energy production from Tidy Tuesday Github
library(readr)
urlfile = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv"
energyprod <- read_csv(url(urlfile), stringsAsFactors = FALSE, check.names=FALSE)

#Load packages
library(tidyverse)
library(vegan)
library(gganimate)
library(ggplot2)
library(transformr)
library(gifski)
library(here)


#Filter by total net production for each country, select rows to use
totalnet <- energyprod %>% 
  filter(type == "Total net production") %>%
  select("country_name", "2016", "2017", "2018") %>%
  rename('Country' = "country_name") 

#Gather years into one column, exclude NA 
totalnet_long <- totalnet %>% gather("Year", "Net_Production", -Country) %>%
  na.exclude() %>%
  arrange(totalnet, desc(Country)) 

#Visualize data
ggplot(data = totalnet_long) + geom_bar(mapping = aes(x= Country, y = Net_Production), stat= "identity") + 
  theme(axis.text.x = element_text(angle=90)) + 
  ggtitle("Total Net Production by Year") +
  coord_flip()

##steps to animate plot
totalnet_long$Year <- as.integer(totalnet_long$Year)

# Make a ggplot, but add frame=year: one image per year
ggplot(data = totalnet_long) + geom_bar(mapping = aes(x= Country, y = Net_Production), stat= "identity") + 
  theme(axis.text.x = element_text(angle=90)) + 
  ggtitle("Total Net Production by Year") +
  coord_flip()+

  # gganimate specific bits:
  labs(title = 'Year: {frame_time}', x = 'Country', y = 'Net Production') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  transition_time(Year) +
  shadow_mark() +
  ease_aes('linear')

# Save as gif:
anim_save("output/totalnet_long.gif")

  