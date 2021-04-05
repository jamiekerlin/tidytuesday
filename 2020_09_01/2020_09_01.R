##Tidy Tuesday 9-1-2020 
##Created by Jamie Kerlin 

#install package (first time)
install.packages("tidytuesdayR")

#load packages
library(tidytuesdayR)
library(tidyverse)

#load data for this week
data <- tidytuesdayR::tt_load('2020-09-01')
key_crop_yields <- data$key_crop_yields
arable_land <- data$arable_land_pin
fertilizer <- data$cereal_crop_yield_vs_fertilizer_application
land_use <- data$land_use_vs_yield_change_in_cereal_production
tractor <- data$cereal_yields_vs_tractor_inputs_in_agriculture

#rename file

crops <- key_crop_yields

#renaming columns
names(crops)[2] <- "country_code"
names(crops)[14] <- "bananas"


#select focus variables only (not using country codes)
bananas_by_country <- crops %>% select(country_code, bananas) %>%
  group_by(country_code) %>%
  summarize(Mean_Banana_Yield = mean(bananas)) 

#na.omit 
bananas_by_country <- bananas_by_country %>% na.omit(bananas_by_country$bananas)
bananas_by_country <- bananas_by_country %>% na.omit(bananas_by_country$country_code)


#create heat map of worlds mean banana yield per country
install.packages("rworldmap")
library(rworldmap)

bananas_joined <- joinCountryData2Map(bananas_by_country, joinCode = "ISO3", nameJoinColumn = "country_code")

mapDevice() #create world map shaped window
mapCountryData(bananas_joined,nameColumnToPlot='Mean_Banana_Yield', 
               mapTitle = "Mean Banana Yield by Country (tonnes per hectare)", 
               colourPalette= "heat",
               lwd = 1.5,
               borderCol = "black")
