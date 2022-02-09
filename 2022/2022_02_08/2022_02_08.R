### Tidy Tuesday 2-8-2022 ##################################################
# Tuskegee Airmen
# Jamie Kerlin
# Created on 2022_02_08
############################################################################

### Load libraries #########################################################
library(tidyverse)
library(here)
library(tidytuesdayR)
library(maps)
library(ggmap)
library(geosphere)

register_google(key = "AIzaSyAezWIEOVUEFn8bZrRnxIoINQ2Pj8Uj-iw")

### Load data ##############################################################
tuesdata <- tidytuesdayR::tt_load('2022-02-08')
airmen <- tuesdata$airmen

### Clean data #############################################################
airmen1 <- airmen %>%
  mutate(rank = case_when(grepl("Lt", rank_at_graduation) ~ "Lieutenant",
                          grepl("Capt", rank_at_graduation) ~ "Captain",
                          grepl("Officer", rank_at_graduation) ~ "Officer")) %>%
  select(state, rank, number_of_aerial_victory_credits, pilot_type) %>%
  group_by(state, pilot_type) %>%
  mutate(total_credits = sum(number_of_aerial_victory_credits)) %>%
  select(!c(number_of_aerial_victory_credits, rank)) %>%
  unique() 

### Visualize data #########################################################
ggplot(data = airmen1, mapping = aes(x = state, y = total_credits)) +
  geom_bar(stat = "identity") +
  facet_wrap(~pilot_type)











### Make a map with where they traveled from #################################
map('state',
    col = "#b3baab", fill = TRUE, bg = "white", lwd = 0.05, border = 0)

airmen_loc <- airmen %>%
  select(state, military_hometown_of_record, graduated_from, graduation_date) %>%
  na.omit() %>%
  unite("location", c(military_hometown_of_record, state), sep = ", ") %>%
  mutate_geocode(location)

airmen_loc1 <- airmen_loc %>%
  rename("lat_hometown" = lat, 
        "lon_hometown" = lon) %>%
  mutate(lat_base = case_when(grepl("TAAF", graduated_from) ~ 32.491944,
                              grepl("Enid", graduated_from) ~ 36.339167,
                              grepl("Stewart", graduated_from) ~ 41.504167,
                              grepl("Williams", graduated_from) ~ 33.308056),
         lon_base = case_when(grepl("TAAF", graduated_from) ~ -85.775556,
                     grepl("Enid", graduated_from) ~ -97.916389,
                     grepl("Stewart", graduated_from) ~ -74.104722,
                     grepl("Williams", graduated_from) ~ -111.659722))

airmen_loc1 %>% write_csv(here("2022/2022_02_08/airmen_location.csv")) #write csv so don't have to wait for it to run again

airmen_loc1 <- read_csv(here("2022/2022_02_08/airmen_location.csv")) #read in csv if need to 

airmen_loc2 <- airmen_loc1 %>%
  select(!graduation_date) %>%
  unique()

points(x = airmen_loc2$lon_hometown, y = airmen_loc2$lat_hometown, col="slateblue", cex=3, pch=20)


plot_my_connection=function(lon_hometown, lat_hometown, lon_base, late_base, ...){
  inter <- gcIntermediate(c(lon_hometown, lat_hometown), c(lon_base, late_base), n=50, addStartEnd=TRUE, breakAtDateLine=F)             
  inter=data.frame(inter)
  diff_of_lon=abs(lon_hometown) + abs(lon_base)
  if(diff_of_lon > 180){
    lines(subset(inter, lon>=0), ...)
    lines(subset(inter, lon<0), ...)
  }else{
    lines(inter, ...)
  }
}

for(i in 1:nrow(airmen_loc2)){
  plot_my_connection(airmen_loc2$lon_hometown[i], airmen_loc2$lat_hometown[i], 
                     airmen_loc2$lon_base[i], airmen_loc2$lat_base[i], col="#69b3a2", lwd=1)
}

points(x = airmen_loc2$lon_hometown, y = airmen_loc2$lat_hometown, col="#69b3a2", cex=3, pch=20)

