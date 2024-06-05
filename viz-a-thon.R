library(tidyverse)
library(lubridate)

storms<-read_csv("~/Downloads/StormEvents.gz",col_types = cols(EPISODE_NARRATIVE="c",EVENT_NARRATIVE="c",WFO='c',EPISODE_ID='c',TOR_OTHER_CZ_STATE='c',SOURCE='c',BEGIN_LOCATION='c',END_LOCATION='c',TOR_OTHER_CZ_FIPS='c',TOR_OTHER_CZ_NAME='c',BEGIN_AZIMUTH='c',END_AZIMUTH='c',DAMAGE_PROPERTY="c",DAMAGE_CROPS='c',MAGNITUDE_TYPE='c',FLOOD_CAUSE='c',TOR_OTHER_WFO='c',CATEGORY='i'))


x<-storms %>% group_by(YEAR,EVENT_TYPE) %>% summarise(count=n())


ts<-dmy_hms(storms$BEGIN_DATE_TIME)
year(ts)<-storms$YEAR
month(ts)->storms$MONTH


storms$month <- month(ts)


storms %>% mutate(MONTH = month(ts))
test<- storms %>% filter(year(ts)==2018) %>% filter(EVENT_TYPE=="Tornado")


storms$begin_ts<-ts




storms <- storms %>% 
  # Do this all in one mutate command
  mutate(
    # Replace all K,M,B with empty string and assign result to new column dam_num
    dam_num=as.numeric(gsub("[KMB]","",DAMAGE_PROPERTY)),
    # Replace all digits / decimal point with empty string and assign result to new column dam_let
    dam_let=gsub("[0-9.]+","",DAMAGE_PROPERTY),
    # Use case_when to get the multiplier based on the letter and assign to colun dam_mult
    dam_mult=case_when(
      dam_let=="K" ~ 1000,
      dam_let=="M" ~ 10^6,
      dam_let=="B" ~ 10^9,
      # This last bit tells us to return a '1' if we haven't matched anything above
      TRUE ~ 1),
    # Calculate actual damage number and assign the result to prop damage
    prop_damage = dam_num*dam_mult)

#Eyeball the result
View(storms %>% select(DAMAGE_PROPERTY,prop_damage))

storms<- storms %>% mutate(prop_damage)



unique(storms$EVENT_TYPE)


storm2000<- storms %>% filter(YEAR>=2000)

newDF <- storm2000 %>% group_by(MONTH)%>% group_by(STATE) %>% filter(EVENT_TYPE == "Thunderstorm Wind") %>% filter(!is.na(prop_damage))%>% summarise(total_dam = sum(prop_damage))

newDF$state_name <- tolower(newDF$STATE)

us <- map_data("state")
us$state_name <- us$region

map <- ggplot(newDF, aes(map_id=state_name))
map <-  map + geom_polygon(data = us, aes(x=long, y = lat, group = group), fill = "white", color ="black") 
map <- map  + geom_map(map=us, aes(fill=total_dam))
map <- map + expand_limits(x=us$long, y=us$lat)
map <- map + coord_map() + ggtitle("Total Property Damage")
map
map + scale_fill_viridis_c()
ggsave("Total Property Damage.pdf")






thunder2018<- storms %>% filter(YEAR==2018) %>% filter(EVENT_TYPE == "Thunderstorm Wind")
head(thunder2018)

typeof(storms$MAGNITUDE)

thunder2018 %>% group_by(BEGIN_LAT, BEGIN_LON, MAGNITUDE)%>% filter(!is.na(BEGIN_LAT)) %>% filter(!is.na(BEGIN_LON)) %>%filter(!is.na(MAGNITUDE)) %>% filter(MAGNITUDE>60)%>% ggplot() + geom_point(aes(x=BEGIN_LON, y=BEGIN_LAT, size=MAGNITUDE, color=MAGNITUDE))


#largest thunderstorms in 2018
torStorm <- storm2000 %>% filter(EVENT_TYPE == "Thunderstorm Wind")
view(torStorm)

#thunderstorm wind
#frequency by month


magDF<- storm2000 %>% group_by(STATE) %>% filter(EVENT_TYPE == "Thunderstorm Wind") %>% filter(!is.na(prop_damage))%>% filter(!is.na(MAGNITUDE))%>%summarise(ave_mag = mean(MAGNITUDE))

magDF$state_name <- tolower(magDF$STATE)

us <- map_data("state")
us$state_name <- us$region

map <- ggplot(magDF, aes(map_id=state_name))
map <-  map + geom_polygon(data = us, aes(x=long, y = lat, group = group), fill = "white", color ="black") 
map <- map  + geom_map(map=us, aes(fill=ave_mag))
map <- map + expand_limits(x=us$long, y=us$lat)
map <- map + coord_map() + ggtitle("Average Thunderstorm Wind Magnitude")
map
map + scale_fill_viridis_c()
ggsave("Average_Thunderstorm_Wind_ Magnitude.pdf")

view(thunder2018)

library(RColorBrewer)

thunder2018 %>% group_by(BEGIN_LAT, BEGIN_LON, MAGNITUDE, MONTH)%>% filter(!is.na(BEGIN_LAT)) %>% filter(!is.na(BEGIN_LON)) %>%filter(!is.na(MAGNITUDE)) %>% ggplot() + geom_point(aes(x=BEGIN_LON, y=BEGIN_LAT, color=MAGNITUDE)) + facet_wrap(~MONTH)


ggsave("2018 Magnitudes.pdf")


storm2000<- storms %>% filter(YEAR>=2000)

newDF <- storm2000 %>% group_by(STATE,MAGNITUDE,MONTH) %>% filter(EVENT_TYPE == "Thunderstorm Wind") %>% filter(!is.na(MAGNITUDE))%>% summarise(ave_mag = mean(MAGNITUDE))
newDF$state_name <- tolower(newDF$STATE)
us <- map_data("state")
us$state_name <- us$region
map <- ggplot(newDF, aes(map_id=state_name))
map <-  map + geom_polygon(data = us, aes(x=long, y = lat, group = group), fill = "white", color ="black") 
map <- map  + geom_map(map=us, aes(fill=ave_mag))
map <- map + expand_limits(x=us$long, y=us$lat)
map <- map + coord_map() + ggtitle("Average Thunderstorm Wind Magnitude Per Month")
map + facet_wrap(~MONTH)+ scale_fill_viridis_c()
ggsave("Average Magnitude Per Month.pdf")





storm2000<- storms %>% filter(YEAR>=2000)
newDF <- storm2000 %>% group_by(STATE,YEAR) %>% filter(EVENT_TYPE == "Thunderstorm Wind")%>% summarise(count=n())
newDF$state_name <- tolower(newDF$STATE)
us <- map_data("state")
us$state_name <- us$region
map <- ggplot(newDF, aes(map_id=state_name))
map <-  map + geom_polygon(data = us, aes(x=long, y = lat, group = group), fill = "white", color ="black") 
map <- map  + geom_map(map=us, aes(fill=count))
map <- map + expand_limits(x=us$long, y=us$lat)
map <- map + coord_map() + ggtitle("Number of Thunderstorms Per Year")
map + facet_wrap(~YEAR, ncol=1)+ scale_fill_viridis_c()
ggsave("Number of Thunderstorms Per Year.pdf")



newDF <- storm2000 %>% group_by(STATE,MAGNITUDE,YEAR) %>% filter(EVENT_TYPE == "Thunderstorm Wind") %>% filter(!is.na(MAGNITUDE))%>% summarise(ave_mag = mean(MAGNITUDE))
newDF$state_name <- tolower(newDF$STATE)
us <- map_data("state")
us$state_name <- us$region
map <- ggplot(newDF, aes(map_id=state_name))
map <-  map + geom_polygon(data = us, aes(x=long, y = lat, group = group), fill = "white", color ="black") 
map <- map  + geom_map(map=us, aes(fill=ave_mag))
map <- map + expand_limits(x=us$long, y=us$lat)
map <- map + coord_map() + ggtitle("Average Thunderstorm Wind Magnitude Per Year")
map + facet_wrap(~YEAR)+ scale_fill_viridis_c()
ggsave("Average Magnitude Per Year 2.pdf")











library(ggplot2); library(maps); library(ggmap); library(mapproj)
tx_counties <- map_data("county","texas")
ggplot(tx_counties) + aes(long,lat, group=group) + geom_polygon(fill= "white", color = "black")


head(tx_counties)



texas<- storms %>% filter(YEAR>=200, STATE=="TEXAS", EVENT_TYPE=="Thunderstorm Winds", !is.na(prop_damage))%>% group_by(CZ_NAME) %>% summarise(total_dam= sum(prop_damage))

texas$CZ_NAME<- tolower(texas$CZ_NAME)

texas1<- tx_counties %>% left_join(texas, by=c('subregion'='CZ_NAME'))

view(texas)
view(texas1)








                           