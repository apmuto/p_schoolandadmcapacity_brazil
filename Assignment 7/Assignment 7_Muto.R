#Assignment 7
#Applied Quantitative Methods for the Social Sciences
#Ana Paula Muto

#===============================================================================
#SET UP
library(sf)
library(spData)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(modelsummary)
setwd("C:/Users/pc/Documents/R-Projects/QSS/aqmss2/Assignment 7")

#===============================================================================
#PART 1
#===============================================================================
#_______________________________________________________________________________
#1.1.Inspecting an sf object

#a)
data(world)
class(world)
names(world)
nrow(world)
#When using "class(world)" it showcases that it includes several types such as "sf","tbl_df", "tbl","data.frame". 
#An "sf" object is a data frame with a geometry column and stores spatial shapes such as lines, points, and polygons. 

#b)
st_crs(world)
#Coordinate Reference System:User input: EPSG:4326/wkt:GEOGCRS["WGS 84",DATUM["World Geodetic System 1984",ELLIPSOID["WGS 84",6378137,298.257223563).
# WGS84 is the global standard coordinate system used by GPS and most web mapping tools. 
# Coordinates are expressed in decimal degrees for both longitude and latitude, which is useful when working geographic data as it is the standard. 

#c)
st_geometry_type(world) 
unique((st_geometry_type(world)))
#The geometry type is MULTIPOLYGON. A MULTIPOLYGON is a collection of one or more polygons treated as a single
#geographic feature. Countries require multiple polygons when their territory is not a single contiguous land mass —
#for example, the United States includes Alaska and Hawaii as separate polygons, and France includes overseas
#territories such as Martinique and Guadeloupe in the Caribbean.


#d)
plot(world)
pdf("world_gdp_base.pdf")
plot((world["gdpPercap"]))
dev.off()
plot(world["gdpPercap"], main = "GDP per capita by country")
# The wealthiest territories are the USA, Canada, Australia, and Western Europe.
#The poorest areas are concentrated in Africa and South-Asia.

#_______________________________________________________________________________
#1.2. Attribute operations

#a)
africa = filter(world, continent == "Africa")
nrow(africa)
#there are 51 countries in the data set.
plot(africa)
pdf("africa_gdp_base.pdf")
plot(africa["gdpPercap"])
dev.off()

#b)
world = world %>%
  mutate(pop_millions = pop / 1e6)
gdp_by_continent = world %>%
  group_by(continent) %>%
  summarise(mean_gdpPercap = mean(gdpPercap, na.rm = TRUE))
print(st_drop_geometry(gdp_by_continent))
#c)
africa_sorted = africa %>%
  arrange(desc(gdpPercap)) %>%
  select(name_long, gdpPercap)
print(head(st_drop_geometry(africa_sorted), 5))
# The five countries are Equatorial Guinea, Gabon, Libya, Botswana, and Algeria. 

#_______________________________________________________________________________
#1.3.Simple visualization with ggplot2

#a)
ggplot(world) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita by country")
ggsave("world_gdp.pdf", width = 10, height = 5)
#It is similar to the previous map produced.

#b)
ggplot(africa) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita by country")
ggsave("africa_gdp.pdf", width = 10, height = 5)
#We can obverse considerably variation across Africa. Central Africa would be the poorest region, whereas North Africa and South Africa have much higher GDP per capita.

#c)
ggplot(africa) +
  geom_sf(aes(fill = gdpPercap),color="white",size=0.01) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita by country")
ggsave("africa_gdp_borders.pdf", width = 10, height = 5)

w_centroid=st_centroid(world)
w_centroid
ggplot()+
  geom_sf(data = world)+
  geom_sf(data = w_centroid, color= "red")+
  geom_sf(data=w_centroid %>% 
            filter(name_long=="France"))

#===============================================================================
#PART 2
#===============================================================================
df=read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/spatial/conflict_events.csv")

#_______________________________________________________________________________
#2.1 Converting tabular data to sf

#a)
events_sf = st_as_sf(
  df, 
  coords = c("longitude", "latitude"),  
  crs = 4326                            
)
#st_as_sf() is converting the data frame into a spatial data frame while crs=4326 sets the CRS to WGS, which uses latitude and longitude.

#b)
nrow(df)
nrow(events_sf)
table(events_sf$event_type)
#the most common event type is state based. 

#c)
map1_p2=ggplot() +
  geom_sf(data = world, fill = "grey90", color = "white") +   # world background
  geom_sf(data = events_sf, aes(color = event_type), size = 1) + # conflict events colored by type
  theme_minimal() +
  labs(
    title = "Conflict Events Around the World",
    color = "Event Type"
  )
ggsave("map1_p2.pdf", width = 10, height = 6, dpi = 300)
map1_p2
#This data set only contains information about Africa. According to this map, Central and West Africa seem to have the largest concentration of conflicts across this region.

#_______________________________________________________________________________
#2.2  Spatial join: events to countries

#a)
st_crs(events_sf)
st_crs(world)

events_with_country <- st_join(
  events_sf, 
  world[, c("name_long", "continent", "gdpPercap")]  
)
nrow(events_with_country)
nrow(events_sf)
#st_join takes each point and matches it with its polygon (territory).
# It is important to check that the CRS to make sure that both are using the same method and the maps merge correctly. 

#b)
sum(is.na(events_with_country$name_long)) / nrow(events_sf)
#the events that don't match any polygon are  0.02305644. This could happen because several reasons. One could be because that conflict happened outside any borders (eg. the sea) or a territory that is not connected to the rest of the country (eg. an island).

#c)
country_summary <- events_with_country %>%
  filter(!is.na(name_long)) %>%                # remove events with no country
  group_by(name_long) %>%                     # group by country
  summarise(
    n_events = n(),                           # count number of events
    total_fatalities = sum(fatalities, na.rm = TRUE)  # sum fatalities
  ) %>%
  arrange(desc(n_events)) %>%                 # sort by most events
  st_drop_geometry()                          # remove spatial column
country_summary
head(country_summary, 10)
#This showcases that conflicts are concentrated in some territories, such as the Democratic Republic of Congo and Nigeria. These conflicts probably are long standing and are likely to be linked with weak state legitimacy, weak state capacity, and/ or long lasting social conflicts. 

#_______________________________________________________________________________
#2.3 Choropleth of conflict intensity

#a)
country_summary_df <- st_drop_geometry(country_summary)
world_events <- world %>%
  left_join(country_summary_df, by = "name_long")
world_events <- world_events %>%
  replace_na(list(
    n_events = 0,
    total_fatalities = 0
  ))
nrow(world_events)
nrow(world)

#b)
map2_p2=ggplot(world_events) +
  geom_sf(aes(fill = n_events), color = "white") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  labs(
    title = "Conflict Event Counts by Country",
    fill = "Number of Events"
  )
ggsave("map2_p2.pdf", width = 10, height = 6, dpi = 300)
#Yes, the map matches with the pattern found in the event-level map from 2.1.

#c)
map3_p2=ggplot(world_events) +
  geom_sf(aes(fill = log1p(n_events)), color = "white") +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "Log(events+1)"
  ) +
  theme_minimal() +
  labs(
    title = "Log-Transformed Conflict Event Counts by Country"
  )
ggsave("map3_p2.pdf", width = 10, height = 6, dpi = 300)
#The raw map is useful to see the overall information, but it emphasizes the countries with more conflicts, making it harder to identify differences with low and medium registered conflicts. By logging we are reducing the effects of extreme values and makes the variation across low and medium countries easier to visualize. 

#_______________________________________________________________________________
#2.4 Bonus (optional): Are events far from the capital city more deadly?

#a)
nigeria_events =events_with_country %>%
  filter(name_long == "Nigeria")

#c)
abuja_df = data.frame(
  name = "Abuja",
  longitude = 7.5,
  latitude = 9
)

abuja_sf = st_as_sf(
  abuja_df,
  coords = c("longitude", "latitude"),
  crs = 4326
)

#d)
nigeria_utm =st_transform(nigeria_events, crs = 32632)
abuja_utm =st_transform(abuja_sf, crs = 32632)

#e)
dist_matrix = st_distance(nigeria_utm, abuja_utm)

nigeria_utm$distance_m = as.numeric(dist_matrix)

nigeria_utm$distance_km = nigeria_utm$distance_m / 1000

#f)

model1 <- lm(
  log1p(fatalities) ~ log1p(distance_km),
  data = nigeria_utm
)#Basic log-log model

model2 <- lm(
  log1p(fatalities) ~ log1p(distance_km) + event_type,
  data = nigeria_utm
)#Model controlling for event type

model3 <- lm(
  log1p(fatalities) ~ log1p(distance_km) * event_type,
  data = nigeria_utm
)#Model with interaction

modelsummary(
  list(model1,model2,model3),
  stars = TRUE,
  gof_map = c("nobs"))
  
#_______________________________________________________________________________
#2.5 Discussion

#a)

#A limitation is that it is based on exact spacial information. For instance, in the case that a conflict takes place in the border it could mean that it is assigned randomly to either country or that it is not assigned. 
#We could handle this by adding a small buffer to the countries polygons or by using st_nearest_feature.

#b) 

# st_join performs a spatial join, which means that matches rows with geographical locations. This is particularly useful when matching polygons.
# On the other hand, left_join is attrituted bases, which means that matches rows with an attribute (eg. a country or region). It could be useful when there are common identifiers or want to merge datasets without considering spatial relationships. 
