#install.packages("crsuggest")
#install.packages("censusapi")
#install.packages("lehdr")
#install.packages("rgdal")
#install.packages("tidyverse")
#install.packages("tigris")
#install.packages("rlang")
install.packages("deckgl")
install.packages("rgbif")
install.packages("geojsonR")



require(tigris)
require(tidyverse)
require(tidycensus)
require(dplyr)
require(sf)
require(stringr)
require(crsuggest)
require(censusapi)
require(lehdr)
require(tmap)
require(ggplot2)
require(rgdal)
require(scales)
require(deckgl)
require(rgbif)
require(geojsonR)
require(openrouteservice)


#link Census API
census_api_key("3**********f08c671990c65ddb6098d49ac0f", install = TRUE, overwrite = TRUE)
#link openrouteservice key
ors_api_key("5b3ce3597****************118345ee9904d79b28c6c0dd", service = "openrouteservice", username = "JJSORS")

#connect file path
path = file.path("C:\\Users\\u1324421\\Documents\\CMP6455\\GIS_LAb\\Project work (group)\\Data")
####setwd(path)

#connect .csv with GNAR community names
utah_gnar <- read.csv(file.path(path, "Utah_GNAR_.csv"))

#download TIGER shapefiles for GNAR communities
ut_gnar_places <- places(state = 49, year = 2019) %>% filter_place(utah_gnar$NAME) %>% st_transform(crs = 4326)

#download utah blocks for year 2019
ut_block <- blocks(state = 49, year = 2019) %>% 
            st_transform(crs = 4326) 

ut_block2 <- st_join(ut_block, ut_gnar_places) %>%
            mutate(place = NAME)

ut_block_centroid <- ut_block2  %>%
                      st_centroid %>% 
                      mutate(lon = st_coordinates(.)[,1],
                             lat = st_coordinates(.)[,2], 
                             GEOID = GEOID10, 
                             STATEFP = STATEFP10, 
                             TRACT = TRACTCE10, 
                             BLOCK = BLOCKCE10, 
                             NAME = NAME10) %>% 
                      select(GEOID, STATEFP, COUNTYFP, TRACT, BLOCK, NAME, lon, lat, place)
                      
#project places and block shapefiles in crs 4326
ut_gnar_places_projected <- st_transform(ut_gnar_places, crs = 4326) 
ut_block_projected <- st_transform(ut_block2, crs = 4326)
ut_block_projected2 <- st_transform(ut_block2, crs = 3742)

acs_place_2019 <- get_acs(year = 2019,
                          survey = "acs5",
                          geography = "place", 
                          state = "UT", 
                          variables = c(med_household_income_ = "B19013_001",
                                        med_housing_cost_= "B25104_001",
                                        med_housing_value_= "B25077_001"),
                          output = "wide"
)

data_acs_gnar <- left_join(ut_gnar_places_projected, acs_place_2019, by = "GEOID") %>% 
  mutate(NAME = NAME.x) %>%
  select(NAMELSAD, NAME, STATEFP, PLACEFP, PLACENS, GEOID, INTPTLAT, INTPTLON, med_household_income_E, med_household_income_M, med_housing_cost_E, med_housing_cost_M,  med_housing_value_E, med_housing_value_M)

#Spatial filter 
ut_gnar_blocks <- st_filter(ut_block2, ut_gnar_places, .predicate = st_intersects)

#Exchange Centroid Points for blocks
ut_gnar_blocks_centroid <- ut_gnar_blocks %>% 
                            st_centroid %>% 
                            mutate(lon = st_coordinates(.)[,1],
                                    lat = st_coordinates(.)[,2], 
                                    GEOID = GEOID10, 
                                    STATEFP = STATEFP10, 
                                    TRACT = TRACTCE10, 
                                    BLOCK = BLOCKCE10, 
                                    NAME = NAME10) %>% 
                            select(GEOID, STATEFP, COUNTYFP, TRACT, BLOCK, NAME, lon, lat, place) 

acs_data <- get
#download lodes origin destination data for utah in 2019
ut_od <- grab_lodes(state = "UT", year = 2019, lodes_type = "od")
          
W_Join <- left_join(ut_od, ut_gnar_blocks_centroid, by = c("w_geocode" = "GEOID"))%>%
        mutate(work = w_geocode,
               home = h_geocode,
               Job_Total = S000,
               less_than_30 = SA01,
               Between_30_and_54 = SA02,
               greater_than_55 = SA03,
               earning_1250_or_less_pm = SE01,
               earning_between_1251_and_3333_pm = SE02,
               earning_over_3333_pm = SE03,
               W_lon = lon,
               W_lat = lat,
               w_geometry = geometry,
               w_state = state,
               w_STATEFP = STATEFP,
               W_COUNTYFP = COUNTYFP, 
               W_TRACT = TRACT, 
               W_BLOCK = BLOCK, 
               W_NAME = NAME,
               w_place = place) %>% 
        select( createdate, year, work, w_place, W_lon, W_lat, w_geometry, w_state, w_STATEFP, W_COUNTYFP, W_TRACT, W_BLOCK,  W_NAME, home, Job_Total, less_than_30, Between_30_and_54, greater_than_55, earning_1250_or_less_pm, earning_between_1251_and_3333_pm, earning_over_3333_pm,)
        
w_ut_gnar_od <- W_Join %>% na.omit(field = "w_geometry", geom = TRUE)

wh_ut_gnar_od <- left_join(w_ut_gnar_od, ut_block_centroid, by = c("home" = "GEOID")) %>%
  mutate(H_lon = lon,
         H_lat = lat,
         H_geometry = geometry,
         H_STATEFP = STATEFP,
         H_COUNTYFP = COUNTYFP, 
         H_TRACT = TRACT, 
         H_BLOCK = BLOCK, 
         H_NAME = NAME) %>% 
  select(createdate, year, work, w_place, W_lon, W_lat, w_geometry, w_state, w_STATEFP, W_COUNTYFP, W_TRACT, W_BLOCK,  W_NAME, home, H_lon, H_lat, H_geometry, H_STATEFP, H_COUNTYFP, H_TRACT, H_BLOCK,  H_NAME, Job_Total, less_than_30, Between_30_and_54, greater_than_55, earning_1250_or_less_pm, earning_between_1251_and_3333_pm, earning_over_3333_pm)

coordinates.ls = list(
  o = c(wh_ut_gnar_od$W_lon, wh_ut_gnar_od$W_lat),
  d = c(wh_ut_gnar_od$H_lon, wh_ut_gnar_od$H_lat)
)


Selected_10_Cities <- wh_ut_gnar_od %>% filter(wh_ut_gnar_od$w_place == "Heber"| wh_ut_gnar_od$w_place == "Salina"| wh_ut_gnar_od$w_place ==  "Morgan"| wh_ut_gnar_od$w_place ==  "Huntington"| wh_ut_gnar_od$w_place ==  "Manti"| wh_ut_gnar_od$w_place ==  "Loa"| wh_ut_gnar_od$w_place ==  "Enoch"| wh_ut_gnar_od$w_place ==  "Fillmore"| wh_ut_gnar_od$w_place == "Monroe")

# Assign an increment number for an ID
Selected_10_Cities = Selected_10_Cities %>% mutate(id = row_number())

demography_1 <- data_1 %>% select (NAME, med_household_income_E, med_housing_cost_E) %>%
 filter(data_1$NAME == "Heber"| data_1$NAME == "Salina"| data_1$NAME ==  "Morgan"| data_1$NAME ==  "Huntington"| data_1$NAME ==  "Manti"| data_1$NAME ==  "Loa"| data_1$NAME ==  "Enoch"| wh_ut_gnar_od$w_place ==  "Fillmore"| data_1$NAME == "Monroe")



output1 = data.frame()

#adding distance and duration data from ORS (can only do 40/min and 500/day with current api key)
#first change projection fo  # res = ors_matrix(coordinates.ls$o, metrics = c("duration", "distance"), units = "ft")
#r coordinate system which projects distance
for(i in 1:2400){
  cat(paste("Calculating matrix for OD Data", i, "\n"))
  
  coords = list(
    o = c(Selected_10_Cities$W_lon[i], Selected_10_Cities$W_lat[i]),
    d = c(Selected_10_Cities$H_lon[i], Selected_10_Cities$H_lat[i])
  )
  
  res = ors_matrix(coords, metrics = c("duration", "distance"), output = "parsed")
  
  distance1 = res$distances[1, 2]
  distance2 = res$distances[2, 1]
  avg_dist = mean(distance1, distance2, na.rm=T)
  
  result = data.frame(id=Selected_10_Cities$id[i], dist1=distance1, dist2=distance2, avg_dist)
  
  Sys.sleep(2)
  output1 = rbind(output1, result)

}
  
output1

write.csv(output1, "C:\\Users\\13074\\Box Sync\\Advanced GIS Final Group Project\\Data\\output1_2400.csv")

for(i in 2401:4900){
  cat(paste("Calculating matrix for OD Data", i, "\n"))
  # res = ors_matrix(coordinates.ls$o, metrics = c("duration", "distance"), units = "ft")
  
  coords = list(
    o = c(Selected_10_Cities$W_lon[i], Selected_10_Cities$W_lat[i]),
    d = c(Selected_10_Cities$H_lon[i], Selected_10_Cities$H_lat[i])
  )
  
  res = ors_matrix(coords, metrics = c("duration", "distance"), output = "parsed")
  
  distance1 = res$distances[1, 2]
  distance2 = res$distances[2, 1]
  avg_dist = mean(distance1, distance2, na.rm=T)
  
  result = data.frame(id=Selected_10_Cities$id[i], dist1=distance1, dist2=distance2, avg_dist)
  
  Sys.sleep(2)
  output1 = rbind(output1, result)
  
}

output1

write.csv(output1, "C:\\Users\\13074\\Box Sync\\Advanced GIS Final Group Project\\Data\\output1_4900.csv")

for(i in 4900:7400){
  cat(paste("Calculating matrix for OD Data", i, "\n"))
  # res = ors_matrix(coordinates.ls$o, metrics = c("duration", "distance"), units = "ft")
  
  coords = list(
    o = c(Selected_10_Cities$W_lon[i], Selected_10_Cities$W_lat[i]),
    d = c(Selected_10_Cities$H_lon[i], Selected_10_Cities$H_lat[i])
  )
  
  res = ors_matrix(coords, metrics = c("duration", "distance"), output = "parsed")
  
  distance1 = res$distances[1, 2]
  distance2 = res$distances[2, 1]
  avg_dist = mean(distance1, distance2, na.rm=T)
  
  result = data.frame(id=Selected_10_Cities$id[i], dist1=distance1, dist2=distance2, avg_dist)
  
  Sys.sleep(2)
  output1 = rbind(output1, result)
  
}

output1

write.csv(output1, "C:\\Users\\13074\\Box Sync\\Advanced GIS Final Group Project\\Data\\output4900_7400.csv")

for(i in 7400:95324){
  cat(paste("Calculating matrix for OD Data", i, "\n"))
  # res = ors_matrix(coordinates.ls$o, metrics = c("duration", "distance"), units = "ft")
  
  coords = list(
    o = c(Selected_10_Cities$W_lon[i], Selected_10_Cities$W_lat[i]),
    d = c(Selected_10_Cities$H_lon[i], Selected_10_Cities$H_lat[i])
  )
  
  res = ors_matrix(coords, metrics = c("duration", "distance"), output = "parsed")
  
  distance1 = res$distances[1, 2]
  distance2 = res$distances[2, 1]
  avg_dist = mean(distance1, distance2, na.rm=T)
  
  result = data.frame(id=Selected_10_Cities$id[i], dist1=distance1, dist2=distance2, avg_dist)
  
  Sys.sleep(2)
  output1 = rbind(output1, result)
  
}

output1

write.csv(output1, "C:\\Users\\13074\\Box Sync\\Advanced GIS Final Group Project\\Data\\output4900_95324.csv")

Distance_Data <- read.csv(file.path(path, "output_data.csv"))

######Distance_10_Cities <- left_join(Selected_10_Cities, Distance_Data, by = c("id" = "id")) %>%
  #mutate(avg_dist = avg_dist * 0.000621371) %>%
 # select(createdate, year, work, w_place, W_lon, W_lat, w_geometry, w_state, w_STATEFP, W_COUNTYFP, W_TRACT, W_BLOCK,  W_NAME, home, H_lon, H_lat, H_geometry, H_STATEFP, H_COUNTYFP, H_TRACT, H_BLOCK,  H_NAME, Job_Total, less_than_30, Between_30_and_54, greater_than_55, earning_1250_or_less_pm, earning_between_1251_and_3333_pm, earning_over_3333_pm, avg_dist) %>%
  #na.omit(field = "avg_dist")

Distance_10_Cities2 <- left_join(Selected_10_Cities, Distance_Data, by = c("id" = "id")) %>%
  mutate(avg_dist = avg_dist * 0.000621371) %>%
  select(year, work, w_place, w_state, Job_Total, avg_dist) %>%
  na.omit(field = "avg_dist")

write.csv(Distance_10_Cities2, "C:\\Users\\u1324421\\Documents\\CMP6455\\GIS_LAb\\Project work (group)\\Data\\distance10.csv")
Dem10 <- read.csv(file.path(path, "distance10_demography.csv"))

#############

  
######TO_GeoJson$Polygon(ut_gnar_places_projected, stringify = FALSE)

#########geo <- sf_geojson(ut_gnar_places_projected, atomise = T)

###########MAPPING OUR COMMUTER SHED

#install.packages("mapdeck")
#install.packages("deck.gl")
#install.packages("rtools")

require(mapdeck)
require(deck.gl)
require(rtools)



key <- 'pk.eyJ1IjoibHVuZDExIiwiYSI6ImNqdW1zYzJzNTJneng0M25xYWcwOGxkZGgifQ.RR3O0jLgQ-feakIrEzOg_A' 

####################Commuter shed map
Selected_10_Cities %>% 
  sample_n(1000) %>%
  mapdeck(token = key, style = 'mapbox://styles/mapbox/dark-v9', pitch = 45) %>% 
  add_animated_arc(
    layer_id = "arc_layer" ,
    origin = c("H_lon", "H_lat"),
    destination = c("W_lon", "W_lat"),
    stroke_from = "W_NAME",
    stroke_to = "H_NAME",
    stroke_width = "Job_Total",
    auto_highlight = TRUE,
    highlight_colour = "#8c43facc",
    tooltip = "tooltip",
    palette = "plasma"
    
  ) %>%
  add_polygon(
    data = data_acs_gnar,
    polyline = "NAME",
    fill_colour = "NAME",
    legend = TRUE
  ) %>%
  add_title(title = list(
    title = "Commuter Shed Map for GNAR Community"
  )
  )


data_1 <- data_acs_gnar[1:50, ]
data_2 <- data_acs_gnar[51:101, ]
Data_3 <- data_acs_gnar[102:153, ]
#visualization of household income for gnar communities

data_1 %>% na.omit %>%
  ggplot(aes(x = med_household_income_E, y = reorder(NAMELSAD, med_household_income_E))) + 
  geom_errorbarh(aes(xmin = med_household_income_E - med_household_income_M, xmax = med_household_income_E + med_household_income_M)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income by GNAR place in Utah",
       subtitle = "2015-2019 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)") +
  scale_x_continuous(labels = label_dollar())

data_2 %>% na.omit %>%
  ggplot(aes(x = med_household_income_E, y = reorder(NAMELSAD, med_household_income_E))) + 
  geom_errorbarh(aes(xmin = med_household_income_E - med_household_income_M, xmax = med_household_income_E + med_household_income_M)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income by GNAR place in Utah",
       subtitle = "2015-2019 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)") +
  scale_x_continuous(labels = label_dollar())

Data_3 %>% na.omit %>%
  ggplot(aes(x = med_household_income_E, y = reorder(NAMELSAD, med_household_income_E))) + 
  geom_errorbarh(aes(xmin = med_household_income_E - med_household_income_M, xmax = med_household_income_E + med_household_income_M)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income by GNAR place in Utah",
       subtitle = "2015-2019 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)") +
  scale_x_continuous(labels = label_dollar())

par(mfrow = c(2,2))

dev.off()



dat <- data_acs_gnar %>% filter(data_acs_gnar$med_household_income_M >= 3000 & data_acs_gnar$med_household_income_M <= 10000, )

png("dat.png", height=1000, width=200)
p<-tableGrob(dat)
grid.arrange(p)
dev.off()

dat %>% na.omit %>% 
  ggplot(aes(x = med_household_income_E, y = reorder(NAMELSAD, med_household_income_E))) + 
  geom_errorbarh(aes(xmin = med_household_income_E - med_household_income_M, xmax = med_household_income_E + med_household_income_M)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income by GNAR place in Utah",
       subtitle = "2015-2019 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)") +
  scale_x_continuous(labels = label_dollar())

#------------------------------------------------------

require(tidyverse)
require(sf)
require(tmap)
require(mapview)
require(matrixStats)
require(openrouteservice)
require(traveltime)
require(leaflet)


df <- dat %>% select(INTPTLAT, INTPTLON)

APP_ID = "07dea5f5"
API_KEY = "914a8051f90653033e54bb4c4e6a11d9"

# 30-min drive time
Drive_30_Monticello_city = traveltime_map(
  appId = APP_ID,
  apiKey = API_KEY,
  location = c(37.8683527, -109.3383035), # Downtown SLC
  traveltime = 30*60,
  type = "driving",
  departure = "2022-03-25T10:00:00" # ISO8601 time format
)

# Let's map the isochrone
mapview(Drive_30_Monticello_city,  col.regions = "grey", Add = TRUE)

# 30-min drive time
Drive_30_Heber_city = traveltime_map(
  appId = APP_ID,
  apiKey = API_KEY,
  location = c(+40.5068483, -111.3983631), # Downtown SLC
  traveltime = 30*60,
  type = "driving",
  departure = "2022-03-25T10:00:00" # ISO8601 time format
)

mapview(Drive_30_Heber_city,  col.regions = "blue")
# Let's map the isochrone
#########################################################################
# 30-min drive time
Drive_30_Salina_city = traveltime_map(
  appId = APP_ID,
  apiKey = API_KEY,
  location = c(+38.9366581, -111.8664554), # Downtown SLC
  traveltime = 30*60,
  type = "driving",
  departure = "2022-03-25T10:00:00" # ISO8601 time format
)

mapview(Drive_30_Salina_city,  col.regions = "purple")

Drive_30_Morgan_city = traveltime_map(
  appId = APP_ID,
  apiKey = API_KEY,
  location = c(+41.0414066, -111.6801531), # Downtown SLC
  traveltime = 30*60,
  type = "driving",
  departure = "2022-03-25T10:00:00" # ISO8601 time format
)
mapview(Drive_30_Morgan_city,  col.regions = "red")

Drive_30_Huntington_city = traveltime_map(
  appId = APP_ID,
  apiKey = API_KEY,
  location = c(+39.3301162, -110.9628090), # Downtown SLC
  traveltime = 30*60,
  type = "driving",
  departure = "2022-03-25T10:00:00" # ISO8601 time format
)

##Joseph's start here
drive_30_Manti_city = traveltime_map(
  appId = APP_ID,
  apiKey = API_KEY,
  location = c(39.2668813, -111.6350437), # Downtown SLC
  traveltime = 30*60,
  type = "driving",
  departure = "2022-03-25T10:00:00" # ISO8601 time format
)
mapview(drive_30_Manti_city,  col.regions = "orange")
# 30-min public drive time - 3
drive_30_Loa_town = traveltime_map(
  appId = APP_ID,
  apiKey = API_KEY,
  location = c(+38.4037692, -111.6451386), # Downtown SLC
  traveltime = 30*60,
  type = "driving",
  departure = "2022-03-25T10:00:00" # ISO8601 time format
)
mapview(drive_30_Loa_town,  col.regions = "orange")

# 30-min public drive time - 4
drive_30_Enoch_city = traveltime_map(
  appId = APP_ID,
  apiKey = API_KEY,
  location = c(37.7669752, -113.0449114), # Downtown SLC
  traveltime = 30*60,
  type = "driving",
  departure = "2022-03-25T10:00:00" # ISO8601 time format
)
##mapview(drive_30_Enoch_city,  col.regions = "orange")

# 30-min public drive time - 5
drive_30_Fillmore_city = traveltime_map(
  appId = APP_ID,
  apiKey = API_KEY,
  location = c(38.9640280, -112.3386525), # Downtown SLC
  traveltime = 30*60,
  type = "driving",
  departure = "2022-03-25T10:00:00" # ISO8601 time format
)
##mapview(drive_30_Fillmore_city,  col.regions = "orange")

# 30-min public drive time - 6
drive_30_Monroe_city = traveltime_map(
  appId = APP_ID,
  apiKey = API_KEY,
  location = c(38.6229290, -112.1198450), # Downtown SLC
  traveltime = 30*60,
  type = "driving",
  departure = "2022-03-25T10:00:00" # ISO8601 time format
)
##mapview(drive_30_Monroe_city,  col.regions = "orange")


##ADD all the 10

mapview(Drive_Monticello_city,  col.regions = "orange")+
  mapview(Drive_4,  col.regions = "orange")+
  mapview(Drive_3,  col.regions = "orange")+
  mapview(Drive_2,  col.regions = "orange")+
  mapview(Drive_1,  col.regions = "orange")+
  mapview(drive_30_5,  col.regions = "orange")+
  mapview(drive_30_4,  col.regions = "orange")+
  mapview(drive_30_3,  col.regions = "orange")+
  mapview(drive_6,  col.regions = "orange")+
  mapview(drive_30_6,  col.regions = "orange")


#######################################
####PUrple for 15 minutes
drive_15_Monticello_city = traveltime_map(
  appId = APP_ID,
  apiKey = API_KEY,
  location = c(37.8683527, -109.3383035), # Downtown SLC
  traveltime = 15*60,
  type = "driving",
  departure = "2022-03-25T10:00:00" # ISO8601 time format
)
mapview(drive_15_Monticello_city,  col.regions = "Purple")

Drive_15_Heber_city = traveltime_map(
  appId = APP_ID,
  apiKey = API_KEY,
  location = c(+40.5068483, -111.3983631), # Downtown SLC
  traveltime = 15*60,
  type = "driving",
  departure = "2022-03-25T10:00:00" # ISO8601 time format
)

mapview(Drive_15_Heber_city,  col.regions = "purple")

Drive_15_Salina_city = traveltime_map(
  appId = APP_ID,
  apiKey = API_KEY,
  location = c(+38.9366581, -111.8664554), # Downtown SLC
  traveltime = 15*60,
  type = "driving",
  departure = "2022-03-25T10:00:00" # ISO8601 time format
)

mapview(Drive_15_Salina_city,  col.regions = "purple")

Drive_15_Morgan_city = traveltime_map(
  appId = APP_ID,
  apiKey = API_KEY,
  location = c(+41.0414066, -111.6801531), # Downtown SLC
  traveltime = 15*60,
  type = "driving",
  departure = "2022-03-25T10:00:00" # ISO8601 time format
)
mapview(Drive_15_Morgan_city,  col.regions = "purple")

Drive_15_Huntington_city = traveltime_map(
  appId = APP_ID,
  apiKey = API_KEY,
  location = c(+39.3301162, -110.9628090), # Downtown SLC
  traveltime = 15*60,
  type = "driving",
  departure = "2022-03-25T10:00:00" # ISO8601 time format
)
mapview(Drive_15_Huntington_city,  col.regions = "purple")

drive_15_Manti_city = traveltime_map(
  appId = APP_ID,
  apiKey = API_KEY,
  location = c(39.2668813, -111.6350437), # Downtown SLC
  traveltime = 15*60,
  type = "driving",
  departure = "2022-03-25T10:00:00" # ISO8601 time format
)
mapview(drive_15_Manti_city,  col.regions = "purple")

drive_15_Loa_town = traveltime_map(
  appId = APP_ID,
  apiKey = API_KEY,
  location = c(+38.4037692, -111.6451386), # Downtown SLC
  traveltime = 15*60,
  type = "driving",
  departure = "2022-03-25T10:00:00" # ISO8601 time format
)
mapview(drive_30_Loa_town,  col.regions = "purple")

# 30-min public drive time - 4
drive_15_Enoch_city = traveltime_map(
  appId = APP_ID,
  apiKey = API_KEY,
  location = c(37.7669752, -113.0449114), # Downtown SLC
  traveltime = 15*60,
  type = "driving",
  departure = "2022-03-25T10:00:00" # ISO8601 time format
)
mapview(drive_30_Enoch_city,  col.regions = "purple")

drive_15_Fillmore_city = traveltime_map(
  appId = APP_ID,
  apiKey = API_KEY,
  location = c(38.9640280, -112.3386525), # Downtown SLC
  traveltime = 15*60,
  type = "driving",
  departure = "2022-03-25T10:00:00" # ISO8601 time format
)
mapview(drive_15_Fillmore_city,  col.regions = "purple")

drive_15_Monroe_city = traveltime_map(
  appId = APP_ID,
  apiKey = API_KEY,
  location = c(38.6229290, -112.1198450), # Downtown SLC
  traveltime = 15*60,
  type = "driving",
  departure = "2022-03-25T10:00:00" # ISO8601 time format
)
mapview(drive_15_Monroe_city,  col.regions = "purple")

############################## 45-min drive time
##############33______________________________________________--------------------------------------------
Drive_45_Monticello = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(37.8683527, -109.3383035), # Downtown SLC
  
  traveltime = 45*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)


# 45-min drive time

Drive_45_Heber_City = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(+40.5068483, -111.3983631), # Downtown SLC
  
  traveltime = 45*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)

# 45-min drive time

Drive_45_Salina_City = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(+38.9366581, -111.8664554), # Downtown SLC
  
  traveltime = 45*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)



Drive_45_Morgan_City = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(+41.0414066, -111.6801531), # Downtown SLC
  
  traveltime = 45*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)


Drive_45_Huntington_city = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(+39.3301162, -110.9628090), # Downtown SLC
  
  traveltime = 45*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)



# 45-min public drive time

Drive_45_Manti_city = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(+39.2668813, -111.6350437), # Downtown SLC
  
  traveltime = 45*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)

mapview(Drive_45_Manti_city, col.regions = "blue")

# 45-min public drive time - 2

Drive_45_loa = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(39.2668813, -111.6350437), # Downtown SLC
  
  traveltime = 45*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)



# 45-min public drive time - 3

Drive_45_Enoch = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(+38.4037692, -111.6451386), # Downtown SLC
  
  traveltime = 45*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)



# 45-min public drive time - 4

Drive_45_Fillmore = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(37.7669752, -113.0449114), # Downtown SLC
  
  traveltime = 45*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)



# 45-min public drive time - 5

Drive_45_Monroe = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(38.9640280, -112.3386525), # Downtown SLC
  
  traveltime = 45*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)


#####################################45minutes drive####blue
mapview(Drive_45_Monticello,  col.regions = "blue") +
  
  mapview(Drive_45_Heber_City,  col.regions = "blue") +
  
  mapview(Drive_45_Salina_City,  col.regions = "blue") +
  
  mapview(Drive_45_Morgan_City,  col.regions = "blue") +
  
  mapview(Drive_45_Huntington_city,  col.regions = "blue") +
  
  mapview(Drive_45_Manti_city,  col.regions = "blue") +
  
  mapview(Drive_45_loa,  col.regions = "blue") +
  
  mapview(Drive_45_Enoch,  col.regions = "blue") +
  
  mapview(Drive_45_Fillmore,  col.regions = "blue") +
  
  mapview(Drive_45_Monroe,  col.regions = "blue")

###########################################################################
###############60minute########Green
# 60-min drive time

Drive_60_Monticello = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(37.8683527, -109.3383035), # Downtown SLC
  
  traveltime = 60*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)



# 60-min drive time

Drive_60_Heber_City = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(+40.5068483, -111.3983631), # Downtown SLC
  
  traveltime = 60*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)



# 60-min drive time

Drive_60_Salina_City = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(+38.9366581, -111.8664554), # Downtown SLC
  
  traveltime = 60*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)



Drive_60_Morgan_City = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(+41.0414066, -111.6801531), # Downtown SLC
  
  traveltime = 60*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)



Drive_60_Huntington_city = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(+39.3301162, -110.9628090), # Downtown SLC
  
  traveltime = 60*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)



# 60-min public drive time

Drive_60_Manti_city = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(39.3301162, -111.6350437), # Downtown SLC
  
  traveltime = 60*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)



# 60-min public drive time - 2

Drive_60_loa = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(39.2668813, -111.6350437), # Downtown SLC
  
  traveltime = 60*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)



# 60-min public drive time - 3

Drive_60_Enoch = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(+38.4037692, -111.6451386), # Downtown SLC
  
  traveltime = 60*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)



# 60-min public drive time - 4

Drive_60_Fillmore = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(37.7669752, -113.0449114), # Downtown SLC
  
  traveltime = 60*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)



# 60-min public drive time - 5

Drive_60_Monroe = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(38.9640280, -112.3386525), # Downtown SLC
  
  traveltime = 60*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)



# 60-min public drive time - 6

Drive_60_Parowan_City = traveltime_map(
  
  appId = APP_ID,
  
  apiKey = API_KEY,
  
  location = c(38.6229290, -112.1198450), # Downtown SLC
  
  traveltime = 60*60,
  
  type = "driving",
  
  departure = "2022-03-25T10:00:00" # ISO8601 time format
  
)


##############ADD ALL
mapview(Drive_60_Monticello,  col.regions = "green") +
  
  mapview(Drive_60_Heber_City,  col.regions = "green") +
  
  mapview(Drive_60_Salina_City,  col.regions = "green") +
  
  mapview(Drive_60_Morgan_City,  col.regions = "green") +
  
  mapview(Drive_60_Huntington_city,  col.regions = "green") +
  
  mapview(Drive_60_Manti_city,  col.regions = "green") +
  
  mapview(Drive_60_loa,  col.regions = "green") +
  
  mapview(Drive_60_Enoch,  col.regions = "green") +
  
  mapview(Drive_60_Fillmore,  col.regions = "green") +
  
  mapview(Drive_60_Monroe,  col.regions = "green") +
  
  mapview(Drive_60_Parowan_City,  col.regions = "green")+

 mapview(Drive_45_Monticello,  col.regions = "blue") +
  
  mapview(Drive_45_Heber_City,  col.regions = "blue") +
  
  mapview(Drive_45_Salina_City,  col.regions = "blue") +
  
  mapview(Drive_45_Morgan_City,  col.regions = "blue") +
  
  mapview(Drive_45_Huntington_city,  col.regions = "blue") +
  
  mapview(Drive_45_Manti_city,  col.regions = "blue") +
  
  mapview(Drive_45_loa,  col.regions = "blue") +
  
  mapview(Drive_45_Enoch,  col.regions = "blue") +
  
  mapview(Drive_45_Fillmore,  col.regions = "blue") +
  
  mapview(Drive_45_Monroe,  col.regions = "blue")+
  mapview(Drive_30_Heber_city,  col.regions = "orange")+
  mapview(Drive_30_Huntington_city,  col.regions = "orange")+
  mapview(Drive_30_Monticello_city,  col.regions = "orange")+
  mapview(Drive_30_Morgan_city,  col.regions = "orange")+
  mapview(Drive_30_Salina_city,  col.regions = "orange")+
  mapview(drive_30_Enoch_city,  col.regions = "orange")+
  mapview(drive_30_Fillmore_city,  col.regions = "orange")+
  mapview(drive_30_Loa_town,  col.regions = "orange")+
  mapview(drive_30_Manti_city,  col.regions = "orange")+
  mapview(drive_30_Monroe_city,  col.regions = "orange")+
  mapview(drive_15_Monticello_city,  col.regions = "Purple")+
  mapview(drive_15_Enoch_city,  col.regions = "Purple")+
  mapview(drive_15_Fillmore_city,  col.regions = "Purple")+
  mapview(drive_15_Loa_town,  col.regions = "Purple")+
  mapview(drive_15_Manti_city,  col.regions = "Purple")+
  mapview(drive_15_Monroe_city,  col.regions = "Purple")+
  mapview(Drive_15_Heber_city,  col.regions = "Purple")+
  mapview(Drive_15_Huntington_city,  col.regions = "Purple")+
  mapview(Drive_15_Morgan_city,  col.regions = "Purple")+
  mapview(Drive_15_Salina_city,  col.regions = "Purple")
  
###########################################################################################
#################### Statistics###########

read.csv(file.choose())

#######data_acs_gnar <- left_join(ut_gnar_places_projected, acs_place_2019, by = "GEOID") %>% 
  mutate(NAME = NAME.x) %>%
  select(NAMELSAD, NAME, STATEFP, PLACEFP, PLACENS, GEOID, INTPTLAT, INTPTLON, med_household_income_E, med_household_income_M, med_housing_cost_E, med_housing_cost_M,  med_housing_value_E, med_housing_value_M)

Distance_10_Cities2 <- left_join(Selected_10_Cities, Distance_Data, by = c("id" = "id")) %>%
    mutate(avg_dist = avg_dist * 0.000621371) %>%
    select(year, work, w_place, w_state, Job_Total, avg_dist) %>%
    na.omit(field = "avg_dist")
  
write.csv(Distance_10_Cities2, "C:\\Users\\u1324421\\Documents\\CMP6455\\GIS_LAb\\Project work (group)\\Data\\distance10.csv")
Dem10 <- read.csv(file.choose())

require(ellipse)
Var10city <- data.frame(Dem10$M_Income, Dem10$M_H_Value, Dem10$M_H_Cost, Dem10$Job_Total, Dem10$avg_dist)
plotcorr(cor(Var10city))
cor(Var10city)  

lm.var <- lm(Dem10.avg_dist~Dem10.M_Income+Dem10.M_H_Cost+Dem10.M_H_Value+Dem10.Job_Total, data= Var10city)
summary(lm.var)

####################
Var10city$M_inc <- ifelse(Var10city$Dem10.M_Income <= 55000, 1, ifelse ((Var10city$Dem10.M_Income > 55000) & (Var10city$Dem10.M_Income <= 65000), 2, ifelse (Var10city$Dem10.M_Income > 65000, 3, Var10city$Dem10.M_Income)))
Var10city$M_inc=factor(Var10city$M_inc)

Var10city$M_hvalue <- ifelse(Var10city$Dem10.M_H_Value <= 150000, 1, ifelse ((Var10city$Dem10.M_H_Value > 150000) & (Var10city$Dem10.M_H_Value <= 250000), 2, ifelse (Var10city$Dem10.M_H_Value > 250000, 3, Var10city$Dem10.M_H_Value)))
Var10city$M_hvalue=factor(Var10city$M_hvalue)
#############
Var10city$M_hcost <- ifelse(Var10city$Dem10.M_H_Cost <= 1200, 1, ifelse ((Var10city$Dem10.M_H_Cost > 1200) & (Var10city$Dem10.M_H_Cost <= 3000), 2, ifelse (Var10city$Dem10.M_H_Cost > 3000, 3, Var10city$Dem10.M_H_Cost)))
Var10city$M_hcost=factor(Var10city$M_hcost)

lm.var3 <- lm(Dem10.avg_dist~M_inc+M_hcost+M_hvalue+Dem10.Job_Total, data= Var10city)
summary(lm.var3) ######highest R squared value

lm.var4 <- lm(Dem10.avg_dist~M_inc+M_hcost+M_hvalue, data= Var10city)
summary(lm.var4)

Var10city$M_hcost1 <- ifelse(Var10city$Dem10.M_H_Cost <= 1500, 1, ifelse ((Var10city$Dem10.M_H_Cost > 1500) & (Var10city$Dem10.M_H_Cost <= 3200), 2, ifelse (Var10city$Dem10.M_H_Cost > 3200, 3, Var10city$Dem10.M_H_Cost)))
Var10city$M_hcost1=factor(Var10city$M_hcost1)

lm.var5 <- lm(Dem10.avg_dist~M_inc+M_hcost1+M_hvalue+Dem10.Job_Total, data= Var10city)
summary(lm.var5) ######highest R squared value


Var10city$JobCat <- ifelse(Var10city$Dem10.Job_Total <= 5, 1, ifelse ((Var10city$Dem10.M_H_Value > 4) & (Var10city$Dem10.M_H_Value <= 7), 2, ifelse (Var10city$Dem10.M_H_Value > 10, 3, Var10city$Dem10.M_H_Value)))
Var10city$JobCat=factor(Var10city$JobCat)

lm.var6 <- lm(Dem10.avg_dist~M_inc+M_hcost+M_hvalue+JobCat, data= Var10city)
summary(lm.var6) ###categorizing job gives a insignificant result

Var10city
Dev.off()
par(mfrow = c(2,2))
plot(lm.var5)
require(ggplot2)


ggplot(Var10city, aes(fill= M_inc, y= M_hvalue, x= Dem10.avg_dist)) +
  geom_bar(position='dodge', stat='identity')+
  ggtitle("Income and Household Value", subtitle = "By Distance")

ggplot(Var10city, aes(fill= M_hcost, y= JobCat, x= Dem10.avg_dist)) +
  geom_bar(position='dodge', stat='identity')+
  ggtitle("Housing Cost and Total Job", subtitle = "By Distance")







