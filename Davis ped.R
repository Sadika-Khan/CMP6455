####################################################
#' CMP 6455 - Advanced GIS
#' Assignment 3 - Spatial Operations in R
#' 
#' Date: 2/25/2022
#' Name: Sadika Khan
#' 
####################################################

# -------------------------------------
# 1. Load packages
# -------------------------------------

install.packages("tidyverse")
install.packages("sf")
install.packages("tmap")
install.packages("tidycensus")
install.packages("dplyr")
install.packages("spData")

require(tidyverse)
require(sf)
require(tmap)
require(dplyr)
require(tidycensus)
require(spData)


# -------------------------------------
# 2. Read data
# -------------------------------------

# Set my main data folder

# For Windows
path = "C:\\Users\\u1324421\\Documents\\CMP6455\\GIS_LAb\\Mapping in R"


##Tidycenuss to get my data

Davis_acs = get_acs(
  year = 2018, survey = "acs5", geography = "tract", state = "UT", county = "Davis",
  variables = "B03002_001", output = "wide", geometry = TRUE
)


# Read Utah county boundaries shapefile
utah_counties_sf = st_read(file.path(path, "Utah_County_Boundaries", "Counties.shp"))

# Read Utah Census Tracts shapefile
utah_tracts_sf = st_read(file.path(path, "tl_2018_49_tract", "tl_2018_49_tract.shp"))

# Read Utah crash csv file
utah_crashes = read_csv(file.path(path, "State_of_Utah_Crash_Data_2015-2019.csv"))

# Read ACS 2018 5-yr data for SLCo (B01001: population by age)
slco_acs = read.csv(file.path(path, "ACSDT5Y2018.B01001_2022-02-26T214803", 
                              "ACSDT5Y2018.B01001_data_with_overlays_2022-02-22T173024.csv"))
##########
slco_acs = read.csv(file.path(path, "ACSDT5Y2018.B01001_2022-02-26T214803", 
                              "ACSDT5Y2018.B01001_data_with_overlays_2022-02-22T173024.csv"))

# -------------------------------------
# 3. Processing crash and ACS data
# -------------------------------------

# Crash data processing
# Convert the crash data to a spatial data
utah_crashes_sf = utah_crashes %>%
  filter(!is.na(GCS_Long) & !is.na(GCS_Lat)) %>% 
  st_as_sf(coords = c("GCS_Long", "GCS_Lat" ), crs = 4326) 


# Examine the GEO_ID column
Davis_acs$GEO_ID

# ACS data processing
# Select only the total population column (B01001_001E: total population)
slco_acs_pop = slco_acs %>% 
  slice(-1) %>% 
  mutate(
    GEOID = substr( GEO_ID , 10, 21),  
    total_pop = as.numeric(B01001_001E) 
  ) %>% 
  select(GEOID, total_pop) 
#########
davis_acs_pop = slco_acs %>% 
  slice(-1) %>% 
  mutate(
    GEOID = substr( GEO_ID , 10, 21),  
    total_pop = as.numeric(B01001_001E) 
  ) %>% 
  select(GEOID, total_pop) 

# Examine the tract FIPS GEOID
slco_acs_pop$GEOID

# ---------------------------------------------------
# 4. Filter to select data only for Davis county
#    And transform CRS to Utah central: 3566
# ---------------------------------------------------

# Examine the NAME column
utah_counties_sf$NAME

Davis_boundary2 = 
  utah_counties_sf %>% 
  filter(NAME == "DAVIS") %>% 
  st_transform(3566) 

# Examine the COUNTYFP column
utah_tracts_sf$COUNTYFP

davis_tracts = 
  utah_tracts_sf %>% 
  filter(COUNTYFP == "011") %>% 
  st_transform(3566)

# Examine the COUNTY_NAM column
utah_crashes_sf$COUNTY_NAM

davis_crashes = 
  utah_crashes_sf %>% 
  filter(COUNTY_NAM == "DAVIS" ) %>% 
  st_transform(3566)

# Check data
Davis_boundary2 %>% st_geometry %>% plot
davis_tracts %>% st_geometry %>% plot
davis_crashes %>% st_geometry %>% plot # Still there are erroneous data


# -----------------------------------------
# 5. Clean SLCo crash data
# -----------------------------------------

# Spatial filter crash data
# Only select crash counts contained by davis boundary
davis_crashes_filtered = davis_crashes %>% 
  st_filter(Davis_boundary2, .predicate = st_intersects)

# Check
davis_crashes_filtered %>% st_geometry %>% plot

# Extract year variable and remove 2019 data
# Examine the CRASH_DATE column
davis_crashes_filtered$CRASH_DATE

davis_crashes_cleaned = davis_crashes_filtered %>%
  mutate(
    year = substr( CRASH_DATE, 7, 10)
  ) %>%
  filter(year != "2019")

# Filter to pedestrian crashes and save it as an object
davis_ped_crashes = davis_crashes_cleaned %>%
  filter(PEDESTRIAN == TRUE)

# Check
davis_crashes_cleaned %>% glimpse
davis_crashes_cleaned %>% ggplot(aes(x=year)) + geom_bar()
davis_crashes_cleaned %>% ggplot(aes(x=CITY)) + geom_bar()

davis_ped_crashes %>% glimpse
davis_ped_crashes %>% ggplot(aes(x=year)) + geom_bar() 

ggplot(data, aes(x = factor(x), fill = factor(x))) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")

# -----------------------------------------------------
# 6. Spatial join (summary)
# -----------------------------------------------------


# Spatial join (summary) between SLCo tracts and SLCo crashes
# And use group by and summarize to create two new variables 
# Examine the pedestrian crash column


davis_tracts_join = 
  st_join(davis_tracts,davis_crashes, left = F) %>%
  group_by(GEOID) %>%
  summarise(
    total_crashes = n(),
    ped_crashes = sum(PEDESTRIAN, na.rm = TRUE),
  ) 


# Check
davis_tracts_join$total_crashes
davis_tracts_join$ped_crashes

# -----------------------------------------
# 7. Plot the results
# -----------------------------------------
# Population choropleth map
tm_shape(davis_tracts_join) +
  tm_polygons("total_crashes", palette="Oranges", style="jenks") 


# Pedestrian crash rate choropleth map
tm_shape(davis_tracts_join) + 
  tm_polygons("ped_crashes", palette="Blues", style="jenks") 


# -----------------------------------------
# 8. Map the final result
# -----------------------------------------

# Pedestrian crash rate choropleth map with the crash counts
tm_shape( davis_tracts_join ) + tm_polygons( "ped_crashes", palette= "Oranges", style = "jenks" ) +
  tm_shape(davis_crashes_cleaned %>% filter(PEDESTRIAN == TRUE)) + tm_dots(alpha = 0.8) +
  #tm_text("CITY", size = 0.6)+
  tm_layout(
    title = "Pedestrian Crashes in Davis County (2015-2018)"
  )

tm_shape(jnk) + tm_polygons("NAME_1", legend.show = F) +
  tm_text("NAME_1", size = 3)

#--------------------------------------
## For a better map viewing experience
#--------------------------------------
install.packages("mapview")
require(mapview)

tmap_mode("view")
tmap_last()

