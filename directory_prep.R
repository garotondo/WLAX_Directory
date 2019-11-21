#Create a directory and load in the two datasets. Make sure they are in the
#right fiolder.
dir.create("raw_data")
wlax <- read_excel("raw_data/WLAX_LETTERWINNER_DATA_complete.xlsx")
alumnae <- read_excel("raw_data/FoHL_Alumnae_List_10.3.19_final.xls")

#Name the datasets and rename column names to establish uniform data. Both
#datasets have the same variables, just with different titles.
wlax <- wlax %>% 
  rename("Other Website" = "Website other") %>% 
  select("Name", "First Name", "Maiden Name", "Last Name", "Graduation Year", 
         "Last Year Earned Varsity Letter", "House", "Concentration", 
         "Secondary Concentration", "Citations", "Graduate School", "Company",
         "Role", "Industry", "Area", "LinkedIn", "Other Website") %>% 
  clean_names() %>% 
  group_by(last_name, graduation_year) %>%
  mutate(count = sequence(n()))

alumnae <- alumnae %>% 
  select("Name", "First Name", "Last Name", "Graduation Year", 
         "Home City", "Home State", "Preferred Email Address", 
         "Area Code", "Phone Number", "Company", "Role", 
         "Company City","Company State") %>% 
  clean_names() %>% 
  group_by(last_name, graduation_year) %>%
  mutate(count = sequence(n())) %>% 
  ungroup() %>% 
  mutate(graduation_year = as.numeric(graduation_year))

#Merge the datasets and split the industries. This is necessary so that
#individuals within multiple industries can be searched  as an individual in one
#of the industries (not in a pair).
data <- full_join(alumnae, wlax, by = c('last_name', 'graduation_year', 'count')) %>% 
  mutate(industry = str_split(industry, ",")) 

#Separate the areas into city and state columns 
data_1 <- separate(data, area, into = c("city", "state"), sep = " (?=[^ ]+$)") %>% 
  mutate(clean_city = substr(city, 1, nchar(city)-1)) %>% 
  select(-city) %>% 
  rename(city = "clean_city")


#Load in coordinate data of us cities
us_cities <- read_excel("raw_data/uscities.xlsx")

#Clean us_cities data
coords <- us_cities %>% 
  select(city_ascii, state_id, lat, lng) %>% 
  rename(city = "city_ascii",
         state = "state_id")

#Merge data_1 with us_cities data to match the coordinates with the cities
full_data <- left_join(data_1, coords, by = c('city', 'state'))


#Using leaflet to create the map.
#mymap <- mapStates = map("state", fill = TRUE, plot = FALSE)
map <- leaflet(data = mapStates) %>% 
  addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)


#Mark clusters on map
leaflet(location_points) %>% 
  addTiles() %>% 
  addMarkers(
    clusterOptions = markerClusterOptions(
      showCoverageOnHover = TRUE
    )
  )

##Copied from ps_7
#Data to use:
full_data
coords
map

#Coords needs to use the sf package for locations
locations <- st_as_sf(coords, coords = c("lng", "lat"))

#write RDS files to prep for map in shiny
write_rds(full_data, "/Users/gracerotondo/Desktop/GOV1005\ /Project/WLAX_Directory/directory_app/raw_data/data.rds")
write_rds(locations, "/Users/gracerotondo/Desktop/GOV1005\ /Project/WLAX_Directory/directory_app/raw_data/locations.rds")
write_rds(map, "/Users/gracerotondo/Desktop/GOV1005\ /Project/WLAX_Directory/directory_app/raw_data/map.rds" )




