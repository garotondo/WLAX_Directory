library(readxl)
library(sf)
library(fs)
library(DT)
library(leaflet)
library(raster)
library(sp)
library(janitor)
library(usmap)
library(maps)
library(purrr)
library(tidyverse)

#Create a directory and load in the two datasets. Make sure they are in the
#right folder.
wlax <- read_excel("/Users/gracerotondo/Desktop/GOV1005\ /Project/WLAX_Directory/directory_app/WLAX_LETTERWINNER_DATA.xlsx")
alumnae <- read_excel("/Users/gracerotondo/Desktop/GOV1005\ /Project/WLAX_Directory/directory_app/ALumnae_List.xlsx")

#Name the datasets and rename column names to establish uniform data. Both
#datasets have the same variables, just with different titles.
wlax_clean <- wlax %>% 
  rename("Other Website" = "Website other") %>% 
  select("Name", "First Name", "Maiden Name", "Last Name", "Graduation Year", 
         "Last Year Earned Varsity Letter", "House", "Concentration", 
         "Secondary Concentration", "Citations", "Graduate School", "Company",
         "Role", "Industry", "Area", "LinkedIn", "Other Website") %>% 
  clean_names() %>% 
  group_by(last_name, graduation_year) %>%
  mutate(count = sequence(n()))

alumnae_clean <- alumnae %>% 
  dplyr::select("Name", "First Name", "Maiden Name", "Last Name", "Graduation Year", 
                "Home City", "Home State", "Preferred Email Address", 
                "Area Code", "Number", "Phone Number 1", "Company Name", 
                "Company City","Company State") %>% 
  clean_names() %>% 
  group_by(last_name, graduation_year) %>%
  mutate(count = sequence(n())) %>% 
  ungroup() %>% 
  mutate(graduation_year = as.numeric(graduation_year))


#Merge the datasets and split the industries. This is necessary so that
#individuals within multiple industries can be searched  as an individual in one
#of the industries (not in a pair).
data <- full_join(alumnae_clean, wlax_clean, by = c('last_name', 'graduation_year')) %>%
  mutate(industry = str_split(industry, ",")) %>% 
  mutate(graduate_school = str_split(graduate_school, ";")) %>% 
  select(-first_name.y, -maiden_name.y, -name.y, -count.y, 
         -secondary_concentration, -citations, -other_website)


#Separate the areas into city and state columns and clarify that those cities
#and states are work locations
data_1 <- separate(data, area, into = c("city", "state"), sep = " (?=[^ ]+$)") %>% 
  mutate(clean_city = substr(city, 1, nchar(city)-1)) %>% 
  select(-city) %>% 
  rename(work_city = "clean_city",
         work_state = "state")

#Load in coordinate data of US cities
us_cities <- read_excel("~/Desktop/GOV1005\ /Project/WLAX_Directory/directory_app/uscities.xlsx")

#Clean us_cities data
coords <- us_cities %>% 
  select(city_ascii, state_id, lat, lng) %>% 
  rename(city = "city_ascii",
         state = "state_id")

#Merge data_1 with us_cities data to match the coordinates with the cities. Use
#left_join so that the coordinates data is just being added to the greater
#dataset.
full_data <- left_join(data_1, coords, by = c('home_city'='city', 'home_state'='state')) %>% 
  select(name.x, first_name.x, maiden_name.x, last_name, graduation_year, house, 
         concentration, home_city, home_state, lat, lng, preferred_email_address, 
         area_code, number, phone_number_1, graduate_school, company, role, industry, 
         work_city, work_state, linked_in) %>% 
  filter(name.x != "NA")

#Make the dataframe an rds file that can be used in the shiny app.
write_rds(full_data, "directory_app/data.rds")

#Using leaflet to create the map. This code will go directly into the shiny
#server.
library(maps)
mapStates = maps::map("state", fill = TRUE, plot = FALSE)
map <- leaflet(data = mapStates) %>% 
  addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)


