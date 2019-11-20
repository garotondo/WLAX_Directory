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


