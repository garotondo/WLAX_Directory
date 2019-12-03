library(shiny)
library(dplyr)
library(readr)
library(shinythemes)
library(janitor)
library(base)
library(readxl)
library(ggplot2)
library(leaflet)
library(purrr)
library(maps)
library(usmap)
library(sf)
library(fs)
library(js)
library(leaflet)
library(DT)
library(tidyverse)

#Create a directory and load in the two datasets. Make sure they are in the
#right folder.
full_data <- readRDS("~/Desktop/GOV1005 /Project/WLAX_Directory/directory_app/raw_data/data.rds")

# Define UI for application to include 3 tabs.
ui <- fluidPage(
    navbarPage("Harvard Women's Lacrosse Alumni", theme = shinytheme("cerulean"),
               
               #The first tab is the map which was made using leaflet
               tabPanel("Map",
                        fluidPage(
                            titlePanel("Home Locations of Alumni Based in the United States"),
                            
                            hr(),
                            
                            leafletOutput("mymap", height = "500"),
                            p("This is an interactive map that shows the home cities and states of US-based alums. Click on a point to reveal a name.")
                        )),
               
               
               #The second tab is the search tab which was made using DT in the server.
               tabPanel("Search",
                              titlePanel("Harvard Women's Lacrosse Alumni"),
                              mainPanel(
                                      DTOutput('search')
                                  )),
               
               #The third tab is the About page.
               tabPanel("About",
                        titlePanel(h2("Fixing the Flaws of Networking: An Accurate Alumni Directory of the Harvard Women's Lacrosse Program")),
                        
                        hr(),
                        h4("Overview"),
                        p("'If your not Networking, you're doing Harvard wrong.' - David Kane"),
                        p("Networking is one of the most important skills a person can develop while at Harvard. 
                           Though academics are important, your network of people is what is going to help you 
                           ultimately land a job and launch a career after college. Though this is the case, I 
                           found in my own job search that Harvard's alumni resources are out-of-date and unreliable. 
                           To solve this problem, I set out to develop an alumni directory - for the Harvard Women's 
                           Lacrosse Program. "),
                                      
                        # The br() function adds white space to the app.
                        # Data Collection Explanation
                        br(),
                        h4("Data Collection"), 
                        p("To develop the directory, I used five sources of data: 1) an excel spreadsheet from the 
                        Harvard Varsity Club that included contact information of alums listed in the Varsity Club's 
                        records; 2) Names of the Varsity Letterwinners of Harvard Women's Lacrosse, from 
                        <https://www.gocrimson.com/sports/wlax/history/letterwinners>; 3) House and Concentration 
                        information from the official Harvard Alumni Directory; 4) LinkedIn profiles; and 5) Coordinates 
                        of US cities to be used for the map, from <https://simplemaps.com/data/us-cities>. I personally 
                        researched and collected the data from the second, third, and fourth sources to compile my own 
                        excel spreadsheet, named *wlax*. I wanted to manually research individuals' LinkedIn accounts 
                        to include in *wlax* to ensure that they were accurate matches to the alum profiles so that if 
                        the given emails (by the Varsity Club) were not accurate, users would have the option to reach 
                        out to an alum through LinkedIn. After manually collecting the data, I cleaned it and joined it 
                        to the Varsity Club dataset. From there, I joined the US cities' coordinates data to the full 
                        alumni dataset to create my full dataset."),
                        br(),
                        
                        # Map Page Explanation
                        h4("Data Use: Interactive Map"),
                        p("This data was used to create an interactive map using Leaflet in which individuals could find 
                        the home locations of alums. I then added points that indicate the alums' home locations, and 
                        Leaflet allows for a user to click on a point and see who is from that particular location."),
                        br(),
                        
                        # Search Page Explanation
                        h4("Data Use: The Directory"),
                        p("I used the data to create an interface, which was designed to allow for individuals to search 
                           women's lacrosse alumni by any term they would like. The dashboard includes an interactive search 
                           table created with DT, allows for people to input whatever terms they would like into the search bar, 
                           and produces any rows of information containing the relevant search term."),
                        br(),

                        h5("About Me: Grace Rotondo"),
                        p("I am a junior at Harvard College studying Psychology and Economics. Additionally, I am a 
                            member of the Women's Lacrosse Team, The Student-Athlete Advisory Committee, and Harvard 
                            Undergraduate Women in Business. Contact me at: grotondo@college.harvard.edu."),
                        br(),
                        hr(),
                        # Repository Link
                        h5("Github Repository: https://github.com/garotondo/WLAX_Directory "))))
                    
                    
#The server portion of the app, which takes the inputs and produces outputs
server <- function(input, output, session) {
    
    output$search <- renderDT({
        table_data <- full_data %>% 
            select(name.x, home_city, home_state, graduation_year, 
                   house, concentration, company, role, industry,
                   preferred_email_address, phone_number_1, linked_in)
        
        colnames(table_data) <- c("Name", "Home City", "Home State", "Graduation Year", 
                         "House", "Concentration", "Company", "Role", "Industry", "Email", 
                         "Phone Number", "LinkedIn")
        table_data
        })
    

    
    output$mymap <- renderLeaflet({
    leaflet(data = mapStates) %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addTiles() %>%
        # Making sure the map has the shapes of the states and is colorful
        addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%
        addMarkers(data = full_data, ~lng, ~lat, popup = ~as.character(name.x))
})
}

                             
# Run the application 
shinyApp(ui = ui, server = server)
