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
library(leaflet)
library(tidyverse)

#Create a directory and load in the two datasets. Make sure they are in the
#right folder.
full_data <- read_rds("raw_data/data.rds")

# Define UI for application that draws a histogram
ui <- navbarPage("Harvard Women's Lacrosse Alumni", theme = shinytheme("simplex"),
                 
                 ###################################
                 # SEARCH PAGE
                 ###################################
                 
                 tabPanel("Search",
                          
                          fluidPage(
                              
                              titlePanel("Harvard Women's Lacrosse Alumni"),
                              
                              hr(),
                              
                              sidebarLayout(
                                  sidebarPanel(
                                      helpText("Input a Name to search the Alumni Community"),
                                      h3("Search"),
                                      
                                      # DTable Keyword Input - input$name
                                      
                                      textInput(c("name", "first_name.x", "last_name"), "Please enter a full, first, or last name"),
                        
                                  
                                  # Industry Input - input$industry (see Quantmod "industry" argument")
                        
                                  selectInput("industry", "Industry:",
                                              c("Finance" = "Finance",
                                                "Consulting" = "Consulting",
                                                "Law" = "Law",
                                                "Public Policy" = "Public Policy",
                                                "Hospitality" = "Hospitrality",
                                                "Food & Beverages" = "Food & Beverages",
                                                "Non-Profit" = "Non-Profit",
                                                "Public Sector" = "Public Sector",
                                                "Cosmetics" = "Cosmetics",
                                                "Marketing" = "Marketing",
                                                "Health Care" = "Health Care",
                                                "Higher Education" = "Higher Education",
                                                "Real Estate" = "Real Estate",
                                                "Journalism" = "Journalism",
                                                "Technology" = "Technology",
                                                "Computer Software" = "Computer Software",
                                                "Media" = "Media",
                                                "Education" = "Education",
                                                "Sales" = "Sales",
                                                "Media Production" = "Media Production",
                                                "Music" = "Music",
                                                "Sports" = "Coaching")),
                                  
                                  # Concentration Input - input$industry (see Quantmod "concentration" argument")
                                  selectInput("concentration", "Concentration:",
                                              c("Economics" = "Economics",
                                                "Government" = "Government",
                                                "Psychology" = "Psychology",
                                                "Statistics" = "Statistics",
                                                "Human Evolutionary Biology" = "Human Evolutionary Biology",
                                                "Biology" = "Biology",
                                                "Organismic & Evolution Biology" = "Organismic & Evolution Biology",
                                                "English & American Literature and Language" = "English & American Literature and Language",
                                                "Social Studies" = "Social Studies",
                                                "History" = "History",
                                                "History of Art & Architecture" = "History of Art & Architecture",
                                                "East Asian Studies" = "East Asian Studies",
                                                "Anthropology" = "Anthropology",
                                                "Physics" = "Physics",
                                                "Computer Science" = "Computer Science",
                                                "Neurobiology" = "Neurobiology",
                                                "Sociology" = "Sociology",
                                                "Musicology" = "Musicology"))),
                          mainPanel("Output of Search")))),
                   
                    ###################################
                    # MAP PAGE
                    ###################################                            
                    #Use leaflet to create the map          
                    tabPanel("Map",
                             fluidPage(
                                 titlePanel("Locations of Alumni Based in the United States"),
                                 
                                 hr(),
                                 
                                 leafletOutput("mymap", height = "500"),
                                 p()
                             )),
                             
                                  
                     ###################################
                     # ABOUT PAGE
                     ###################################   
                             tabPanel("About",
                                      titlePanel(h1("Fixing the Flaws of Networking: Creating an Accurate Alumni Directory for the Harvard Women's Lacrosse Program")),
                                      
                                      
                                      hr(),
                                      
                                      # Overview Explanation
                                      
                                      h3("Overview"),
                                      h4("'If your not Networking, you're doing Harvard wrong.' - David Kane"),
                                      h4("Networking is one of the most important skills an individual can develop at Harvard. 
                                         Though academics are important, the people you know are what is going to help you ultimately 
                                         land a job and launch a career after college. However this is the case, I found in my own job 
                                         search that Harvard's alumni resources are out-of-date and unreliable. To solve this problem, 
                                         I set out to develop an alumni directory - for the Harvard Women's Lacrosse Program."),
                                      
                                      # The br() function adds white space to the app.
                                    
                                      # Data Collection Explanation
                                      
                                      br(),
                                      br(),
                                      h3("Data Collection"), 
                                      h4("To develop the directory, I used five sources of data: 1) an excel spreadsheet from 
                                         the Harvard Varsit Club that included contact information; 2) Names of the Varsity 
                                         Letterwinners of Harvard Women's Lacrosse; 3) House and Concentration information from 
                                         the official Harvard Alumni Directory; 4) Information from LinkedIn; and 5) georgraphic 
                                         locations of US cities. The second, third, and fourth sources I personally researched and 
                                         collected their data to put in an excel spreadsheet. I especially wanted to manually search 
                                         individuals' LinkedIn accounts to ensure that they were accurate matches so that if the given 
                                         emails (by the Varsity Club) were not accurate, then users would have the option to reach out 
                                         to someone through LinkedIn."),
                                      br(),
                                      br(),
                                      
                                      # Search Page Explanation
                                      
                                      h3("Data Use: The Directory"),
                                      h4("I used the data to create an interface, which was designed to allow for individuals to search 
                                         women's lacrosse alums by Name (full, first, and last), Industry, and Concentration. The 
                                         dashboard includes an interactive search table for the name search and drop down options for 
                                         the Industry and Concentration searches"),
                                      
                                      br(),
                                      br(),
                                      
                                      # Map Page Explanation
                                      
                                      h3("Data Use: Interactive Map"),
                                      h4("This data was used to create an interactive map in which individuals could search locations 
                                         where alums resided based on their LinkedIn profiles. This is where the locations data was 
                                         necessary to match the cities in the alumnae data with proper coordinates so that the cities 
                                         could be located on the map."),
                                      
                                      br(),
                                      br(),
                                      
                                      
                                      h3("About Me: Grace Rotondo"),
                                      h4("I am a junior at Harvard College studying Psychology and Economics. Additionally, I am a 
                                         member of the Women's Lacrosse Team, The Student-Athlete Advisory Committee, and Harvard 
                                         Undergraduate Women in Business. Contact me at: grotondo@college.harvard.edu."),
                                      
                                      
                                      br(),
                                      hr(),
                                      
                                      # Repository Link
                                      
                                      h4("Github Repository: https://github.com/garotondo/WLAX_Directory ")))
                    
                    
#The server portion of the app, which takes the inputs and produces outputs
server <- function(input, output, session) {

points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
}, ignoreNULL = FALSE)

#Using leaflet to create the map. Need to have the markers to show alum locations
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
