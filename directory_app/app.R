library(readxl)
library(purrr)
library(sf)
library(fs)
library(DT)
library(leaflet)
library(raster)
library(sp)
library(janitor)
library(tidyverse)
library(usmap)
library(maps)
library(shiny)
library(shinythemes)

# Define UI for application to include 3 tabs.
ui <- fluidPage(
    navbarPage("Harvard Women's Lacrosse Alumni", theme = shinytheme("cerulean"),
               
               #The first tab is the map which was made using leaflet
               tabPanel("Map",
                        fluidPage(
                            titlePanel("Home Locations of Alumni Based in the United States"),
                            
                            hr(),
                            
                            leafletOutput("mymap", height = "600"),
                            p("This is an interactive map that shows the home cities and states of US-based alums. 
                              Click on a point to reveal a name, then look up the name in the Search tab.")
                        )),
               
               
               #The second tab is the search tab which was made using DT in the server.
               tabPanel("Search",
                        titlePanel("Harvard Women's Lacrosse Alumni"),
                        mainPanel(
                            DTOutput('search'), width = "100%", height = "auto"
                        )),
               
               #The third tab is the About page.
               tabPanel("About",
                        titlePanel(h3("Fixing the Flaws of Networking: An Alumni Directory of the Harvard Women's Lacrosse Program")),
                        
                        hr(),
                        h4("Overview"),
                        p("Networking is one of the most important skills a person can develop while at Harvard, 
                        and in life. Academic credentials are important; however, your network of people is what 
                        is going to help you ultimately land a job and launch a career after college. Though this 
                        is the case, I found that Harvard's alumni resources are out-of-date and unreliable. To solve 
                        this problem, I developed an accurate and functional alumni directory for the Harvard Women's 
                        Lacrosse Program."),
                        
                        # The br() function adds white space to the app.
                        # Data Collection Explanation
                        br(),
                        h4("Data Collection"), 
                        p("To develop the directory, I used five sources of data: 1) an excel spreadsheet from the 
                        Harvard Varsity Club that included contact information of alums listed in the Varsity Club's 
                        records; 2) Names of the Varsity Letterwinners of Harvard Women's Lacrosse, from", 
                        tags$a("GoCrimson.com;", href = "https://www.gocrimson.com/sports/wlax/history/letterwinners"), 
                        "3) House and Concentration information from the official Harvard Alumni Directory; 4) LinkedIn 
                        profiles; and 5) Coordinates of US cities to be used for the map, from the", 
                        tags$a("United States Cities Database.", href = "https://simplemaps.com/data/us-cities"),"I personally 
                        researched and collected the data from the second, third, and fourth sources to compile my own 
                        excel spreadsheet, named wlax. I wanted to manually research individuals' LinkedIn accounts 
                        to include in wlax to ensure that they were accurate matches to the alum profiles so that if 
                        the given emails (by the Varsity Club) were not accurate, users would have the option to reach 
                        out to an alum through LinkedIn. After manually collecting the data, I cleaned it and joined it 
                        to the Varsity Club dataset. From there, I joined the US cities' coordinates data to the full 
                        alumni dataset to create my full dataset."),
                        br(),
                        
                        # Map Page Explanation
                        h4("Data Use: Interactive Map"),
                        p("This data was used to create an interactive map using Leaflet with which people can find 
                        the home locations of alums. Leaflet allows for a user interface, so I added points that 
                        indicate the alums' home locations and show the alum's name so that users can see who is from 
                        a particular location."),
                        br(),
                        
                        # Search Page Explanation
                        h4("Data Use: The Directory"),
                        p("I used the data to create an interface, which was designed to allow for individuals to search 
                           women's lacrosse alumni by any term they would like. The dashboard includes an interactive search 
                           table created with DT, allows for people to input whatever terms they would like into the search bar, 
                           and produces any rows of information containing the relevant search term."),
                        br(),
                        
                        #Bio
                        h4("About Me: Grace Rotondo"),
                        p("I am a junior at Harvard College studying Psychology and Economics. I am also a 
                            member of the Varsity Women's Lacrosse Team, the Student-Athlete Advisory Committee, and Harvard 
                            Undergraduate Women in Business. You can contact me at: grotondo@college.harvard.edu."),
                        br(),
                        
                        #Embed the video link into the About Page.
                        h4("Project Video"),
                        p("Here is a", tags$a("link", href = "https://youtu.be/OxUhK1O_Hko"), "to a video I created explaining the project."),
                        br(),
                        
                        #Note at the bottom includes a link to a form that alums
                        #can add their information to so that I can update the
                        #directory as needed.
                        h4("Note"),
                        p("I obtained the information used for this directory from the Harvard Varsity Club, which I found 
                        is not very reliable. I did my best to include LinkedIn information so that if the information in the 
                        directory is not accurate, there is a way for users to still find accurate information somewhere. If 
                        you are a Harvard Women's Lacrosse alum or a member of the current team and don't see your information 
                        in this directory, or if you would like to update your information, please fill out", 
                        tags$a("this form.", href = "https://forms.gle/Robh4N3u7dMDZ7JX9")), 
                    
                        br(),
                        hr(),
                        # Repository Link
                        h5("Github Repository:", tags$a("https://github.com/garotondo/WLAX_Directory", href = "https://github.com/garotondo/WLAX_Directory")))))


#The server portion of the app, which takes the inputs and produces outputs
server <- function(input, output, session) {
    
    output$search <- renderDT({
        table_data <- full_data %>% 
            select(name.x, home_city, home_state, graduation_year, 
                   house, concentration, company, role, industry,
                   preferred_email_address, linked_in) 
        
        colnames(table_data) <- c("Name", "Home City", "Home State", "Graduation Year", 
                                  "House", "Concentration", "Employer", "Role", "Industry", "Email", 
                                  "LinkedIn")
        table_data %>% 
            datatable(extensions = c('Responsive', 'Buttons'), options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'excel', 'pdf')
            ), rownames = FALSE)
    })
    
    
    #Create the map ouput using leaflet
    output$mymap <- renderLeaflet({
        mapStates = maps::map("state", fill = TRUE, plot = FALSE)
        leaflet(data = mapStates) %>% 
            addTiles() %>%
            addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)
        
        leaflet(data = mapStates) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addTiles() %>%
            # Making sure the map has the shapes of the states, is colorful, and has markers.
            addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%
            addMarkers(data = full_data, ~lng, ~lat, popup = ~as.character(name.x))
    })
}


# Run the application 
shinyApp(ui = ui, server = server)