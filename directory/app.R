
library(shiny)
library(dplyr)
library(janitor)
library(readxl)
library(ggplot2)
library(leaflet)
library(maps)
library(tidyverse)

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

#Merge the datasets and get rid of repeat names.
data <- full_join(alumnae, wlax, by = c('last_name', 'graduation_year', 'count'))



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
                                      
                                      textInput(c("name", "first_name.x", "last_name"), "Please enter a full, first, or last name")
                                      
                                  ),
                                  
                                  br(),
                                  
                                  # Industry Input - input$industry (see Quantmod "industry" argument")
                        
                                  selectInput("industry", "Industry:",
                                              c("Finance" = c("Finance", "Venture Capital & Private Equity"),
                                                "Consulting" = "Consulting",
                                                "Law" = "Law",
                                                "Public Policy" = "Public Policy",
                                                "Hospitality" = "Hospitrality",
                                                "Food & Beverages" = "Food & Beverages",
                                                "Non-Profit" = "Non-Profit",
                                                "Public Sector" = "Public Sector",
                                                "Cosmetics" = "Cosmetics",
                                                "Marketing" = "Marketing",
                                                "Health Care" = c("Health Care", "Healthcare"),
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
                                                "Government" = c("Government", "Political Science and Government"),
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
                   
                    ###################################
                    # MAP PAGE
                    ###################################                            
                              
                    tabPanel("Map",
                             fluidPage(
                                 titlePanel("Locations of Alumni Based in the United States"),
                                 
                                 hr(),
                                 
                                 leafletOutput("mymap"),
                                 p(),
                                 actionButton("recalc", "New points")
                             )
                             
                             server <- function(input, output, session) {
                                 
                                 points <- eventReactive(input$recalc, {
                                     cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
                                 }, ignoreNULL = FALSE)
                                 
                                 output$mymap <- renderLeaflet({
                                     leaflet() %>%
                                         addProviderTiles(providers$Stamen.TonerLite,
                                                          options = providerTileOptions(noWrap = TRUE)
                                         ) %>%
                                         addMarkers(data = points())
                                 })
                             }
                                  
                     ###################################
                     # ABOUT PAGE
                     ###################################   
                             tabPanel("About",
                                      titlePanel(h1("Fixing the Flaws of Networking: Creating an Accurate Alumni Directory for the Harvard Women's Lacrosse Program")),
                                      
                                      
                                      hr(),
                                      
                                      # Overview Explanation
                                      
                                      h3("Overview"),
                                      h4("'You're doing Harvard wrong if you're not networking.' - David Kane"),
                                      h4("Networking is one of the most important skills an individual can develop at Harvard. 
                                         Though academics are important, the people you know are what is going to help you ultimately 
                                         land a job and launch a career after college. However this is the case, I found in my own job 
                                         search that Harvard's alumni resources are out-of-date and unreliable. To solve this problem, 
                                         I set out to develop an alumni directory - for the Harvard Women's Lacrosse Program.")
                                      
                                      # The br() function adds white space to the app.
                                    
                                      # Data Collection Explanation
                                      
                                      br(),
                                      br(),
                                      h3("Data Collection"), 
                                      h4("To develop the directory, I used four sources of data: 1) an excel spreadsheet from 
                                         the Harvard Varsit Club that included contact information; 2) Names of the Varsity 
                                         Letterwinners of Harvard Women's Lacrosse; 3) House and Concentration information from 
                                         the official Harvard Alumni Directory; and 4) Information from LinkedIn. The second, 
                                         third, and fourth sources I personally researched and collected their data to put in an 
                                         excel spreadsheet. I especially wanted to manually search individuals' LinkedIn accounts 
                                         to ensure that they were accurate matches so that if the given emails (by the Varsity Club) 
                                         were not accurate, then users would have the option to reach out to someone through LinkedIn.")
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
                                         where alums resided based on their LinkedIn profiles."),
                                      
                                      br(),
                                      br(),
                                      
                                      
                                      h3("About Me: Grace Rotondo"),
                                      h4("I am a junior at Harvard College studying Psychology and Economics. Additionally, I am a 
                                         member of the Women's Lacrosse Team, The Student-Athlete Advisory Committee, and Harvard 
                                         Undergraduate Women in Business. Contact me at: grotondo@college.harvard.edu."),
                                      
                                      
                                      br(),
                                      hr(),
                                      
                                      # Repository Link
                                      
                                      h4(a("Github Repository", href="https://github.com/wsmiles000/Trumps-Tweets-Stock-Market")),
                                      br()
                                      
                             )
                    )
                    
                    
                                  
                                  
                                  
                                      
                                      #Search bar to input names 
                                      searchInput(
                                          inputId = "search", label = "Enter your text",
                                          placeholder = "A placeholder",
                                          btnSearch = icon("search"),
                                          btnReset = icon("remove"),
                                          width = "450px"
                                      ),
                                      br(),
                                      verbatimTextOutput(outputId = "res")
                                  ),
                                  
                                  
                                    
                       
                                      
                                      
                                      

# Run the application 
shinyApp(ui = ui, server = server)
