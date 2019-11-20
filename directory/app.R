
library(shiny)
library(dplyr)
library(janitor)
library(readxl)
library(ggplot2)
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
                                  
                                  # Industry Input - input$industry (see Quantmod "period" argument")
                        
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
                                                "Musicology" = "Musicology"))
                                                
                                  br(),
                                  hr(),
                                  br(),
                                  
                                  
                                  
                                  
                                  
                                      
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
                                  
                                  
                                    
                       
                                      
                                      
                                      
                                    
                
                
                

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
