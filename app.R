# load pacakages which is used in this shiny application
library(shiny)
library(leaflet)
library(plotly)
library(shinydashboard)
library(ggplot2)
library(dplyr)

###########################
# Load all data from file #
###########################

# Load all Australia schools information (included longitude and latitude which can used in map)
school_list <- read.csv("Australian_school_lists.csv")
school_list_update <- read.csv("Australian_school_lists_update.csv")

# Add popup information which can show school information when clicking school marker
school_list$popup <- paste0('<b>', school_list$School_Name, '</b><br>',
                            'State: ', school_list$State, '<br>',
                            'Type: ', school_list$Type, '<br>',
                            'Sector: ', school_list$Sector, '<br>',
                            'Suburb: ', school_list$Suburb, '<br>',
                            'Postcode: ', school_list$Postcode)

# Load data about people counts under different school levels and genders
type_education_sex <- read.csv("type_of_education_institution_attending_by_sex.csv")

# Load data about people count proportions under different school levels from 2006 to 2021
type_education_sex_year <- read.csv('proportion_student_by_sex_and_school_level.csv')

##################
# USER INTERFACE #
##################

# Dashboard Header # fixed width: 200
header <- dashboardHeader(title = 'EDU DATA', titleWidth = 200)

# Dashboard sidebar # fixed width: 200
sidebar <- dashboardSidebar(
  width = 200,
  
  # In this sidebar it have four tabs
  sidebarMenu(
    
    # Introduction to introduct this app
    menuItem('Introduction', tabName = 'introduction', icon = icon('house')),
    
    # School Map to show shcools on the map
    menuItem('School Map', tabName = 'map',icon = icon('map')),
    
    # Education data to show some statistical data about students
    menuItem('Education Data', tabName = 'education', icon = icon('book')),
    
    # School Search to search school and download filtered school lists
    menuItem('School Search', tabName = 'search', icon = icon('check'))
  )
)


# Dashboard Body
body <- dashboardBody(
  
  # The head information (when opening it in a browser)
  HTML("<head>
            <title>Australia Education</title>
        </head>"
  ),
  tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;}'))),
  
  # Set dashboard sidebar content format
  tags$style(HTML('.main-sidebar {
                  font-family: "Georgia", Times, "Times New Roman", serif;
                  font-size: 20px; }')),
  
  # Show the different tab interface 
  tabItems(
    
    # Introduction Tab
    tabItem(tabName = 'introduction',
            
            # Title
            h1("Australia Education Information", align = 'center',style = "font-family: 'times'; font-sie: 24px;font-weight: bold;"),
            br(),
            
            # Show a backgroud image and set its format
            p(img(src = 'https://i0.wp.com/oecdedutoday.com/wp-content/uploads/2021/10/Career-readiness-project-findings-blog.png?w=1200&ssl=1', 
                id = 'intro_background',
                width = '450px'),
              align = 'center'
            ),
            br(),
            
            # Use fluidRow layout
            fluidRow(
              
              # Use column layout
              column(width = 11, aligh = 'center',
                     
                     # Show app simple introduction
                     box(
                       title = 'Information about this application',
                       background = 'yellow',
                       width = '100%',
                       id = 'intro_info',
                       p('This shiny application is used to help users to find appropriate schools in Australia, 
                         and also show some statistical data about students in different school levels, states, and sex.')
                     ),
                     
                     # Show note Information - data resources etc.
                     box(
                       title = "Note",
                       background = 'green',
                       width = '100%',
                       id = 'intro_note',
                       p('The data is from these two websites:'),
                       p('https://www.abs.gov.au/'),
                       p('https://asl.acara.edu.au/'),
                       p('If you have more interest, you can check more information here.'),
                       p('All of the data we used is up to year 2021.')
                     )
              )
            )
    ),
    
    # Show tab school map interface
    tabItem(tabName = 'map',
            
            # Show Schools on the map
            absolutePanel(
              width = '100%',
              height = '100%',
              leafletOutput('school_map', width = '100%', height = '100%'),
              style = 'background:#ECF0F5;'
            ),
            
            # Show filter bar
            absolutePanel(
              h2('School Filter', style = "font-family: 'times'; font-sie: 24px;font-weight: bold;"),
              id = 'filter',
              class = "panel panel-default", fixed = TRUE,
              draggable = FALSE, top = 70, left = "auto", right = 20,
              width = '300px', height = "650px",
              style = 'background-color: white;
                       opacity:0.8;
                       padding: 20px 20px 20px 20px;
                       z-index = 50;
                       font-family: times; 
                       margin: auto;',
              
              # Filter state
              selectInput(
                'state',
                label = 'State or Territory',
                choices = c('All',sort(unique(school_list$State))),
                selected = 'VIC',
              ),
              
              # Filter school level
              selectInput(
                'type',
                label = 'School Type',
                choices = c('All',sort(unique(school_list$Type))),
                selected = 'Primary'
              ),
              
              # choose school sector
              radioButtons(
                'sector',
                label = "Sector",
                choices = c('All', 'Non-Gov','Gov'),
                selected = 'All'
              ),
              
              # Show a picture about student enrolment counts by State
              p('Student enrolment counts by State', align = 'center', style = "font-family: 'times'; font-sie: 20px;font-weight: bold;"),
              p(img(src = 'https://www.abs.gov.au/system/files/styles/complex_image/private/25aeeebdf20b1ffe71bf0f60ecacd2a3/Map%201%20-%20Student%20enrolment%20counts%20by%20state%20and%20territory%20and%20school%20affiliation%2C%202021.gif?itok=_lmfqVSl', 
                    id = 'count',
                    width = '270px'
                    ),
                align = 'center'
              )
            )
    ),
    
    # Show Tab education data interface
    tabItem(tabName = 'education',
            
            # Title
            h1("Australia Education Situation", align = 'center', style = "font-family: 'times'; font-sie: 24px;font-weight: bold;"),
            br(),
            
            # Use fluidRow layout
            fluidRow(
              
              # Use value box to show student count numbers in different school levels
              valueBox(484184, 'Preschool', icon = icon('child'), color = 'yellow'),
              valueBox(2075221, 'Primary School', icon = icon('plane'), color = 'green'),
              valueBox(1629622, 'Secondary school', icon = icon('clock'), color = 'orange'),
              valueBox(821800, 'University', icon = icon('car'), color = 'blue'),
              valueBox(601891, 'Vocational Education', icon = icon('eye'), color = 'red'),
              valueBox(242822, 'Other Education', icon = icon('user'), color = 'purple')
            ),
            
            # Use fluidRow layout
            fluidRow(
              
              # Show bar chart about people number counts in different school levels and genders
              box(
                plotlyOutput('plot_type_sex_education'),
                width = '100%'
              ),
              
              # Show line chart about student number proportion change in different school levels from 2006 to 2021
              box(
                plotlyOutput('plot_propotion_education_type_sex'),
                width = '100%'
              ),
                
              style = 'background:#ECF0F5;
                       padding: 10px 10px 10px 10px;
                       font-family: times; 
                       margin: auto;'
            )
    ),
    
    # Show tab school search interface
    tabItem(tabName = 'search',
            
            # title
            h1("School Search and Download", align = 'center', style = "font-family: 'times'; font-sie: 24px;font-weight: bold;"),
            br(),
            
            # Use fluidRow layout
            fluidRow(
              
              # Filter state
              column(width = 2,
                     selectInput('sstate',
                                 label = 'State or Territory',
                                 choices = c('All',sort(unique(school_list$State))),
                                 selected = 'VIC'
                     )
              ),
              
              # Filter school levels
              column(width = 2,
                     selectInput('stype',
                                 label = 'School Type',
                                 choices = c('All',sort(unique(school_list$Type))),
                                 selected = 'Primary'
                     )
              ),
              
              # Filter school sector
              column(width = 2,
                     selectInput('ssector',
                                 label = "Sector",
                                 choices = c('All', 'Non-Gov','Gov'),
                                 selected = 'All'
                     )
              ),
              
              # Filter school geolocation
              column(width = 2,
                     selectInput('sgeo',
                                 label = "Geolocation",
                                 choices = c('All', 'Major Cities','Inner Regional','Outer Regional','Remote','Very Remote'),
                                 selected = 'All'
                     )
              ),
              
              # Filter postcode
              column(width = 2,
                     textInput('spostcode',
                               label = "Postcode"
                     )
              ),
              
              # Show a download button to download filtered school lists
              column(width = 2,
                     downloadButton('download','Download',icon = icon('download'))
              ),
              style = 'background:#ECF0F5;
                       font-family: times; 
                       font-size: 10px;
                       font-colour: green;'
            ),
            
            # Show the filtered schoolinformation table
            fluidRow(
              column(12,
                     tableOutput('table')
              )
            )
    )
  )
)

# Define UI for application
ui <- dashboardPage(
  skin = 'green',
  header,
  sidebar,
  body
)

################
# SHINY SERVER #
################

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Tab school map
  
  # Filter school states
  filter_state <- reactive({
    # If choose all, filter nothing
    if(input$state == 'All'){
      school_list
    # else choose filtered school information
    }else{
      filter(school_list, State == input$state)
    }
  })
  
  # Filter school levels
  filter_school_type <- reactive({
    if(input$type == 'All'){
      filter_state()
    }else{
      filter(filter_state(), Type == input$type)
    }
  })
  
  # Filter school sectors
  filter_sector <- reactive({
    if(input$sector == 'All'){
      filter_school_type()
    }else{
      filter(filter_school_type(), Sector == input$sector)
    }
  })
  
  # Show map
  output$school_map <- renderLeaflet({
    leaflet(filter_sector()) %>%
      
      # add all school markers in the map
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addAwesomeMarkers(lng = ~Longitude,lat = ~Latitude,
                        icon = ~awesomeIcons(icon='fa-graduation-cap',library='fa',
                                             # school by government is green, else markers are red
                                             markerColor=case_when(Sector == 'Gov' ~ 'green',
                                                                   TRUE ~ 'red'),iconColor='#ffffff'),
                        label = ~School_Name,
                        # Show school info when click marker
                        popup = ~popup)
  })
  
  # Show bar char
  values <- reactiveValues()
  output$plot_type_sex_education <- renderPlotly({
    p <- ggplot(data = type_education_sex, aes(x = factor(Type, levels = unique(Type)),
                                               y = Value,
                                               fill = factor(Sex, levels = unique(Sex))
                                              )) +
      labs(x = 'type of education institution',
           y = 'number of people',
           fill = 'Gender'
           ) +
      geom_bar(
        stat = 'identity',
        position = position_dodge(0.9),
        width = 0.6,
        colour = 'black'
      ) +
      scale_fill_brewer(palette="Pastel1")+
      ggtitle("Student Count in different School Type and Sex")
    
    values$loaded <- TRUE
    ggplotly(p)
  })
  
  # Show line char
  output$plot_propotion_education_type_sex <- renderPlotly({
    p <- ggplot(data = type_education_sex_year, aes(x = factor(Time, levels = unique(Time)),
                                               y = Value,
                                               group = Type,
                                               color = Type
    )) +
      labs(x = 'Time',
           y = 'Proportion',
           fill = 'Type'
      ) +
      geom_line(
      ) +
      ggtitle("Studetn Count Proportion change in different Year")
    values$loaded <- TRUE
    ggplotly(p)
  })
  
  # Search tab server
  # Show school list table after filtering and download the filtered data
  
  # Filter states
  filter_sstate <- reactive({
    if(input$sstate == 'All'){
      school_list_update
    }else{
      filter(school_list_update, State == input$sstate)
    }
  })
  
  # Filter school levels
  filter_school_stype <- reactive({
    if(input$stype == 'All'){
      filter_sstate()
    }else{
      filter(filter_sstate(), Type == input$stype)
    }
  })
  
  # Filter school sectors
  filter_ssector <- reactive({
    if(input$ssector == 'All'){
      filter_school_stype()
    }else{
      filter(filter_school_stype(), Sector == input$ssector)
    }
  })
  
  # Filter school geolocation
  filter_sgeo <- reactive({
    if(input$sgeo == 'All'){
      filter_ssector()
    }else{
      filter(filter_ssector(), Geolocation == input$sgeo)
    }
    
  })
  
  # Choose postcode
  filter_spostcode <- reactive({
    if(!isTruthy(input$spostcode)){
      filter_sgeo()
    }else{
      filter(filter_sgeo(), Postcode == input$spostcode)
    }
  })
  
  # Show filtered school lists as a table
  output$table <- renderTable(filter_spostcode())
  
  # Execute the download operation
  output$download <- downloadHandler(
    filename = function() {
      paste('school list-', Sys.Date(), '.csv', sep='')
      },
    content = function(con) {
      write.csv(filter_spostcode(), con)
      }
    )
}

#############
# RUN SHINY #
#############

# Run the application 
shinyApp(ui = ui, server = server)
