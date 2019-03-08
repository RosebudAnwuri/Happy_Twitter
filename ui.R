#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny) #for... well shiny
library(argonDash) #for UI interface
library(argonR) #for UI compenents
library(shinycssloaders) #for css loaders
library(rtweet) # for connecting to twitter API
library(tidytext) #for NLP
library(flexdashboard) #for guage ouput (seems a waster having to load a whole library just for that)
library(dplyr) #for the tidy data manipulation
library(stringr) #for string manipulation
library(sentimentr) #for calculating dictionary based sentiment which better accuracy on average: https://github.com/trinker/sentimentr#comparing-sentimentr-syuzhet-meanr-and-stanford
library(stringi) #for the is_ascii function to deal with non ASCII characters
library(BioCircos) #for the Biocircos visualization
library(lubridate) #for data manipulation
library(DBI) #for handling database connections
library(glue) #for 'glueing' strings together in a much tidier format
library(widyr) #for carrying out wide data calculation like similarity
library(httr) #connecting to RESTful APIs
library(bpexploder) #for exploding box plots visualization
library(tidyr) #for tidying data
library(htmlwidgets) #imported for saving d3.js visalizations at html files due to d3 version clashes
library(waffle) #for waffle visualization
library(rfrappe) #for frappe visulizations
library(rChartsCalmap) #for calendar heatmap
library(sunburstR) #for sunburst diagram
library(shinyWidgets) #for knobinput
library(ggpage) #ggpage viz
library(leaflet) #map viz
library(dbplyr) #for communicating with databases
library(fullcalendar) #for calendar viz
token <- create_token(app = 'TheArtandScienceofData','xxxx'
                      ,'xxxx'
                      ,'xxxx'
                      ,'xxxx'
                      ,set_renv = FALSE)

# Define UI for application
ui=argonDashPage(
    title = 'How Happy is Twitter Today?',
    author = 'Rosebud',
    description = "Rstudio's Shiny Contest",
    sidebar = argonDashSidebar(
        vertical = TRUE,
        skin = 'dark',
        background = "white",
        size = "md",
        side = "left",
        id = "my_sidebar",
        brand_url = "http://theartandscienceofdata.wordpress.com/blog",
        brand_logo = "icon.png",
        argonSidebarMenu(
            #Defining the tabs
            argonSidebarItem(
                tabName = 'main',
                icon = 'chart-bar-32',
                icon_color = '#36F1CD',
                'Dashboard'
            ),
            argonSidebarItem(
                tabName = 'about',
                icon = 'circle-08',
                icon_color = '#36F1CD',
                'About'
            )
        )
        
    ),
    navbar = argonDashNavbar(
        argonDropNav(
            title = 'How Happy is Twitter Today?',
            src = "icon.png", 
            orientation = "right"
            
        )
    ),
    header = argonDashHeader(
        gradient = FALSE,
        color = 'info',
        separator = TRUE,
        separator_color = "info",
        
        #Add the happiness gauge to the header aling with a hover description
        h4('The Happiness Gauge',style='color:white;text-align:center;',
           tags$sup(style='font-size: 7pt;',argonTooltip(icon('question-circle'),
                        position = 'top',
                        title = 'This calculates the number of the the top 10 trends that have a positive sentiment.'))
           ),
        
        uiOutput('happiness_icon'),
        
        gaugeOutput('happiness_gauge') %>% withSpinner(
            type = 2,
            color = '#2DCE89',
            color.background = '#11cdef'
            
        )
    ),
    body = argonDashBody(
        #include css and js files to control style and fix issue with visualizing calendar heatmap iframe
        tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        tags$script(src="heatmap.js")
    ),
        argonTabItems(
            argonTabItem(
                'main',
                #show loading SVG while page is loading
                conditionalPanel( condition="typeof output.happiness_gauge == 'undefined'",
                                  
                            div(img(src='loading.svg',width='30%'),style='text-align:center;')        
                                  
                ),
                #show output of modules only if Shiny calculations are complete
               conditionalPanel( condition="typeof output.happiness_gauge !== 'undefined' ",
               
                                 overviewModuleInput('default'),
                                 demographicsModuleInput('default'),
                                 timelineModuleInput('default')
                                 
               )
            
            ),
            argonTabItem(
                #About Me!
                'about',
                conditionalPanel( condition="typeof output.happiness_gauge == 'undefined'",
                                  
                                  div(img(src='loading.svg',width='30%'),style='text-align:center;')        
                                  
                ),
                conditionalPanel( condition="typeof output.happiness_gauge !== 'undefined' ",
                argonUser(
                    title = 'Rosebud',
                    subtitle = "R & Dog Lover.",
                    url = 'http://theartandscienceofdata.wordpress.com/blog',
                    src='profile.jpg',
                    #stats=NULL,
                    p("Hi There! I'm so glad you visited this page! My name is Rosebud Anwuri. A Black, Female and Self-Taught Data Scientist working with a consulting firm in London (where I recently moved to from Nigeria)
                    I am passionate about democratizing data science knowledge and showcasing the power of data through story-telling and data-driven applications. I write about all data sciency stuff here: http://theartandscienceofdata.wordpress.com/blog.
                    "),
                    br(),
                    h3('Why This Project?',style='text-align:center;'),
                    p('As an avid twitter user (A fancy way of saying that I am always on twitter) it is easy to get overwhelmed by the constant stream of negative news and toxicity so I created a simple Shiny app that tells you what people are happy about on Twitter, who they are and why they are happy. Hopefully you enjoy using it as much as I enjoyed created this! :)')
                )
            )
                
            )
        )
    )
)
