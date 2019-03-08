demographicsModuleInput <- function(id){
  ns <- NS(id)
  tagList(
    argonRow(
      argonCard(
        title = 'Who Is Happy?',
        status = 'info',
        icon = 'circle-08',
        width = 12,
        
        argonRow(
          
        argonColumn(width = 6,
        h3('Spread of Sentiment by Gender', tags$sup(style='font-size: 7pt;',argonTooltip(icon('question-circle'),
                                                                                         position = 'top',
                                                                                         title = "Using the genderize API to predict a twitter user's gender based on their first name"))),
        em('Click on the boxplots to explode it to individual tweets. Double-click to restore.'),
        br(),
        bpexploderOutput(ns('gender_bplot'),
                         width = '100%',height = '100%')
        ),
        argonColumn(width=6,
        h3('Sunburst of Demographics',
           tags$sup(style='font-size: 7pt;',argonTooltip(icon('question-circle'),
                                                           position = 'top',
                                                           title = "Exploring the demographics of people tweeting on this trend using Source of tweet, Number of followers, Age of Account, Language and Verified Status."))
           )
       
        ,sunburstOutput(ns('demographics_sunburst'))
        )),
        argonRow(
          h3('Location of Tweets',style='text-align:center;'),
          em('Click on the circles to drill down into countries'),
          leafletOutput(ns('location_map'))
        )
      )
    )
  )
}

  demographicsModule <- function(input,output,session,data){
 
 
  #Read in sample tweets with gender categorization
  output$gender_bplot <- renderBpexploder({

    top_500_sentiment <-dbReadTable(tweets_database$con,'Top_500')
  
  bpexploder(data = top_500_sentiment,
             settings = list(
               groupVar = "gender",
               levels = c('female','male','Unknown'),
               levelColors = c("#8DD3C7", "#BEBADA",'#E7EAED'),
               yVar = "sentiment",
               tipText = list(
                 gender='Gender',
                 short_text = "Tweet",
                 sentiment='Average Sentiment'
               ),
               aspect=0.8,
               relativeWidth = 0.75
            ),width = 384,height = 480
  )
  
  })
  
  #Read in sunbusrt data and make visual
  output$demographics_sunburst <- renderSunburst({

      
      sunburst_data<-dbReadTable(tweets_database$con,'Sunburst')
      
      
     
      colors = c("#8DD3C7" ,"#FCB461" ,"#BEBADA" ,"#FB8072", "#80B1D3",'#FFFFB3',
                 "#FDB462","#B3DE69" , "#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F")
      
     sunburst(sunburst_data,colors =colors ,percent = T,legend = FALSE,width = '60%' )  
      
    
  })
  
  output$location_map <- renderLeaflet({
    geo_data <- dbReadTable(tweets_database$con,'Map_Table')
    icons <- awesomeIcons(
      icon = 'twitter',
      iconColor = '#80B0D2',
      library = 'fa',
      markerColor = 'white'
    )
    
    leaflet(geo_data)%>%
      addTiles() %>% addMarkers(
        clusterOptions = markerClusterOptions(),icon = list(iconUrl='https://upload.wikimedia.org/wikipedia/fr/thumb/c/c8/Twitter_Bird.svg/300px-Twitter_Bird.svg.png',iconSize=c(15,15)),
        label = ~text
      )
    
  })
}