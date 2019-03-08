

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #Read in the overview table from the sqlite database refresh every 24 hours using a job scheduler
  #this contains the overall trends available with the sentiment of a sample of tweets comupted as a column called sentiment
    trends_sentiment <-reactive({

     
        dbReadTable(tweets_database$con,'OverView')
      
    })
    
    #Get number of trends with positive sentiments for the guage 
    no_of_happy<- reactive({
       
      trends_sentiment()%>%
            filter(sentiment>0)%>%
            nrow()
    })
    output$happiness_gauge <- renderGauge({
      
        gauge(
            no_of_happy()*10,
            min = 0,
            max = 100,
            symbol = '%',
            abbreviate = T,
            abbreviateDecimals = 1,
            gaugeSectors(
                danger = c(0, 59.9),
                warning = c(60, 79.9),
                success = c(80, 100),
                colors = c('#6EEB83', '#FDCB6E', '#FF7675')
            )
        )
    })
    #Determine the icon to be used based on the number of trneds with positive sentiment
    output$happiness_icon<- renderUI({
        
        icon_class <-case_when(no_of_happy()<6 ~ "far fa-sad-tear",
                  no_of_happy()<8 ~ "fas fa-meh",
                  no_of_happy()>=8 ~ "fas fa-grin-beam"
                  )
        icon(icon_class)
        
      
    })
    res<-callModule(overviewModule,'default',trends_sentiment())
    callModule(demographicsModule,'default',res())
    callModule(timelineModule,'default',res())
    

})
