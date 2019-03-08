timelineModuleInput <- function(id) {
  ns <- NS(id)
  tagList(
    argonRow(
      argonCard(
        title = 'When Were They Happy?',
        status = 'info',
        icon = 'watch-time',
        width = 12,
        
        argonRow(
          
          argonColumn(width = 12,
                      h5('Percentage of Tweets Every 4 Hours'),
        frappeOutput(ns('tweets_perc'),height = '130px')
        )),
  argonRow(
        argonColumn(width=6,
                    uiOutput(ns('textitle')),
                    
                    htmlOutput(ns('top1_cloud')),
                    
                    
                    div(
                      style = 'text-align:right;',
                      
                      argonBadge('12AM-5AM', pill = TRUE, status = 'primary'),
                      argonBadge('6AM-11AM', pill = TRUE, status = 'success'),
                      argonBadge('12PM-5PM', pill = TRUE, status = 'warning'),
                      argonBadge('6PM-11PM', pill = TRUE, status = 'info')
                    )
        ),
      argonColumn(
        width = 6,
        h3('Happy Trends Over Time'),
        fullcalendarOutput(ns('timeline_calendar'))
      )
        ),
        h5('Calendar Heatmap for Sentiment of Tweets'),
        htmlOutput(ns('calendar_heatmap1')),
        htmlOutput(ns('calendar_heatmap2')),
        htmlOutput(ns('calendar_heatmap3')),
        htmlOutput(ns('calendar_heatmap4'))
        ,em('All times are GMT',style='text-align:center;'))
  )
  )
}

timelineModule <- function(input, output, session, data) {
  
  #Calculate the number of tweets every 4 hours
  output$tweets_perc <- renderFrappe({
  
  time_frame_df <- data%>%
    count(time_frame)
  
  time_frame_df <- time_frame_df %>%
    arrange(factor(time_frame, levels = c(
      '12am-4am',
      '4am-8am',
      '8am-12pm',
      '12pm-4pm',
      '4pm-8pm',
      '8pm-12am'
    )))
  data <- list(
    labels= time_frame_df%>%pull(time_frame),
    datasets=list(
      list(
        title='Number of Tweets',
        values= time_frame_df%>%pull(n)
      )
    )
  )
  rfrappe(list(
    #title = "My Awesome Percentage Chart",
    data = data,
    type = "percentage"
    
  ))
  })
  
  #Title for the biocricos plot
  output$textitle <- renderUI({
    
    
    h5(
      glue(
        'How Our Conversation on this trend Changed Throughout The Day'
      ),
      style = 'text-align:center;',
      tags$sup(
        style = 'font-size: 7pt;',
        argonTooltip(
          icon('question-circle'),
          position = 'top',
          title = 'The words at the edge of each circle represent the most common words in each hour of the day
                                                         The lines show the trend in number of tweets in every hour
                                                         The links show the similarity in tweets between in each hour'
        )
      )
    )
    
    
    
  })
  
  output$top1_cloud <- renderUI({
    
    
    
    
    tags$iframe(
      seamless = 'seamless1',
      src = 'biocircos.html',
      scrolling = 'no',
      height = 260,
      width = 300
    )
    
    
  })
  
  #Getting the calendar heatmap to work was... annoying to say the least.
  #First off, the itemSelector option which lets you hadd a next and previous button
  #to the calendar when it cannot fit into the width of the page does not come up
  #so I had to be somewhat creative and creae four calendar maps for every 6 hours
  output$calendar_heatmap1<- renderUI({
    
    
    tags$iframe(
      seamless = 'seamless1',
      src = 'calheatmap1.html',
      scrolling = 'no',
      height = 150,
      width = 610
    )
    
    
    })
  
  
  output$calendar_heatmap2<- renderUI({
    
    
    tags$iframe(
      seamless = 'seamless1',
      src = 'calheatmap2.html',
      scrolling = 'no',
      height = 150,
      width = 600
    )
    
    
  })
  
  output$calendar_heatmap3<- renderUI({
    
    
    tags$iframe(
      seamless = 'seamless1',
      src = 'calheatmap3.html',
      scrolling = 'no',
      height = 150,
      width = 600
    )
    
    
  })
  
  output$calendar_heatmap4<- renderUI({
    
    
    tags$iframe(
      seamless = 'seamless1',
      src = 'calheatmap4.html',
      scrolling = 'no',
      height = 150,
      width = 600
    )
    
    
  })
  
  output$timeline_calendar <- renderFullcalendar({
    calendar_df <- dbReadTable(tweets_database$con,'Top_Words_Table')
    calendar_df$stringsasFactors <- NULL
    calendar_df <- calendar_df%>%
      mutate_if(is.numeric,~as.Date(.,origin='1970-01-01'))
    fullcalendar(calendar_df,)
  })
  
  
  
  
}