overviewModuleInput <- function(id) {
  ns <- NS(id)
  tagList(argonRow(
    argonColumn(
    width = 4,
    #Define the overview cards similar to valueBoxOutput in shiny dashboard
    argonInfoCard(
      textOutput(ns('tweet_volume')),
                 'Number of Tweets',
                 '',
                 NULL,
                 NULL,
                 'fab fa-twitter',
      icon_background = "lightskyblue",
      
                 TRUE,
      background_color = 'default',width = 12,shadow = T
      )
    )
  ,
  argonColumn(
    width = 4,
    argonInfoCard(
      textOutput(ns('n_tweeters')),
      'Unique Users',
      '',
      NULL,
      NULL,
      'users',
      icon_background = "green",
      
      TRUE,
      background_color = 'default',width = 12,shadow = T
    )
              ),
  argonColumn(
    width = 4,
    argonInfoCard(
      textOutput(ns('avg_sentiment')),
      'Sentiment',
      '',
      NULL,
      NULL,
      'grin',
      icon_background = "orange",
      
      TRUE,
      background_color = 'default',width = 12,shadow = T
    )
    )
    
  ),
  #Define card for the visuals
  argonRow(
    argonCard(
      title = 'Why Are We Happy?',
      status = 'info',
      icon = 'chat-round',
      width = 12,
      div(class = 'animated infinite pulse', uiOutput(ns('top_word'))),
     
      argonRow(
      argonColumn(width = 12,
     
     plotOutput(ns('top5'),height  = 200)
     )),argonRow(
     argonColumn(width=4,
         knobInput(ns('top_retweets'),
                   label = h3('Select Top N Tweets'),
                   value = 10,
                   inputColor = '#8DD2C6',
                   fgColor = '#8DD2C6',
                   skin='tron')
       
       
     ),
     argonColumn(width=8,
                 uiOutput(ns('page_plot_title')),
                 plotOutput(ns('tweet_pages'))%>% withSpinner(
                   type = 2,
                   color = '#2DCE89',
                   color.background = '#fff'
                   
                 )
                 )
     ),br(),
     argonRow(
       argonColumn(width=12,h3('Top Three Tweets',style='text-align:center;')),
       argonColumn(width = 4,
                   htmlOutput(ns('first_tweet'))
       ),
       argonColumn(width = 4,
                   htmlOutput(ns('second_tweet'))
       ),
       argonColumn(width = 4,
                   htmlOutput(ns('third_tweet'))
       )
     )
    
    )
  ))
}

overviewModule <- function(input, output, session, data) {
  
  #Top tweet twitter is happy about is one with a top 5 sentiment and the most tweets
  top_tweet <- reactive({
    data %>%
      filter(sentiment>0)%>%
      top_n(5, sentiment) %>%
      top_n(1, tweet_volume) %>%
      pull(trend)
  })
  
  #getting tweet volume and converting it to a nice format. e.g 1000 will be 1K
  output$tweet_volume<- renderText({
    x=data %>%
      filter(sentiment>0)%>%
      top_n(5, sentiment) %>%
      top_n(1, tweet_volume) %>%
      pull(tweet_volume)
    case_when(
      nchar(as.character(x))>=7~ paste0(round(x/1000000,0),'M'),
      nchar(as.character(x))>=5~ paste0(round(x/1000,0),'K'),
      TRUE ~ paste0(round(x,0))
    )
      
  })
  
  #getting average sentiment of that trend from a sample number of tweets
  output$avg_sentiment<- renderText({
    data %>%
      filter(sentiment>0)%>%
      top_n(5, sentiment) %>%
      top_n(1, tweet_volume) %>%
      pull(sentiment)%>%
      round(2)
    
    
  })
  
  #getting unique number of users tweeting about that trend and converting to a nice format
  output$n_tweeters<- renderText({
    x=full_tweets()%>%
      distinct(screen_name)%>%
      count()%>%
      pull()
    
    case_when(
      nchar(as.character(x))>=7~ paste0(round(x/1000000,0),'M'),
      nchar(as.character(x))>=5~ paste0(round(x/1000,0),'K'),
      TRUE ~ paste0(round(x,0))
    )
  })
  
  #HTML for the top word which is animated using animate.css
  output$top_word <- renderUI({
   
    h5(
      glue('{top_tweet()}'),
      style = 'font-size: 3rem;
    color: #80B0D2;',
      tags$sup(
        style = 'font-size: 7pt;',
        argonTooltip(
          icon('question-circle'),
          position = 'top',
          title = 'The trend with the highest sentiment and most tweets'
        )
      )
    )
  })
  
  
  #Read in the total number of tweets on the top trend and convert the date back to date from numeric
  full_tweets <- reactive({
      full_tweets = dbGetQuery(tweets_database$con, 'SELECT * FROM Happy_Tweets')
      full_tweets$created_at <-
        as.POSIXct(full_tweets$created_at, origin = '1970-01-01')
      full_tweets
    
  })
  
  #read in tweet tokens
  tokens <- reactive({
   
      dbReadTable(tweets_database$con, 'Tokens')
    
  })
  
  #Create waffle plot of the top 5 words
  output$top5 <- renderPlot({
    
    top_5 <- tokens()%>%
      count(word)%>%
      arrange(-n)%>%
      top_n(5,n)%>%
      slice(1:5)
    
    values=top_5%>%pull(n)
    
    names(values)=top_5%>%mutate(text=paste0(str_to_title(word),': ',n))%>%pull(text)
    
    #A bit "mathy" but this scaling value always makes sure that the values are in hundreds except when the
    #number is close to 100 already to make for a nicer-looking waffle plot 
    
    scaling_value<-ifelse(max(values)>50,1,round(exp(-2.303*ceiling(log10(max(values))))*1000,0))
    
    #This is really the Rcolorbrewer set3 pallete but shuffle around a bit and to avoid
    #loaidng yet another library
    
    colors = c("#8DD3C7" ,"#FCB461" ,"#BEBADA" ,"#FB8072", "#80B1D3",'#FFFFB3',
               "#FDB462","#B3DE69" , "#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F")
   
    w=waffle(values*scaling_value,rows = 10,size = 0.5,legend_pos = 'bottom',colors = colors[1:length(values)],
             xlab = glue('{scaling_value} square(s) = 1 occurrence'),title = 'Top 5 Words')
    
   
    #Fix font family to whichever of this three is available
    
    
    w
  })
  
  
  
  #Activate a sleep to delay invoking the change of the viz becuase
  #the knob will move incrementally
  knob <- reactive({input$top_retweets})
  knob_debounced <- knob %>% debounce(1000)
  
  #Read in page visualization
  output$tweet_pages <- renderPlot({
    
   
    
    top100 <- full_tweets()%>%
      filter(is_retweet==0)%>%
      top_n(100,retweet_count)%>%
      arrange(created_at)%>%
      mutate(text=sapply(text, short_text)%>%as.character(),
             rank = dense_rank(desc(retweet_count)),
             status_id=as.numeric(as.factor(status_id)),
             high_retweets=rank<=as.numeric(knob_debounced()))
    
    top100%>%
      select(status_id,text,rank,high_retweets)%>%
      nest_paragraphs(text)%>%
      ggpage_build(page.col = "status_id", lpp = 4, ncol = 6)%>%
     mutate(title=TRUE)%>%
      ggpage_plot(paper.show = TRUE, 
                  paper.color = ifelse(top100$high_retweets, "#FFEC6E", "#B3E1CD"))
  })
  
  output$page_plot_title <- renderUI({
    div(
      h3(glue('Top 100 retweeted tweets on {top_tweet()} by time tweeted')),
      em(glue('Highlighting the top {input$top_retweets} retweeted tweets'))
      )
  })
  
  top_three <- reactive({
    full_tweets()%>%
      filter(is_retweet==0)%>%
      top_n(3,retweet_count)%>%
      arrange(-retweet_count)%>%
      slice(1:3)
  })
  
  output$first_tweet <- renderUI({
    url=top_three()%>%
      slice(1)%>%
      pull(status_url)
    
    tweet_url=glue('https://publish.twitter.com/oembed?url={url}&hide_media=true')
    text=httr::GET(tweet_url)%>%
      content()
    HTML(text$html)
      
  })
  
  output$second_tweet <- renderUI({
    url=top_three()%>%
      slice(1)%>%
      pull(status_url)
    
    tweet_url=glue('https://publish.twitter.com/oembed?url={url}&hide_media=true')
   
    text=httr::GET(tweet_url)%>%
      content()
    HTML(text$html)
    
  })
  
  output$third_tweet <- renderUI({
    url=top_three()%>%
      slice(1)%>%
      pull(status_url)
    
    tweet_url=glue('https://publish.twitter.com/oembed?url={url}&hide_media=true')
    
    text=httr::GET(tweet_url)%>%
      content()
    
    HTML(text$html)
    
  })
  output$tweet_timeline <- renderUI({
    tags$iframe(
      seamless = 'seamless1',
      src = 'timeline.html',
      scrolling = 'no',
      height = '100%',
      width = '100%'
    )
  })

  
  #return the full_tweets reactive object to be used in the other two modules
  return(full_tweets)
}
