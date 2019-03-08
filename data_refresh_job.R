#########################
#This script is a scheduled job which runs everyday at 00:00 hours to refresh the database 
#and re-deploy the application. The idea of this is to abstract the app away from heavy comuptations 
#as possible and make it faster for users
#########################
library(rtweet)
library(tidyverse)
library(widyr)
library(sentimentr)
library(stringi)
library(glue)
library(tidytext)
library(DBI)
library(httr)
library(BioCircos)
library(xml2)
library(lubridate)
library(rChartsCalmap)
library(htmlwidgets)

#Set the working directory
setwd('C:/Users/tf452yw/Documents/TheArtandScienceofData/Angry_Happy_Twitter')

#create token
token <- create_token(app = 'TheArtandScienceofData','wn7M23NbDoOAWogW3KpebX5Cq','cwynWugYkE3Dn63qgKapLgxNegEPJWfquiqWfibBeQTmRYWxvG','370018889-n8oOECLBk89f19bvU9uUt2nlISAnLqF9cfczjTaf','xxxx',set_renv = FALSE)



#Read in the mapping between ISO language codes and its descriptions
language_codes <- read.csv('language_codes.csv',stringsAsFactors = F)
geo_mapping <- read.csv('geo_mapping.csv',stringsAsFactors = F)

#--Global Functions--#

#This predicts the gender of a person given the first name using the genderize API
get_gender <- function(name){
  first_name= str_split(name,' |_',simplify = T)[1]%>%str_to_lower()
  res=GET(glue('https://api.genderize.io/?name={first_name}'))%>%httr::content()
  gender=res$gender
  return(gender)
}

#This creates a short text of 10 characters (or less) of the tweet which is used in the exploding box plots
short_text <- function(text){
  text=str_split(text,' ',simplify = T)
  if(length(text)<10){
    text%>%paste(collapse = ' ')%>%
      paste0('...')
  }
  else{
    text <- text[1:10]%>%paste(collapse = ' ')%>%
      paste0('...')
  }
  return(text)
}


#This gets the sample sentiment for each trend used in the parent module for the happiness guage and
#used to determine the top trend for the other modules
get_sample_sentiment <- function(trend){
  #'''
  #Takes a sample of 100 tweets and gets the average sentiment
  #Input: Trend as a string
  #Output:Average Sentiment
  #'''
  tweets <- search_tweets(trend,token = token)
  sentiment <-with(
    tweets, 
    sentiment_by(
      get_sentences(text)
    )
  )
  sentiment <- sentiment%>%
    filter(ave_sentiment !=0)
  mean_sentiment <- mean(sentiment$ave_sentiment)
  return(mean_sentiment)
}



#---Main App---#

#Get worldwide trends and keep the top 10 by tweet volume
trends <- get_trends("worldwide",token = token)
trends<-trends%>%
  top_n(10,tweet_volume)

#Remove trends that are not ASCII characters. 
#This is because the tend to be harder to use for most of the rest of the analysis 
trend_sentiment <-trends%>%
  filter(stringi::stri_enc_isascii(trend))%>%
  mutate(sentiment=sapply(trend, get_sample_sentiment))


#Get the top trend
top_tweet <- trend_sentiment %>%
  filter(sentiment>0)%>%
  top_n(5, sentiment) %>%
  top_n(1, tweet_volume) %>%
  pull(trend)


#Get the tweet volume of the top trend
tweet_volume<-trend_sentiment %>%
  filter(sentiment>0)%>%
  top_n(5, sentiment) %>%
  top_n(1, tweet_volume) %>%
  pull(tweet_volume)

#use the lower value between 100k and the tweet volume to reduce the time it take to do the calculations
tweet_volume <- min(tweet_volume,100000)
#---Modules---#

#Get all tweets for the trend
full_tweets <- search_tweets(
  top_tweet,
  tweet_volume,
  token = token,
  retryonratelimit = TRUE,
include_rts=FALSE
)


#Keep only tweets created between the time of running this app which is 00:00 hours everyday
#and the day before 

full_tweets <- full_tweets%>%
  filter(created_at>=Sys.Date()-1)

#Create a bin of followers
full_tweets <- full_tweets%>%
  mutate(follower_groups=case_when(
    followers_count<=100 ~ '0 to 100 Followers',
    followers_count<=1000 ~ '101 to 1000 Followers',
    followers_count<=10000 ~ '1001 to 10000 Followers',
    followers_count<=100000 ~ '10001 to 100000 Followers',
    TRUE ~ '1000001+ Followers'
  ))


#Create a bin of type of accounts
full_tweets <- full_tweets%>%
  
  mutate(account_created_at= 
           as.POSIXct(account_created_at,origin = '1970-01-01'),
         
         created_group=case_when(
           year(account_created_at)<=2009 ~ 'Veteran Tweeters',
           year(account_created_at)<=2012 ~ 'Middle Aged Tweeters',
           TRUE ~ 'New Tweeters'
         ))


#Create a bin of tweet time for every 4 hours
full_tweets <- full_tweets%>%
  arrange(created_at)%>%
  mutate(time_frame=case_when(
    hour(created_at)<=4 ~  '12am-4am',
    hour(created_at)<=8 ~  '4am-8am',
    hour(created_at)<=12 ~ '8am-12pm',
    hour(created_at)<=16 ~ '12pm-4pm',
    hour(created_at)<=20 ~ '4pm-8pm',
    TRUE ~                 '8pm-12am'
  ))

#Add the language code
full_tweets <- full_tweets%>%
  left_join(language_codes)


#Get tokens for tweets in english (because our stopword dictionary only works for that and another 
#reason I filtered for only ASCII characters. Doesn't catch all the cases to be honest but does 
#a good job)

tokens <- full_tweets %>%
  filter(lang == 'en') %>%
  #Ensure to remove the actual trend name from each tweet
  mutate(
    text = str_replace_all(str_to_lower(text), str_to_lower(top_tweet), '') %>%
      str_trim(),
    hour = hour(created_at),
    minute = ceiling(minute(created_at) / 6)
  ) %>%
  mutate(text = str_replace_all(text, 'https.*', '')) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(
    nchar(word) > 2 &
      !str_detect(word, '[0-9]+') &
      stri_enc_isascii(word) &
      !str_detect(word, '[:punct:]+') &
      !str_detect(source, 'bot')
  ) %>%
  count(hour, minute, word) %>%
  filter(
    word != str_replace_all(str_to_lower(top_tweet), '[:punct:]', '') &
      word != 'https' & word != 'amp'
  ) %>%
  
  rename(freq = n)


#Create a reference table for the naming convetion for each hour
hour_ref_table <- tokens %>%
  distinct(hour) %>%
  mutate(description = 
           glue(
             "{ifelse(hour<=12,hour,hour-12)}{ifelse(hour<12,'am','pm')}"
           )
         
  )


#Calculate the similarity between tweets at different times of the day
similarity <- tokens %>%
  left_join(hour_ref_table) %>%
  pairwise_similarity(description, word, freq, upper = FALSE) %>%
  arrange(-similarity) %>%
  filter(similarity >= 0.5)

#Get sample of 500 tweets
top_500 <-full_tweets%>%
  sample_n(500)

#Create a refrence table for distinct number of names and predict gender. Using distinct and a
# reference table because there might be duplicate names and we do not want to make unneccessary 
#calls to the API

ref_gender <- top_500%>%
  distinct(name)%>%
  mutate(gender=sapply(name,get_gender))

ref_gender$gender <- as.character(ref_gender$gender)

top_500 <- full_tweets%>%
  semi_join(top_500,'screen_name')

#Calculate the sentiment using sentimentr
sentiment <-with(
  top_500, 
  sentiment_by(
    get_sentences(text)
  )
)
top_500$sentiment <- sentiment$ave_sentiment

#Add gender to the main table of 500 tweets
top_500 <- top_500%>%
  left_join(ref_gender)


#Add the short text
top_500_sentiment <- top_500%>%
  mutate(short_text=sapply(text,short_text)%>%as.character())%>%
  filter(sentiment !=0)%>%
  as.data.frame()


top_500_sentiment <-top_500_sentiment%>%
  mutate(gender=if_else(is.na(gender)|gender=='NULL','Unknown',gender))

#Connect to the database and write all the relevant objects to their respective tables
tweets_database=src_sqlite("Tweets",create = F)
dbWriteTable(tweets_database$con,'Happy_Tweets',full_tweets%>%
               select_if(Negate(is.list)), overwrite = TRUE)
dbWriteTable(tweets_database$con, 'Tokens', tokens, overwrite = TRUE)
dbWriteTable(tweets_database$con, 'Overview', trend_sentiment, overwrite = TRUE)
dbWriteTable(tweets_database$con,'Top_500',top_500_sentiment%>%
               select_if(Negate(is.list)),overwrite=TRUE)

#Create the biocricos visual and save as html
chrVert <- tokens %>%
  group_by(hour) %>%
  top_n(1, freq) %>%
  
  mutate(word = paste0(word, collapse = ', ')) %>%
  arrange(hour) %>%
  distinct(hour, word) %>%
  mutate(word = glue(
    "{ifelse(hour<=12,hour,hour-12)}{ifelse(hour<12,'am','pm')}"
  )) %>%
  pull(word)

colors <- tokens %>%
  group_by(hour) %>%
  top_n(1, freq) %>%
  
  mutate(word = paste0(word, collapse = ', ')) %>%
  arrange(hour) %>%
  distinct(hour, word) %>%
  mutate(color=case_when(
    hour<=6 ~  '#BDB9DA',
    hour<=12 ~  '#8DD2C6',
    hour<=18 ~ '#FCB461',
    TRUE ~                 '#80B0D2'
  )) %>%
  pull(color)

lengthChr = numeric(0)

for (i in 1:length(chrVert)) {
  lengthChr[i] = 12
}
names(lengthChr) = chrVert
posVert = tokens %>%
  group_by(hour, minute) %>%
  summarise(num = n()) %>%
  
  arrange(hour, minute) %>%
  pull(num)

no_of_hours <- unique(tokens$hour)%>%length()
tracks = BioCircosLineTrack(
  'LineTrack',
  rep(as.character(chrVert), rep(11, no_of_hours)),
  rep(1:11, length(chrVert)),
  values = posVert,
  color = '#172b4d'
)

tracks = tracks + BioCircosBackgroundTrack('Bg', fillColors = '#f4f5f7', borderSize = 0)


link_1 <- similarity %>%
  pull(item1) %>%
  as.character()

link_2 <- similarity %>%
  pull(item2) %>%
  as.character()

link_1_pos <- rep(5, length(link_1))
link_2_pos <- rep(5, length(link_1))

tracks = tracks + BioCircosLinkTrack(
  'myLinkTrack',
  link_1,
  link_1_pos,
  link_1_pos,
  link_2,
  link_2_pos,
  link_2_pos,
  maxRadius = 0.5,
  axisPadding  = 0,color = '#80B0D2'
)
bc=BioCircos(
  tracks,
  chrPad = 0.05,
  genomeFillColor = colors,
  zoom = FALSE,
  genome = as.list(lengthChr),
  #displayGenomeBorder = FALSE,
  LINEMouseOutDisplay = TRUE,
  LINEMouseOverTooltipsHtml01 = "Trend of tweets in this hour",
  LINKMouseOverStrokeColor = "#172b4d",
  LINKMouseOverTooltipsHtml01 = "Similarity of tweets between time periods",
  width = '300px',
  height = '250px'
  
)

bc$sizingPolicy$padding=0

#save as widget
saveWidget(bc, "biocircos.html",selfcontained = F,libdir ='C:/Users/tf452yw/Documents/TheArtandScienceofData/Angry_Happy_Twitter')
file.copy("biocircos.html", "www/biocircos.html",overwrite = T)
file.remove("biocircos.html")

#collect sample of 150 tweets every hour
hour_data <- full_tweets%>%
  mutate(Hour=hour(created_at))
  

num <-min(pull(count(hour_data,Hour)),150) 

hour_data <- hour_data%>%
  group_by(Hour)%>%
  sample_n(num)

#get sentiment for each tweets
sentiment <-with(
  hour_data,
  sentiment_by(
    get_sentences(text)
  )
)
hour_data$sentiment=sentiment$ave_sentiment



counted_hour_data<-hour_data%>%
  ungroup()%>%
  mutate(   rounded_date=floor_date(created_at,unit='minute'))%>%
  group_by(rounded_date)%>%
  summarise(sentiment=mean(sentiment))%>%
  arrange(rounded_date)


counted_hour_data=counted_hour_data%>%
  select(rounded_date,sentiment)%>%
  mutate(sentiment=round(sentiment,2),
         percrank=rank(sentiment)/length(sentiment))


#Get the 25%, 75% and 100% percentile for legend of the heatmap
lower <- 0

middle <- counted_hour_data%>%
  filter(percrank<=0.75 & percrank>0.5)%>%
  pull(sentiment)%>%
  max()

upper <- counted_hour_data%>%
  filter(percrank<=1& percrank>0.75)%>%
  pull(sentiment)%>%
  max()

day1=min(counted_hour_data%>%pull(rounded_date)%>%date()%>%unique())
day2=max(counted_hour_data%>%pull(rounded_date)%>%date()%>%unique())
cal1 = calheatmap(x = 'rounded_date', y = 'sentiment',
                  data = counted_hour_data,
                  domain = 'hour',
                  subdomail='x_min',
                  start = day1,
                  legend=c(lower,middle,upper),
                  legendColors =list(
                    min= "#FA8071",
                    max="#8DD2C6",
                    empty='#efefef'
                  ),
                  itemName = 'average sentiment',
                  range = 8,cellRadius=5,
                  displayLegend=FALSE
)
cal1$sizingPolicy$padding=0
cal2 = calheatmap(x = 'rounded_date', y = 'sentiment',
                  data = counted_hour_data,
                  domain = 'hour',
                  subdomail='x_min',
                  start = glue('{day1}@08:00'),
                  legend=c(lower,middle,upper),
                  legendColors =list(
                    min= "#FA8071",
                    max="#8DD2C6",
                    empty='#efefef'
                  ),
                  itemName = 'average sentiment',
                  range = 8,cellRadius=5,
                  displayLegend=FALSE
)
cal2$sizingPolicy$padding=0
cal3 = calheatmap(x = 'rounded_date', y = 'sentiment',
                  data = counted_hour_data,
                  domain = 'hour',
                  subdomail='x_min',
                  start = glue('{day1}@16:00'),
                  legend=c(lower,middle,upper),
                  legendColors =list(
                    min= "#FA8071",
                    max="#8DD2C6",
                    empty='#efefef'
                  ),
                  itemName = 'average sentiment',
                  range = 8,cellRadius=5,
                  displayLegend=FALSE
)
cal3$sizingPolicy$padding=0
cal4 = calheatmap(x = 'rounded_date', y = 'sentiment',
                  data = counted_hour_data,
                  domain = 'hour',
                  subdomail='x_min',
                  start = glue('{day1}@24:00'),
                  legend=c(lower,middle,upper),
                  legendColors =list(
                    min= "#FA8071",
                    max="#8DD2C6",
                    empty='#efefef'
                  ),
                  itemName = 'average sentiment',
                  range = 1,cellRadius=5
)
cal4$sizingPolicy$padding=0

#function to save heatmaps
save_heatmaps <- function(num){
  
  saveWidget(eval(parse(text=paste0('cal',num))), sprintf("calheatmap%s.html",num),selfcontained = F,libdir ='C:/Users/tf452yw/Documents/TheArtandScienceofData/Angry_Happy_Twitter')
  file.remove(sprintf("www/calheatmap%s.html",num))
  file.copy(sprintf("calheatmap%s.html",num), sprintf("www/calheatmap%s.html",num))
  file.remove(sprintf("calheatmap%s.html",num))
}
sapply(1:4, save_heatmaps)


#create sunburst data
sunburst_data<-full_tweets%>%
  add_count(source)%>%
  filter(n>=300)%>%
  select(-n)%>%
  add_count(language)%>%
  filter(n>=500)%>%
  mutate(verified=ifelse(verified==0,'Not Verified','Verified'))%>%
  count(source,follower_groups,created_group,language,verified)

sunburst_data <-sunburst_data%>%
  unite('sequence',-n,sep = '-')%>%
  as_tibble()

geo_data <-full_tweets%>%
  filter(!is.na(country_code) & country_code!='')%>%
  count(country_code,country)%>%
  left_join(geo_mapping,by=c('country_code'='country'))%>%
  mutate(text=glue('{country}
  
                    Number of Tweets: {n*100}'))

dbWriteTable(tweets_database$con,'Map_Table',geo_data,overwrite=TRUE)


dbWriteTable(tweets_database$con,'Sunburst',sunburst_data,overwrite=TRUE)

colors =c('#8DD2C6','#BDB9DA','#FA8071','#80B0D2','#FCB461','#BB80BC')
calendar_df <- data.frame(title=top_tweet,
                          start=Sys.Date()-1,
                          end=Sys.Date()-1,
                          color=colors[ceiling(runif(1,1,6))],stringsasFactors=F)


dbAppendTable(tweets_database$con, "Top_Words_Table", calendar_df)
#copy all dependency folders into the www folder
files=list.dirs(recursive = F)
files=files[!str_detect(files,'www|modules')]
files=str_sub(files,3)
copy_directories <- function(folder){
  
  dir.create(glue('./www/{folder}'))
  
  inner_files <-list.files(folder)
  for (i in inner_files){
    file.copy(glue('{folder}/{i}'),glue('./www/{folder}'),overwrite = TRUE)
    
  }
}

walk(files,copy_directories)

#delete folders in root folder
unlink(files,recursive = T)

#Deploy App
library(rsconnect)
deployApp(appDir = 'C:/Users/tf452yw/Documents/Happy_Twitter')
