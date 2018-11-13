# Twitter Scraping Demo for WWC 11/7/18
# Emily Jones
# emily.jones@sony.com

# Load the required Packages
library(twitteR)
library(RCurl)
library(httr)
library(tm)
library(wordcloud)
library(syuzhet)
library(plotly)
library(rlang)

# Twitter API
twitterAPIKey <- "____" #Insert Your Key from Twitter
twitterAPISecret <- "____" #Insert Your Key from Twitter
twitterAccessToken <- "____" #Insert Your Key from Twitter
twitterTokenSecret <- "____" #Insert Your Key from Twitter

# Connect to Twitter
setup_twitter_oauth(consumer_key = twitterAPIKey,
                    consumer_secret = twitterAPISecret,
                    access_token = twitterAccessToken,
                    access_secret = twitterTokenSecret)

# Retrieve info from Twitter
# Set search string to #Bitcoin Set number of tweets to 1000 Set language to English
searchString <- "#WomenWhoCode"
numberOfTweets <- 4000
tweets <- searchTwitter(searchString, n = numberOfTweets, lang="en")

# store the tweets into dataframe
tweets.df = twListToDF(tweets)

# CLEANING TWEETS
tweets.df$text=gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text)
tweets.df$text = gsub("@\\w+", "", tweets.df$text)
tweets.df$text = gsub("[[:punct:]]", "", tweets.df$text)
tweets.df$text = gsub("[[:digit:]]", "", tweets.df$text)
tweets.df$text = gsub("http\\w+", "", tweets.df$text)
tweets.df$text = gsub("[ \t]{2,}", "", tweets.df$text)
tweets.df$text = gsub("^\\s+|\\s+$", "", tweets.df$text)

tweets.df$text <- iconv(tweets.df$text, "UTF-8", "ASCII", sub="")

# Getting sentiments score for each tweet
# Lets score the emotions on each tweet as syuzhet breaks emotion into 10 different categories.

# Emotions for each tweet using a sentiment dictionary
emotions <- get_nrc_sentiment(tweets.df$text)
emo_bar <- colSums(emotions)
emo_sum <- data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion <- factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

# Visualize the emotions
p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion Type for hashtag: #WomeWhoCode")
p

# Create comparison word cloud data
wordcloud_tweet = c(
  paste(tweets.df$text[emotions$anger > 0], collapse=" "),
  paste(tweets.df$text[emotions$anticipation > 0], collapse=" "),
  paste(tweets.df$text[emotions$disgust > 0], collapse=" "),
  paste(tweets.df$text[emotions$fear > 0], collapse=" "),
  paste(tweets.df$text[emotions$joy > 0], collapse=" "),
  paste(tweets.df$text[emotions$sadness > 0], collapse=" "),
  paste(tweets.df$text[emotions$surprise > 0], collapse=" "),
  paste(tweets.df$text[emotions$trust > 0], collapse=" ")
)

# create corpus
corpus = Corpus(VectorSource(wordcloud_tweet))

# remove punctuation, convert every word in lower case and remove stop words

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english")))
corpus = tm_map(corpus, stemDocument)

# create document term matrix

tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)
tdmnew <- tdm[nchar(rownames(tdm)) < 11,]

# column name binding
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdmnew) <- colnames(tdm)
comparison.cloud(tdmnew, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)