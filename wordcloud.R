# This code is intended to load tweets from the downloadable data users can obtain from Twitter
# This code assumes the Twitter data file is in the Downloads folder of a Mac.
# The filepath variable below must be modified to match the location of your data file

# Install required packages

install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes

# Load required libraries
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# Load the user's tweets, which are in the 6th column of a Twitter user's downloadable data file, at least in my case
filepath<- "~/Downloads/some_long_number/tweets.csv"
tweets <- read.csv(filepath, header=TRUE)[ ,6]

tweetsDf<-as.data.frame(tweets)

# Remove emojis and other characters that will cause errors
tweetsDf$tweets <- sapply(tweetsDf$tweets,function(row) iconv(row, "latin1", "ASCII", sub=""))

# Convert tweets to a 'tm' package-compatible corpus
docs <- Corpus(VectorSource(tweetsDf))

# Take a look at the tweets in the corpus to make sure things are going ok
inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector, meaning that these words will be ignored. Customize as needed based on your wordcloud.
docs <- tm_map(docs, removeWords, c("t.co", "thebyrdlab", "https", "amp")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

# Make a term document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Make the wordcloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq,min.freq = 1,max.words=400,
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
          
# References
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
# https://stackoverflow.com/questions/18153504/removing-non-english-text-from-corpus-in-r-using-tm
