#Libraries required for the wordcloud and sentiment analysis
library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)

#For ownloading tweets you need to create an App and authenticate it. Pls refer online.
#creating an .csv file from tweets
kochadaiiyaan.tweets = searchTwitter("#kochadaiiyaan", n = 1500)
rajini.tweets = searchTwitter("@superstarrajini", n = 1500)
soundarya.tweets = searchTwitter("@soundaryaarajni", n = 1500)

kochadaiiyaan= kochadaiiyaan.tweets
rajini=rajini.tweets
soundarya=soundarya.tweets

df1 <- do.call("rbind", lapply(kochadaiiyaan, as.data.frame))
df2 <- do.call("rbind", lapply(rajini, as.data.frame))
df3 <- do.call("rbind", lapply(soundarya, as.data.frame))

#Writing the tweets to csv
write.csv(df1, file = "H:/Hobby/kochadaiiyaan-sentiment/kochadaiiyaan.csv")
write.csv(df2, file = "H:/Hobby/kochadaiiyaan-sentiment/rajini.csv")
write.csv(df3, file = "H:/Hobby/kochadaiiyaan-sentiment/soundarya.csv")


#positive words, negative words and stop words are seperately stored
positive.words=scan('H:/Hobby/kochadaiiyaan-sentiment/positive-words-tot.csv',what='character',comment.char=',')
negative.words=scan('H:/Hobby/kochadaiiyaan-sentiment/negative-words-tot.csv',what='character',comment.char=';')
stop.words=scan('H:/Hobby/kochadaiiyaan-sentiment/stopwords.csv',what='character',comment.char=';')


poswords=c(positive.words)
negwords=c(negative.words)
stopwords=c(stop.words)


#Reading the csv data
koch <- read.csv("H:/Hobby/kochadaiiyaan-sentiment/kochadaiiyaan.csv", header=T)
koch<-as.data.frame(koch)
rajini <- read.csv("H:/Hobby/kochadaiiyaan-sentiment/rajini.csv", header=T)
rajini<-as.data.frame(rajini)
soundarya <- read.csv("H:/Hobby/kochadaiiyaan-sentiment/soundarya.csv", header=T)
soundarya<-as.data.frame(soundarya)
#combining the tweet data
merge1<-rbind(koch,rajini)
kochadaiiyaan<- rbind(merge1,soundarya)

#Split the tweet into words
words <- strsplit(as.character(kochadaiiyaan$text), " ")



#Formating the words
words <- lapply(words, function(x) x[grep("^[A-Za-z0-9]+$", x)])
words <- unlist(words)
words <- tolower(words)
words <- words[-grep("^[rm]t$", words)]

#Remove stop words
"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0
words <- words[words %!in% stopwords]


#Top 10 words frequency
top10words <- as.data.frame(table(words))
top10words <- top10words[sort.list(top10words$Freq, decreasing = T),]
print(head(top10words,10))


#Top 10 positive words-High with scoring
positivewords <- words[words %in% poswords]
positivewords.t <- as.data.frame(table(positivewords))
positivewords.t <- positivewords.t[sort.list(positivewords.t$Freq, decreasing = T),]
a<-head(positivewords.t ,10)
totpositive<-as.data.frame(a)
totpositive <- totpositive[sort.list(totpositive$Freq, decreasing = T),]
print(totpositive)




#Top 10 negative words-High with scoring
negativewords <- words[words %in% negwords]
negativewords.t <- as.data.frame(table(negativewords))
negativewords.t <- negativewords.t[sort.list(negativewords.t$Freq, decreasing = T),]
b<-head(negativewords.t ,10)
totnegative<-as.data.frame(b)
totnegative <- totnegative[sort.list(totnegative$Freq, decreasing = T),]
print(totnegative)

#Print the top 10 words used frequently
print(head(top10words,10))
wordcloud(top10words$words, top10words$Freq, random.order = FALSE, min.freq=2, colors = brewer.pal(2, "Dark2"))


head(top10words)

#Total number of retweets
#Removing duplicate retweet count
samp <- kochadaiiyaan[!duplicated(kochadaiiyaan[,1]),]
sum(unique(samp$retweetCount))

