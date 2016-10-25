# Reading Data:

library('stringr')
library('readr')
library('wordcloud')
library('tm')
library('SnowballC')
# library('RWeka')
library('RSentiment')
library('data.table')
library('DT')
library('ggplot2')
debate <- read.csv("../input/debate.csv",stringsAsFactors = F)

# Tidy data
debate$Speaker <- as.factor(debate$Speaker)
debate <- as.data.table(debate)

# Number of interventions
interventions <- debate[,.N,by=Speaker][order(-N)]
ggplot(interventions,aes(x = Speaker,y = N, fill=Speaker))+
geom_bar(stat = "identity") +
  ggtitle("Number of Interventions")
interventions


# Trump

## Preparing Data

trump <- debate$Text[debate$Speaker=="Trump"]
corpus = Corpus(VectorSource(list(trump)))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers) 
corpus = tm_map(corpus, stripWhitespace)
# corpus = tm_map(corpus, removeWords, stopwords('englist'))

dtm_trump = DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))
freq_trump <- colSums(as.matrix(dtm_trump))


## Sentiments trump
sentiments_trump = calculate_sentiment(names(freq_trump))
sentiments_trump = cbind(sentiments_trump, as.data.frame(freq_trump))

sent_pos_trump = sentiments_trump[sentiments_trump$sentiment == 'Positive',]
sent_neg_trump = sentiments_trump[sentiments_trump$sentiment == 'Negative',]


cat("We have less negative Sentiments: ",sum(sent_neg_trump$freq_trump)," than positive: ",sum(sent_pos_trump$freq_trump))

### Trump Positive and Negative
wordcloud(sent_pos_trump$text,sent_pos_trump$freq, min.freq=10,colors=brewer.pal(6,"Dark2"))

### Trump negative
wordcloud(sent_neg_trump$text,sent_neg_trump$freq, min.freq=20,colors=brewer.pal(6,"Dark2"))

# Clinton 

## Preparing Data
clinton <- debate$Text[debate$Speaker=="Clinton"]
corpus = Corpus(VectorSource(list(clinton)))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers) 
corpus = tm_map(corpus, stripWhitespace)
# corpus = tm_map(corpus, removeWords, stopwords('englist'))

dtm_clinton = DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))
freq_clinton <- colSums(as.matrix(dtm_clinton))


## Sentiments clinton
sentiments_clinton = calculate_sentiment(names(freq_clinton))
sentiments_clinton = cbind(sentiments_clinton, as.data.frame(freq_clinton))

sent_pos_clinton = sentiments_clinton[sentiments_clinton$sentiment == 'Positive',]
sent_neg_clinton = sentiments_clinton[sentiments_clinton$sentiment == 'Negative',]

cat("We have less negative Sentiments: ",sum(sent_neg_clinton$freq_clinton)," than positive: ",sum(sent_pos_clinton$freq_clinton))


### clinton Positive
wordcloud(sent_pos_clinton$text,sent_pos_clinton$freq, min.freq=10,colors=brewer.pal(6,"Dark2"))

### clinton Negative
wordcloud(sent_neg_clinton$text,sent_neg_clinton$freq, min.freq=20,colors=brewer.pal(6,"Dark2"))


# Holt

## Preparing Data
holt <- debate$Text[debate$Speaker=="Holt"]
corpus = Corpus(VectorSource(list(holt)))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers) 
corpus = tm_map(corpus, stripWhitespace)
# corpus = tm_map(corpus, removeWords, stopwords('englist'))

dtm_holt = DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))
freq_holt <- colSums(as.matrix(dtm_holt))


## Sentiments holt
sentiments_holt = calculate_sentiment(names(freq_holt))
sentiments_holt = cbind(sentiments_holt, as.data.frame(freq_holt))

sent_pos_holt = sentiments_holt[sentiments_holt$sentiment == 'Positive',]
sent_neg_holt = sentiments_holt[sentiments_holt$sentiment == 'Negative',]


cat("We have less negative Sentiments: ",sum(sent_neg_holt$freq_holt)," than positive: ",sum(sent_pos_holt$freq_holt))

### holt Positive
wordcloud(sent_pos_holt$text,sent_pos_holt$freq, min.freq=10,colors=brewer.pal(6,"Dark2"))

### holt Negative
wordcloud(sent_neg_holt$text,sent_neg_holt$freq, min.freq=20,colors=brewer.pal(6,"Dark2"))


# Audience Reactions

audience <- factor(debate$Text[debate$Speaker=="Audience"])
qplot(audience,fill=audience)+
  ggtitle("Audience Reactions")+
  ylab("Interventions")+xlab("Reaction")
summary(audience)

# Sentiments by Interventions


interv <- debate[Speaker%in%c("Trump","Clinton","Holt"),.N,by=Speaker]

sentiments_trump <- as.data.table(sentiments_trump)
st <- sentiments_trump[,sum(freq_trump),by=sentiment]
sentiments_clinton <- as.data.table(sentiments_clinton)
sc <- sentiments_clinton[,sum(freq_clinton),by=sentiment]
sentiments_holt <- as.data.table(sentiments_holt)
sh <- sentiments_holt[,sum(freq_holt),by=sentiment]


interv1 <- cbind(sh$V1,sc$V1,st$V1)
interv <- cbind(interv,t(interv1))
colnames(interv) <- c("Speaker","Interventions","Neutral","Negative","Positive")
interv

## Neutral Sentiments per Intervention
interv[,.("Neutral"=Neutral/Interventions),by=Speaker][order(-Neutral)]

## Positive Sentiments per Intervention
interv[,.("Positive"=Positive/Interventions),by=Speaker][order(-Positive)]

## Negative Sentiments per Intervention
interv[,.("Negative"=Negative/Interventions),by=Speaker][order(-Negative)]

# % Sentiments per word
numholt <- sum(sentiments_holt$freq_holt)
numtrump <- sum(sentiments_trump$freq_trump)
clinton <- sum(sentiments_clinton$freq_clinton)

interv[,Interventions:=c(numholt,clinton,numtrump)]
colnames(interv)[2] <- c("Words")

## % Neutral Sentiments per Word
interv[,.("Neutral"=100*Neutral/Words),by=Speaker][order(-Neutral)]

## % Positive Sentiments per Word
interv[,.("Positive"=100*Positive/Words),by=Speaker][order(-Positive)]

## % Negative Sentiments per Word
interv[,.("Negative"=100*Negative/Words),by=Speaker][order(-Negative)]
