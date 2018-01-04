setwd("/Users/eavy/Downloads/7390/Assignment/Assignment8")
install.packages("rvest")
install.packages("NLP")
install.packages("tm")
install.packages("SnowballC")
install.packages("stringr")
install.packages("RTextTools")
install.packages("e1071")
install.packages("dplyr")
install.packages("caret")
library(rvest)
library(NLP)
library(tm)
library(SnowballC)
library(stringr)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)
# Library for parallel processing
library(doMC)

#count for number of pages.
pages <- 500 

all_reviews <- NULL

for(page_num in 1:pages){
  
  #reading one page at a time
  single_page <- paste0("https://www.amazon.com/All-New-Fire-TV-Stick-With-Alexa-Voice-Remote-Streaming-Media-Player/product-reviews/B00ZV9RDKK/ref=cm_cr_getr_d_show_all?ie=UTF8&reviewerType=all_reviews&showViewpoints=1&sortBy=helpful&pageNumber=", page_num)
  
  #Converting to HTML
  single_doc <- read_html(single_page)
  
  #Parsing review text
  review <-html_nodes(x= single_doc, css = ".review-text") %>%
    html_text()
  
  #Matrix conversion of data for columner form 
  review <- as.matrix(review)
  
  #consolidating all reviews in one column
  all_reviews  <- rbind(all_reviews,review)
}

write.csv(all_reviews, file = "AmazonReviews.csv" )

all_reviews <- str_split(all_reviews, pattern="\\s+")
str(all_reviews)

myCorpus <- Corpus(VectorSource(all_reviews))
inspect(myCorpus)

# trans to lowcase
myCorpus <- tm_map(myCorpus, tolower)
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)

# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)

# remove stopwords
myStopwords <- c(stopwords('english'), "available", "via")
idx <- which(myStopwords == "r")
myStopwords <- myStopwords[-idx]
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

dictCorpus <- myCorpus

# stem words in a text document with the snowball stemmers,
# which requires packages Snowball, RWeka, rJava, RWekajars
myCorpus <- tm_map(myCorpus, stemDocument)
# inspect the first three ``documents"
inspect(myCorpus[1:10])

# stem completion
myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=dictCorpus)

token <- Token_Tokenizer(scan_tokenizer)
tokenized_reviews <- token(all_reviews)
write.csv(tokenized_reviews, file = "AmazonReviews1.csv" )
str(tokenized_reviews)
scan_tokenizer <- function(x){
  scan(text = as.character(x), what = "character", quote = "", quiet = TRUE)
}

pos <- scan('positive-words.txt', what ='character', comment.char = ";")
neg <- scan('negative-words.txt', what ='character', comment.char = ";")

# sentiment analysis

score<-function(comments,pos.words,neg.words){
  scores=laply(comments,function(comment,pos.words,neg.words){
    score=sum(!is.na(match(tokenized_reviews, pos)))-sum(!is.na(match(tokenized_reviews,neg)))
    return(score)
  },pos.words,neg.words)
  scores.df=data.frame(score=scores,text=comments)
  return(scores.df)
}

myData <- read.csv("/Users/eavy/Downloads/7390/Assignment/Assignment8/AmazonReviews.csv")
set.seed(50)
index <- sample(1:nrow(myData),round(0.8*nrow(myData)))
train_data <- myData[index,]
test_data <- myData[-index,]

corpus <- Corpus(VectorSource(train_data))
dtm <- DocumentTermMatrix(corpus.clean)
train_mat <- create_matrix(train[,4], language="english", removeStopwords=FALSE, removeNumbers=TRUE, 
                           stemWords=FALSE, tm::weightTfIdf)
train_mat <- as.matrix(train_mat)
test_mat <- create_matrix(test[,4], language="english",removeStopwords=FALSE, removeNumbers=TRUE, 
                          stemWords=FALSE, tm::weightTfIdf)
test_mat <- as.matrix(test_mat)
system.time( classifier <- naiveBayes(trainNB, train_data$class, laplace = 1) )
predicted <- predict(classifier, test_mat)
system.time( pred <- predict(classifier, newdata=testNB) )
table("Predictions"= pred,  "Actual" = df.test$class )

