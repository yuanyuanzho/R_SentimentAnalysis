install.packages("httpuv")
install.packages("tidytext")
install.packages("hunspell")
install.packages("tidyr")
install.packages("dplyr")
install.packages("RTextTools")
install.packages("e1071")
library("httpuv")
library("tidytext")
library("hunspell")
library("tidyr")
library("dplyr")
library("RTextTools")
library("e1071")

#setup_twitter_oauth("zPjyUO6nqhDZD7OCGhgVj8YG8", "B7T5aAMlLo9G3RN9KT0WVzEDw4zDwCcR0177WfOtAV75Z4uzh8")
#review_JB <- searchTwitter('Justin Bieber', n=8000, retryOnRateLimit=1, lang = "en")
#review_text <- sapply(review_JB,function(x) x$getText())
#clean_text = function(x)
#{
#  x = gsub("rt", "", x) # remove Retweet
#  x = gsub("@\\w+", "", x) # remove at(@)
#  x = gsub("[[:punct:]]", "", x) # remove punctuation
#  x = gsub("[[:digit:]]", "", x) # remove numbers/Digits
#  x = gsub("http\\w+", "", x)  # remove links http
#  x = gsub("[ |\t]{2,}", "", x) # remove tabs
#  x = gsub("^ ", "", x)  # remove blank spaces at the beginning
#  x = gsub(" $", "", x) # remove blank spaces at the end
#  x = gsub("\n", "", x) # remove line break
#  return(x)
#}
for(j in 1:8000){
  words <- hunspell_parse(words)
  stems <- unlist(hunspell_stem(unlist(words)))
  words <- as.data.frame(sort(table(stems), decreasing = TRUE))
  words$stems <- as.character(words$stems)
  join <- inner_join(as.data.frame(words), get_sentiments("bing"), by=c("stems" = "word"))
  positive <- sum(join[join$sentiment == "positive",]$Freq)
  negative <- sum(join[join$sentiment == "negative",]$Freq)
  sentiment <- positive - negative
  output[j,] <- c(j, sentiment, review_txt[j])
}

output <- read.csv('/Users/lucyy/Documents/AdvanceDataSci/Sentiment Analysis/output.csv')
output <- iconv(output, "UTF-8", "ASCII") 

set.seed(99)
index <- sample(1:nrow(output),round(0.8*nrow(output)))
train <- output[index,]
test <- output[-index,]
str(train)

train_mat <- create_matrix(train[,4], language="english", 
                           removeStopwords=FALSE, removeNumbers=TRUE, 
                           stemWords=FALSE, tm::weightTfIdf)
train_mat <- as.matrix(train_mat)
test_mat <- create_matrix(test[,4], language="english", 
                          removeStopwords=FALSE, removeNumbers=TRUE, 
                          stemWords=FALSE, tm::weightTfIdf)
test_mat <- as.matrix(test_mat)
classifier <- naiveBayes(train_mat, as.factor(train[,3]))
predicted <- predict(classifier, test_mat)
str(test_mat)
table(test[, 3], predicted)


#########################Example###########################
pos_tweets =  rbind(
  c('I love this car', 'positive'),
  c('This view is amazing', 'positive'),
  c('I feel great this morning', 'positive'),
  c('I am so excited about the concert', 'positive'),
  c('He is my best friend', 'positive')
)

neg_tweets = rbind(
  c('I do not like this car', 'negative'),
  c('This view is horrible', 'negative'),
  c('I feel tired this morning', 'negative'),
  c('I am not looking forward to the concert', 'negative'),
  c('He is my enemy', 'negative')
)

test_tweets = rbind(
  c('feel happy this morning', 'positive'),
  c('larry friend', 'positive'),
  c('not like that man', 'negative'),
  c('house not great', 'negative'),
  c('your song annoying', 'negative')
)

tweets = rbind(pos_tweets, neg_tweets, test_tweets)

matrix= create_matrix(tweets[,1], language="english", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE) 
mat = as.matrix(matrix)
classifier = naiveBayes(mat[1:10,], as.factor(tweets[1:10,2]) )

predicted = predict(classifier, mat[11:15,]); predicted
table(tweets[11:15, 2], predicted)
recall_accuracy(tweets[11:15, 2], predicted)




