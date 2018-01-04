library(rvest)
library(xml2)
#Get Assassin's Creed Origins comments on Steam
#origin_review<-html("http://steamcommunity.com/app/582160/reviews")



install.packages("RSelenium")
library(RSelenium)
require(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4445L
                      , browserName = "firefox"
)
remDr$open()
remDr$getStatus()
remDr$navigate("http://steamcommunity.com/app/582160/reviews?filterLanguage=english")
#remDr$setAsyncScriptTimeout(10000)

remDr$getCurrentUrl()
webElem <- remDr$findElement('xpath', "//*[@id='GetMoreContentBtn']/a")
#Try click get-more-content link to get more comments.It may show error when you do this, just try to run this code repeatly
for(i in 1:50){
  Sys.sleep(1)
  webElem$clickElement()
  
}
#Get comments
webElem2<-remDr$findElements("css","div.apphub_CardTextContent")
#Get recommended or not by the comment
pos_neg<-remDr$findElements("css","div.title")
length(webElem2)
tests<-unlist(lapply(webElem2,function(x){x$getElementText()}))
tests2<- gsub("[^A-Za-z0-9\\-\\,\\.]|\\.+", " ", tests)
test.clean<- gsub("\n|[ \t]+", " ", tests2)  

tests2
test.clean

judgement<-unlist(lapply(pos_neg,function(x){x$getElementText()}))
tests


all_info<-matrix(c(judgement,test.clean),nrow=1178,ncol=2)
View(all_info)
all_info_frame<-as.data.frame(all_info)
colnames(all_info_frame)<-c("Recommend","Comment")
View(all_info_frame)
write.csv(all_info_frame,"~/origin.csv")
remDr$screenshot(display = TRUE)

#Do text analysis
#install.packages("openNLP")
#install.packages("quanteda")
library(openNLP)
library(quanteda)
library(NLP)
#import downloaded dic
dic <- read_csv("~/Advanced Datascience/homework/Homework8/dic.csv",  col_types = cols(Word = col_character()))
View(dic)

dic$Word


sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()

a1<-annotate(all_info_frame$Comment[1178],
             list(sent_token_annotator,word_token_annotator,pos_tag_annotator))
install.packages("tidytext")
library(stringr)
library(tidytext)
library(plyr)
#import positive and negative word sets
pos <- read_csv("~/Advanced Datascience/homework/Homework8/pos.csv")
neg <- read_csv("~/Advanced Datascience/homework/Homework8/neg.csv")

sentiment.score<-function(comments,pos.words,neg.words){
  scores=laply(comments,function(comment,pos.words,neg.words){
    comment = gsub('[[:punct:]]', '', comment)
    comment = gsub('[[:cntrl:]]', '', comment)
    comment = gsub('\\d+', '', comment)
    comment=tolower(comment)
    
    word.list<-str_split(comment,'\\s+')
    words<-unlist(word.list)
    pos.matches=match(words,pos.words)
    neg.matches=match(words,neg.words)
    
    pos.num=!is.na(pos.matches)
    neg.num=!is.na(neg.matches)
    
    score=sum(pos.num)-sum(neg.num)
    return(score)
  },pos.words,neg.words)
  scores.df=data.frame(score=scores,text=comments)
  return(scores.df)
}

result<-sentiment.score(all_info_frame$Comment,pos$PosWord,neg$NegWord)
result$score

#Split dataset to train set and test set
indexes <- sample(1:nrow(all_info_frame), size=0.8*nrow(all_info_frame))
trainCom<-all_info_frame[indexes,]
testCom<-all_info_frame[-indexes,]

library(forecast)