library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
rm(list=ls(all.names = TRUE))
obama<-read.csv("obama_title", stringsAsFactors = F)
docs <- Corpus(VectorSource(obama))
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)

docs <- tm_map(docs, toSpace, "歐巴馬")
docs <- tm_map(docs, toSpace, "‧")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
mixseg = worker()
new_user_word(mixseg,"哈利王子")
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
freqFrame_ordered<- freqFrame[order(freqFrame$Freq, decreasing = T),]
freqFrame_f<-freqFrame_ordered[-which(nchar(as.character(freqFrame_ordered$Var1))==1),]

obama_word <- barplot(freqFrame_f$Freq[1:30], names.arg = freqFrame_f$Var1[1:30],
                      col ="lightblue", main ="Obama-Most frequent words", las = 2,
                      ylab = "Word frequencies")

obama_cloud <- wordcloud2(freqFrame_f[1:30,], size = 0.8)
obama_cloud_modified <- wordcloud2(freqFrame_f[2:30,], size = 0.8)

barplot(freqFrame_f$Freq[1:30], names.arg = freqFrame_f$Var1[1:30],
        col ="lightblue", main ="Obama-Most frequent words", las = 2,
        ylab = "Word frequencies" )

obama_cloud_modified
