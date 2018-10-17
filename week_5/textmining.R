library(NLP)
library(tm)
library(stats)
library(proxy)
library(dplyr)
library(readtext)
library(slam)
library(Matrix)
library(tidytext)
library(stringr) 
library(wordcloud2)
library(ggplot2)
library(rvest)
library(SnowballC)

bioterm <- read_html("https://en.wikipedia.org/wiki/Glossary_of_biology#D") %>% html_nodes(".glossary .glossary .glossary a") %>% html_text()
splitBioterm <- unlist(strsplit(bioterm," "))

filesToProcess <- list.files("pubmed",pattern = "\\.txt$")
filesToProcess <- paste0("pubmed/",filesToProcess)
all.text <- do.call("rbind", lapply(filesToProcess, readtext))
all.text <- iconv(enc2utf8(all.text),sub="byte") 

# data clean
docs = Corpus(VectorSource(all.text))
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
})
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))

#docs[[1]][1]

tokenizer = function(d){
  unlist(strsplit(d[[1]], split = " "))
}
seg = lapply(docs, tokenizer)

d.corpus <- Corpus(VectorSource(seg))
tdm <- TermDocumentMatrix(d.corpus)
print( tf <- as.matrix(tdm) )
DF <- tidy(tf)

# tf-idf computation
N = tdm$ncol
tf <- apply(tdm, 2, sum)
idfCal <- function(word_doc)
{ 
  log2( N / nnzero(word_doc) ) 
}
idf <- apply(tdm, 1, idfCal)

doc.tfidf <- as.matrix(tdm)
for(x in 1:nrow(tdm))
{
  for(y in 1:ncol(tdm))
  {
    doc.tfidf[x,y] <- (doc.tfidf[x,y] / tf[y]) * idf[x]
  }
}

findZeroId = as.matrix(apply(doc.tfidf, 1, sum))
tfidfnn = doc.tfidf[-which(findZeroId == 0),]

write.csv(tfidfnn, "show08-17.csv")

freqFrame.all<-list() 
excludeWords <- c("nature","doi","pmid","university","department","pmc","pmcid","epub","institute"
                  ,"index", "author", "science", "can", "new", "research", "school")
for(i in 1:10){
  f = as.data.frame(table(seg[[i]]))
  freqFrameClean<- f[-which(f$Var1 %in% stopwords("en")),]
  freqFrameClean<- freqFrameClean[-which(freqFrameClean$Var1 %in% excludeWords),]
  freqFrameClean<- freqFrameClean[-which(nchar(as.character(freqFrameClean$Var1))<3),]
  freqFrame.all[[i]] <- freqFrameClean[order(freqFrameClean$Freq, decreasing = T),]
}

freqFrame.all[[10]][which(freqFrame.all[[10]]$Var1 %in% splitBioterm),]

barplot(freqFrame.all[[9]]$Freq[1:30], names.arg = freqFrame.all[[9]]$Var1[1:30],
        col ="lightblue", main ="frequent words", las = 2,
        ylab = "Word frequencies" )

ggplot(freqFrame.all[[1]] ,aes( x = Var1, y = freq , fill = book)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ book, scales = "free_y") +
  labs(x = "NULL", y = "Frequency") +
  coord_flip() +
  theme(legend.position="none")




head(doc.tfidf[order(doc.tfidf[,2], decreasing = T),],100)
