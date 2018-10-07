library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)
library(rvest)

trump_title<-read_html("https://udn.com/search/tagging/2/%E5%B7%9D%E6%99%AE")%>%
  html_nodes("#search_content h2") %>% html_text()
trump_title<-c()
for(i in 1:50){
  t<-read_html(paste0("https://udn.com/search/tagging/2/%E5%B7%9D%E6%99%AE/",i))%>%
    html_nodes("#search_content h2") %>% html_text() 
  trump_title<-c(trump_title,t)
  i<-i+1
}

#trump_link<-read_html("https://udn.com/search/tagging/2/%E5%B7%9D%E6%99%AE")%>%
#  html_nodes("dt>a") %>% html_attr("href")
#trump_link<-grep("news/story",trump_link,value=T)

#df_trump<-data.frame(trump_title,trump_link)
#for(i in 1:length(trump_link)){
#  res<-read_html(trump_link[i]) %>% html_nodes("p , #story_art_title")%>% html_text()
#  df_trump$trump_text[i]<-res
#  i<-i+1
#}
#read_html("https://udn.com/news/story/6813/3401160") %>% html_nodes("p , #story_art_title")%>% html_text()

docs <- Corpus(VectorSource(trump_title))
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)

docs <- tm_map(docs, toSpace, "川普")
docs <- tm_map(docs, toSpace, "‧")
docs <- tm_map(docs, toSpace, "的")
docs <- tm_map(docs, toSpace, "是")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
mixseg = worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))

wordcloud(freqFrame$Var1,freqFrame$Freq,
          scale=c(5,0.1),min.freq=50,max.words=150,
          random.order=TRUE, random.color=FALSE, 
          rot.per=.1, colors=brewer.pal(8, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)
file.rename(from = "wordcloud.Rmd", to = "Rmd/wordcloud.Rmd")
