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
library(cluster)
library(factoextra)
rawtext <- readLines("dbdrugs.txt")
rawtext <- rawtext[1:10000]
x <- grep("^AB\\s", rawtext)
ab <- list()

i <- 1
while(i %in% c(1:length(x))){
  j<-1
  while(j %in% c(1:200)){
    n <- x[i]+j
    if(grepl("^\\s", rawtext[n])==T){
      j <- j+1
    }
    else{
      ab[[i]] <- rawtext[x[i]:(n-1)]
      break
    }
  } 
  i <- i+1
}

tl <- c()
i <- 1
while(i %in% c(1:length(x))){
  j<-1
  while(j %in% c(1:20)){
    n <- x[i]-j
    if(grepl("^TI", rawtext[n])==T){
      tl[i] <- rawtext[n]
      break
    }
    else{
      j <- j+1
    }
  } 
  i <- i+1
}

names(ab) <- tl

dt <- c()
i <- 1
while(i %in% c(1:length(x))){
  j<-1
  while(j %in% c(1:20)){
    n <- x[i]-j
    if(grepl("^LR", rawtext[n])==T){
      dt[i] <- rawtext[n]
      break
    }
    else{
      j <- j+1
    }
  } 
  i <- i+1
}
#write.table(as.data.frame(ab),file="abstract.csv", quote=F,sep=",",row.names=F)

type1db<- grep("type 1", ab)
type2db<- grep("type 2", ab)
gestdb<- grep("gestational", ab)
alldb <- sort(unique(c(type1db,type2db,gestdb)))
cleanabtt <- ab[alldb]
type1dbtt <- ab[type1db]
type2dbtt <- ab[type2db]
gestdbtt <- ab[gestdb]
alltt <- list(type1dbtt, type2dbtt, gestdbtt)

toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
})
tokenizer = function(d){
  unlist(strsplit(d[[1]], split = " "))
}


  docs = Corpus(VectorSource(cleanabtt))
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, toSpace, "cab")
  seg = lapply(docs, tokenizer)
  
tabletype2 <- table(unlist(seg))[-1]
sort(tabletype1, decreasing = T)

idfCal <- function(word_doc)
{ 
  log2( N / nnzero(word_doc) ) 
}



  d.corpus <- Corpus(VectorSource(seg))
  tdm <- TermDocumentMatrix(d.corpus)
  
  tf <- as.matrix(tdm)
  DF <- tidy(tf)
  
  N = tdm$ncol
  tf <- apply(tdm, 2, sum)
  
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

sort(table(unlist(seg)),decreasing = T)


pca<-prcomp(tfidfnn, na.action=na.omit, scale=TRUE)
plot(pca)
fviz_eig(pca)

top3.pca.eigenvector <- pca$rotation[, 1:3]
top3.pca.eigenvector
first.pca <- top3.pca.eigenvector[, 1]   #  第一主成份
second.pca <- top3.pca.eigenvector[, 2]  #  第二主成份
third.pca <- top3.pca.eigenvector[, 3] 

dotchart(first.pca ,   # 排序後的係數
         main="Loading Plot for PC1",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="red")  

biplot(pca, choices=1:2)  


colnames(pca$rotation)<-tl[1:100]
library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(pca, obs.scale = 1, var.scale = 1, 
              groups = colnames(pca$rotation), ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

tfidf_db <- doc.tfidf[which(row.names(doc.tfidf) %in% dbterm ),]
#clustering
truth.k <- 5
dist.matrix <- proxy::dist(doc.tfidf[], method = "cosine") 
clustering.kmeans <- kmeans(doc.tfidf, truth.k) 
clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2") 
#clustering.dbscan <- dbscan::hdbscan(dist.matrix, minPts = 10)
master.cluster <- clustering.kmeans$cluster 
slave.hierarchical <- cutree(clustering.hierarchical, k = truth.k) 
#slave.dbscan <- clustering.dbscan$cluster 
#stacked.clustering <- rep(NA, length(master.clu  ster))  
#names(stacked.clustering) <- 1:length(master.cluster) 

#for (cluster in unique(master.cluster)) { 
#  indexes = which(master.cluster == cluster, arr.ind = TRUE) 
#  slave1.votes <- table(slave.hierarchical[indexes]) 
#  slave1.maxcount <- names(slave1.votes)[which.max(slave1.votes)]   
#  slave1.indexes = which(slave.hierarchical == slave1.maxcount, arr.ind = TRUE) 
#  slave2.votes <- table(slave.dbscan[indexes]) 
#  slave2.maxcount <- names(slave2.votes)[which.max(slave2.votes)]   
#  stacked.clustering[indexes] <- slave2.maxcount 
#} 
points <- cmdscale(dist.matrix, k = truth.k) 
palette <- colorspace::diverge_hcl(truth.k) # Creating a color palette 
#previous.par <- par(mfrow=c(2,2), mar = rep(1.5, 4)) 

plot(points, main = 'K-Means clustering', col = as.factor(master.cluster), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
ggplot()+ geom_point(data = as.data.frame(points))

plot(points, main = 'Hierarchical clustering', col = as.factor(slave.hierarchical), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),  
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 



tfidf_mt <- data.matrix(tfidf[3:ncol(tfidf)], rownames.force = NA) 
row.names(tfidf_mt) <- tfidf$X 
pca<-prcomp(tfidf_bio, na.action=na.omit, scale=TRUE)
plot(pca)

top3.pca.eigenvector <- pca$rotation[, 1:3]
top3.pca.eigenvector
first.pca <- top3.pca.eigenvector[, 1]   #  第一主成份
second.pca <- top3.pca.eigenvector[, 2]  #  第二主成份
third.pca <- top3.pca.eigenvector[, 3] 

dotchart(first.pca ,   # 排序後的係數
         main="Loading Plot for PC1",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="red")  

biplot(pca, choices=1:2)  

dbterm <- read_html("https://www.webmd.com/diabetes/guide/diabetes-glossary-terms#1") %>%
  html_nodes("b") %>% html_text() %>%
  gsub("(\\w)","\\L\\1",.,perl=TRUE) %>% gsub("^[[:space:]]+|\\(.*\\)|:$","",.) 