library(rvest)
tfidf <- read.csv("C:/Github/datacourse/week_5/show1970-2017.csv", sep = ",",header = T, stringsAsFactors = F)
head(tfidf)
tfidf_mt <- data.matrix(tfidf[3:ncol(tfidf)], rownames.force = NA) 
row.names(tfidf_mt) <- tfidf$X 

bioterm <- read_html("https://en.wikipedia.org/wiki/Glossary_of_biology#D") %>% html_nodes(".glossary .glossary .glossary a") %>% html_text()
splitBioterm <- unlist(strsplit(bioterm," "))
tfidf_bio <- tfidf_mt[which(row.names(tfidf_mt) %in% splitBioterm ),]


 
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


colnames(pca$rotation)<-c("1980","1990","2000","2010","2017")
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

library(plotly)
chart_link = api_create(g, filename="C:/Github/datacourse/week_6/ggplot-user-guide/1")
chart_link

