rm(list = ls(pattern = "^pat"))
setwd("C:/GitHub/datacourse/安寧照護data/黃獻樑醫師的資料 (病程)")

library(textreadr)
library(rvest)
library(dplyr)
library(NLP)
library(tm)
library(stats)
library(proxy)
library(readtext)
library(slam)
library(Matrix)
library(tidytext)
library(stringr) 
require(cluster)
require(factoextra)
require(wordcloud2)
library(plotly)
require(textclean)
library(igraph)


#讀取同一病人資料
x <-1
readraw <- function(x){
  tryCatch({t <- paste0(x)
           filesToProcess <- list.files(t)
           n <- length(filesToProcess)
           ls <- list()
           i <- 1
           if(grepl("doc", filesToProcess[1])==T){
             for(i in 1:n){
               ls[[i]] <- read_doc(file = paste0(x,"/",filesToProcess[i]))
             }
           }
           if(grepl("txt", filesToProcess[1])==T){
             for(i in 1:n){
               ls[[i]] <- readLines(paste0(x,"/",filesToProcess[i])) %>% replace_non_ascii() %>% paste(collapse = "")
             }
           }
           
           ls <- unlist(ls)}, error = function(e){}) 
  return(ls)
}

x <- 76


findday <- function(x){
  tryCatch({filename <- list.files(as.character(x), pattern = paste0("^",x))
  n <- length(filename)
  dates <- c()
  for(i in 1:n){
    dates[i] <- as.Date(strsplit(str_squish(filename[i]), " ")[[1]][2], "%Y%m%d")
  }
  class(dates) <- "Date"
  day <- difftime(max(dates), min(dates)) %>% as.numeric()
  return(day)}, error = function(e){return("X")}) 
}

days <- sapply(1:124, findday) %>% unlist()
days[which(days == "X")] <- NA   #NA,五個
class(days) <- "numeric"
write.csv(days, "days.csv")


median(days, na.rm = T) #22,四個
days[which(days==22)]

dfdays <- data.frame("patient"= c(1:124), "days" = days)
dfdaysdec <- dfdays[order(dfdays$days, decreasing = T),]
c1 <- paste0("X", dfdaysdec$patient[1:61])
c2 <- paste0("X", dfdaysdec$patient[62:119])

rawpatlist1 <- lapply(1:40, readraw) 
rawpatlist2 <- lapply(41:80, readraw) 
rawpatlist3 <- lapply(81:124, readraw) 
names(rawpatlist1) <- c(1:40)
names(rawpatlist2) <- c(41:80)
names(rawpatlist3) <- c(81:124)
rawpatlist <- lapply(1:124, readraw)
readraw(1)
names(rawpatlist) <- c(1:124)

medterm <- readLines("https://raw.githubusercontent.com/glutanimate/wordlist-medicalterms-en/master/wordlist.txt")
medterm <- tolower(medterm)
#關鍵字縮寫列表

Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
Sys.setlocale("LC_ALL", "English")
medabbr <- list()
for(i in 1:26){
  URL <- paste0("https://en.wikipedia.org/wiki/List_of_medical_abbreviations:_", LETTERS[i])
  tab <- URL %>% read_html() %>%
    html_nodes(xpath='//*[@id="mw-content-text"]/div/table') %>% html_table(fill = T)
  tab <- tab[[1]] 
  if(ncol(tab)==3){
    tab <- tab[,-3]
  }
  medabbr[[i]] <- tab
}

dfMed <- bind_rows(medabbr)
View(dfMed)

write.csv(dfMed, file = "Medabbr.csv", row.names = F, na = "" )
abbr <- dfMed$Abbreviation[-which(nchar(dfMed$Abbreviation)<=1)]


#藥品名


drugls <- list()
for(i in 1:26){
  URL <- paste0("https://www.rxlist.com/drugs/alpha_", letters[i], ".htm")
  tab <- URL %>%  read_html()%>%
    html_nodes("#AZ_container li a") %>% html_text() %>% tolower()
  tab <- tab %>% gsub("\\(.*\\)|\\s+$","",.) %>% unique()
  drugls[[i]] <- tab
}

rawdrugs <- unlist(drugls) %>% gsub("\\s+$","",.) 
drugs <- lapply(rawdrugs, function(x){
  a <- strsplit(x, " ")%>% unlist()
  return(a[1])
} ) %>% unlist() %>% tolower() %>% unique() 

View(drugs)
dfdrugs2 <- DF[which(DF$.rownames %in% drugs),]
dfdrugs2 <- dfdrugs2[!(dfdrugs2$.rownames %in% stopwords("en")),]
write.csv(dfdrugs2, "dfdrugs2.csv")


#疾病名
dis <- list()
for(i in 1:26){
  URL <- paste0("https://en.wikipedia.org/wiki/List_of_diseases_(", LETTERS[i], ")")
  tab <- URL %>%  read_html()%>%
    html_nodes(".mw-parser-output > ul a")%>% html_text() 
  tab <- gsub("\\(.*\\)","",tab)
  tab <- gsub("Symptoms and Signs","",tab)
  dis[[i]] <- tab
}

dis <- unlist(dis) 
View(dis)
strsplit()
disword <- sapply(dis, function(x){strsplit(x," ")}) %>% unlist %>% unique() %>% tolower()

dfdis <- DF[which(DF$.rownames %in% disword),]
write.csv(dfdis, "dfdis.csv")

#症狀名
sym <- list()
for(i in 1:26){
  URL <- paste0("https://www.medicinenet.com/symptoms_and_signs/alpha_", letters[i], ".htm")
  tab <- URL %>%  read_html()%>%
    html_nodes("#AZ_container li a")%>% html_text() 
  tab <- gsub("\\(.*\\)","",tab)
  tab <- gsub("Symptoms and Signs","",tab)
  sym[[i]] <- tab
}

sym <- unlist(sym)
View(sym)

#treatment
trm <- list()
for(i in 1:26){
  URL <- paste0("https://www.medicinenet.com/procedures_and_tests/alpha_", letters[i], ".htm")
  tab <- URL %>%  read_html()%>%
    html_nodes("#AZ_container li a")%>% html_text() 
  tab <- gsub("\\(.*\\)","",tab)
  tab <- gsub("Symptoms and Signs","",tab)
  trm[[i]] <- tab
}

trm <- unlist(trm)
View(trm)



findmed <- function(x){
  m <- c()
  for(i in 1:length(abbr)){
    if(length(grep(abbr[i], x, ignore.case = F))>0){
      m <- c(m, abbr[i])
    }
  }
  for(i in 1:length(medtermvec)){
    if(length(grep(paste0("\\s", medtermvec[i]), x, ignore.case = T))>0){
      m <- c(m, medtermvec[i])
    }
  }
  return(m)
}


findmed2 <- function(x,y){
  m <- c()
  for(i in 1:length(y)){
    if(length(grep(paste0("\\s", y[i]), x, ignore.case = T))>0){
      m <- c(m, y[i])
    }
  }
  return(m)
}
words <- strsplit(unlist(rawpatlist1[[1]]), "\\s|[[:punct:]]") %>% lapply(function(x){x[!x ==""]}) %>% 
  unlist()%>% unique()
"thyroid" %in% words
keyword <- sapply(rawpatlist, findmed)
findmed(rawpatlist[[7]])  

words <- strsplit(rawpatlist[[4]], "\\s|[[:punct:]]") %>% lapply(function(x){x[!x ==""]}) %>% 
  unlist()%>% unique()

findall <- function(x){
 if(class(x)=="character") { words <- strsplit(x, "\\s|[[:punct:]]") %>% lapply(function(x){x[!x ==""]}) %>% 
    unlist()%>% unique()
  a <- c()
  b <- c()
  c <- c()

  for(i in 1:length(words)){
    if(words[i] %in% abbr)
      a <- c(a, words[i])
  }
  words <- words %>% tolower() %>% unique()
 
  for(i in 1:length(words)){
    if(words[i] %in% drugs)
      b <- c(b, words[i])
  }

  for(i in 1:length(words)){
    if(words[i] %in% medterm)
      c <- c(c, words[i])
  }
 
  ls <- list(a,b,c)
  names(ls) <- c("abbreviation", "drugs", "medical terms")
  return(ls)}
}

findall(unlist(rawpatlist1[[1]]))
words
"thyroid" %in% medterm

rm(dfdis)
dfdis <- data.frame(".rownames"=dis,"P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13","P14","P15","P16","P17","P18","P19","P20","P21","P22","P23","P24","P25","P26","P27","P28",
                    "P29","P30","P31","P32","P33","P34","P35","P36","P37")

colnames(dfdis)<- c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13","P14","P15","P16","P17","P18","P19","P20","P21","P22","P23","P24","P25","P26","P27","P28","P29","P30","P31","P32","P33","P34","P35","P36","P37")
                   



rawpatlist[[2]] %>% unlist() %>% findall()

keyword2 <- lapply(rawpatlist, function(x){
  x <- x %>% unlist() %>% findall()
  return(x)
  })


patabbr <- c()
for(i in 1:124){
  patabbr <- c(patabbr, keyword2[[i]]$abbreviation) 
  patabbr <- patabbr %>% unique()
}

patdrugs <- c()
for(i in 1:124){
  patdrugs<- c(patdrugs, keyword2[[i]]$drugs) 
  patdrugs <- patdrugs %>% unique()
}
patdrugs <- patdrugs[which(patdrugs %in% rawdrugs )]

patmedterm <- c()
for(i in 1:124){
  patmedterm<- c(patmedterm, keyword2[[i]]$`medical terms`) 
  patmedterm <- patmedterm %>% unique()
}


keyword2[[4]]$drugs
unlist(keyword2)


docs = Corpus(VectorSource(rawpatlist))
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
DF <- tidy(tf) %>% as.data.frame()


a <- grepl(patabbr[1], rawpatlist, ignore.case = F)
mtabbr <- matrix(data = 1, ncol = 125, nrow = length(patabbr))
mtabbr[,1] <- patabbr

for(i in 1:length(patabbr)){
  a <- grepl(patabbr[i], rawpatlist, ignore.case = F)
  mtabbr[i,2:125] <- as.numeric(a)
}
dfabbr <- tidy(mtabbr) %>% as.data.frame()


colnames(dfabbr) <- colnames(DF)
write.csv(dfabbr, "dfabbr.csv")

dfdrugs <- DF[which(DF$.rownames %in% patdrugs==T),] 
write.csv(dfdrugs, "dfdrugs.csv")

dfmedterm <- DF[which(DF$.rownames %in% patmedterm==T),]
write.csv(dfmedterm, "dfmedterm.csv")

dfall <- rbind.data.frame(dfabbr, dfdrugs, dfmedterm)
write.csv(dfall, "dfall.csv")

keyword2
dfall[c(4,29,6,26,10,9,28,19,1,18,4),]
wordcloud2(cbind(dfall$.rownames, dfall$X4), size = 0.8)
wordcloud2()
drugclus <- kmeans(DF[,-1], centers = 3)

df2 <- data.frame(t(dfall[-1]))
colnames(df2) <- dfall[, 1]
df2
allclus <- kmeans(df2, centers = 6)
fviz_cluster(allclus, data = df2)
plotallclus <- fviz_cluster(allclus, data = df2)

dfclus <- kmeans(DF[,-1], centers = 5)
fviz_cluster(dfclus, data = DF[,-1])
DF1 <- DF[,-1]
row.names(DF1) <- DF[,1]


dfall2 <- dfall[,-1]
row.names(dfall2) <- dfall[,1]
dfall3 <- dfall2[-which(rownames(dfall2) %in% c("and","with","for","when","the")),]
clus <- kmeans(dfall2, centers = 6)
#dfall2$cluster <- clus$cluster
ggplotly(plotallclus)

keyword2



dfdays <- read.csv("days.csv")
dfdaysdec <- dfdays[order(dfdays$x, decreasing = T),]
c1 <- paste0("X", dfdaysdec$X[1:61])
c2 <- paste0("X", dfdaysdec$X[62:119])

cleaneddrugdiff <- read.csv("cleaneddrugdiff.csv", header = T)
dfdrugs2 <- read.csv("dfdrugs2.csv", header = T)
sel <- cleaneddrugdiff[which(abs(cleaneddrugdiff$diff)>=3),]

all.data <- dfdrugs2[which(dfdrugs2$.rownames %in% sel$X),-1]
all.data[all.data>0] <- 1


#apriori
factordf <- all.data[which(rownames(all.data) %in% c1),] %>% data.frame()



for(i in 1:ncol(factordf)){
  factordf[,i] <- as.numeric(factordf[,i])
}

factordf = as(factordf, "transactions")
rules <- apriori(factordf, 
                 parameter = list(supp = 0.5, conf = 0.9, target = "rules", maxlen = 5))
summary(rules)


factordf <- all.data[,which(colnames(all.data) %in% c1)]
factordf <- t(factordf)
factordf <- data.frame(factordf)
drawnames = as.character(all.data$.rownames)
names(factordf) = drawnames

factordf = as.matrix(factordf)
co_occurrence <- t(factordf) %*% factordf
total_occurrences <- colSums(factordf)
smallid = total_occurrences[which(total_occurrences < median(total_occurrences))]
co_occurrence_d = co_occurrence / total_occurrences
co_occurrence_s = co_occurrence_d[-as.vector(smallid),-as.vector(smallid)]


graph <- graph.adjacency(round(co_occurrence_s),
                         weighted=TRUE,
                         mode="undirected",
                         diag=FALSE)

plot(graph,
     vertex.label=names(data),
     edge.arrow.mode=0,
     vertex.size=1,
     edge.width=E(graph)$weight,
     layout=layout_with_fr)





