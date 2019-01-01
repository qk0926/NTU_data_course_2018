rm(list = ls(pattern = "^pat"))
setwd("C:/GitHub/datacourse/安寧照護data/臺大醫院")

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

#讀取同一病人資料
rawpatlist <- list()
for(x in 1:37){
  tryCatch({t <- paste0("^",x, "\\s" )
           filesToProcess <- list.files(pattern = t)
           n <- length(filesToProcess)
           ls <- list()
           i <- 1
           for(i in 1:n){
             ls[[i]] <- read_doc(filesToProcess[i])
           }
           rawpatlist[[x]] <- unlist(ls)}, error = function(e){}) 

}
f <- list.files(pattern = "all.txt$")
n <- c(1,2,5,6,9,16,18,19,22,23,26,34,35)

rawpatlist[[1]] <-  readLines("1-all.txt")
rawpatlist[[2]] <-  readLines("2-all.txt") 
rawpatlist[[5]] <-  readLines("5-all.txt") 
rawpatlist[[6]] <-  readLines("6-all.txt")
rawpatlist[[9]] <-  readLines("9-all.txt")
rawpatlist[[16]] <-  readLines("16-all.txt")
rawpatlist[[18]] <-  readLines("18-all.txt")
rawpatlist[[19]] <-  readLines("19-all.txt")
rawpatlist[[22]] <-  readLines("22-all.txt")
rawpatlist[[23]] <-  readLines("23-all.txt")
rawpatlist[[26]] <-  readLines("26-all.txt")
rawpatlist[[34]] <-  readLines("34-all.txt")
rawpatlist[[35]] <-  readLines("35-all.txt")
rawpatlist[[37]] <-  readLines("37-all.txt") 
a <- list()
for(x in 1:37){
  tryCatch({t <- paste0("^",x, "\\s" )
  filesToProcess <- list.files(pattern = t)
  n <- length(filesToProcess)
  ls <- list()
  i <- 1
  for(i in 1:n){
    ls[[i]] <- read_doc(filesToProcess[i])
  }
  a[[x]] <- unlist(ls)%>% replace_non_ascii() %>% paste(collapse = "")}, error = function(e){}
  ) 
}
a <- readLines("2-all.txt") %>% replace_non_ascii() %>% paste(collapse = "")
a[[1]] <-  readLines("1-all.txt") %>% replace_non_ascii() %>% paste(collapse = "")
a[[2]] <-  readLines("2-all.txt")  %>% replace_non_ascii() %>% paste(collapse = "")
a[[5]] <-  readLines("5-all.txt")  %>% replace_non_ascii() %>% paste(collapse = "")
a[[6]] <-  readLines("6-all.txt") %>% replace_non_ascii() %>% paste(collapse = "")
a[[9]] <-  readLines("9-all.txt") %>% replace_non_ascii() %>% paste(collapse = "")
a[[16]] <-  readLines("16-all.txt") %>% replace_non_ascii() %>% paste(collapse = "")
a[[18]] <-  readLines("18-all.txt") %>% replace_non_ascii() %>% paste(collapse = "")
a[[19]] <-  readLines("19-all.txt") %>% replace_non_ascii() %>% paste(collapse = "")
a[[22]] <-  readLines("22-all.txt") %>% replace_non_ascii() %>% paste(collapse = "")
a[[23]] <-  readLines("23-all.txt") %>% replace_non_ascii() %>% paste(collapse = "")
a[[26]] <-  readLines("26-all.txt") %>% replace_non_ascii() %>% paste(collapse = "")
a[[34]] <-  readLines("34-all.txt") %>% replace_non_ascii() %>% paste(collapse = "")
a[[35]] <-  readLines("35-all.txt") %>% replace_non_ascii() %>% paste(collapse = "")
a[[37]] <-  readLines("37-all.txt")  %>% replace_non_ascii() %>% paste(collapse = "")

x <- 1
  t <- paste0("^",x, "\\s" )
  filesToProcess <- list.files(pattern = t)
  ls <- list()
  i <- 1
  for(i in 1:length(filesToProcess)){
    ls[[i]] <- read_doc(filesToProcess[i])
    
  }
  rawpatlist[[x]] <- ls
  assign(paste0("pat",x),ls)
  
unlist(readpatient(3))

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



#藥品名
drugs <- read_html("https://www.drugs.com/alpha/a.html")%>%
  html_nodes(".boxListPopular~ .boxListPopular a") %>% html_text()

drugs <- list()
for(i in 1:26){
  URL <- paste0("https://www.rxlist.com/drugs/alpha_", letters[i], ".htm")
  tab <- URL %>%  read_html()%>%
    html_nodes("#AZ_container li a") %>% html_text() %>% tolower()
  tab <- tab %>% gsub("\\(.*\\)|\\s+$","",.) %>% unique()
  drugs[[i]] <- tab
}

drugs <- unlist(drugs) %>% gsub("\\s+$","",.)

View(drugs)



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
dis["[1]"]
View(dis)


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




disadd <- strsplit(dis, " ") %>% unlist() %>% unique()
disadd <- disadd[-which(nchar(disadd)<=3)]

abbr <- dfMed$Abbreviation[-which(nchar(dfMed$Abbreviation)<=1)]

medtermvec <- c(dfMed$Meaning, drugs, dis, disadd, trm, sym ) %>% gsub("[[:punct:]]", " ",.)%>%
  gsub("[1-9]|+\\s$", " ",.) %>% tolower() %>% unique()
medtermvec <- medtermvec[-which(nchar(medtermvec)<=1)]


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
  d <- c()
  e <- c()
  f <- c()
  g <- c()
  h <- c()
  for(i in 1:length(words)){
    if(words[i] %in% abbr)
      a <- c(a, words[i])
  }
  words <- words %>% tolower()%>% unique()
  for(i in 1:length(words)){
    if(words[i]%>% tolower() %in% dfMed$Meaning %>% tolower() %>% strsplit(" ") %>% unlist())
      b <- c(b, words[i])
  }
  for(i in 1:length(words)){
    if(words[i]%>% tolower() %in% drugs%>% tolower())
      c <- c(c, words[i])
  }

      d <- findmed2(x,dis)
  
  for(i in 1:length(words)){
    if(words[i]%>% tolower() %in% disadd%>% tolower() %>% strsplit(" ") %>% unlist())
      e <- c(e, words[i])
  }
  trm <- trm %>% tolower() %>% strsplit(" ") %>% unlist()
  for(i in 1:length(words)){
    if(words[i]%>% tolower() %in% trm)
      f <- c(f, words[i])
  }
  sym <- sym %>% tolower() %>% strsplit(" ") %>% unlist()
  for(i in 1:length(words)){
    if(words[i]%>% tolower() %in% sym)
      g <- c(g, words[i])
  }
  for(i in 1:length(words)){
    if(words[i]%>% tolower() %in% c(dfMed$Meaning, drugs, dis, disadd, trm, sym ) %>% gsub("[[:punct:]]", " ",.)%>%
       gsub("[1-9]|+\\s$", " ",.) %>% tolower() %>% unique())
      h <- c(h, words[i])
  }
  ls <- list(a,b,c,d,e,f,g,h)
  names(ls) <- c("abbreviation", "abbreviation(expand)","drugs", "disease", "disease(expand)", "treatment","symptoms", "all" )
  return(ls)}
}

lapply(x,findmed2)
grepl
keyword2 <- lapply(rawpatlist, findall)

capture.output(keyword, file = "keyword.txt")
capture.output(keyword2, file = "keyword2.txt")
crawpatlist <- gsub('[^ -~]', '', rawpatlist)
capture.output(rawpatlist, file = "rawpatlist.txt")
capture.output(a, file = "rawpubtator.txt")

patdisexp <- c()
for(i in 1:37){
  patdisexp <- c(patdisexp, keyword2[[i]]$`disease(expand)`) 
  patdisexp <- patdisexp %>% unique()
}
patdrug <- c()
for(i in 1:37){
  patdrug<- c(patdrug, keyword2[[i]]$drugs) 
  patdrug <- patdrug %>% unique()
}
pattrm <- c()
for(i in 1:37){
  pattrm<- c(pattrm, keyword2[[i]]$treatment) 
  pattrm <- pattrm %>% unique()
}
patsym <- c()
for(i in 1:37){
  patsym<- c(patsym, keyword2[[i]]$symptoms) 
  patsym <- patsym %>% unique()
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

dfdisexp <- DF[which(DF$.rownames %in% patdisexp==T),]
write.csv(dfdisexp, "disease-expand.csv")

dfdrug <- DF[which(DF$.rownames %in% patdrug==T),]
write.csv(dfdrug, "drug.csv")

dftrm <- DF[which(DF$.rownames %in% pattrm==T),]
write.csv(dfdrug, "treatment.csv")

dfsym <- DF[which(DF$.rownames %in% patsym==T),]
write.csv(dfdrug, "symptoms.csv")

dfwords <- DF[which(DF$.rownames %in% patsym==T),]

dfall <- DF[which(DF$.rownames %in% unlist(keyword2) ==T),]
write.csv(dfall, "dfall.csv")
write.csv(DF, "DF.csv")

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










