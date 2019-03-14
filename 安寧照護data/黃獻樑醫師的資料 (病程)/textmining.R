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


###讀取病人檔案

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

rawpatlist <- lapply(1:124, readraw)


###讀取病人檔案日期

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
dfdays <- data.frame("patient"= c(1:124), "days" = days)
dfdaysdec <- dfdays[order(dfdays$days, decreasing = T),]

#c1:照會早組
#c2:照會晚組
c1 <- paste0("X", dfdaysdec$patient[1:61])
c2 <- paste0("X", dfdaysdec$patient[62:119])

###辭典
#縮寫
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

###文字探勘
docs = Corpus(VectorSource(rawpatlist))
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
})
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))


tokenizer = function(d){
  unlist(strsplit(d[[1]], split = " "))
}
seg = lapply(docs, tokenizer)

d.corpus <- Corpus(VectorSource(seg))
tdm <- TermDocumentMatrix(d.corpus)
print( tf <- as.matrix(tdm) )
DF <- tidy(tf) %>% as.data.frame()


###取DF中在drugs中的字
dfdrugs2 <- DF[which(DF$.rownames %in% drugs),]
dfdrugs2 <- dfdrugs2[!(dfdrugs2$.rownames %in% stopwords("en")),]
write.csv(dfdrugs2, "dfdrugs2.csv")




