rm(list=ls(all.names = TRUE))
setwd("C:/GitHub/datacourse/安寧照護data/臺大醫院")

library(textreadr)
library(rvest)
library(dplyr)
readpatient <- function(x){
  t <- paste0("^",x, "\\s" )
  filesToProcess <- list.files(pattern = t)
  n <- length(filesToProcess)
  ls <- list()
  i <- 1
  for(i in 1:n){
    ls[[i]] <- read_doc(filesToProcess[i])
  }
  return(ls)
}

x <- 7
  t <- paste0("^",x, "\\s" )
  filesToProcess <- list.files(pattern = t)
  ls <- list()
  i <- 1
  for(i in 1:length(filesToProcess)){
    ls[[i]] <- read_doc(filesToProcess[i])
    ls[[i]] <- iconv(ls[[i]], "big5", "utf8")
  }
  assign(paste0("pat",x),ls)


#關鍵字縮寫列表
r <- read.table("C:/GitHub/datacourse/安寧照護data/abbr.xlsx") 
r <- read.delim("C:/GitHub/datacourse/安寧照護data/abbr.xlsx", sep = ",",header = T, stringsAsFactors = F)

  medabbr <- read_html("https://en.wikipedia.org/wiki/List_of_medical_abbreviations:_A") %>% 
    html_nodes("jquery-tablesorter") %>% html_text()%>% gsub("\n", "",.) 
  
  
  
  
medabbr <- read_html("https://en.wikipedia.org/wiki/List_of_medical_abbreviations:_A") %>% 
  html_nodes("tbody>tr>td") %>% html_text()%>% gsub("\n", "",.) 
medabbr <- medabbr[-c(1:3,(length(medabbr)-2):length(medabbr))] 
is.odd <- rep(c(TRUE, FALSE), length = length(medabbr))
dfmedabbr <- data.frame(abbr = medabbr[is.odd], meaning = medabbr[!is.odd])
lsallabbr <- ls()
for(i in 1){
  url <- paste0("https://en.wikipedia.org/wiki/List_of_medical_abbreviations:_", LETTERS[i])
  medabbr <- read_html(url) %>% html_nodes("tbody>tr>td") %>% html_text()%>% gsub("\n", "",.) 
  medabbr <- medabbr[-c(1:3,(length(medabbr)-2):length(medabbr))] 
  is.odd <- rep(c(TRUE, FALSE), length = length(medabbr))
  dfmedabbr <- data.frame(abbr = medabbr[is.odd], meaning = medabbr[!is.odd])
  lsallabbr[i] <- dfmedabbr
}

medmean <- read_html("https://en.wikipedia.org/wiki/List_of_medical_abbreviations:_D") %>%
  html_nodes("td+ td") %>% html_text() %>% gsub("\n", "",.)   
select(medabbr, )
  
