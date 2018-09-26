library(rvest)
i<-1
for(i in 1:5){
  page<-paste("https://www.amazon.com/s/ref=sr_pg_2?fst=as%3Aon&rh=n%3A230659011%2Cn%3A172282%2Cn%3A%21493964%2Cn%3A541966%2Cn%3A13896617011%2Ck%3Aacer&page=",i,"&bbn=230659011&keywords=acer&ie=UTF8&qid=1537412681",sep = "")
  res<-read_html(page)
  d<-res %>% html_nodes("div>a") %>%  html_attr("title")
  e<-res %>% html_nodes("div>a") %>%  html_attr("href")
  e_clean<-e[is.na(d)==F]
  d_clean<-d[is.na(d)==F]
  for(k in 1:length(e_clean)){
    ls<-strsplit(e_clean[k],"")
    if(ls[[1]][1]=="/"){
      e_clean[k]<-paste("https://www.amazon.com",e_clean[k],sep="")
    }
    k<-k+1
  }
  assign(noquote(paste("page",i,sep = "")),data.frame(product_name = d_clean, website = e_clean, stringsAsFactors = F))
  rm(e_clean)
  rm(d_clean)
    i<-i+1
}

