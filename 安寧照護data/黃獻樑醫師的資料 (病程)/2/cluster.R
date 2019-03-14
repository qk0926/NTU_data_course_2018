library(factoextra)
library(plotly)
library(dplyr)

library(arules)
library(spatstat)

#######
dfdrugs <- read.csv("dfdrugs.csv", header = T)
dfdrugs2 <- read.csv("dfdrugs2.csv", header = T)
 dfall <- read.csv("dfall.csv", header = T)

dfdays <-  read.csv("days.csv", header = T)
dfdaysdec <- dfdays[order(dfdays$x, decreasing = T),]
c1 <- paste0("X", dfdaysdec$X[1:61])
c2 <- paste0("X", dfdaysdec$X[62:119])
c1

c1


all.data <- rbind(dfdrugs2, dfdis)
all.data <- all.data[,-1]
all.data[all.data>0] <- 1
all.data <- unique(all.data)
rownames(all.data) <- all.data$.rownames
all.data <- all.data[!(rownames(all.data) %in% c("and","with","for","when","the", "[1]")),]


seldata <- all.data[,which(colnames(all.data) %in% c("row.names",c1))] 

seldata <- filter(seldata, rowSums(seldata)>0)

seldata[order(rowSums(seldata), decreasing = T),] %>% rownames()

diff <- rowSums(all.data[,which(colnames(all.data) %in% c1)])-rowSums(all.data[,which(colnames(all.data) %in% c2)])
ratio <- rowSums(all.data[,which(colnames(all.data) %in% c1)])/rowSums(all.data[,which(colnames(all.data) %in% c2)])
diffordered <- diff[order(diff, decreasing = T)]
dfdiff <- data.frame("groupA"=rowSums(all.data[,which(colnames(all.data) %in% c1)]),"groupB"=rowSums(all.data[,which(colnames(all.data) %in% c2)]), "diff"=diff, "ratio(A/B)"=ratio)
dfdiff <- dfdiff[order(dfdiff$diff, decreasing = T),]

drugdiff <- dfdiff[which(row.names(dfdiff) %in% dfdrugs2$.rownames),]
write.csv(drugdiff, "drugdiff.csv")
write.csv(diff, "diff.csv")
write.csv(dfdiff, "dfdiff.csv")
###c1, rid of rows that has 0 as sum

apply_cosine_similarity <- function(df){
  cos.sim <- function(df, ix) 
  {
    A = df[ix[1],]
    B = df[ix[2],]
    return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
  }   
  n <- nrow(df) 
  cmb <- expand.grid(i=1:n, j=1:n) 
  C <- matrix(apply(cmb,1,function(cmb){ cos.sim(df, cmb) }),n,n)
  C
}
dfcos <- data.frame()

for(i in 1:3){
  veccos <- c()
  for(j in 1:nrow(all.data)){
    if(i==j){
      veccos[j] <- NA
    }
    else{
      A <- all.data[i,-1]
      B <- all.data[j,-1]
      veccos[j] <- sum(A*B)/sqrt(sum(A^2)*sum(B^2))
    }
  }
  dfcos[i,] <- veccos
}
A <- all.data[1,-1]
B <- all.data[2,-1]
sum(A*B)/sqrt(sum(A^2)*sum(B^2))

mtcos <- apply_cosine_similarity(all.data[,-1])

max(nndist(all.data[,2], k=(nrow(all.data))-1))
nndist(all.data[,3], k=3)


#apriori
factordf <- all.data[,which(colnames(all.data) %in% c1)]
for(i in 1:nrow(factordf)){
  factordf[,i] <- as.factor(factordf [,i])
}
factordf = as(factordf, "transactions")
rules <- apriori(factordf, 
                 parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
summary(rules)

vis <- function(x,y,center,min){
  
  sel <- all.data[-1]
  sel <- sel[  , which(colnames(sel) %in% x)] 
  sel[sel>0]<-1
  sel <- sel[rowSums(sel)>=min,]
  sel <- sel[,colSums(sel)>0]
  set.seed(20)
  km <- kmeans(sel, centers = center, nstart = 5)
  
  
  return(ggplotly(fviz_cluster(km, data = sel, main = y)))
}




vis(c2, "disease & drug cluster, 119 patients, <22days",5,2)

gp3 <- vis(c21, "drug cluster(day=21~30)",3)

gp4 <- vis(c(c1,c11) , "disease & drug cluster(day<21), times>=1",3)

gp1
gp2
gp3
gp4
dd <-all.data[  , which(colnames(all.data) %in% c(c1, c11))] 
dd[dd>0]<-1
dd
z <- vis(c(c1, c11), "drug & symptoms cluster(day=1~20)",2)

fviz_nbclust(run, 
             FUNcluster = kmeans, # K-Means
             method = "wss",     # total within sum of square
             k.max = 12          # max number of clusters to consider
) +labs(title="Elbow Method for K-Means") + geom_vline(xintercept = 6, 
                                                       linetype = 2)
#####
rule <- apriori(t(as.matrix(all.data)),parameter=list(supp=0.2,conf=0.8,maxlen=5))
 
inspect(rule)
summary(rule)
inspect(head(sort(rule,by="support"),10))

rule





