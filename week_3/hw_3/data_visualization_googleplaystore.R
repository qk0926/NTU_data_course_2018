library(ggplot2)
googleplay<-read.delim("googleplaystore.csv", sep = ",",header = T, stringsAsFactors = F)
googleplay$Category<-as.factor(googleplay$Category)
googleplay$Reviews<-as.numeric((googleplay$Reviews))
googleplay$Price<-as.numeric(gsub("[$]","",googleplay$Price))
googleplay$Installs<-as.numeric(gsub("[+,]","",googleplay$Installs))
googleplay$Type<-as.factor(googleplay$Type)
googleplay<-googleplay[-10473,]

plot(googleplay$Installs)
finstall<- cut(googleplay$Installs, breaks = c(0,5000, 50000, 500000,5000000, 50000000, Inf))
table(finstall)

ggplot(data = googleplay[which(googleplay$Category=="GAME"),], aes(x = Reviews, y = Installs)) +
  geom_bar(stat = "identity")

b_ins<- c(0, 5000, 50000, 500000,5000000, 50000000, Inf)
lev_ins<- c("0~5,000", "5,000~50,000", "50,000~500,000", "500,000~5,000,000", "5,000,000~50,000,000", ">50,000,000")
ggplot(na.omit(googleplay), aes(x = cut(na.omit(googleplay)$Installs, breaks = b_ins, labels = lev_ins), y = Rating)) + 
  geom_boxplot()

ggplot(subset(googleplay,Category=="GAME"& Reviews<=10000000), aes(x = Rating, y = Reviews)) +
  geom_point() + theme(
    plot.background = element_rect(colour = "black",size = 3, linetype = 4, fill = "lightblue"), 
    plot.title = element_text(colour = "black", face = "bold", size = 30, vjust = 1),
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches") ) + 
  scale_y_continuous(breaks=c(2000000, 4000000, 6000000, 8000000, 1000000)
                     )


#各種類app的數量
my.plot1 <- ggplot(googleplay, aes(x = Category)) + layer(
  geom = "bar",
  stat = "count",
  position = "identity",
  params = list(
    fill = "steelblue",
    binwidth = 0.1,
    na.rm = FALSE
  )
)  + labs(title = "App Types") + theme(
  plot.background = element_rect(colour = "black",size = 3, linetype = 4, fill = "lightblue"), 
  plot.title = element_text(colour = "black", face = "bold", size = 30, vjust = 1, hjust = 0.5),
  plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"),
  axis.text.x = element_text(angle = 90, family = "Calibri", hjust = 1, vjust = 0.5)
) 
my.plot1

#付費app各種類的數量
my.plot1b <- ggplot(googleplay[which(googleplay$Price!=0),], aes(x = Category)) + layer(
  geom = "bar",
  stat = "count",
  position = "identity",
  params = list(
    fill = "steelblue",
    binwidth = 0.1,
    na.rm = FALSE
  )
)  + labs(title = "Paid App Types") + theme(
  plot.background = element_rect(colour = "black",size = 3, linetype = 4, fill = "lightblue"), 
  plot.title = element_text(colour = "black", face = "bold", size = 30, vjust = 1, hjust = 0.5),
  plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"),
  axis.text.x = element_text(angle = 90, family = "Calibri", hjust = 1, vjust = 0.5)
) 
my.plot1b

#所有App的平均Rating分布
my.plot2 <- ggplot(googleplay, aes(x = Rating))+ xlim(0, 5) + layer(
  geom = "density",
  stat = "bin",
  position = "identity",
  params = list(
    fill = "steelblue",
    binwidth = 0.2,
    na.rm = FALSE
  )
)  + labs(title = "Rating distribution") + theme(
  plot.background = element_rect(colour = "black",size = 3, linetype = 4, fill = "lightblue"), 
  plot.title = element_text(colour = "black", face = "bold", size = 30, vjust = 1, hjust = 0.5),
  plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches")
)
my.plot2
