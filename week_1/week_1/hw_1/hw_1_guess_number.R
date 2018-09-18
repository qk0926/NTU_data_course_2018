# 猜數字遊戲
# 基本功能
# 1. 請寫一個由"電腦隨機產生"不同數字的四位數(1A2B遊戲)
# 2. 玩家可"重覆"猜電腦所產生的數字，並提示猜測的結果(EX:1A2B)
# 3. 一旦猜對，系統可自動計算玩家猜測的次數

# 額外功能：每次玩家輸入完四個數字後，檢查玩家的輸入是否正確(錯誤檢查)

ans<-sample(0:9,4)
guess<-c(0,0,0,0)
n<-0

while(all(guess==ans)==F){
  x<-readline(prompt = "請輸入四個數字\n")
  y<-unlist(strsplit(as.character(x),""))
  guess<-as.numeric(y)
  if((class(guess)!="numeric")||(length(y)!=4)||(y[1]==y[2])||(y[2]==y[3])||(y[3]==y[4])||(y[1]==y[3])||(y[2]==y[4])||(y[1]==y[4])){
    print("INPUT ERROR QQ，請輸入四個不同數字")
    next
  }
  else if(all(guess==ans)==T){
    n<-n+1
    cat("恭喜答對！共猜了",n,"次")
  }  
  else{
    a<-sum(guess==ans)
    b<-0
    for(i in 1:4){
      if((guess[i]!=ans[i])&&((guess[i]-ans[1])*(guess[i]-ans[2])*(guess[i]-ans[3])*(guess[i]-ans[4])==0)){
        b<-b+1
      }
    }
    cat(a,"A",b,"B")  
    n<-n+1
    }
  }




