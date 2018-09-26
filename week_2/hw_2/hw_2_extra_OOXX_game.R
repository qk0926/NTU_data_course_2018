########################################################### 
# 加分作業：
# 當要完成的目標變複雜後，學習如何將複雜的問題拆解成一個一個小問題來解決
# 練習 R function 的使用

# OOXX 遊戲練習
# 1. 設計一個兩人的OOXX遊戲。
# 2. 遊戲玩家分為A、B。A 先手，使用的符號為'O'; B 後手，使用的符號為'X'
# 3. 遊戲一開始，請輸出以下遊戲提示，並且停留等待玩家A輸入


#    Round 0
#    Now is player A's term!
#    Player A input(1~9) :


# 4. 玩家們可以輸入的數字範圍為 1~9，依序對應九宮格的九格位置。
#    如果輸入錯誤，請抓錯！輸出以下遊戲提示。

#    Invalid input! Please re-enter! 
#    Round 0
#    Now is player A's term!
#    Player A input(1~9) : 

# 5. 待玩家正確輸入完後，請輸出以下遊戲提示(當時的遊戲圖形狀況)，並且等待切換到另外一位玩家等待輸入。
#    * 提醒，記得增加'Round'次數，以及切換使用者

#    O| | 
#    _____
#     | | 
#    _____
#     | | 
#    **************
#    Round 1
#    Now is player B's term!
#    Player B input(1~9) : 

# 6. 當玩家輸入的位置之前已經有'O'或'X'時，請輸出以下遊戲提示。

#    This position is already occupied!
#    Round 1
#    Now is player B's term!
#    Player B input(1~9) : 

# 7. 當使用者輸入'exit'時，結束遊戲並印出以下遊戲提示 

#    Bye-Bye!!

# 8. 判斷遊戲結束！當三個直排、橫排、或者斜排時，請輸出以下遊戲提示(當時的遊戲圖形狀況)，並且輸出勝利的玩家。

#    O|X|O
#    _____
#    X|O|X
#    _____
#    O| | 
#    **************
#    Player A wins!!! 
#

# 9. 當空格皆被填滿且無玩家獲勝時，請輸出以下遊戲提示(當時的遊戲圖形狀況)以及和局遊戲提示。

#   O|O|X
#   _____
#   X|X|O
#   _____
#   O|X|O
#   **************
#   End in a draw!!!

m<-rep(" ",9)
tictactoe<-function(){
  cat("", m[1],"|",m[2],"|",m[3],"\n____________\n",m[4],"|",m[5],"|",m[6],"\n____________\n",m[7],"|",m[8],"|",m[9],"\n**************\n")
}
i<-0
repeat{
  while(i%in% c(0,2,4,6,8)){
    x<-readline(prompt = paste("Round",i,"\n Now is player A's turn!\n Player A input(1~9) : "))
    y<-as.integer(x)
    if(x=="exit"){
      print("Bye-bye!!")
      i<-10
    }else if((is.na(y)==T)||y %in% c(1:9)==F){
      print(paste("Invalid input! Please re-enter!"))
      next
    }else if(m[y]!=" "){
      print("This position is already occupied!")
      next
    }else{
      m[y]<-"O"
      tictactoe()
      if(((m[1]=="O")&&(m[2]=="O")&&(m[3]=="O"))||((m[4]=="O")&&(m[5]=="O")&&(m[6]=="O"))
         ||((m[7]=="O")&&(m[8]=="O")&&(m[9]=="O"))||((m[1]=="O")&&(m[4]=="O")&&(m[7]=="O"))
         ||((m[2]=="O")&&(m[5]=="O")&&(m[8]=="O"))||((m[3]=="O")&&(m[6]=="O")&&(m[9]=="O"))
         ||((m[1]=="O")&&(m[5]=="O")&&(m[9]=="O"))||((m[3]=="O")&&(m[5]=="O")&&(m[7]=="O"))){
        print("Player A wins!!! ")
        i<-10
        break
      }
      else{
        i<-i+1
        rm(x)
        rm(y)
      }
      
    }
    
  }
  
  while(i%in% c(1,3,5,7)){
    p<-readline(prompt = paste("Round",i,"\n Now is player B's turn!\n Player B input(1~9) : "))
    q<-as.integer(p)
    if(p=="exit"){
      print("Bye-bye!!")
      i<-10
    }else if((is.na(q)==T)||q %in% c(1:9)==F){
      print(paste("Invalid input! Please re-enter!"))
      next
    }else if(m[q]!=" "){
      print("This position is already occupied!")
      next
    }else{
      m[q]<-"X"
      tictactoe()
      if(((m[1]=="X")&&(m[2]=="X")&&(m[3]=="X"))||((m[4]=="X")&&(m[5]=="X")&&(m[6]=="X"))
         ||((m[7]=="X")&&(m[8]=="X")&&(m[9]=="X"))||((m[1]=="X")&&(m[4]=="X")&&(m[7]=="X"))
         ||((m[2]=="X")&&(m[5]=="X")&&(m[8]=="X"))||((m[3]=="X")&&(m[6]=="X")&&(m[9]=="X"))
         ||((m[1]=="X")&&(m[5]=="X")&&(m[9]=="X"))||((m[3]=="X")&&(m[5]=="X")&&(m[7]=="X"))){
        print("Player B wins!!! ")
        i<-10
        break
      }
      else{
        i<-i+1
        rm(p)
        rm(q)
      }
      
    }
    
  }
  while(i==9){
    print("End in a draw!!!")
    i<-i+1
  }
  if(i==10){
    rm(m)
    break
  }
}