setwd("C:/Users/acer/Desktop/PROS/Data/fMRI_PilotData")

library(ggplot2)
library(ggpubr)
library(gtools)

File.list = mixedsort(list.files("behaviorD"))
#list.files命令將behavior文件夾下所有文件名輸入File.list

combined = paste("./behaviorD/", File.list, sep="")
#用paste命令構建路徑變量combined

leng = length(combined)
#讀取combined長度，也就是文件夾下的文件個數

Subject.number =leng/6
#每個受試者有6個檔案, 除六可得幾位受試者

merge.data = read.csv(file = combined[ 1], header=T, sep=",")
#讀入第一個文件內容（可以不用先讀一個，但是為了簡單，省去定義data.frame的時間，選擇先讀入一個文件。

for (i in 2:leng){
  new.data = read.csv(file = combined[ i], header=T, sep=",")
  merge.data = rbind(merge.data,new.data)
}

behavior.df <- data.frame(merge.data)

############################## Adding columns ########################################

youngnum <- table(behavior.df$GroupN)[1]/64
oldnum <- table(behavior.df$GroupN)[2]/64
#calculate the subjects number in groups 

ncolbehavior.df <- ncol(behavior.df) #計算column number

for (i in c(1:nrow(behavior.df))){
  behavior.df[i, ncolbehavior.df+1] <- behavior.df[i, 12] - behavior.df[i, 11] # MDRT  - MDFirstP = 給錢情境的反應時間 ( 12 - 11 ) 
  behavior.df[i, ncolbehavior.df+2] <- behavior.df[i, 15] - behavior.df[i, 14] # EmoRT - EFirstP = 情緒反應的反應時間 ( 15 - 14 )
  behavior.df[i, ncolbehavior.df+3] <- behavior.df[i, 27] - behavior.df[i, 22] # TrialEnd - fixOnsettime  = ITI duration = ITI ( 27 - 22 )
  behavior.df[i, ncolbehavior.df+4] <- behavior.df[i, 24] - behavior.df[i, 23] # ISIstart - MDOnsettime = 給錢情境的duraiton ( 24 - 23 )
  behavior.df[i, ncolbehavior.df+5] <- behavior.df[i, 25] - behavior.df[i, 24] # EmoOnsettime  - ISIstart = ISI duration = ISI ( 25 - 24 )
  behavior.df[i, ncolbehavior.df+6] <- behavior.df[i, 26] - behavior.df[i, 25] # EmoEndtime - EmoOnsettime = 情緒選擇的duration ( 26 - 25 )
  behavior.df[i, ncolbehavior.df+7] <- behavior.df[i, 27] - behavior.df[i,  5] # TrialEnd - TriggerS = 從Trigger開始到當前Trial結束的時間 ( 27 - 5 )
  
  if (i >= 2){ 
    behavior.df[i, ncolbehavior.df+8] <- behavior.df[i, ncolbehavior.df+7] - behavior.df[(i-1), ncolbehavior.df+7] #一個Trial的總時間
  }
  
}

for (i in c(1:nrow(behavior.df))){
  behavior.df[i, ncolbehavior.df+9] <- behavior.df[i, 21] - behavior.df[i, 5] #LongFixation總時間( 21 -5 )
  behavior.df[i, ncolbehavior.df+10] <- behavior.df[i, 19] + behavior.df[i, 20] + 24000 #default duartion per trial =  behavior.df[i, ncolbehavior.df+8]
}
behavior.df[1, ncolbehavior.df+8] <- behavior.df[1, ncolbehavior.df+7] - behavior.df[1, ncolbehavior.df+9] #第一個Trial的總時間
colnames(behavior.df)[(ncolbehavior.df+1):(ncolbehavior.df+10)] <- c("MoneyD_RT", "EmoD_RT", "ITI_D", "MoneyD", "ISI_D","EmoD","DTriggerOnset","TrialD","LongD","DefaultT")
# adding tags

behavior.con <- behavior.df
behavior.con$SIT <- NULL 
behavior.con$EmoRESP <- NULL
write.csv(behavior.con, file = sprintf("behavior.CSV"),  row.names=FALSE)

########

num.sit <- 16  

pros.give.y <- behavior.df[behavior.df$GroupN==1 & behavior.df$SITtag==1,]
pros.give.o <- behavior.df[behavior.df$GroupN==2 & behavior.df$SITtag==1,]

sit.count.y <- rep(1:num.sit,nrow(pros.give.y)/num.sit)
pros.give.y <- cbind(pros.give.y, sit.count.y)

sit.count.o <- rep(1:num.sit,nrow(pros.give.o)/num.sit)
pros.give.o <- cbind(pros.give.o, sit.count.o)

par(mfrow = c(1,2))
plot(pros.give.y$sit.count.y, pros.give.y$giveM, type="l",lwd=1,col=2,lty=1, axes = FALSE,xlab="times",ylab="money(NTD)",main="Young")
axis(1, at = pros.give.y$sit.count.y)
axis(2)

plot(pros.give.o$sit.count.o, pros.give.o$giveM, type="l",lwd=1,col=2,lty=1, axes = FALSE,xlab="times",ylab="money(NTD)",main="Old")
axis(1, at = pros.give.o$sit.count.o)
axis(2)

for (sub in c(1:num.sit,nrow(pros.give.o)/num.sit)){
lines(pros.give.o$sit.count.o[1:16],pros.give.o$giveM[(((sub-1)*16)+1):(((sub-1)*16)+16)],lty=1,lwd=1,col=3,type="l")
}


