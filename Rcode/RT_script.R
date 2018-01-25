setwd("C:/Users/acer/Desktop/PROS/Data/fMRI_PilotData")

library(ggplot2)
library(ggpubr)
library(gtools)
library(gridExtra)
library(magrittr)
library(tidyr)
library(dplyr)
library(gridExtra)
library(ggsignif)

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

youngnum <- round(table(behavior.df$GroupN)[1]/64)
oldnum <- round(table(behavior.df$GroupN)[2]/64)
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

for (i in c(1,2,3,4,13,17,18)){
  behavior.df[ ,i] <- as.factor(behavior.df[ ,i])
}

############## preprocessing data #### 

ss <- 13

mean.RT <- tapply(behavior.df$MDRT, list(behavior.df$SubjectN, behavior.df$SITtag, behavior.df$GroupN), mean)
mean.first.press <- tapply(behavior.df$MDFirstP, list(behavior.df$SubjectN, behavior.df$SITtag, behavior.df$GroupN), mean)
mean.Duration <- tapply(behavior.df$MoneyD_RT, list(behavior.df$SubjectN, behavior.df$SITtag, behavior.df$GroupN), mean)

mean.emo.RT.a <- aggregate(behavior.df$EmoRT, list(behavior.df$SubjectN, behavior.df$SITtag, behavior.df$GroupN), mean)
mean.emo.first.press.a <- aggregate(behavior.df$EFirstP, list(behavior.df$SubjectN, behavior.df$SITtag, behavior.df$GroupN), mean)
mean.emo.Duration.a <- aggregate(behavior.df$EmoD_RT, list(behavior.df$SubjectN, behavior.df$SITtag, behavior.df$GroupN), mean)

Total.emo.RT.df <- cbind(mean.emo.RT.a, mean.emo.first.press.a[4], mean.emo.Duration.a[4])
Total.emo.RT.df$Situation <- as.factor(Total.emo.RT.df$Situation) 
Total.emo.RT.df$Group <- as.factor(Total.emo.RT.df$Group)
names(Total.emo.RT.df) <- c("subjectID", "Situation", "Group", "mean.rt", "mean.firstpress", "mean.duration")
levels(Total.emo.RT.df$Situation) <- list(Prosocial = "1", Purchase = "2", Neutral = "3", Uncommon = "4")
levels(Total.emo.RT.df$Group) <- list(Young = "1", Old = "2")
str(Total.emo.RT.df)

young.mean.RT <- mean.RT[,,1][!is.na(mean.RT[,,1])]
old.mean.RT <- mean.RT[,,2][!is.na(mean.RT[,,2])]
all.mean.RT <- c(young.mean.RT, old.mean.RT)

young.firstP <- mean.first.press[,,1][!is.na(mean.first.press[,,1])]
old.firstP <- mean.first.press[,,2][!is.na(mean.first.press[,,2])]
all.firstP <- c(young.firstP, old.firstP)

young.mean.Dur <- mean.Duration[,,1][!is.na(mean.Duration[,,1])]
old.mean.Dur <- mean.Duration[,,2][!is.na(mean.Duration[,,2])]
all.mean.Dur <- c(young.mean.Dur, old.mean.Dur)

RT.group.tag <- as.factor(c(rep("Young",length(young.mean.RT)), rep("Old", length(old.mean.RT))))
RT.sit.tag <- as.factor(c(rep(c("Prosocial","Purchase","Neutral","Uncommon"), each = youngnum), rep(c("Prosocial","Purchase","Neutral","Uncommon"), each = oldnum)))

levels(RT.sit.tag) <- list(Prosocial = "Prosocial", Purchase = "Purchase", Neutral = "Neutral", Uncommon = "Uncommon")
levels(RT.group.tag) <- list(Young = "Young", Old = "Old")
all.RT.dataF <- data.frame(all.mean.RT, all.firstP, all.mean.Dur, RT.group.tag, RT.sit.tag)
 
############### RT plot ####

RT.plot <- ggline(all.RT.dataF, x = "RT.sit.tag", y = "all.mean.RT", add = c("mean_se", "jitter"), size = 1,
       color = "RT.group.tag", palette = "jco") +
  labs(title = "Group difference of reaction time by situations",
       x = "Situation", y = "Time (ms)", colour = "Group") +
  stat_compare_means(aes(group = RT.group.tag), label = "p.signif",
                     label.y = 12000) + 
  ylim(0,12000) +
  
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size=ss, face="bold"),
        legend.text = element_text(size=ss),
        legend.title = element_text(size=ss),
        axis.text = element_text(size=ss),
        axis.title = element_text(size=ss,face="bold")
  )

emo.RT.plot <- ggline(Total.emo.RT.df, x = "Situation", y = "mean.rt", add = c("mean_se", "jitter"), size = 1,
                  color = "Group", palette = "jco") +
  labs(title = "Group difference of RT by situations in emotion rating",
       x = "Situation", y = "Time (ms)", colour = "Group") +
  stat_compare_means(aes(group = Group), label = "p.signif",
                     label.y = 12000) + 
  ylim(0,12000) +
  
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size=ss, face="bold"),
        legend.text = element_text(size=ss),
        legend.title = element_text(size=ss),
        axis.text = element_text(size=ss),
        axis.title = element_text(size=ss,face="bold")
  )

############ First press plot ####

firstP.plot <- ggline(all.RT.dataF, x = "RT.sit.tag", y = "all.firstP", add = c("mean_se", "jitter"), size = 1,
       color = "RT.group.tag", palette = "jco") +
  labs(title = "Group difference of first press time by situations",
       x = "Situation", y = "Time (ms)", colour = "Group") +
  stat_compare_means(aes(group = RT.group.tag), label = "p.signif",
                     label.y = 12000) + 
  ylim(0,12000) +
  
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size=ss, face="bold"),
        legend.text = element_text(size=ss),
        legend.title = element_text(size=ss),
        axis.text = element_text(size=ss),
        axis.title = element_text(size=ss,face="bold")
  )

emo.firstP.plot <- ggline(Total.emo.RT.df, x = "Situation", y = "mean.firstpress", add = c("mean_se", "jitter"), size = 1,
                      color = "Group", palette = "jco") +
  labs(title = "Group difference of first press time by situations",
       x = "Situation", y = "Time (ms)", colour = "Group") +
  stat_compare_means(aes(group = Group), label = "p.signif",
                     label.y = 12000) + 
  ylim(0,12000) +
  
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size=ss, face="bold"),
        legend.text = element_text(size=ss),
        legend.title = element_text(size=ss),
        axis.text = element_text(size=ss),
        axis.title = element_text(size=ss,face="bold")
  )

########## Duration plot ####

Duration.plot <- ggline(all.RT.dataF, x = "RT.sit.tag", y = "all.mean.Dur", add = c("mean_se", "jitter"), size = 1,
                      color = "RT.group.tag", palette = "jco") +
  labs(title = "Group difference of duration time by situations in emotion rating",
       x = "Situation", y = "Time (ms)", colour = "Group") +
  stat_compare_means(aes(group = RT.group.tag), label = "p.signif",
                     label.y = 12000) + 
  ylim(0,12000) +
  
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size=ss, face="bold"),
        legend.text = element_text(size=ss),
        legend.title = element_text(size=ss),
        axis.text = element_text(size=ss),
        axis.title = element_text(size=ss,face="bold")
  )

emo.Duration.plot <- ggline(Total.emo.RT.df, x = "Situation", y = "mean.duration", add = c("mean_se", "jitter"), size = 1,
                        color = "Group", palette = "jco") +
  labs(title = "Group difference of duration time by situations in emotion rating",
       x = "Situation", y = "Time (ms)", colour = "Group") +
  stat_compare_means(aes(group = Group), label = "p.signif",
                     label.y = 12000) + 
  ylim(0,12000) +
  
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size=ss, face="bold"),
        legend.text = element_text(size=ss),
        legend.title = element_text(size=ss),
        axis.text = element_text(size=ss),
        axis.title = element_text(size=ss,face="bold")
  )

############ t-test ####

md.RT.split <- split(all.RT.dataF, list(all.RT.dataF$RT.group.tag, all.RT.dataF$RT.sit.tag))
Total.emo.RT.df.cc <- Total.emo.RT.df[,c(4,5,6,3,2)]
emo.RT.split <- split(Total.emo.RT.df.cc, list(Total.emo.RT.df.cc$Group, Total.emo.RT.df.cc$Situation))

RT.list <- list(md.RT.split, emo.RT.split)

stringt <- c()

for(var in 1:length(RT.list)){
  iter <- 1
  RT.split <- RT.list[[var]]
  for(i in 1:2){
    for(j in 1:(length(RT.split[[i]])-2)){
      for(k in seq(2,6,2)){
          temp <- t.test(RT.split[[i]][j], RT.split[[i+k]][j])
          if(temp$p.value < 0.05){
            if(temp$p.value < 0.001){
              star <- "***"
            } else if(temp$p.value > 0.001 & temp$p.value < 0.005){
              star <- "**"
            } else {
              star <- "*"
            }
          } else {
            star <- ""
          }
          stringt[iter] <- sprintf("t-test in %17s: %16s and %16s : %8f %3s", colnames(RT.split[[i]][j]), names(RT.split)[[i]], names(RT.split)[[i+k]], temp$p.value, star)
          iter <- iter + 1 
      }
    }
  }
  print(stringt)
}

########## print plot ####

png(sprintf("RT_ALL.png"), width = 1500, height = 700)  
  grid.arrange(RT.plot, firstP.plot, Duration.plot, emo.RT.plot, emo.firstP.plot, emo.Duration.plot, nrow=2, ncol=3)
dev.off()

for(g in c("Young", "Old")){
  for(sit in c("Prosocial", "Purchase", "Neutral", "Uncommon")){
    tempt <- t.test(all.RT.dataF$all.mean.RT[all.RT.dataF$RT.sit.tag == sit & all.RT.dataF$RT.group.tag == g])
    print(tempt$p.value)
  }
}
