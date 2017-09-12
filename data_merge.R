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
#  prepare the csv for MATLAB, delete the chinese columns

########################## End of adding columns #####################################

for (j in c(1:Subject.number)){
  
  tryy.1 <- behavior.df[(1+((j-1)*64)):(j*64),]
  MD.mm <- matrix(list(), 4, 6)
  ED.mm <- matrix(list(), 7, 6)
  
  for (i in c(1:6)) {
    for (k in c(1:4)) {
      MD.mm[[k, i]] <- tryy.1[tryy.1$SessionN ==i & tryy.1$SITtag==k,]$MDOnsettime
    }
  }
  
  for (i in c(1:6)) {
    for (k in c(1:7)) {
      ED.mm[[k, i]] <- tryy.1[tryy.1$SessionN ==i & tryy.1$RegMtag ==k,]$EmoOnsettime
    }
  }
  # write.csv(MD.mm, file = sprintf("%d-MD.csv", j), row.names = FALSE)
  # write.csv(ED.mm, file = sprintf("%d-ED.csv", j), row.names = FALSE)
}

########################## loop preprocessing ########################################

for (i in c(1,2,3,4,13,17,18)){
  behavior.df[ ,i] <- as.factor(behavior.df[ ,i])
}

MG.plot.width = 600

for (i in c(1:Subject.number)){
  
  Money <- as.vector(tapply(behavior.df$giveM[(1+((i-1)*64)):(i*64)], behavior.df$SITtag[(1+((i-1)*64)):(i*64)], mean))
  Situation <- as.vector(levels(behavior.df$SITtag[(1+((i-1)*64)):(i*64)]))
  
  ########################## start plotting ##########################################
  
  if (behavior.df$GroupN[(1+((i-1)*64))] == 1) { sub.group <- "Young" } else { sub.group <- "Old" }
  if (behavior.df$SexN[(1+((i-1)*64))] == 1) { sub.gender <- "Male" } else { sub.gender <- "Female" }
  
  money.sd <- as.vector(tapply(behavior.df$giveM[(1+((i-1)*64)):(i*64)], behavior.df$SITtag[(1+((i-1)*64)):(i*64)], sd)/8)
  
  title.name <- sprintf("Average of money giving pilot_%d_%s_%s.", i+1, sub.group, sub.gender)
  title.name.emotion <- sprintf("Emotional degree_%d_%s_%s.", i+1, sub.group, sub.gender)
  
  png(sprintf("Average of money giving_%d.png", i+1), width = MG.plot.width, height = 700)
  print(MD.plot <- ggplot() +
          
          geom_bar(mapping = aes(x = Situation, y = Money),
                   stat = 'identity', position = 'dodge', color="black") +
          
          labs(title = title.name, x = "Conditions", y = "Unit: dollars") +
          ylim(c(0, 300)) +
          
          theme(plot.title = element_text(hjust = 0.5),
                title = element_text(size=15),
                legend.text = element_text(size=15),
                legend.title = element_text(size=15),
                axis.text = element_text(size=13),
                axis.title = element_text(size=13,face="bold")) +
          
          geom_text(mapping = aes(x = Situation, y = Money),
                    size = 4, colour = 'black', vjust = -0.5, hjust = .5,
                    label=format(Money, digits=4),
                    position = position_dodge(.9)) +
          
          scale_x_discrete(labels=c("1" = "Prosocial", "2" = "Purchase",
                                    "3" = "Neutral", "4" = "Uncommon")) +
          
          geom_errorbar(aes(x = Situation, ymin = Money, ymax = Money+money.sd), width = .3,
                        position = position_dodge(.9))
  )
  dev.off()
  
  ##### emotion plotting ######
  
  Emo.mean.bySIT <- tapply(behavior.df$EmoTag[(1+((i-1)*64)):(i*64)], list(behavior.df$RegMtag[(1+((i-1)*64)):(i*64)], behavior.df$SITtag[(1+((i-1)*64)):(i*64)]), mean)
  moneyReg.type <- as.factor(rep(c("300", "+50", "+20", "same", "-20", "-50", "0"),4))
  SIT.type <- as.factor(c(rep("prosocial",7),rep("purchase",7),rep("neutral",7),rep("Uncommon",7)))
  levels(moneyReg.type) <- list(all_give = "300", fifty_more = "+50", twenty_more = "+20", same = "same", twenty_less = "-20", fifty_less = "-50", none_give = "0")
  levels(SIT.type) <- list(prosocial = "prosocial", purchase = "purchase",neutral = "neutral", Uncommon = "Uncommon")
  
  Emo.mean <- c(Emo.mean.bySIT[1:28])
  Emo.dataframe <- data.frame(Emo.mean, SIT.type, moneyReg.type)
  Emo.dataframe$moneyReg.type = factor(Emo.dataframe$moneyReg.type, levels = c('none_give','fifty_less','twenty_less','same','twenty_more','fifty_more','all_give'), order = T)
  
  png(sprintf("Emotional degree_%d.png", i+1), width = 1000, height = 700)
  print(Emo.plot <- ggplot(data = Emo.dataframe, aes(x = SIT.type, y = Emo.mean)) +
          
          geom_bar(aes(fill = moneyReg.type),
                   stat = 'identity', position = 'dodge', color="black") +
          
          labs(title = title.name.emotion, x = "Situations", y = "Mean emotion degree", fill = "money regulation type") +
          
          ylim(c(-4, 4)) +
          
          theme(plot.title = element_text(hjust = 0.5),
                title = element_text(size=15),
                legend.text = element_text(size=12),
                legend.title = element_text(size=15),
                axis.text = element_text(size=13),
                axis.title = element_text(size=13,face="bold")) +
          
          geom_text(mapping = aes(x = SIT.type, y = Emo.mean, group = moneyReg.type),
                    size = 4, colour = 'black', vjust = -0.5, hjust = .5,
                    label=format(Emo.mean, digits=2),
                    position = position_dodge(width= .9))
  )
  dev.off()
  
  png(sprintf("Subject_%d_mergedplot.png", i+1), width = 1200, height = 700)
  print(subj_plot <- ggarrange(MD.plot, Emo.plot,
                               ncol = 2, nrow = 1))
  dev.off()
  
}

############################## ALL plotting MD + Emo ##########################################

ALL_Money <- as.vector(tapply(behavior.df$giveM, list(behavior.df$SITtag, behavior.df$GroupN), mean))
ALL_Money <- replace(ALL_Money, c(2,3), ALL_Money[c(3,2)])
ALL_Money <- replace(ALL_Money, c(2,5), ALL_Money[c(5,2)])
ALL_Money <- replace(ALL_Money, c(4,6), ALL_Money[c(6,4)])
ALL_Money <- replace(ALL_Money, c(6,7), ALL_Money[c(7,6)])

# ALL_money.sd <- as.vector(tapply(behavior.df$giveM, list(behavior.df$SITtag, behavior.df$GroupN), sd)/sqrt(Subject.number)) 
ALL_money.sd <- as.vector(tapply(behavior.df$giveM, list(behavior.df$SITtag, behavior.df$GroupN), sd)/sqrt(Subject.number)) 

ALL_Money_Y.se <- (apply(tapply(behavior.df$giveM, list(behavior.df$SITtag, behavior.df$SubjectN, behavior.df$GroupN), mean)[,,1], 1, sd, na.rm = T))/sqrt(youngnum)
ALL_Money_O.se <- (apply(tapply(behavior.df$giveM, list(behavior.df$SITtag, behavior.df$SubjectN, behavior.df$GroupN), mean)[,,2], 1, sd, na.rm = T))/sqrt(oldnum)
ALL_Money.se <- c(ALL_Money_Y.se, ALL_Money_O.se)
ALL_Money.se <- replace(ALL_Money.se, c(2,3), ALL_Money.se[c(3,2)])
ALL_Money.se <- replace(ALL_Money.se, c(2,5), ALL_Money.se[c(5,2)])
ALL_Money.se <- replace(ALL_Money.se, c(4,6), ALL_Money.se[c(6,4)])
ALL_Money.se <- replace(ALL_Money.se, c(6,7), ALL_Money.se[c(7,6)])

x <- as.factor(c(rep("prosocial",2),rep("purchase",2),rep("neutral",2),rep("Uncommon",2)))
Group <- as.factor(rep(c('Young','Old'),times = 4)) 
x <- factor(x, levels = levels(x))
levels(x) <- list(prosocial = "prosocial", purchase = "purchase",neutral = "neutral", Uncommon = "Uncommon")
levels(Group) <- list(Young = "Young", Old = "Old")
Group <- factor(Group , levels = c('Old','Young'), order = T)
title.name <- sprintf("Average of money givilang pilot_ALL(Old: %d, Young: %d)", oldnum, youngnum)

png(sprintf("Average of money giving_pilot_ALL.png"), width = MG.plot.width, height = 700)
print(total.MD.plot <- ggplot() +
        
        geom_bar(mapping = aes(x = x, y = ALL_Money, fill = Group),
                 stat = 'identity', position = 'dodge', color="black") +
        
        labs(title = title.name, x = "Conditions", y = "Unit: dollars") +
        ylim(c(0,300)) +
        
        theme(plot.title = element_text(hjust = 0.5),
              title = element_text(size=15),
              legend.text = element_text(size=15),
              legend.title = element_text(size=15),
              axis.text = element_text(size=13),
              axis.title = element_text(size=13,face="bold")) +
        
        geom_text(mapping = aes(x = x, y = ALL_Money, group = Group),
                  size = 4, colour = 'black', vjust = -0.5, hjust = .5,
                  label=format(ALL_Money, digits=4),
                  position = position_dodge(.9)) +
        
        geom_errorbar(aes(x = x, ymin = ALL_Money, ymax = ALL_Money + ALL_Money.se, group = Group), width= .3,
                      position = position_dodge(.9)) 
)
dev.off()

Emo.mean.bySIT <- tapply(behavior.df$EmoTag, list(behavior.df$RegMtag, behavior.df$SITtag), mean)
moneyReg.type <- as.factor(rep(c("300", "+50", "+20", "same", "-20", "-50", "0"),4))
SIT.type <- as.factor(c(rep("prosocial",7),rep("purchase",7),rep("neutral",7),rep("Uncommon",7)))
levels(moneyReg.type) <- list(all_give = "300", fifty_more = "+50", twenty_more = "+20", same = "same", twenty_less = "-20", fifty_less = "-50", none_give = "0")
levels(SIT.type) <- list(prosocial = "prosocial", purchase = "purchase",neutral = "neutral", Uncommon = "Uncommon")

Emo.means <- c(Emo.mean.bySIT[1:28])
Emo.dataframe <- data.frame(Emo.means, SIT.type, moneyReg.type)
Emo.dataframe$moneyReg.type = factor(Emo.dataframe$moneyReg.type, levels = c('none_give','fifty_less','twenty_less','same','twenty_more','fifty_more','all_give'), order = T)

png(sprintf("Emotional degree_All.png"), width = 1000, height = 700)
print(total.emo.plot <- ggplot(data = Emo.dataframe, aes(x = SIT.type, y = Emo.means)) +
        
        geom_bar(aes(fill = moneyReg.type, group = moneyReg.type),
                 stat = 'identity', position = 'dodge', color="black") +
        
        labs(title = sprintf("Emotional degree_All (Old: %d, Young: %d)", oldnum, youngnum), x = "Situations", y = "Mean emotion degree", fill = "money regulation type") +
        
        ylim(c(-4, 4)) +
        
        theme(plot.title = element_text(hjust = 0.5),
              title = element_text(size=15),
              legend.text = element_text(size=12),
              legend.title = element_text(size=15),
              axis.text = element_text(size=13),
              axis.title = element_text(size=13,face="bold")) +
        
        geom_text(mapping = aes(x = SIT.type, y = Emo.means, label = "labs", group = moneyReg.type),
                  size = 4, colour = 'black', vjust = -0.5, hjust = .5,
                  label=format(Emo.means, digits=2),
                  stat = 'identity',
                  position = position_dodge(width = 0.9))
)
dev.off()

png(sprintf("Total_merge.png"), width = 1200, height = 700)
print(final_plot <- ggarrange(total.MD.plot, total.emo.plot,
                              ncol = 2, nrow = 1))
dev.off()

count_trial <- c(1:length(behavior.df$MDRT))
png(sprintf("RTplot_ALL.png"), width = 1200, height = 700)
print(RTplot <- ggplot(behavior.df, aes(count_trial, MDRT, colour = SubjectN)) 
      + geom_point(aes(shape = factor(GroupN))) + geom_smooth(method = "lm")
      + geom_linerange(aes(ymin = MDFirstP, ymax = MDRT)))
dev.off()
dev.off()

for (i in c(1:Subject.number)){
  boxplot(behavior.df$giveM[(1+((i-1)*64)):(i*64)] ~ behavior.df$SITtag[(1+((i-1)*64)):(i*64)])
}

################################### T-test ###########################################
Y.PRO.mean <- as.vector(na.omit(as.vector(tapply(behavior.df$giveM, list(behavior.df$SITtag, behavior.df$SubjectN, behavior.df$GroupN), mean)[1,,1])))
O.PRO.mean <- as.vector(na.omit(as.vector(tapply(behavior.df$giveM, list(behavior.df$SITtag, behavior.df$SubjectN, behavior.df$GroupN), mean)[1,,2])))
Y.PUR.mean <- as.vector(na.omit(as.vector(tapply(behavior.df$giveM, list(behavior.df$SITtag, behavior.df$SubjectN, behavior.df$GroupN), mean)[2,,1])))
O.PUR.mean <- as.vector(na.omit(as.vector(tapply(behavior.df$giveM, list(behavior.df$SITtag, behavior.df$SubjectN, behavior.df$GroupN), mean)[2,,2])))
Y.NEU.mean <- as.vector(na.omit(as.vector(tapply(behavior.df$giveM, list(behavior.df$SITtag, behavior.df$SubjectN, behavior.df$GroupN), mean)[3,,1])))
O.NEU.mean <- as.vector(na.omit(as.vector(tapply(behavior.df$giveM, list(behavior.df$SITtag, behavior.df$SubjectN, behavior.df$GroupN), mean)[3,,2])))
Y.UNC.mean <- as.vector(na.omit(as.vector(tapply(behavior.df$giveM, list(behavior.df$SITtag, behavior.df$SubjectN, behavior.df$GroupN), mean)[4,,1])))
O.UNC.mean <- as.vector(na.omit(as.vector(tapply(behavior.df$giveM, list(behavior.df$SITtag, behavior.df$SubjectN, behavior.df$GroupN), mean)[4,,2])))

T.PRO <- t.test(Y.PRO.mean,O.PRO.mean)
T.PUR <- t.test(Y.PUR.mean,O.PUR.mean)
T.NEU <- t.test(Y.NEU.mean,O.NEU.mean)
T.UNC <- t.test(Y.UNC.mean,O.UNC.mean)

###################### ALL boxplot ###########################

total.boxplot.mean_money.vector <- c(Y.PRO.mean, O.PRO.mean, Y.PUR.mean, O.PUR.mean, Y.NEU.mean, O.NEU.mean, Y.UNC.mean, O.UNC.mean)
total.boxplot.sit.vector <- as.factor(c(rep("PROS", Subject.number), rep("PUR", Subject.number), rep("NEU", Subject.number), rep("UNC", Subject.number)))
levels(total.boxplot.sit.vector) <- list(PROS = "PROS", PUR = "PUR", NEU = "NEU", UNC = "UNC")
total.boxplot.group.vector <- as.factor(c(rep(c(rep("Young", youngnum),rep("Old", oldnum)),4)))

total.boxplot <- data.frame(total.boxplot.mean_money.vector, total.boxplot.sit.vector, total.boxplot.group.vector)

png(sprintf("Average of money giving_pilot_boxplot_ALL.png"), width = MG.plot.width, height = 700)  
print(total.MD.boxplot <- ggplot(total.boxplot, 
                                 aes(x = total.boxplot.sit.vector, 
                                     y = total.boxplot.mean_money.vector, fill = total.boxplot.group.vector)) +
        geom_boxplot(aes(fill = total.boxplot.group.vector), 
                     position=position_dodge(.9)) +
        geom_dotplot(binaxis='y', stackdir='center', binwidth=3,
                     position=position_dodge(.9)) +
        stat_summary(fun.y=mean, geom="point", shape=18, size=3,
                     position=position_dodge(.9)) +
        labs(title = title.name, x = "Conditions", y = "Unit: dollars", fill = "Group") +
        theme(plot.title = element_text(hjust = 0.5),
              title = element_text(size=15),
              legend.text = element_text(size=15),
              legend.title = element_text(size=15),
              axis.text = element_text(size=13),
              axis.title = element_text(size=13,face="bold")) +
        ylim(c(0,300))
)
dev.off()


