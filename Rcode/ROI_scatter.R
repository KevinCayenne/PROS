setwd("c:/Users/acer/Desktop/PROS/Data/fMRI_PilotData/ROI/")
library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)
library(gtools)
library(gridExtra)

ROI_try <- read.csv("ROI_PFC.csv", header = T)

setwd("c:/Users/acer/Desktop/PROS/Data/fMRI_PilotData/")
behavior.df <- read.csv("behavior.CSV", header = T)

behavior.df <- behavior.df[-c(1985:2368),]

young.num <- 20
old.num <- 11

total.num <- young.num + old.num
cond.num <- 11
timec <- 12
phase.tag <- c(rep("Money_Decision", 4*timec*young.num), rep("Emotion_Decision", 7*timec*young.num), rep("Money_Decision", 4*timec*old.num), rep("Emotion_Decision", 7*timec*old.num))

age.tag <- c(rep("Young", young.num*cond.num*timec), rep("Old", old.num*cond.num*timec))

cond.tag <- c()
for(i in 1:cond.num){
  cond.tag <- c(cond.tag, rep(i, young.num*timec))
}
for(i in 1:cond.num){
  cond.tag <- c(cond.tag, rep(i, old.num*timec))
}

tc.tag <- c(rep(rep(0:11, each = young.num), cond.num), rep(rep(0:11, each = old.num), cond.num))
sub.tag <- c(rep(1:young.num, timec*cond.num), rep(1:old.num, timec*cond.num))

ROI_try <- cbind(ROI_try, age.tag, cond.tag, tc.tag, phase.tag, sub.tag)
tydi.ROI <- gather(ROI_try, ROI, value, -age.tag, -cond.tag, -tc.tag, -phase.tag, -sub.tag)

levels(tydi.ROI$phase.tag) <- list(Money_Decision = "Money_Decision", Emotion_Decision = "Emotion_Decision")
levels(tydi.ROI$age.tag) <- list(Young = "Young", Old = "Old")

#View(ROI_try)
#View(tydi.ROI)

print(ggline(tydi.ROI, x = "tc.tag", y = "value", add = "mean_se",
             color = "age.tag", palette = c("#EFC000", "#0073C2"))+
        labs(x = "Time(TR)", y = "value", 
             colour = "Group", fill = "Group") 
)

print(ggline(tydi.ROI, x = "tc.tag", y = "value", add = "mean_se",
             color = "age.tag", palette = c("#EFC000", "#0073C2"), facet.by = "cond.tag") +
        labs(x = "Time(TR)", y = "value", 
             colour = "Group", fill = "Group")
)

print(ggline(tydi.ROI, x = "tc.tag", y = "value", add = "mean_se",
             color = "age.tag", palette = c("#EFC000", "#0073C2"), facet.by = "phase.tag") +
        labs(x = "Time(TR)", y = "value", 
             colour = "Group", fill = "Group")
)

print(ggline(tydi.ROI, x = "tc.tag", y = "value", add = "mean_se",
             color = "age.tag", palette = c("#EFC000", "#0073C2"), facet.by = "ROI") +
        labs(x = "Time(TR)", y = "value", 
             colour = "Group", fill = "Group")
)

print(ggline(tydi.ROI, x = "tc.tag", y = "value", add = "mean_se",
             color = "age.tag", palette = c("#EFC000", "#0073C2"), facet.by = c("ROI","phase.tag")) +
        labs(x = "Time(TR)", y = "value", 
             colour = "Group", fill = "Group")
)

print(ggline(tydi.ROI, x = "tc.tag", y = "value", add = "mean_se",
             color = "age.tag", palette = c("#EFC000", "#0073C2"), facet.by = c("phase.tag", "ROI")) +
        labs(x = "Time(TR)", y = "value",
             colour = "Group", fill = "Group")
)

old.phase1.tb <- c()
old.phase2.tb <- c()
young.phase1.tb <- c()
young.phase2.tb <- c()

for (i in 1:12){
  old.phase1.tb[i] <- T.mean[,,1][,1][i]/sum(T.mean[,,1][,1])
  old.phase2.tb[i] <- T.mean[,,1][,2][i]/sum(T.mean[,,1][,2])
  young.phase1.tb[i] <- T.mean[,,2][,1][i]/sum(T.mean[,,2][,1])
  young.phase2.tb[i] <- T.mean[,,2][,2][i]/sum(T.mean[,,2][,2])
}

tc <- c(0:11)
norm.phase1.df <- data.frame(old.phase1.tb, young.phase1.tb, tc)
norm.phase2.df <- data.frame(old.phase2.tb, young.phase2.tb, tc)

g1 <- ggplot(norm.phase1.df, aes(x=tc))
g1 <- g1 + geom_line(aes(y=old.phase1.tb), colour="#EFC000")
g1 <- g1 + geom_line(aes(y=young.phase1.tb), colour="#0073C2")
g1 <- g1 + labs(title = "MD phase", x = "time(TR)", y = "value(ratio)", colour = "Group")
g1

g2 <- ggplot(norm.phase2.df, aes(x=tc))
g2 <- g2 + geom_line(aes(y=old.phase2.tb), colour="#EFC000")
g2 <- g2 + geom_line(aes(y=young.phase2.tb), colour="#0073C2")
g2 <- g2 + labs(title = "ED phase" , x = "time(TR)", y = "value(ratio)", colour = "Group")
g2

par(mfrow=c(1,1))
barplot(main="Original one bin contrast setting", c(1,rep(-1/11,11)))

par(mfrow=c(2,2))
barplot(main="Young MD phase", norm.phase1.df$young.phase1.tb)
barplot(main="Old MD phase", norm.phase1.df$old.phase1.tb)
barplot(main="Young ED phase", norm.phase2.df$young.phase2.tb)
barplot(main="Old ED phase", norm.phase2.df$old.phase2.tb)

######
corrmergelist <- list()
for(i in 1:4){
  sub.mean.df <- aggregate(tydi.ROI$value, list(tydi.ROI$sub.tag, tydi.ROI$cond.tag, tydi.ROI$phase.tag, tydi.ROI$age.tag), mean)
  colnames(sub.mean.df) <- c("sub.tag", "sit", "phase", "group", "signalvalue")
  
  sub.mean.df <- sub.mean.df[sub.mean.df$sit==i & sub.mean.df$phase=="Money_Decision",]
  
  Mgive.df <- aggregate(behavior.df$giveM, list(behavior.df$SubjectN, behavior.df$SITtag, behavior.df$GroupN), mean)
  Mgive.df <- Mgive.df[Mgive.df$Group.2==i,]
  colnames(Mgive.df) <- c("sub.id", "situation", "group.tag", "mgive")
  
  corrmerge <- cbind(sub.mean.df, Mgive.df)
  corrmerge <- cbind(corrmerge, id = c(sub.Y.ID[-c(21,22)], sub.O.ID[-c(12:14)]))
  corrmergelist[[i]] <- corrmerge
  
  print(ggscatter(corrmerge, x = "mgive", y = "signalvalue", 
            group = "group", 
            color = "group",
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "mean_money", ylab = "signal",
            title = "-9,44,23"
            )
        )
}

ordered.corrmerge <- corrmergelist[[1]][order(corrmergelist[[1]]$id),]
