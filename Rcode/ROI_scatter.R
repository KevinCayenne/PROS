library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)
library(gtools)
library(gridExtra)
library(ggforce)
library(ggpmisc)

setwd("c:/Users/acer/Desktop/PROS/Data/fMRI_PilotData/ROI/")
ROI_try <- read.csv("ROI_PFC.csv", header = T)
ROI_try <- as.data.frame(ROI_try, header = T)

setwd("c:/Users/acer/Desktop/PROS/Data/fMRI_PilotData/")
behavior.df <- read.csv("behavior.CSV", header = T)

young.num <- 26
old.num <- 20

total.num <- young.num + old.num
cond.num <- 11
timec <- 12
phase.tag <- c(rep("Money_Decision", 4*timec*young.num), rep("Emotion_Decision", 7*timec*young.num), rep("Money_Decision", 4*timec*old.num), rep("Emotion_Decision", 7*timec*old.num))
length(phase.tag)
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
tydi.ROI <- gather(ROI_try, ROI_try, value, -age.tag, -cond.tag, -tc.tag, -phase.tag, -sub.tag)

levels(tydi.ROI$phase.tag) <- list(Money_Decision = "Money_Decision", Emotion_Decision = "Emotion_Decision")
levels(tydi.ROI$age.tag) <- list(Young = "Young", Old = "Old")
# 
# print(ggline(tydi.ROI, x = "tc.tag", y = "value", add = "mean_se",
#              color = "age.tag", palette = c("#EFC000", "#0073C2"))+
#         labs(x = "Time(TR)", y = "value", 
#              colour = "age.tag", fill = "age.tag") 
# )
# 
# print(ggline(tydi.ROI, x = "tc.tag", y = "value", add = "mean_se",
#              color = "age.tag", palette = c("#EFC000", "#0073C2"), facet.by = "cond.tag") +
#         labs(x = "Time(TR)", y = "value", 
#              colour = "age.tag", fill = "age.tag")
# )
# 
# print(ggline(tydi.ROI, x = "tc.tag", y = "value", add = "mean_se",
#              color = "age.tag", palette = c("#EFC000", "#0073C2"), facet.by = "phase.tag") +
#         labs(x = "Time(TR)", y = "value", 
#              colour = "age.tag", fill = "age.tag")
# )
# 
# print(ggline(tydi.ROI, x = "tc.tag", y = "value", add = "mean_se",
#              color = "age.tag", palette = c("#EFC000", "#0073C2"), facet.by = "ROI") +
#         labs(x = "Time(TR)", y = "value", 
#              colour = "age.tag", fill = "age.tag")
# )
# 
# print(ggline(tydi.ROI, x = "tc.tag", y = "value", add = "mean_se",
#              color = "age.tag", palette = c("#EFC000", "#0073C2"), facet.by = c("ROI","phase.tag")) +
#         labs(x = "Time(TR)", y = "value", 
#              colour = "age.tag", fill = "age.tag")
# )
# 
# print(ggline(tydi.ROI, x = "tc.tag", y = "value", add = "mean_se",
#              color = "age.tag", palette = c("#EFC000", "#0073C2"), facet.by = c("phase.tag", "ROI")) +
#         labs(x = "Time(TR)", y = "value",
#              colour = "age.tag", fill = "age.tag")
# )
# 
# old.phase1.tb <- c()
# old.phase2.tb <- c()
# young.phase1.tb <- c()
# young.phase2.tb <- c()
# T.mean <- tapply(tydi.ROI$value, list(tydi.ROI$tc.tag, tydi.ROI$phase.tag, tydi.ROI$age.tag), mean)
# 
# for (i in 1:12){
#   young.phase1.tb[i] <- T.mean[,,1][,1][i]/sum(T.mean[,,1][,1])
#   young.phase2.tb[i] <- T.mean[,,1][,2][i]/sum(T.mean[,,1][,2])
#   old.phase1.tb[i] <- T.mean[,,2][,1][i]/sum(T.mean[,,2][,1])
#   old.phase2.tb[i] <- T.mean[,,2][,2][i]/sum(T.mean[,,2][,2])
# }
# 
# tc <- c(0:11)
# norm.phase1.df <- data.frame(old.phase1.tb, young.phase1.tb, tc)
# norm.phase2.df <- data.frame(old.phase2.tb, young.phase2.tb, tc)
# 
# g1 <- ggplot(norm.phase1.df, aes(x=tc))
# g1 <- g1 + geom_line(aes(y=old.phase1.tb), colour="#EFC000")
# g1 <- g1 + geom_line(aes(y=young.phase1.tb), colour="#0073C2")
# g1 <- g1 + labs(title = "MD phase", x = "time(TR)", y = "value(ratio)", colour = "Group")
# g1
# 
# g2 <- ggplot(norm.phase2.df, aes(x=tc))
# g2 <- g2 + geom_line(aes(y=old.phase2.tb), colour="#EFC000")
# g2 <- g2 + geom_line(aes(y=young.phase2.tb), colour="#0073C2")
# g2 <- g2 + labs(title = "ED phase" , x = "time(TR)", y = "value(ratio)", colour = "Group")
# g2
# 
# # par(mfrow=c(1,1))
# # barplot(main="Original one bin contrast setting", c(1,rep(-1/11,11)))
# 
# par(mfrow=c(2,2))
# barplot(main="Young MD phase", norm.phase1.df$young.phase1.tb)
# barplot(main="Old MD phase", norm.phase1.df$old.phase1.tb)
# barplot(main="Young ED phase", norm.phase2.df$young.phase2.tb)
# barplot(main="Old ED phase", norm.phase2.df$old.phase2.tb)
# 
# ######

corrmergelist <- list()
corrmerge.total <- data.frame()
size.pro <- 6
hjustvalue <- 300

addstar <- function(num){
  tempstar <- c()
  if (num <= 0.05 & num > 0.01){
    tempstar <- "*  "
  } else if (num <= 0.01 & num > 0.005) {
    tempstar <- "** "
  } else if (num <= 0.005 ) {
    tempstar <- "***"
  } else {
    tempstar <- "   "
  }
  return(tempstar)
}

for(i in 1){
  sub.mean.df <- aggregate(tydi.ROI$value, list(tydi.ROI$sub.tag, tydi.ROI$cond.tag, tydi.ROI$phase.tag, tydi.ROI$age.tag), mean)
  colnames(sub.mean.df) <- c("sub.tag", "sit", "phase", "Groups", "signalvalue")
  
  sub.mean.df <- sub.mean.df[sub.mean.df$sit==i & sub.mean.df$phase=="Money_Decision",]
  
  Mgive.df <- aggregate(behavior.df$giveM, list(behavior.df$SubjectN, behavior.df$SITtag, behavior.df$GroupN), mean)
  Mgive.df <- Mgive.df[Mgive.df$Group.2==i,]
  colnames(Mgive.df) <- c("sub.id", "situation", "age.tag", "mgive")
  tmp.Mgive <- Mgive.df
  
  corrmerge <- cbind(sub.mean.df, tmp.Mgive)
  # corrmerge <- cbind(corrmerge, id = c(sub.Y.ID[-c(21:25)], sub.O.ID[-c(12:16)]))
  corrmergelist[[i]] <- corrmerge
  corrmerge.total <- rbind(corrmerge.total, corrmerge)
  
  cor.test.pro <- cor.test(corrmerge$mgive, corrmerge$signalvalue)
  cor.test.pro.Y <- cor.test(corrmerge[corrmerge$age.tag == 1,]$mgive, corrmerge[corrmerge$age.tag == 1,]$signalvalue)
  cor.test.pro.O <- cor.test(corrmerge[corrmerge$age.tag == 2,]$mgive, corrmerge[corrmerge$age.tag == 2,]$signalvalue)
  
  print(scatter.pro <- ggscatter(corrmerge, x = "mgive", y = "signalvalue", 
            color = "Groups",
            palette = c("#C6922C","#3A5BA0"),
            xlab = "Mean Money Given (NTD)", ylab = "Parameter Estimate",
            size = 5
            ) + 
          geom_smooth(aes(color = Groups),method = lm, se = FALSE, size = 2) +
          geom_hline(yintercept=0, linetype="dashed", color = "black", size=1) +
          theme(plot.title = element_text(hjust = 0.5),
                title = element_text(size=30, face="bold"),
                legend.text = element_text(size=30),
                legend.title = element_text(size=30),
                axis.text = element_text(size=20),
                axis.title = element_text(size=30,face="bold"),
                text = element_text(size=30)) +
          annotate(geom="text", x=hjustvalue, y=4.5, col=c("#C6922C"), 
                   label=paste("Young: r =", round(cor.test.pro.Y$estimate, digit = 3), addstar(round(cor.test.pro.Y$p.value, digit = 3))), 
                   size = size.pro, hjust = 1) +
          annotate(geom="text", x=hjustvalue, y=4, col=c("#3A5BA0"), 
                   label=paste("  Old: r =", round(cor.test.pro.O$estimate, digit = 3), addstar(round(cor.test.pro.O$p.value, digit = 3))), 
                   size = size.pro, hjust = 1) +
          annotate(geom="text", x=hjustvalue, y=3.5, col="black", 
                   label=paste("  All: r =", round(cor.test.pro$estimate, digit = 3), addstar(round(cor.test.pro$p.value, digit = 3))), 
                   size = size.pro, hjust = 1)
  )
}

ordered.corrmerge <- corrmergelist[[1]][order(corrmergelist[[1]]$sub.id),]

head(corrmerge.total)
str(corrmerge.total)
corrmerge.total$sit <- as.factor(corrmerge.total$sit)
levels(corrmerge.total$sit) <- list(PRO = "1", PUR = "2", NEU = "3", UNC = "4")

ggscatter(corrmerge.total, x = "mgive", y = "signalvalue",
          color = "group", 
          cor.coef = TRUE,
          conf.int = TRUE, cor.method = "pearson",
          xlab = "mean money giving (NTD)", ylab = "Parameter Estimate",
          title = "12,47,-10",
          #facet.by = "group",
          size = 4
) + geom_hline(yintercept = 0) + 
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size=30, face="bold"),
        legend.text = element_text(size=30),
        legend.title = element_text(size=30),
        axis.text = element_text(size=20),
        axis.title = element_text(size=30,face="bold"),
        text = element_text(size=30)) +
  labs(colour = "Groups") +
  scale_color_manual(values = c("#0075C9","#E5BF21")) + 
  geom_smooth(method = "lm", color = "black")


t.test(corrmerge.total[corrmerge.total$group == "Old" & corrmerge.total$sit == "NEU",]$signalvalue)
t.test(tydi.ROI[tydi.ROI$age.tag == "Old" & cond.tag == 2,]$value)

ggbarplot(tydi.ROI, x = "cond.tag", y = "value",add = "mean_se",
          color = "age.tag",
          add.params = list(group = "age.tag"),
          fill = "age.tag",
          xlab = "Groups", ylab = "Parameter Estimate",
          palette = "jco",
          position = position_dodge(0.8)
          
) + theme(plot.title = element_text(hjust = 0.5),
          title = element_text(size=30, face="bold"),
          legend.text = element_text(size=30),
          legend.title = element_text(size=30),
          axis.text = element_text(size=20),
          axis.title = element_text(size=30,face="bold"),
          text = element_text(size=30)) +
  labs(color = "Groups", fill = "Groups", x = "Situations") +
  stat_compare_means(aes(group = age.tag), label = "p.signif", label.y = 3.5, size=10)


ggbarplot(corrmerge.total, x = "sit", y = "signalvalue",add = "mean_se",
          color = "Groups",
          add.params = list(group = "group"),
          fill = "Groups",
          xlab = "Group", ylab = "Parameter Estimate",
          palette = "jco",
          position = position_dodge(0.8)
          
) + theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size=30, face="bold"),
        legend.text = element_text(size=30),
        legend.title = element_text(size=30),
        axis.text = element_text(size=20),
        axis.title = element_text(size= 30,face="bold"),
        text = element_text(size=30)) +
  labs(color = "Groups", fill = "Groups", x = "Situations") +
  stat_compare_means(aes(group = Groups), label = "p.signif", label.y = 3.5, size=10)

summary(lm(mgive ~ signalvalue*Groups, data =corrmerge.total))
anova(lm(mgive ~ signalvalue*Groups, data =corrmerge.total))
