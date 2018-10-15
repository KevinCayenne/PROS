
library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)
library(gtools)
library(gridExtra)

setwd("c:/Users/acer/Desktop/PROS/Data/fMRI_PilotData/ROI")
ROI_try <- read.csv("ER_diff_roi_final.csv", header = T)
ROI_try <- as.data.frame(ROI_try, header = T)

setwd("c:/Users/acer/Desktop/PROS/Data/fMRI_PilotData")
behavior.df <- read.csv("behavior.CSV", header = T)

young.num <- 26
old.num <- 20

total.num <- young.num + old.num
situation.num <- 2
cond.num <-7
timec <- 12

phase.tag <- c(rep("PRO", cond.num*timec*young.num), rep("PUR", cond.num*timec*young.num), rep("PRO", cond.num*timec*old.num), rep("PUR", cond.num*timec*old.num))
age.tag <- c(rep("Young", young.num*situation.num*cond.num*timec), rep("Old", old.num*situation.num*cond.num*timec))

cond.tag <- c()
cond.tag.y <- c()
cond.tag.o <- c()
for(i in 1:cond.num){
  cond.tag.y <- c(cond.tag.y, rep(i, young.num*timec))
}
cond.tag.y <- rep(cond.tag.y, situation.num)
for(i in 1:cond.num){
  cond.tag.o <- c(cond.tag.o, rep(i, old.num*timec))
}
cond.tag.o <- rep(cond.tag.o, situation.num)
cond.tag <- c(cond.tag.y, cond.tag.o)

tc.tag <- c(rep(rep(rep(0:11, each = young.num), cond.num), situation.num), rep(rep(rep(0:11, each = old.num), cond.num), situation.num))
sub.tag <- c(rep(1:young.num, timec*cond.num*situation.num), rep(1:old.num, timec*cond.num*situation.num))

length(cond.tag)

ROI_try <- cbind(ROI_try, age.tag, cond.tag, tc.tag, phase.tag, sub.tag)
tydi.ROI <- gather(ROI_try, ROI_try, value, -age.tag, -cond.tag, -tc.tag, -phase.tag, -sub.tag)

levels(tydi.ROI$phase.tag) <- list(PRO = "PRO", PUR = "PUR")
levels(tydi.ROI$age.tag) <- list(Young = "Young", Old = "Old")
palette.p <- c("#0073C2", "#EFC000")

print(ggline(tydi.ROI, x = "tc.tag", y = "value", add = "mean_se",
             color = "age.tag", palette = palette.p, add.params = list(group = "age.tag"))+
        labs(x = "Time(TR)", y = "value", 
             colour = "age.tag", fill = "age.tag") 
)

print(ggline(tydi.ROI, x = "tc.tag", y = "value", add = "mean_se",
             color = "age.tag", palette = palette.p, facet.by = "cond.tag", add.params = list(group = "age.tag")) +
        labs(x = "Time(TR)", y = "value", 
             colour = "age.tag", fill = "age.tag")
)

print(ggline(tydi.ROI, x = "tc.tag", y = "value", add = "mean_se",
             color = "age.tag", palette = palette.p, facet.by = "phase.tag", add.params = list(group = "age.tag")) +
        labs(x = "Time(TR)", y = "value", 
             colour = "age.tag", fill = "age.tag")
)

# print(ggline(tydi.ROI, x = "tc.tag", y = "value", add = "mean_se",
#              color = "age.tag", palette = palette.p, facet.by = "ROI", add.params = list(group = "age.tag")) +
#         labs(x = "Time(TR)", y = "value", 
#              colour = "age.tag", fill = "age.tag")
# )
# 
# print(ggline(tydi.ROI, x = "tc.tag", y = "value", add = "mean_se",
#              color = "age.tag", palette = c("#EFC000", "#0073C2"), facet.by = c("ROI","phase.tag"), add.params = list(group = "age.tag")) +
#         labs(x = "Time(TR)", y = "value", 
#              colour = "age.tag", fill = "age.tag")
# )
# 
# print(ggline(tydi.ROI, x = "tc.tag", y = "value", add = "mean_se",
#              color = "age.tag", palette = c("#EFC000", "#0073C2"), facet.by = c("phase.tag", "ROI"), add.params = list(group = "age.tag")) +
#         labs(x = "Time(TR)", y = "value",
#              colour = "age.tag", fill = "age.tag")
# )

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

# par(mfrow=c(1,1))
# barplot(main="Original one bin contrast setting", c(1,rep(-1/11,11)))
# 
# par(mfrow=c(2,2))
# barplot(main="Young MD phase", norm.phase1.df$young.phase1.tb)
# barplot(main="Old MD phase", norm.phase1.df$old.phase1.tb)
# barplot(main="Young ED phase", norm.phase2.df$young.phase2.tb)
# barplot(main="Old ED phase", norm.phase2.df$old.phase2.tb)
# 
# ######

# corrmergelist <- list()
# corrmerge.total <- data.frame()
# 
# for(i in 1:4){
#   sub.mean.df <- aggregate(tydi.ROI$value, list(tydi.ROI$sub.tag, tydi.ROI$cond.tag, tydi.ROI$phase.tag, tydi.ROI$age.tag), mean)
#   colnames(sub.mean.df) <- c("sub.tag", "sit", "phase", "group", "signalvalue")
#   
#   sub.mean.df <- sub.mean.df[sub.mean.df$sit==i & sub.mean.df$phase=="Money_Decision",]
#   
#   Mgive.df <- aggregate(behavior.df$giveM, list(behavior.df$SubjectN, behavior.df$SITtag, behavior.df$GroupN), mean)
#   Mgive.df <- Mgive.df[Mgive.df$Group.2==i,]
#   colnames(Mgive.df) <- c("sub.id", "situation", "group.tag", "mgive")
#   tmp.Mgive <- Mgive.df
#   
#   corrmerge <- cbind(sub.mean.df, tmp.Mgive)
#   # corrmerge <- cbind(corrmerge, id = c(sub.Y.ID[-c(21:25)], sub.O.ID[-c(12:16)]))
#   corrmergelist[[i]] <- corrmerge
#   corrmerge.total <- rbind(corrmerge.total, corrmerge)
#   
#   print(ggscatter(corrmerge, x = "mgive", y = "signalvalue", 
#                   color = "group", conf.int = TRUE, 
#                   cor.method = "pearson",
#                   cor.coef = TRUE,
#                   xlab = "Mean Money Given (NTD)", ylab = "Parameter Estimate",
#                   size = 5
#   ) + 
#     theme(plot.title = element_text(hjust = 0.5),
#           title = element_text(size=30, face="bold"),
#           legend.text = element_text(size=30),
#           legend.title = element_text(size=30),
#           axis.text = element_text(size=20),
#           axis.title = element_text(size=30,face="bold"),
#           text = element_text(size=30)) +
#     labs(colour = "Groups") +
#     scale_color_manual(values = c("#0075C9","#E5BF21")) + 
#     geom_smooth(method = "lm", color = "black") 
#   ) 
# }
# 
# ordered.corrmerge <- corrmergelist[[1]][order(corrmergelist[[1]]$sub.id),]
# 
# head(corrmerge.total)
# str(corrmerge.total)
# corrmerge.total$sit <- as.factor(corrmerge.total$sit)
# levels(corrmerge.total$sit) <- list(PRO = "1", PUR = "2", NEU = "3", UNC = "4")
# 
# ggscatter(corrmerge.total, x = "mgive", y = "signalvalue",
#           color = "group", 
#           cor.coef = TRUE,
#           conf.int = TRUE, cor.method = "pearson",
#           xlab = "mean money giving (NTD)", ylab = "Parameter Estimate",
#           title = "12,47,-10",
#           #facet.by = "group",
#           size = 4
# ) + geom_hline(yintercept = 0) + 
#   theme(plot.title = element_text(hjust = 0.5),
#         title = element_text(size=30, face="bold"),
#         legend.text = element_text(size=30),
#         legend.title = element_text(size=30),
#         axis.text = element_text(size=20),
#         axis.title = element_text(size=30,face="bold"),
#         text = element_text(size=30)) +
#   labs(colour = "Groups") +
#   scale_color_manual(values = c("#0075C9","#E5BF21")) + 
#   geom_smooth(method = "lm", color = "black")
# 
# 
# t.test(corrmerge.total[corrmerge.total$group == "Old" & corrmerge.total$sit == "NEU",]$signalvalue)
# t.test(tydi.ROI[tydi.ROI$age.tag == "Old" & cond.tag == 2,]$value)

ggbarplot(tydi.ROI, x = "cond.tag", y = "value",add = "mean_se",
          color = "age.tag",
          add.params = list(group = "age.tag"),
          fill = "age.tag",
          xlab = "Group", ylab = "Parameter Estimate",
          palette = "jco",
          position = position_dodge(0.8)
          
) + theme(plot.title = element_text(hjust = 0.5),
          title = element_text(size=30, face="bold"),
          legend.text = element_text(size=30),
          legend.title = element_text(size=30),
          axis.text = element_text(size=20),
          axis.title = element_text(size=30,face="bold"),
          text = element_text(size=30)) +
  labs(color = "Group", fill = "Group", x = "Situations") +
  stat_compare_means(aes(group = age.tag), label = "p.signif", label.y = 3.5, size=10)
# 
# 
# ggbarplot(corrmerge.total, x = "sit", y = "signalvalue",add = "mean_se",
#           color = "group",
#           add.params = list(group = "group"),
#           fill = "group",
#           xlab = "Group", ylab = "Parameter Estimate",
#           palette = "jco",
#           position = position_dodge(0.8)
#           
# ) + theme(plot.title = element_text(hjust = 0.5),
#           title = element_text(size=30, face="bold"),
#           legend.text = element_text(size=30),
#           legend.title = element_text(size=30),
#           axis.text = element_text(size=20),
#           axis.title = element_text(size=30,face="bold"),
#           text = element_text(size=30)) +
#   labs(color = "Groups", fill = "Groups", x = "Situations") +
#   stat_compare_means(aes(group = group), label = "p.signif", label.y = 3.5, size=10)
# 
# summary(lm(mgive ~ signalvalue*group*sit, data =corrmerge.total))
# anova(lm(mgive ~ signalvalue*group, data =corrmerge.total))

head(tydi.ROI)

ggline(tydi.ROI, x = "cond.tag", y = "value", add = "mean_se",
       color = "age.tag",
       add.params = list(group = "age.tag"),
       fill = "age.tag", facet.by = "phase.tag",
       xlab = "Money regulation", ylab = "Parameter Estimate",
       palette = "jco"
      ) + stat_compare_means(aes(group = age.tag), label = "p.signif",
                           label.y = 2)

ER.ROI <- aggregate(tydi.ROI$value, list(tydi.ROI$sub.tag, tydi.ROI$phase.tag, tydi.ROI$cond.tag, tydi.ROI$age.tag), mean)

ER.ROI.final <- cbind(ER.ROI, ER.temp.PP$emo.rate, ER.temp.PP$dist, ER.temp.PP$sub.tag)
colnames(ER.ROI.final) <- c("sub.tag", "sit.tag", "reg.tag", "age.tag", "signal_value", "emo.rate", "dist", "ori.sub.tag")

rate.signal <- ggscatter(ER.ROI.final, x = "emo.rate", y = "signal_value",
          color = "age.tag", 
          cor.coef = TRUE,
          conf.int = TRUE, cor.method = "spearman",
          xlab = "Emotion rating", ylab = "Parameter Estimate",
          facet.by = c("age.tag", "sit.tag"),
          size = 2
          ) + 
         geom_hline(yintercept = 0) + 
         theme(plot.title = element_text(hjust = 0.5),
              title = element_text(size=30, face="bold"),
              legend.text = element_text(size=30),
              legend.title = element_text(size=30),
              axis.text = element_text(size=20),
              axis.title = element_text(size=30,face="bold"),
              text = element_text(size=30)) +
        labs(colour = "Groups") +
        scale_color_manual(values = c("#0075C9","#E5BF21")) +
        geom_smooth(method = 'lm', formula = y ~ poly(x,1), aes(colour = age.tag), size=1)


dist.signal <- ggscatter(ER.ROI.final, x = "dist", y = "signal_value",
          color = "age.tag", 
          cor.coef = TRUE,
          conf.int = TRUE, cor.method = "spearman",
          xlab = "The amount of change", ylab = "Parameter Estimate",
          facet.by = c("age.tag", "sit.tag"),
          size = 2
) + 
  geom_hline(yintercept = 0) + 
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size=30, face="bold"),
        legend.text = element_text(size=30),
        legend.title = element_text(size=30),
        axis.text = element_text(size=20),
        axis.title = element_text(size=30,face="bold"),
        text = element_text(size=30)) +
  labs(colour = "Groups") +
  scale_color_manual(values = c("#0075C9","#E5BF21")) +
  geom_smooth(method = 'lm', formula = y ~ poly(x,1), aes(colour = age.tag), size=1)

reg.signal <- ggscatter(ER.ROI.final, x = "reg.tag", y = "signal_value",
          color = "age.tag", 
          cor.coef = TRUE,
          conf.int = TRUE, cor.method = "spearman",
          xlab = "Money regulation", ylab = "Parameter Estimate",
          facet.by = c("age.tag", "sit.tag"),
          size = 2
) + 
  geom_hline(yintercept = 0) + 
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size=30, face="bold"),
        legend.text = element_text(size=30),
        legend.title = element_text(size=30),
        axis.text = element_text(size=20),
        axis.title = element_text(size=30,face="bold"),
        text = element_text(size=30)) +
  labs(colour = "Groups") +
  scale_color_manual(values = c("#0075C9","#E5BF21")) +
  geom_smooth(method = 'lm', formula = y ~ poly(x,1), aes(colour = age.tag), size=1)

# ggscatter(ER.ROI.final, x = "dist", y = "emo.rate",
#           color = "age.tag",
#           cor.coef = TRUE,
#           conf.int = TRUE, cor.method = "spearman",
#           xlab = "values", ylab = "Parameter Estimate",
#           facet.by = c("sit.tag"),
#           size = 2
# ) +
#   geom_hline(yintercept = 0) +
#   theme(plot.title = element_text(hjust = 0.5),
#         title = element_text(size=30, face="bold"),
#         legend.text = element_text(size=30),
#         legend.title = element_text(size=30),
#         axis.text = element_text(size=20),
#         axis.title = element_text(size=30,face="bold"),
#         text = element_text(size=30)) +
#   labs(colour = "Groups") +
#   geom_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = age.tag), size=1) +
#   scale_color_manual(values = c("#0075C9","#E5BF21"))

sit.compare <- ggline(ER.ROI.final, x = "sit.tag", y = "signal_value", add = "mean_se",
       color = "age.tag",
       
       add.params = list(group = "age.tag"),
       fill = "age.tag",
       xlab = "Situation", ylab = "Parameter Estimate",
       palette = "jco"
) + stat_compare_means(aes(group = age.tag), label = "p.signif",
                       label.y = 2)

png(sprintf("ER_ROI.png"), width = 2000, height = 800)
grid.arrange(rate.signal, dist.signal, reg.signal,sit.compare, ncol=4)
dev.off()

ER.ROI.final$ori.sub.tag <- as.factor(ER.ROI.final$ori.sub.tag)

# lmer.ee <- lmer(emo.rate ~ age.tag*sit.tag*poly(dist, 2, raw = TRUE) + 
#        (1+sit.tag|ori.sub.tag), data = ER.ROI.final)
# summary(lmer.ee)
# anova(lmer.ee)

sd.ER <- aggregate(list(ER.ROI.final$emo.rate, ER.ROI.final$dist) , list(ER.ROI.final$sit.tag, ER.ROI.final$age.tag) , mean)
colnames(sd.ER)
mean.ER <- aggregate(list(ER.ROI.final$emo.rate, ER.ROI.final$dist) , list(ER.ROI.final$sit.tag, ER.ROI.final$age.tag) , sd)

