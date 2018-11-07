library(ggplot2)
library(ggpubr)
library(tidyr)
library(plyr)
library(dplyr)
library(gtools)
library(gridExtra)
library(ggforce)
library(ggpmisc)
library(data.table)
library(devtools)
library(mni2aal)

setwd("c:/Users/acer/Desktop/PROS/Data/fMRI_PilotData/ROI/")
ROI_try <- read.csv("ROI_PFC.csv", header = T)
ROI_try <- as.data.frame(ROI_try, header = T)

ROI_try <- ROI_try[,1:9]

setwd("c:/Users/acer/Desktop/PROS/Data/fMRI_PilotData/")
behavior.df <- read.csv("behavior.CSV", header = T)

setwd("c:/Users/acer/Desktop/")
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
tydi.ROI.pre <- gather(ROI_try, ROI_try, value, -age.tag, -cond.tag, -tc.tag, -phase.tag, -sub.tag)

tydi.ROI.pre$ROI_try <- as.factor(tydi.ROI.pre$ROI_try)
tydi.ROI.pre$cond.tag <- as.factor(tydi.ROI.pre$cond.tag)
levels(tydi.ROI.pre$phase.tag) <- list(Money_Decision = "Money_Decision", Emotion_Decision = "Emotion_Decision")
levels(tydi.ROI.pre$age.tag) <- list(Young = "Young", Old = "Old")

temp.factor.MDgiven <- list()
temp.factor.MDgiven.PROPUR <- list()
temp.factor.MDgiven.PRONEU <- list()
temp.factor.IRI_EC <- list()
temp.factor.IRI_PD <- list()
temp.factor.IRI_PT <- list()
temp.factor.IRI_FS <- list()
temp.factor.IRI <- list()
temp.factor.EQ <- list()
temp.factor.Income <- list()
temp.factor.logInc <- list()
temp.factor.Eom <- list()

analysis.col <- c(1:ncol(ROI_try))
PURorNEU <- 2 # 2 (PUR) or 3(NEU)

for (j in analysis.col){
  tydi.ROI <- tydi.ROI.pre[tydi.ROI.pre$ROI_try==levels(tydi.ROI.pre$ROI_try)[j],]
  intercation.ROI.pro.pur <- cbind(tydi.ROI[tydi.ROI$cond.tag == 1, c(1:6)], value = tydi.ROI[tydi.ROI$cond.tag == 1,]$value - tydi.ROI[tydi.ROI$cond.tag == PURorNEU,]$value) 
  
  sub.mean.df.inter <- aggregate(intercation.ROI.pro.pur$value, list(intercation.ROI.pro.pur$sub.tag, intercation.ROI.pro.pur$cond.tag, intercation.ROI.pro.pur$phase.tag, intercation.ROI.pro.pur$age.tag), mean)
  colnames(sub.mean.df.inter) <- c("sub.tag", "sit", "phase", "Groups", "signalvalue")
  
  Mgive.df <- aggregate(behavior.df$giveM, list(behavior.df$SubjectN, behavior.df$SITtag, behavior.df$GroupN), mean)
  Mgive.df.pro <- Mgive.df[Mgive.df$Group.2 == 1,]
  colnames(Mgive.df.pro) <- c("sub.id", "situation", "age.tag", "mgive")
  colnames(Mgive.df) <- c("sub.id", "situation", "age.tag", "mgive")
  tmp.Mgive <- cbind(Mgive.df.pro ,
                     PROmPUR = Mgive.df[Mgive.df$situation == 1,]$mgive - Mgive.df[Mgive.df$situation == 2,]$mgive,
                     PROmNEU = Mgive.df[Mgive.df$situation == 1,]$mgive - Mgive.df[Mgive.df$situation == 3,]$mgive)
  
  corrmerge <- cbind(sub.mean.df.inter, tmp.Mgive)
  
  new.corrmerge <- corrmerge
  new.corrmerge <- new.corrmerge[order(new.corrmerge$sub.id),]
  new.corrmerge <- cbind(new.corrmerge,
                         Mean_emotion_rating = ER.PRO.id.aggre$mean.em
  ) 
  
  ## set variables
  P.title <- c("(12, 47, -10)", "(-15, 29, -7)", "(-3, 32, -1)", "(12, 41, 8)",
               "(18, 47, 17)", "(-15, 41, 20)", "(-9, 50, 26)", "(-21, -7, 41)","(-12, 17, 53)")
  size.pro <- 6
  hjustvalue <- 300
  y.color <- "#C6922C"
  o.color <- "#3A5BA0"
  size.pro <- 10
  
  hjustvalue <- c(400, 400, 400, 4)
  select.col <- c(9:12)
  xlab.names <- c("Mean amount of money apportion (NTD) PRO", 
                  "Mean amount of money apportion (NTD) PRO-PUR", 
                  "Mean amount of money apportion (NTD) PRO-NEU",
                  "Mean emotion reaction")
  ##
  
  addstar <- function(num){
    tempstar <- c()
    if (num <= 0.05 & num > 0.01){
      tempstar <- "*  "
    } else if (num <= 0.01 & num > 0.001) {
      tempstar <- "** "
    } else if (num <= 0.001) {
      tempstar <- "***"
    } else {
      tempstar <- "   "
    }
    return(tempstar)
  }
  
  corrmergelist <- list()
  corrmerge.total <- data.frame()
  pros.scatter <- list()
  
  itera = 1
  for (i in select.col){
    Y.lm <- summary(lm(new.corrmerge[new.corrmerge$age.tag == 1,][,i] ~ new.corrmerge[new.corrmerge$age.tag == 1,]$signalvalue))
    O.lm <- summary(lm(new.corrmerge[new.corrmerge$age.tag == 2,][,i] ~ new.corrmerge[new.corrmerge$age.tag == 2,]$signalvalue))
    All.lm <- summary(lm(new.corrmerge[,i] ~ new.corrmerge$Groups*new.corrmerge$signalvalue))
    
    cor.test.pro.Y <- cor.test(new.corrmerge[new.corrmerge$age.tag == 1,][,i], new.corrmerge[new.corrmerge$age.tag == 1,]$signalvalue)
    cor.test.pro.O <- cor.test(new.corrmerge[new.corrmerge$age.tag == 2,][,i], new.corrmerge[new.corrmerge$age.tag == 2,]$signalvalue)
    cor.test.pro <- cor.test(new.corrmerge[,i], new.corrmerge$signalvalue)
    
    pros.scatter[[itera]] <- ggscatter(new.corrmerge, x = colnames(new.corrmerge)[i], y = "signalvalue", 
                                       color = "Groups",
                                       palette = c("#C6922C","#3A5BA0"),
                                       xlab = xlab.names[itera], ylab = "Parameter Estimate (a.u.)",
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
      annotate(geom="text", x=hjustvalue[itera], y=4.5, col=c("#C6922C"), 
               label=paste("Young: r =", 
                           round(cor.test.pro.Y$estimate, digit = 3), 
                           addstar(round(cor.test.pro.Y$p.value, digit = 3)),
                           ";£] = ",
                           round(Y.lm$coefficients[2,1], digit = 3),
                           "(",
                           round(Y.lm$coefficients[2,2], digit = 3),
                           ")"), 
               size = size.pro, hjust = 1) +
      annotate(geom="text", x=hjustvalue[itera], y=4, col=c("#3A5BA0"), 
               label=paste("Old: r =", 
                           round(cor.test.pro.O$estimate, digit = 3), 
                           addstar(round(cor.test.pro.O$p.value, digit = 3)),
                           ";£] = ",
                           round(O.lm$coefficients[2,1], digit = 3),
                           "(",
                           round(O.lm$coefficients[2,2], digit = 3),
                           ")"), 
               size = size.pro, hjust = 1) +
      annotate(geom="text", x=hjustvalue[itera], y=3.5, col="black", 
               label=paste("Interaction: ",
                           "£] = ",
                           round(All.lm$coefficients[4,1], digit = 3),
                           "(",
                           round(All.lm$coefficients[4,2], digit = 3),
                           ")",
                           addstar(round(All.lm$coefficients[4,4], digit = 3))), 
               size = size.pro, hjust = 1)
    
    itera <- itera + 1
  }
  
  temp.factor.MDgiven[[j]] <- pros.scatter[[1]] 
  temp.factor.MDgiven.PROPUR[[j]] <- pros.scatter[[2]]
  temp.factor.MDgiven.PRONEU[[j]] <-pros.scatter[[3]]
  temp.factor.Eom[[j]] <- pros.scatter[[4]]
  
  temp.K <- ggarrange(pros.scatter[[1]],
                      pros.scatter[[2]],
                      pros.scatter[[3]],
                      pros.scatter[[4]],
                      nrow = 1, ncol = 4, 
                      labels = c("A", "B", "C", "D"),
                      common.legend = TRUE, legend = "bottom", 
                      font.label = list(size= 40))
  
  temp.P <- annotate_figure(temp.K,
                            top = text_grob(P.title[j], 
                                            color = "black", 
                                            face = "bold", 
                                            size = 100))
  
  jpeg(file = paste(P.title[j], ".jpg"), width = 3000, height = 800)
  print(temp.P)
  dev.off()
}

temp.factor.list <- list(temp.factor.MDgiven, temp.factor.MDgiven.PROPUR, temp.factor.MDgiven.PRONEU, temp.factor.Eom)

title.factor.names <- c("Mean amount of money apportion (NTD) PRO", 
                        "Mean amount of money apportion (NTD) PRO-PUR", 
                        "Mean amount of money apportion (NTD) PRO-NEU",
                        "Mean emotion reaction")

title.factor <- c("Mean_money_given_in_PRO", "Mean_money_given_in_PROPUR", "Mean_money_given_in_PRONEU", "Mean emotion reaction")

for (ii in 1:length(temp.factor.list)){
  temp.factor.K <-  ggarrange(temp.factor.list[[ii]][[1]],
                              temp.factor.list[[ii]][[2]],
                              temp.factor.list[[ii]][[3]],
                              temp.factor.list[[ii]][[4]],
                              temp.factor.list[[ii]][[5]],
                              temp.factor.list[[ii]][[6]],
                              temp.factor.list[[ii]][[7]],
                              temp.factor.list[[ii]][[8]],
                              temp.factor.list[[ii]][[9]],
                              nrow = 2, ncol = 5, 
                              labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"),
                              common.legend = TRUE, legend = "bottom", 
                              font.label = list(size= 40))
  
  temp.factor.K <- annotate_figure(temp.factor.K,
                                   top = text_grob(title.factor.names[ii], 
                                                   color = "black", 
                                                   face = "bold", 
                                                   size = 80))
  
  jpeg(file = paste(title.factor[ii], ".jpg"), width = 4000, height = 1500)
  print(temp.factor.K)
  dev.off()
}

# start barplot ####

tydi.ROI.pre.MD <- tydi.ROI.pre[tydi.ROI.pre$cond.tag %in% c(1:4),]

tydi.ROI.MD.pro.pur.interact <- cbind(tydi.ROI.pre.MD[tydi.ROI.pre.MD$cond.tag == 1, c(1:6)], value = tydi.ROI.pre.MD[tydi.ROI.pre.MD$cond.tag == 1,]$value - tydi.ROI.pre.MD[tydi.ROI.pre.MD$cond.tag == 2,]$value)
tydi.ROI.MD.pro.neu.interact <- cbind(tydi.ROI.pre.MD[tydi.ROI.pre.MD$cond.tag == 1, c(1:6)], value = tydi.ROI.pre.MD[tydi.ROI.pre.MD$cond.tag == 1,]$value - tydi.ROI.pre.MD[tydi.ROI.pre.MD$cond.tag == 3,]$value)

gg.MD.title.inter <- c("PRO-PUR", "PRO-NEU")
gg.MD.signal.interact <- list()

tydi.ROI.MD.pro.pur.interact <- ddply(tydi.ROI.MD.pro.pur.interact, 
                                      c("age.tag", "ROI_try"),
                                      summarise,
                                      N = length(value),
                                      mean = mean(value),
                                      sd = sd(value),
                                      se = sd / sqrt(N)
)

tydi.ROI.MD.pro.neu.interact <- ddply(tydi.ROI.MD.pro.neu.interact, 
                                      c("age.tag", "ROI_try"),
                                      summarise,
                                      N = length(value),
                                      mean = mean(value),
                                      sd = sd(value),
                                      se = sd / sqrt(N)
)

data.ROI.inter.list <- list(tydi.ROI.MD.pro.pur.interact, tydi.ROI.MD.pro.neu.interact)

for (i in 1:2){
  gg.MD.signal.interact[[i]] <- ggplot(data.ROI.inter.list[[i]], aes(x = ROI_try, 
                                                                     y = mean, 
                                                                     fill = age.tag)) + 
    geom_bar(stat="identity", position = position_dodge2(preserve = "single")) +
    theme(panel.background = element_rect(fill = "white", colour = "black")) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = age.tag), 
                  width = 0.5, 
                  position= position_dodge(width=0.9)) +
    scale_color_manual("Groups", values = c("Young" = y.color, "Old" = o.color)) +
    scale_fill_manual("Groups", values = c("Young" = y.color, "Old" = o.color)) +
    
    labs(title = gg.MD.title.inter[i], x = "", y = "",colour = "Groups") +   
    theme(plot.title = element_text(hjust = 0.5, face="bold"),
          title = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title = element_text(size=20),
          axis.text = element_text(size=10),
          axis.title = element_text(size=20,face="bold"),
          strip.text.x = element_text(size=20, face="bold"),
          strip.background = element_rect(colour="black", fill="white")
    ) +
    facet_wrap(~ age.tag) +
    ylim(c(-1, 1.4)) +
    geom_hline(yintercept = 0, color = "black", size = 1) +     
    geom_smooth(data = data.ROI.inter.list[[i]], aes(x = as.numeric(ROI_try), y=mean), color = "black", size = 1.3, se = FALSE, method = "lm") +
    geom_smooth(data = data.ROI.inter.list[[i]], aes(x = as.numeric(ROI_try), y=mean), color = "black", size = 1.3, se = FALSE, linetype="dashed", method = "lm", formula = y ~ poly(x,2))
  
}

temp.signal.ROI.inter <- ggarrange(gg.MD.signal.interact[[1]],
                                   gg.MD.signal.interact[[2]],
                                   nrow = 1, ncol = 2,
                                   common.legend = TRUE, legend = "bottom", 
                                   font.label = list(size= 40))

temp.signal.MD.inter.F <- annotate_figure(temp.signal.ROI.inter,
                                          top = text_grob("", 
                                                          color = "black", 
                                                          face = "bold", 
                                                          size = 30),
                                          bottom = text_grob("ROIs", 
                                                             color = "black", size = 30),
                                          left = text_grob("Parameter Estimate (a.u.)", color = "black", size = 30, rot = 90)
)

jpeg(file = paste("PE_ROI.jpg"), width = 1200, height = 1200)
print(temp.signal.MD.inter.F)
dev.off()

levels(aggre.ROI.MD$cond.tag) <- list("PRO" = 1, "PUR" = 2, "NEU" = 3, "UNC" = 4,
                                      "+300" = 5, "+50" = 6, "+20" = 7, "Same" = 8, 
                                      "-20" = 9, "-50" = 10, "0" = 11)

aggre.ROI.MD$ROI_try <- as.factor(aggre.ROI.MD$ROI_try)

gg.PE.ROI <- ggplot(aggre.ROI.MD, aes(x = ROI_try, 
                                      y = mean,
                                      fill = age.tag)) + 
  geom_bar(stat="identity", position = position_dodge2(.9)) +
  facet_grid(~ cond.tag) +
  
  geom_smooth(aes(x = ROI_try, y = mean, group = age.tag, color = age.tag),
              position = position_dodge2(.9), stat = "smooth",
              size = 1.3, se = FALSE, method = "lm") +
  geom_smooth(aes(x = ROI_try, y = mean, group = age.tag, color = age.tag),
              position = position_dodge2(.9), stat = "smooth",
              size = 1.3, se = FALSE, linetype="dashed", 
              method = "lm", formula = y ~ poly(x,2)) +
  
  scale_color_manual("Groups", values = c("Young" = y.color, "Old" = o.color)) +
  scale_fill_manual("Groups", values = c("Young" = y.color, "Old" = o.color)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = age.tag), 
                width = 0.9, 
                position = position_dodge2(0.9, padding = 0.8)) +
  
  labs(x = "ROIs", y = "Parameter Estimate (a.u.)", colour = "Groups") +   
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        title = element_text(size=30),
        legend.text = element_text(size=30),
        legend.title = element_text(size=30),
        axis.text = element_text(size=30),
        axis.title = element_text(size=30,face="bold"),
        strip.text.x = element_text(size=20, face="bold"),
        strip.background = element_rect(colour="black", fill="white")
  ) +
  geom_hline(yintercept = 0, color = "black", size = 1) 

jpeg(file = paste("gg_PE_ROI.jpg"), width = 1500, height = 800)
print(gg.PE.ROI)
dev.off()

#### ER correlations with questionarries ####

ER_Q_iter_col <- c(9:19)
emo.pros.scatter <- list()
itera <- 1

for (i in ER_Q_iter_col){
  
  Y.lm <- summary(lm(new.corrmerge[new.corrmerge$age.tag == 1,][,i] ~ new.corrmerge[new.corrmerge$age.tag == 1,]$Mean_emotion_rating))
  O.lm <- summary(lm(new.corrmerge[new.corrmerge$age.tag == 2,][,i] ~ new.corrmerge[new.corrmerge$age.tag == 2,]$Mean_emotion_rating))
  All.lm <- summary(lm(new.corrmerge[,i] ~ new.corrmerge$Mean_emotion_rating))
  
  cor.test.pro.Y <- cor.test(new.corrmerge[new.corrmerge$age.tag == 1,][,i], new.corrmerge[new.corrmerge$age.tag == 1,]$Mean_emotion_rating)
  cor.test.pro.O <- cor.test(new.corrmerge[new.corrmerge$age.tag == 2,][,i], new.corrmerge[new.corrmerge$age.tag == 2,]$Mean_emotion_rating)
  cor.test.pro <- cor.test(new.corrmerge[,i], new.corrmerge$Mean_emotion_rating)
  
  emo.pros.scatter[[itera]] <- ggscatter(new.corrmerge, x = colnames(new.corrmerge)[i], y = "Mean_emotion_rating", 
                                         color = "Groups",
                                         palette = c("#C6922C","#3A5BA0"),
                                         xlab = xlab.names[itera], ylab = "Mean emotion reaction",
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
    annotate(geom="text", x=hjustvalue[itera], y=4.5, col=c("#C6922C"), 
             label=paste("Young: r =", 
                         round(cor.test.pro.Y$estimate, digit = 3), 
                         addstar(round(cor.test.pro.Y$p.value, digit = 3)),
                         ";£] = ",
                         round(Y.lm$coefficients[2,1], digit = 3),
                         "(",
                         round(Y.lm$coefficients[2,2], digit = 3),
                         ")"), 
             size = size.pro, hjust = 1) +
    annotate(geom="text", x=hjustvalue[itera], y=4, col=c("#3A5BA0"), 
             label=paste("Old: r =", 
                         round(cor.test.pro.O$estimate, digit = 3), 
                         addstar(round(cor.test.pro.O$p.value, digit = 3)),
                         ";£] = ",
                         round(O.lm$coefficients[2,1], digit = 3),
                         "(",
                         round(O.lm$coefficients[2,2], digit = 3),
                         ")"), 
             size = size.pro, hjust = 1) +
    annotate(geom="text", x=hjustvalue[itera], y=3.5, col="black", 
             label=paste("All: r =", 
                         round(cor.test.pro$estimate, digit = 3), 
                         addstar(round(cor.test.pro$p.value, digit = 3)),
                         ";£] = ",
                         round(All.lm$coefficients[2,1], digit = 3),
                         "(",
                         round(All.lm$coefficients[2,2], digit = 3),
                         ")"), 
             size = size.pro, hjust = 1)
  
  itera <- itera + 1
}

ERQ.temp.factor.K <-  ggarrange(emo.pros.scatter[[1]],
                                emo.pros.scatter[[2]],
                                emo.pros.scatter[[3]],
                                emo.pros.scatter[[4]],
                                emo.pros.scatter[[5]],
                                emo.pros.scatter[[6]],
                                emo.pros.scatter[[7]],
                                emo.pros.scatter[[8]],
                                emo.pros.scatter[[9]],
                                emo.pros.scatter[[10]],
                                emo.pros.scatter[[11]],
                                nrow = 2, ncol = 6, 
                                common.legend = TRUE, legend = "bottom", 
                                font.label = list(size= 40))

jpeg(file = paste("ER_Q_PUR.jpg"), width = 5000, height = 1500)
print(ERQ.temp.factor.K)
dev.off()

####

# pp.new.corrmerge<- cbind(new.corrmerge, pur_give = new.corrmerge$mgive - new.corrmerge$PROmPUR)
# 
# Y.lm <- summary(lm(pp.new.corrmerge[pp.new.corrmerge$age.tag == 1,]$pur_give ~ pp.new.corrmerge[pp.new.corrmerge$age.tag == 1,]$Mean_emotion_rating))
# O.lm <- summary(lm(pp.new.corrmerge[new.corrmerge$age.tag == 2,]$pur_give ~ pp.new.corrmerge[pp.new.corrmerge$age.tag == 2,]$Mean_emotion_rating))
# All.lm <- summary(lm(pp.new.corrmerge$pur_give ~ pp.new.corrmerge$Mean_emotion_rating))
# 
# cor.test.pro.Y <- cor.test(pp.new.corrmerge[new.corrmerge$age.tag == 1,]$pur_give, pp.new.corrmerge[pp.new.corrmerge$age.tag == 1,]$Mean_emotion_rating)
# cor.test.pro.O <- cor.test(pp.new.corrmerge[new.corrmerge$age.tag == 2,]$pur_give, pp.new.corrmerge[pp.new.corrmerge$age.tag == 2,]$Mean_emotion_rating)
# cor.test.pro <- cor.test(pp.new.corrmerge$pur_give, pp.new.corrmerge$Mean_emotion_rating)
# 
# ggscatter(pp.new.corrmerge, x = "pur_give", y = "Mean_emotion_rating", 
#                                        color = "Groups",
#                                        palette = c("#C6922C","#3A5BA0"),
#                                        xlab = "Mean money apportion (PUR)", ylab = "Mean emotion reaction",
#                                        size = 5
# ) + 
#   geom_smooth(aes(color = Groups),method = lm, se = FALSE, size = 2) +
#   geom_hline(yintercept=0, linetype="dashed", color = "black", size=1) +
#   theme(plot.title = element_text(hjust = 0.5),
#         title = element_text(size=30, face="bold"),
#         legend.text = element_text(size=30),
#         legend.title = element_text(size=30),
#         axis.text = element_text(size=20),
#         axis.title = element_text(size=30,face="bold"),
#         text = element_text(size=30)) +
#   annotate(geom="text", x=400, y=4.5, col=c("#C6922C"), 
#            label=paste("Young: r =", 
#                        round(cor.test.pro.Y$estimate, digit = 3), 
#                        addstar(round(cor.test.pro.Y$p.value, digit = 3)),
#                        ";£] = ",
#                        round(Y.lm$coefficients[2,1], digit = 3),
#                        "(",
#                        round(Y.lm$coefficients[2,2], digit = 3),
#                        ")"), 
#            size = size.pro, hjust = 1) +
#   annotate(geom="text", x=400, y=4, col=c("#3A5BA0"), 
#            label=paste("Old: r =", 
#                        round(cor.test.pro.O$estimate, digit = 3), 
#                        addstar(round(cor.test.pro.O$p.value, digit = 3)),
#                        ";£] = ",
#                        round(O.lm$coefficients[2,1], digit = 3),
#                        "(",
#                        round(O.lm$coefficients[2,2], digit = 3),
#                        ")"), 
#            size = size.pro, hjust = 1) +
#   annotate(geom="text", x=400, y=3.5, col="black", 
#            label=paste("All: r =", 
#                        round(cor.test.pro$estimate, digit = 3), 
#                        addstar(round(cor.test.pro$p.value, digit = 3)),
#                        ";£] = ",
#                        round(All.lm$coefficients[2,1], digit = 3),
#                        "(",
#                        round(All.lm$coefficients[2,2], digit = 3),
#                        ")"), 
#            size = size.pro, hjust = 1)

MD_PRONEU_ROI_A <- temp.factor.list[[3]][[1]]

MD_IRIEC_ROI_H <- temp.factor.list[[4]][[8]]
MD_IRIPD_ROI_A <- temp.factor.list[[5]][[1]]
MD_IRIPD_ROI_B <- temp.factor.list[[5]][[2]]
MD_LOGINCOME_ROI_F <- temp.factor.list[[11]][[6]]

MD_BE_Quest_IRI_EC <- MD.cor.scatter[[1]]
MD_BE_Quest_IRI_PD <- MD.cor.scatter[[2]]

MD_BE_Quest <- ggarrange(MD_BE_Quest_IRI_EC, MD_BE_Quest_IRI_PD,
                        font.label = list(size= 50),
                        common.legend = TRUE, legend = "bottom",
                        nrow = 1, ncol = 2)

MD_BE_Quest <- annotate_figure(MD_BE_Quest,
                               left = text_grob("Mean amount of money apportion (NTD) PRO-NEU", 
                                                color = "black", face="bold", size = 30, rot = 90))

MD_ROI_Quest <- ggarrange(MD_IRIPD_ROI_A, MD_IRIPD_ROI_B, MD_LOGINCOME_ROI_F, MD_IRIEC_ROI_H,
                          font.label = list(size= 50),
                          labels = c("   A","   B","   F","   H"),
                          common.legend = TRUE, legend = "bottom",
                          nrow = 2, ncol = 2)

MD_PRONEU_ROI_A.f <- ggarrange(MD_PRONEU_ROI_A,
                              font.label = list(size= 50),
                              labels = c("   A"),
                              legend = "bottom",
                              nrow = 1, ncol = 1)

MD_PRONEU_ROI_A.f.be <- grid.arrange(MD_PRONEU_ROI_A.f, black.gg,  MD_BE_Quest,
                                     layout_matrix = rbind(c(1,1,1,2,2), c(3,3,3,3,3)), nrow = 2, ncol = 5)

jpeg(file = paste("Fig4.jpg"), width = 1500, height = 1000)
print(MD_ROI_Quest)
dev.off()

black.gg <- ggplot() + theme_classic()
temp.MD_ROI.plot <- grid.arrange(MD_PRONEU_ROI_A.f.be ,black.gg, MD_ROI_Quest, 
                                layout_matrix = matrix(c(1,1,1,1,1,2,3,3,3,3,3,3,3,3), ncol = 14))

jpeg(file = paste("Fig4.jpg"), width = 2700, height = 1200)
print(grid.arrange(MD_PRONEU_ROI_A.f.be , MD_ROI_Quest, 
                   layout_matrix = matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2), ncol = 24)))
dev.off()

