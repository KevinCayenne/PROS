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

addstar <- function(num){
  tempstar <- c()
  if (num <= 0.06 & num > 0.05){
    tempstar <- "'  "
  } else if (num <= 0.05 & num > 0.01){
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

Y.beta.cor <- c()
O.beta.cor <- c()

fac.title <- c("+300", "+50", "+20" ,"No adjust", "-20", "-50", "0")
hjustvalue <- c( 300, 300, 40, 25, 25, 16, 70, 60, 115000, 12)
select.col <- c(12:21)

for (er.factor.iter in c(1:7)){
  
  ####
  ER.PRO.id <- ER.temp.PP[ER.temp.PP$sit.tag == "PUR" & ER.temp.PP$reg.tag == er.factor.iter,]
  
  ER.PRO.id.aggre <- aggregate(ER.PRO.id$emo.rate, list(ER.PRO.id$age.tag, ER.PRO.id$sub.tag), mean)
  colnames(ER.PRO.id.aggre) <- c("Group", "sub.id", "mean.emo")
  
  ####

  xlab.names <- c("PUR", "PURmNEU", "IRI EC Score", "IRI PD Score", "IRI PT Score", "IRI FS Score",
                  "IRI Score", "EQ Score", "Self-report Income (NTD)", "log Self-report Income (NTD)"
                  )
  PROPURorPURNEU <- list(c(1,2), c(2,3))
  PROPURorPURNEU.set <- 2 # 1 (PROPUR) or 2(PURNEU)
  pros.emo.rating.scatter <- list()
  
  itera <- 1
  for (j in select.col){
    tydi.ROI <- tydi.ROI.pre[tydi.ROI.pre$ROI_try==levels(tydi.ROI.pre$ROI_try)[1],]
    intercation.ROI.pro.pur <- cbind(tydi.ROI[tydi.ROI$cond.tag == 2, c(1:6)],
                                     value = tydi.ROI[tydi.ROI$cond.tag == PROPURorPURNEU[[PROPURorPURNEU.set]][1],]$value - tydi.ROI[tydi.ROI$cond.tag == PROPURorPURNEU[[PROPURorPURNEU.set]][2],]$value) 
    
    sub.mean.df.inter <- aggregate(intercation.ROI.pro.pur$value, list(intercation.ROI.pro.pur$sub.tag, intercation.ROI.pro.pur$cond.tag, intercation.ROI.pro.pur$phase.tag, intercation.ROI.pro.pur$age.tag), mean)
    colnames(sub.mean.df.inter) <- c("sub.tag", "sit", "phase", "Groups", "signalvalue")
    
    Mgive.df <- aggregate(behavior.df$giveM, list(behavior.df$SubjectN, behavior.df$SITtag, behavior.df$GroupN), mean)
    Mgive.df.pro <- Mgive.df[Mgive.df$Group.2 == 2,]
    colnames(Mgive.df.pro) <- c("sub.id", "situation", "age.tag", "mgive")
    colnames(Mgive.df) <- c("sub.id", "situation", "age.tag", "mgive")
    tmp.Mgive <- cbind(Mgive.df.pro ,
                       PROmPUR = Mgive.df[Mgive.df$situation == 1,]$mgive - Mgive.df[Mgive.df$situation == 2,]$mgive,
                       PROmNEU = Mgive.df[Mgive.df$situation == 1,]$mgive - Mgive.df[Mgive.df$situation == 3,]$mgive,
                       PUR = Mgive.df[Mgive.df$situation == 2,]$mgive,
                       PURmNEU = Mgive.df[Mgive.df$situation == 2,]$mgive - Mgive.df[Mgive.df$situation == 3,]$mgive)
    
    corrmerge <- cbind(sub.mean.df.inter, tmp.Mgive)
    
    new.corrmerge <- corrmerge[corrmerge$sub.id %in% EQ.df$ID,]
    new.corrmerge <- new.corrmerge[order(new.corrmerge$sub.id),]
    new.corrmerge <- cbind(new.corrmerge, 
                           IRI_EC = EQ.df$IRI_EC, 
                           IRI_PD = EQ.df$IRI_PD, 
                           IRI_PT = EQ.df$IRI_PT, 
                           IRI_FS = EQ.df$IRI_FS, 
                           IRI = EQ.df$IRI, 
                           EQ = EQ.df$EQ, 
                           Income = EQ.df$mean_gain, 
                           logIncome = log(EQ.df$mean_gain),
                           Mean_emotion_rating = ER.PRO.id.aggre[ER.PRO.id.aggre$sub.id %in% EQ.df$ID,]$mean.em
    ) 
    
    ## set variables
    y.color <- "#C6922C"
    o.color <- "#3A5BA0"
    size.pro <- 10
    
    ####
    Y.lm <- summary(lm(new.corrmerge[new.corrmerge$age.tag == 1,]$Mean_emotion_rating ~ new.corrmerge[new.corrmerge$age.tag == 1,][,j]))
    O.lm <- summary(lm(new.corrmerge[new.corrmerge$age.tag == 2,]$Mean_emotion_rating ~ new.corrmerge[new.corrmerge$age.tag == 2,][,j]))
    All.lm <- summary(lm(new.corrmerge$Mean_emotion_rating ~ new.corrmerge[,j]))
    
    cor.test.pro.Y <- cor.test(new.corrmerge[new.corrmerge$age.tag == 1,]$Mean_emotion_rating, new.corrmerge[new.corrmerge$age.tag == 1,][,j])
    cor.test.pro.O <- cor.test(new.corrmerge[new.corrmerge$age.tag == 2,]$Mean_emotion_rating, new.corrmerge[new.corrmerge$age.tag == 2,][,j])
    cor.test.pro <- cor.test(new.corrmerge$Mean_emotion_rating, new.corrmerge[,j])
    
    #### ploting
    pros.emo.rating.scatter[[itera]] <- ggscatter(new.corrmerge, x = colnames(new.corrmerge)[j], y = "Mean_emotion_rating", 
                                                 color = "Groups",
                                                 palette = c("#C6922C","#3A5BA0"),
                                                 xlab = xlab.names[itera], 
                                                 ylab = "Mean emotion reaction",
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
    
    Y.beta.cor <- rbind(Y.beta.cor, c(er.factor.iter, colnames(new.corrmerge)[j], 1, Y.lm$coefficients[2,1]))
    O.beta.cor <- rbind(O.beta.cor, c(er.factor.iter, colnames(new.corrmerge)[j], 2, O.lm$coefficients[2,1]))
    
    itera <- itera + 1
  }
  
  EMO.temp.K <- ggarrange(pros.emo.rating.scatter[[1]],
                          pros.emo.rating.scatter[[2]],
                          pros.emo.rating.scatter[[3]],
                          pros.emo.rating.scatter[[4]],
                          pros.emo.rating.scatter[[5]],
                          pros.emo.rating.scatter[[6]],
                          pros.emo.rating.scatter[[7]],
                          pros.emo.rating.scatter[[8]],
                          pros.emo.rating.scatter[[9]],
                          pros.emo.rating.scatter[[10]],
                          nrow = 2, ncol = 5,
                          common.legend = TRUE, legend = "bottom", 
                          font.label = list(size= 40))
  
  temp.signal.MD.inter.F <- annotate_figure(EMO.temp.K,
                                            top = text_grob(fac.title[er.factor.iter], 
                                                            color = "black", 
                                                            face = "bold", 
                                                            size = 30),
                                            bottom = text_grob("ROIs", 
                                                               color = "black", size = 30),
                                            left = text_grob("Parameter Estimate (a.u.)", color = "black", size = 30, rot = 90)
  )
  
  jpeg(file = paste("factor_", fac.title[er.factor.iter], "_PUR_EmoRating_ROI.jpg"), width = 5000, height = 1300)
  print(EMO.temp.K)
  dev.off()
  
}

##### Beta plots

beta.corr <- rbind(Y.beta.cor, O.beta.cor)
beta.corr <- as.data.frame(beta.corr)
colnames(beta.corr) <- c("reg.tag", "question_type", "Groups", "beta")
beta.corr$beta <- as.numeric(as.character(beta.corr$beta))
str(beta.corr)

temp.beta.plot <- list()
for (beta.iter in 1:length(levels(beta.corr$question_type))){
  
  beta.corr.temp <- beta.corr[beta.corr$question_type == levels(beta.corr$question_type)[beta.iter],]
  
  temp.beta.plot[[beta.iter]] <- ggbarplot(beta.corr.temp, x = "reg.tag", y = "beta", 
                                            color = "Groups",
                                            facet.by = "Groups",
                                            fill = "Groups",
                                            palette = c("#C6922C","#3A5BA0"),
                                            xlab = levels(beta.corr$question_type)[beta.iter], 
                                            ylab = "Beta"
                                            )
}

temp.beta.plot.arrange <- ggarrange(temp.beta.plot[[1]],
                                    temp.beta.plot[[2]],
                                    temp.beta.plot[[3]],
                                    temp.beta.plot[[4]],
                                    temp.beta.plot[[5]],
                                    temp.beta.plot[[6]],
                                    temp.beta.plot[[8]],
                                    temp.beta.plot[[9]],
                                    temp.beta.plot[[10]],
                                    nrow = 2, ncol = 5,
                                    common.legend = TRUE, legend = "bottom", 
                                    font.label = list(size= 40))

jpeg(file = paste("PUR_Ques2ER_beta.jpg"), width = 2000, height = 800)
print(temp.beta.plot.arrange)
dev.off()


lm.out.iter <- 1
sum.lm.beta <- list()
for (lm.iter in 1:length(levels(beta.corr$question_type))){
  for (agenum in 1:length(levels(beta.corr$Groups))){
    lm.beta <- beta.corr[beta.corr$question_type == levels(beta.corr$question_type)[lm.iter] & beta.corr$Groups == levels(beta.corr$Groups)[agenum],]
    lm.beta$reg.tag <- as.numeric(as.character(lm.beta$reg.tag))
    sum.lm.beta[[lm.out.iter]] <- summary(lm(lm.beta$beta ~ lm.beta$reg.tag))
    lm.out.iter <- lm.out.iter + 1
  }
}

sum.beta.pur <- data.frame()
fac.rep <- rep(1:10, each = 2)
for (lm.iter in 1:length(sum.lm.beta)){
  sum.beta.pur <- rbind(sum.beta.pur, data.frame(sum.lm.beta[[lm.iter]]$coefficients[2,1], 
                                (lm.iter+1)%%2+1, 
                                levels(beta.corr$question_type)[fac.rep[lm.iter]]
                                ))
}
colnames(sum.beta.pur) <- c("beta", "Groups", "type")
sum.beta

jpeg(file = paste("ER_behavior_PUR_beta.jpeg"), width = 600, height = 600)
print(ggbarplot(sum.beta.pur, x = "Groups", y = "beta", fill = "Groups", facet.by = "type"))
dev.off()

