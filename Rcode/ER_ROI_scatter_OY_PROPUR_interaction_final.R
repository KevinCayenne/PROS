library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)
library(gtools)
library(gridExtra)

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

setwd("c:/Users/acer/Desktop/PROS/Data/fMRI_PilotData/ROI/ER_final_ROI")
ROI_try <- read.csv("ER_diff_roi_final_OY_PROPUR_interaction.csv", header = T)
ROI_try <- ROI_try$A
ROI_try <- as.data.frame(ROI_try, header = T)

setwd("c:/Users/acer/Desktop/PROS/Data/fMRI_PilotData")
behavior.df <- read.csv("behavior.CSV", header = T)

setwd("c:/Users/acer/Desktop/")
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

ROI_try <- cbind(ROI_try, age.tag, cond.tag, tc.tag, phase.tag, sub.tag)
tydi.ROI <- gather(ROI_try, ROI_try, value, -age.tag, -cond.tag, -tc.tag, -phase.tag, -sub.tag)

tydi.ROI$ROI_try <- as.factor(tydi.ROI$ROI_try)
levels(tydi.ROI$phase.tag) <- list(PRO = "PRO", PUR = "PUR")
levels(tydi.ROI$age.tag) <- list(Young = "Young", Old = "Old")

Y.pur.er.list.g <- list()
PROorPUR <- c("PRO", "PUR")

title.ER <- c("
              (15, -88, -7)")

for (j in 1:length(levels(tydi.ROI$ROI_try))){
  Y.pur.er.list <- list()
  out.iter <- 1
  for (jj in 1:length(levels(tydi.ROI$phase.tag))){
    tydi.ROI.ER <- tydi.ROI[tydi.ROI$ROI_try == levels(tydi.ROI$ROI_try)[j] & tydi.ROI$phase.tag == levels(tydi.ROI$phase.tag)[jj],]
    ER.temp.PP.ROI <- ER.temp.PP[ER.temp.PP$sit.tag == levels(ER.temp.PP$sit.tag)[jj],]
     
    tydi.ROI.ER.aggre <- aggregate(tydi.ROI.ER$value, list(tydi.ROI.ER$sub.tag, tydi.ROI.ER$cond.tag, tydi.ROI.ER$age.tag), mean)
    
    ER.ROI.final <- cbind(tydi.ROI.ER.aggre, ER.temp.PP.ROI$emo.rate, ER.temp.PP.ROI$dist, ER.temp.PP.ROI$sub.tag)
    colnames(ER.ROI.final) <- c("sub.tag", "reg.tag", "age.tag", "signal_value", "emo.rate", "dist", "ori.sub.tag")
    
    analysis.factor <- c(2, 5, 6) # reg.tag, emo.rate, dist
    size.pro <- 6
    y.color <- "#C6922C"
    o.color <- "#3A5BA0"
    size.pro <- 10
    
    hjustvalue <- c(7, 4, 400)
    xlab.names.em <- c("Money regulation types", 
                       "Emotion reaction", 
                       "Adjustment for amount of money"
                       )
    
    iter <- 1
    for (i in analysis.factor){
      
      Y.lm <- summary(lm(ER.ROI.final[ER.ROI.final$age.tag == levels(ER.ROI.final$age.tag)[1],][,i] ~ ER.ROI.final[ER.ROI.final$age.tag == levels(ER.ROI.final$age.tag)[1],]$signal_value))
      O.lm <- summary(lm(ER.ROI.final[ER.ROI.final$age.tag == levels(ER.ROI.final$age.tag)[2],][,i] ~ ER.ROI.final[ER.ROI.final$age.tag == levels(ER.ROI.final$age.tag)[2],]$signal_value))
      All.lm <- summary(lm(ER.ROI.final[,i] ~ ER.ROI.final$signal_value))
      
      cor.test.pro.Y <- cor.test(ER.ROI.final[ER.ROI.final$age.tag == levels(ER.ROI.final$age.tag)[1],][,i], ER.ROI.final[ER.ROI.final$age.tag == levels(ER.ROI.final$age.tag)[1],]$signal_value)
      cor.test.pro.O <- cor.test(ER.ROI.final[ER.ROI.final$age.tag == levels(ER.ROI.final$age.tag)[2],][,i], ER.ROI.final[ER.ROI.final$age.tag == levels(ER.ROI.final$age.tag)[2],]$signal_value)
      cor.test.pro <- cor.test(ER.ROI.final[,i], ER.ROI.final$signal_value)
      
      Y.pur.er.list[[out.iter]] <- ggscatter(ER.ROI.final, x = colnames(ER.ROI.final)[i], y = "signal_value", 
                                         color = "age.tag",
                                         palette = c("#C6922C","#3A5BA0"),
                                         xlab = xlab.names.em[iter], ylab = "",
                                         size = 5
      ) + labs(color = "Groups") +
        geom_smooth(aes(color = age.tag),method = lm, se = FALSE, size = 2) +
        geom_hline(yintercept=0, linetype="dashed", color = "black", size=1) +
        geom_segment(aes(x = 0, xend = 0, y = -10, yend = 5), linetype = "dashed", size = 1) +
        theme(plot.title = element_text(hjust = 0.5),
              title = element_text(size=30, face="bold"),
              legend.text = element_text(size=30),
              legend.title = element_text(size=30),
              axis.text = element_text(size=20),
              axis.title = element_text(size=30,face="bold"),
              text = element_text(size=30)) +
        annotate(geom="text", x=hjustvalue[iter], y=9, col=c("#C6922C"), 
                 label=paste("Young: r =", 
                             round(cor.test.pro.Y$estimate, digit = 3), 
                             addstar(round(cor.test.pro.Y$p.value, digit = 3)),
                             ";£] = ",
                             round(Y.lm$coefficients[2,1], digit = 3),
                             "(",
                             round(Y.lm$coefficients[2,2], digit = 3),
                             ")"), 
                 size = size.pro, hjust = 1) +
        annotate(geom="text", x=hjustvalue[iter], y=8, col=c("#3A5BA0"), 
                 label=paste("Old: r =", 
                             round(cor.test.pro.O$estimate, digit = 3), 
                             addstar(round(cor.test.pro.O$p.value, digit = 3)),
                             ";£] = ",
                             round(O.lm$coefficients[2,1], digit = 3),
                             "(",
                             round(O.lm$coefficients[2,2], digit = 3),
                             ")"), 
                 size = size.pro, hjust = 1) +
        annotate(geom="text", x=hjustvalue[iter], y=7, col="black", 
                 label=paste("All: r =", 
                             round(cor.test.pro$estimate, digit = 3), 
                             addstar(round(cor.test.pro$p.value, digit = 3)),
                             ";£] = ",
                             round(All.lm$coefficients[2,1], digit = 3),
                             "(",
                             round(All.lm$coefficients[2,2], digit = 3),
                             ")"), 
                 size = size.pro, hjust = 1)
      
      iter <- iter + 1
      out.iter <- out.iter + 1
      }
  }
  
  Y.pur.er.list.g[[j]] <- ggarrange(Y.pur.er.list[[1]],
                                    Y.pur.er.list[[2]],
                                    Y.pur.er.list[[3]], 
                                    Y.pur.er.list[[4]],
                                    Y.pur.er.list[[5]],
                                    Y.pur.er.list[[6]],
                                    nrow = 2, ncol = 3,
                                    common.legend = TRUE, legend = "bottom", 
                                    font.label = list(size= 40))
  
  Y.pur.er.list.g[[j]] <- annotate_figure(Y.pur.er.list.g[[j]],
                                          top = text_grob(title.ER[j], 
                                                          color = "black", 
                                                          face = "bold", 
                                                          size = 50),
                                          left = text_grob("PUR                                                PRO", 
                                                           color = "black", 
                                                           face = "bold", 
                                                           size = 40, rot = 90))
  Y.pur.er.list.g[[j]] <- annotate_figure(Y.pur.er.list.g[[j]],
                                          left = text_grob("Parameter Estimate (a.u.)
                                                           ", 
                                                           color = "black", 
                                                           face = "bold", 
                                                           size = 50, rot = 90))
}

jpeg(file = "OPRO_YPRO_ER.jpg", width = 2500, height = 1500)
print(Y.pur.er.list.g)
dev.off()

##### ROI group value analysis

all.ER.inter.roi <- aggregate(tydi.ROI$value, list(tydi.ROI$sub.tag, tydi.ROI$phase.tag, tydi.ROI$cond.tag, tydi.ROI$age.tag), mean)
all.ER.ROI.final <- cbind(all.ER.inter.roi, ER.temp.PP$emo.rate, ER.temp.PP$dist, ER.temp.PP$sub.tag)
colnames(all.ER.ROI.final) <- c("sub.tag", "sit.tag", "reg.tag", "Groups", "signal_value", "emo.rate", "dist", "ori.sub.tag")

aggre.all.er.roi.sub <- aggregate(all.ER.ROI.final$signal_value, list(all.ER.ROI.final$Groups, all.ER.ROI.final$sit.tag, all.ER.ROI.final$reg.tag, all.ER.ROI.final$ori.sub.tag), mean)
colnames(aggre.all.er.roi.sub) <- c("Groups", "Situation", "reg.tag", "sub.tag", "mean_signal_value")
aggre.all.er.roi.sub$reg.tag <- as.factor(aggre.all.er.roi.sub$reg.tag)

aggre.all.er.roi <- aggregate(all.ER.ROI.final$signal_value, list(all.ER.ROI.final$Groups, all.ER.ROI.final$sit.tag, all.ER.ROI.final$reg.tag), mean)
colnames(aggre.all.er.roi) <- c("Groups", "Situation", "reg.tag", "mean_signal_value")

# str(all.ER.ROI.final)
aggre.all.ER.ROI.final <- ddply(all.ER.ROI.final, 
                                c("Groups", "sit.tag"),
                                summarise,
                                N = length(signal_value),
                                mean = mean(signal_value),
                                sd = sd(signal_value),
                                se = sd / sqrt(N)
                                )

aggre.all.ER.ROI.final.sit <- ddply(all.ER.ROI.final, 
                                    c("Groups", "sit.tag", "reg.tag"),
                                    summarise,
                                    N = length(signal_value),
                                    mean = mean(signal_value),
                                    sd = sd(signal_value),
                                    se = sd / sqrt(N)
                                    )

ggbarplot(all.ER.ROI.final, 
          x = "Groups", 
          y = "signal_value", 
          palette = c("#C6922C","#3A5BA0"),
          fill = "Groups", 
          add = c("mean_se"),
          facet.by = c("sit.tag", "reg.tag"),
          add.params = list(group = "sit.tag"),
          position = position_dodge2(.9),
          ylab = "Parameter Estimate (a.u.)"
) +
  stat_compare_means(method = "t.test")

ggbarplot(aggre.all.er.roi, 
          x = "Groups", 
          y = "mean_signal_value", 
          palette = c("#C6922C","#3A5BA0"),
          fill = "Groups", 
          facet.by = "Situation", 
          add.params = list(group = "reg.tag"),
          position = position_dodge2(.9),
          ylab = "Parameter Estimate (a.u.)")

ggbarplot(aggre.all.er.roi, 
          x = "Groups", 
          y = "mean_signal_value", 
          palette = c("#C6922C","#3A5BA0"),
          fill = "Groups", 
          add = c("mean_se"),
          facet.by = "Situation",
          ylab = "Parameter Estimate (a.u.)"
          ) +
          stat_compare_means(method = "t.test")

##### geom_bar

aggre.all.ER.ROI.final
aggre.all.ER.ROI.final.sit

aggre.all.ER.ROI.final.plot <- ggplot(aggre.all.ER.ROI.final, aes(x=sit.tag, y=mean, fill=Groups)) + 
                                        geom_bar(position=position_dodge2(), stat="identity",
                                                 size=.3) +
                                        theme_classic() +
                                        geom_errorbar(aes(ymin=mean-se, ymax=mean+se, color = Groups),
                                                      size=.5,    # Thinner lines
                                                      width=.5,
                                                      position=position_dodge(.9)
                                        ) +
                                        scale_color_manual("Groups", values = c("Young" = y.color, "Old" = o.color)) +
                                        scale_fill_manual("Groups", values = c("Young" = y.color, "Old" = o.color)) +
                                        geom_hline(yintercept = 0, color = "black", size = 1) +
                                        
                                        labs(x = "Situation", y = "Parameter Estimate (a.u.)", colour = "Groups") +   
                                        theme(plot.title = element_text(hjust = 0.5, face="bold"),
                                              
                                              title = element_text(size=30),
                                              legend.text = element_text(size=30),
                                              legend.title = element_text(size=30),
                                              axis.text.y = element_text(size=30),
                                              axis.text.x = element_text(size=30),
                                              axis.title.x = element_text(size=30, face="bold"),
                                              axis.title.y = element_text(size=30, face="bold")
                                        ) +
                                        ylim(c(-0.5,1)) +
                                        geom_signif(y_position=c(0.9), xmin=c(0.8), xmax=c(1.2),
                                                    annotation=c("**"), textsize=20, tip_length=0)

aggre.all.ER.ROI.final.sit.plot <- ggplot(aggre.all.ER.ROI.final.sit[aggre.all.ER.ROI.final.sit$reg.tag == 1,], aes(x=sit.tag, y=mean, fill=Groups)) + 
                                          geom_bar(position=position_dodge2(), stat="identity",
                                                   size=.3) +
                                          theme_classic() +
                                          geom_errorbar(aes(ymin=mean-se, ymax=mean+se, color = Groups),
                                                        size=.5,    # Thinner lines
                                                        width=.5,
                                                        position=position_dodge(.9)
                                          ) +
                                          scale_color_manual("Groups", values = c("Young" = y.color, "Old" = o.color)) +
                                          scale_fill_manual("Groups", values = c("Young" = y.color, "Old" = o.color)) +
                                          geom_hline(yintercept = 0, color = "black", size = 1) +
                                          
                                          labs(x = "Situation (in +300NT)", y = "Parameter Estimate (a.u.)", colour = "Groups") +   
                                          theme(plot.title = element_text(hjust = 0.5, face="bold"),
                                                
                                                title = element_text(size=30),
                                                legend.text = element_text(size=30),
                                                legend.title = element_text(size=30),
                                                axis.text.y = element_text(size=30),
                                                axis.text.x = element_text(size=30),
                                                axis.title.x = element_text(size=30, face="bold"),
                                                axis.title.y = element_text(size=30, face="bold")
                                          ) +
                                          ylim(c(-1.5,2.5)) +
                                          geom_signif(y_position=c(2), xmin=c(0.8), xmax=c(1.2),
                                                      annotation=c("***"), textsize=20, tip_length=0)


FS.300.PRO.YO.plot <- FS.list[[1]]
FS.300.PRO.YO.plot.emo <- FS.list.emo[[1]]
FS.ER.plot <- ggarrange(aggre.all.ER.ROI.final.plot, aggre.all.ER.ROI.final.sit.plot,
                        FS.300.PRO.YO.plot, FS.300.PRO.YO.plot.emo,
                        labels = c("B", "C", "D", "E"),
                        nrow = 2, ncol = 2,
                        common.legend = TRUE, legend = "bottom", 
                        font.label = list(size= 40))

jpeg(file = paste("ER_FS.jpg"), width = 1400, height = 1200)
print(FS.ER.plot)
dev.off()

##### 

t.aggre.all.er.roi.sub <- aggre.all.er.roi.sub[aggre.all.er.roi.sub$sub.tag %in% EQ.df$ID,]
t.Mgive.df <- Mgive.df[Mgive.df$sub.id %in% EQ.df$ID,]

Y.beta.cor <- c()
O.beta.cor <- c()
FS.list <- list()

sel.col <- c(6:17)
all.pur.er.list.gg <- list()

for(all.iter.er.roi in 1:length(levels(t.aggre.all.er.roi.sub$reg.tag))){
  
  tt.aggre.all.er.roi.sub <- t.aggre.all.er.roi.sub[t.aggre.all.er.roi.sub$reg.tag == levels(t.aggre.all.er.roi.sub$reg.tag)[all.iter.er.roi],]
 
  ER.PRO.id <- ER.temp.PP[ER.temp.PP$sit.tag == "PRO" & ER.temp.PP$reg.tag == all.iter.er.roi,]
  ER.PRO.id.aggre <- aggregate(ER.PRO.id$emo.rate, list(ER.PRO.id$age.tag, ER.PRO.id$sub.tag), mean)
 
  colnames(ER.PRO.id.aggre) <- c("Group", "sub.id", "mean.emo")
  
  final.tt.ERROI.PRO <- cbind(tt.aggre.all.er.roi.sub[tt.aggre.all.er.roi.sub$Situation == "PRO",], 
                              PROmPUR = t.Mgive.df[t.Mgive.df$situation == 1,]$mgive - t.Mgive.df[t.Mgive.df$situation == 2,]$mgive,
                              PURmNEU = t.Mgive.df[t.Mgive.df$situation == 2,]$mgive - t.Mgive.df[t.Mgive.df$situation == 3,]$mgive,
                              IRI_EC = EQ.df$IRI_EC, 
                              IRI_PD = EQ.df$IRI_PD, 
                              IRI_PT = EQ.df$IRI_PT, 
                              IRI_FS = EQ.df$IRI_FS, 
                              IRI = EQ.df$IRI, 
                              EQ = EQ.df$EQ, 
                              Income = EQ.df$mean_gain, 
                              logIncome = log(EQ.df$mean_gain),
                              Mean_emotion_rating = ER.PRO.id.aggre[ER.PRO.id.aggre$sub.id %in% EQ.df$ID,]$mean.em,
                              Income_Spend = EQ.df$mean_gain - EQ.df$mean_spend
                              )
  final.tt.ERROI.PUR <- cbind(tt.aggre.all.er.roi.sub[tt.aggre.all.er.roi.sub$Situation == "PUR",], 
                              PROmPUR = t.Mgive.df[t.Mgive.df$situation == 1,]$mgive - t.Mgive.df[t.Mgive.df$situation == 2,]$mgive,
                              PURmNEU = t.Mgive.df[t.Mgive.df$situation == 2,]$mgive - t.Mgive.df[t.Mgive.df$situation == 3,]$mgive,
                              IRI_EC = EQ.df$IRI_EC, 
                              IRI_PD = EQ.df$IRI_PD, 
                              IRI_PT = EQ.df$IRI_PT, 
                              IRI_FS = EQ.df$IRI_FS, 
                              IRI = EQ.df$IRI, 
                              EQ = EQ.df$EQ, 
                              Income = EQ.df$mean_gain, 
                              logIncome = log(EQ.df$mean_gain),
                              Mean_emotion_rating = ER.PRO.id.aggre[ER.PRO.id.aggre$sub.id %in% EQ.df$ID,]$mean.em,
                              Income_Spend = EQ.df$mean_gain - EQ.df$mean_spend
                              )
  final.tt.ERROI <- rbind(final.tt.ERROI.PRO)
  
  y.color <- "#C6922C"
  o.color <- "#3A5BA0"
  size.pro <- 10
  fac.title <- c("+300", "+50", "+20" ,"No adjust", "-20", "-50", "0")
  
  hjustvalue <- c(300, 300, 60, 50, 50, 50, 90, 90, 125000, 15, 4)
  select.col <- c(9:19)
  xlab.names.em <- c("Mean amount of money apportion (NTD) PRO-PUR", 
                     "Mean amount of money apportion (NTD) PUR-NEU", 
                     "IRI EC Score", "IRI PD Score", "IRI PT Score", "IRI FS Score",
                     "IRI Score", "EQ Score", "Self-report Income (NTD)", "log Self-report Income (NTD)", "Emotion reaction", "Income - spend")
  ##
  
  all.pur.er.list <- list()
  
  iter = 1
  
  for(er.roi.ques.iter in sel.col){
    
    Y.lm <- summary(lm(final.tt.ERROI[final.tt.ERROI$Groups == levels(final.tt.ERROI$Groups)[1],][,er.roi.ques.iter] ~ final.tt.ERROI[final.tt.ERROI$Groups == levels(final.tt.ERROI$Groups)[1],]$mean_signal_value))
    O.lm <- summary(lm(final.tt.ERROI[final.tt.ERROI$Groups == levels(final.tt.ERROI$Groups)[2],][,er.roi.ques.iter] ~ final.tt.ERROI[final.tt.ERROI$Groups == levels(final.tt.ERROI$Groups)[2],]$mean_signal_value))
    All.lm <- summary(lm(final.tt.ERROI[,er.roi.ques.iter] ~ final.tt.ERROI$Groups*final.tt.ERROI$mean_signal_value))
    
    cor.test.pro.Y <- cor.test(final.tt.ERROI[final.tt.ERROI$Groups == levels(final.tt.ERROI$Groups)[1],][,er.roi.ques.iter], final.tt.ERROI[final.tt.ERROI$Groups == levels(final.tt.ERROI$Groups)[1],]$mean_signal_value)
    cor.test.pro.O <- cor.test(final.tt.ERROI[final.tt.ERROI$Groups == levels(final.tt.ERROI$Groups)[2],][,er.roi.ques.iter], final.tt.ERROI[final.tt.ERROI$Groups == levels(final.tt.ERROI$Groups)[2],]$mean_signal_value)
    cor.test.pro <- cor.test(final.tt.ERROI[,er.roi.ques.iter], final.tt.ERROI$mean_signal_value)
    
    all.pur.er.list[[iter]] <- ggscatter(final.tt.ERROI, 
                                           x = colnames(final.tt.ERROI)[er.roi.ques.iter], 
                                           y = "mean_signal_value", 
                                           color = "Groups",
                                           palette = c("#C6922C","#3A5BA0"),
                                           xlab = xlab.names.em[iter], ylab = "",
                                           size = 5) + 
      labs(color = "Groups") +
      geom_smooth(aes(color = Groups),method = lm, se = FALSE, size = 2) +
      geom_hline(yintercept=0, linetype="dashed", color = "black", size=1) +
      geom_segment(aes(x = 0, xend = 0, y = -10, yend = 5), linetype = "dashed", size = 1) +
      theme(plot.title = element_text(hjust = 0.5),
            title = element_text(size=30, face="bold"),
            legend.text = element_text(size=30),
            legend.title = element_text(size=30),
            axis.text = element_text(size=20),
            axis.title = element_text(size=30,face="bold"),
            text = element_text(size=30)) +
      annotate(geom="text", x=hjustvalue[iter], y=9, col=c("#C6922C"), 
               label=paste("Young: r =", 
                           round(cor.test.pro.Y$estimate, digit = 3), 
                           addstar(round(cor.test.pro.Y$p.value, digit = 3)),
                           ";£] = ",
                           round(Y.lm$coefficients[2,1], digit = 3),
                           "(",
                           round(Y.lm$coefficients[2,2], digit = 3),
                           ")"), 
               size = size.pro, hjust = 1) +
      annotate(geom="text", x=hjustvalue[iter], y=8, col=c("#3A5BA0"), 
               label=paste("Old: r =", 
                           round(cor.test.pro.O$estimate, digit = 3), 
                           addstar(round(cor.test.pro.O$p.value, digit = 3)),
                           ";£] = ",
                           round(O.lm$coefficients[2,1], digit = 3),
                           "(",
                           round(O.lm$coefficients[2,2], digit = 3),
                           ")"), 
               size = size.pro, hjust = 1) +
      annotate(geom="text", x=hjustvalue[iter], y=7, col="black", 
               label=paste("Interaction:",
                           " £] = ",
                           round(All.lm$coefficients[4,1], digit = 3),
                           "(",
                           round(All.lm$coefficients[4,2], digit = 3),
                           ")",
                           addstar(round(cor.test.pro$p.value, digit = 3))), 
               size = size.pro, hjust = 1)
    
    Y.beta.cor <- rbind(Y.beta.cor, c(all.iter.er.roi, colnames(final.tt.ERROI)[er.roi.ques.iter], 1, Y.lm$coefficients[2,1]))
    O.beta.cor <- rbind(O.beta.cor, c(all.iter.er.roi, colnames(final.tt.ERROI)[er.roi.ques.iter], 2, O.lm$coefficients[2,1]))
    # Y.beta.summary <- rbind(Y.beta.summary, c(all.iter.er.roi, colnames(final.tt.ERROI)[er.roi.ques.iter], 1, Y.lm$))
    # O.beta.summary <- rbind(O.beta.summary, c(all.iter.er.roi, colnames(final.tt.ERROI)[er.roi.ques.iter], 2, O.lm$coefficients[2,1]))
    # 
    
    iter <- iter + 1
  }
  
  FS.list[[all.iter.er.roi]] <- all.pur.er.list[[6]]
  
  all.pur.er.list.gg[[all.iter.er.roi]] <- ggarrange(all.pur.er.list[[1]],
                                                     all.pur.er.list[[2]],
                                                     all.pur.er.list[[3]], 
                                                     all.pur.er.list[[4]],
                                                     all.pur.er.list[[5]],
                                                     all.pur.er.list[[6]],
                                                     all.pur.er.list[[7]],
                                                     all.pur.er.list[[8]],
                                                     all.pur.er.list[[9]], 
                                                     all.pur.er.list[[10]],
                                                     all.pur.er.list[[11]],
                                                     all.pur.er.list[[12]],
                                                     nrow = 2, ncol = 6,
                                                     common.legend = TRUE, legend = "bottom", 
                                                     font.label = list(size= 40))
  
  all.pur.er.list.gg[[all.iter.er.roi]] <- annotate_figure(all.pur.er.list.gg[[all.iter.er.roi]],
                                                           left = text_grob("Parameter Estimate (a.u.)
                                                                            ", 
                                                                            color = "black", 
                                                                            face = "bold", 
                                                                            size = 50, rot = 90))
  
  jpeg(file = paste("factor_", fac.title[all.iter.er.roi], "_PRO_Question_ER_ROI.jpg"), width = 5000, height = 1300)
  print(all.pur.er.list.gg[[all.iter.er.roi]])
  dev.off()
}

##### beta plot 

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
                                    temp.beta.plot[[7]],
                                    temp.beta.plot[[8]],
                                    temp.beta.plot[[9]],
                                    temp.beta.plot[[10]],
                                    temp.beta.plot[[11]],
                                    temp.beta.plot[[12]],
                                    nrow = 2, ncol = 6,
                                    common.legend = TRUE, legend = "bottom", 
                                    font.label = list(size= 40))

jpeg(file = paste("PRO_Ques2ERROI_beta.jpg"), width = 2000, height = 800)
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

sum.beta.pro <- data.frame()
fac.rep <- rep(1:11, each = 2)
for (lm.iter in 1:length(sum.lm.beta)){
  sum.beta.pro <- rbind(sum.beta.pro, data.frame(sum.lm.beta[[lm.iter]]$coefficients[2,1], 
                                                 (lm.iter+1)%%2+1, 
                                                 levels(beta.corr$question_type)[fac.rep[lm.iter]]
  ))
}
colnames(sum.beta.pro) <- c("beta", "Groups", "type")
sum.beta.pro

jpeg(file = paste("ER_behavior_PRO_beta.jpeg"), width = 600, height = 600)
print(ggbarplot(sum.beta.pro, x = "Groups", y = "beta", fill = "Groups", facet.by = "type"))
dev.off()
