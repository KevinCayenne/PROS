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
ROI_try <- read.csv("ER_diff_roi_final_OPRO_YPRO.csv", header = T)
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
              (-24, -46, -10)")

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
