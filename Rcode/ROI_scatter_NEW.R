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

setwd("c:/Users/acer/Desktop/PROS/Data/fMRI_PilotData/ROI/")
ROI_try <- read.csv("ROI_PFC.csv", header = T)
ROI_try <- as.data.frame(ROI_try, header = T)

ROI_try <- ROI_try[,1:11]

setwd("c:/Users/acer/Desktop/PROS/Data/fMRI_PilotData/")
behavior.df <- read.csv("behavior.CSV", header = T)

setwd("c:/Users/acer/Desktop/")
young.num <- 26
old.num <- 20

analysis.col <- c(1:ncol(ROI_try))
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

## start scatter plots
for (j in analysis.col){
  
    tydi.ROI <- tydi.ROI.pre[tydi.ROI.pre$ROI_try==levels(tydi.ROI.pre$ROI_try)[j],]
    
    corrmergelist <- list()
    corrmerge.total <- data.frame()
    
    ## set variables
    P.title <- c("(12, 47, -10)", "(-15, 29, -7)", "(-3, 32, -1)", "(12, 41, 8)",
                 "(18, 47, 17)", "(-15, 41, 20)", "(-6, 56, 20)", "(-9, 50, 26)",
                 "(12, 53, 29)", "(-21, -7, 41)","(-12, 17, 53)")
    size.pro <- 6
    hjustvalue <- 300
    y.color <- "#C6922C"
    o.color <- "#3A5BA0"
    size.pro <- 10
    hjustvalue <- c(400, 60, 50, 90, 125000, 15)
    select.col <- c(9:14)
    xlab.names <- c("Mean Money Given (NTD)", "IRI EC Score", "IRI PD Score", "EQ Score", "Self-report Income (NTD)", "log Self-report Income (NTD)")
    ##
    
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
    
    sub.mean.df <- aggregate(tydi.ROI$value, list(tydi.ROI$sub.tag, tydi.ROI$cond.tag, tydi.ROI$phase.tag, tydi.ROI$age.tag), mean)
    colnames(sub.mean.df) <- c("sub.tag", "sit", "phase", "Groups", "signalvalue")
    
    sub.mean.df <- sub.mean.df[sub.mean.df$sit==1 & sub.mean.df$phase=="Money_Decision",]
    
    Mgive.df <- aggregate(behavior.df$giveM, list(behavior.df$SubjectN, behavior.df$SITtag, behavior.df$GroupN), mean)
    Mgive.df <- Mgive.df[Mgive.df$Group.2==1,]
    colnames(Mgive.df) <- c("sub.id", "situation", "age.tag", "mgive")
    tmp.Mgive <- Mgive.df
    
    corrmerge <- cbind(sub.mean.df, tmp.Mgive)
    
    new.corrmerge <- corrmerge[corrmerge$sub.id %in% EQ.df$ID,]
    new.corrmerge <- new.corrmerge[order(new.corrmerge$sub.id),]
    new.corrmerge <- cbind(new.corrmerge, IRI_EC = EQ.df$IRI_EC, IRI_PD = EQ.df$IRI_PD, EQ = EQ.df$EQ, Income = EQ.df$mean_gain, logIncome = log(EQ.df$mean_gain)) 
    
    pros.scatter <- list()
    
    itera = 1
    for (i in select.col){
      Y.lm <- summary(lm(new.corrmerge[new.corrmerge$age.tag == 1,][,i] ~ new.corrmerge[new.corrmerge$age.tag == 1,]$signalvalue))
      O.lm <- summary(lm(new.corrmerge[new.corrmerge$age.tag == 2,][,i] ~ new.corrmerge[new.corrmerge$age.tag == 2,]$signalvalue))
      All.lm <- summary(lm(new.corrmerge[,i] ~ new.corrmerge$signalvalue))
      
      cor.test.pro.Y <- cor.test(new.corrmerge[new.corrmerge$age.tag == 1,][,i], new.corrmerge[new.corrmerge$age.tag == 1,]$signalvalue)
      cor.test.pro.O <- cor.test(new.corrmerge[new.corrmerge$age.tag == 2,][,i], new.corrmerge[new.corrmerge$age.tag == 2,]$signalvalue)
      cor.test.pro <- cor.test(new.corrmerge[,i], new.corrmerge$signalvalue)
      
      pros.scatter[[itera]] <- ggscatter(new.corrmerge, x = colnames(new.corrmerge)[i], y = "signalvalue", 
                                         color = "Groups",
                                         palette = c("#C6922C","#3A5BA0"),
                                         xlab = xlab.names[itera], ylab = "Parameter Estimate",
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
    
    temp.K <- ggarrange(pros.scatter[[1]],
                        pros.scatter[[2]],
                        pros.scatter[[3]],
                        pros.scatter[[4]],
                        pros.scatter[[5]],
                        pros.scatter[[6]],
                        nrow = 2, ncol = 3, 
                        labels = c("A", "B", "C", "D", "E", "F"),
                        common.legend = TRUE, legend = "bottom", 
                        font.label = list(size= 40))
    
    temp.P <- annotate_figure(temp.K,
                    top = text_grob(P.title[j], 
                                    color = "black", 
                                    face = "bold", 
                                    size = 50))

    jpeg(file = paste(P.title[j], ".jpg"), width = 2500, height = 1500)
    print(temp.P)
    dev.off()

}

## start barplot 

tydi.ROI.pre.MD <- tydi.ROI.pre[tydi.ROI.pre$cond.tag %in% c(1:4),]

gg.MD.title <- c("PRO", "PUR", "NEU", "UNC")
gg.MD.signal <- list()

for (MDnum in 1:4){
    
  tydi.ROI.pre.pros <- tydi.ROI.pre[tydi.ROI.pre$cond.tag == MDnum,]
  
  aggre.ROI.pros <- ddply(tydi.ROI.pre.pros, 
                          c("age.tag", "ROI_try"),
                          summarise,
                          N = length(value),
                          mean = mean(value),
                          sd = sd(value),
                          se = sd / sqrt(N)
                          )
  
  aggre.ROI.MD <- ddply(tydi.ROI.pre.MD, 
                          c("age.tag", "ROI_try", "cond.tag"),
                          summarise,
                          N = length(value),
                          mean = mean(value),
                          sd = sd(value),
                          se = sd / sqrt(N)
  )

 gg.MD.signal[[MDnum]] <- ggplot(aggre.ROI.pros, aes(x = ROI_try, 
                             y = mean, 
                             fill = age.tag)) + 
    geom_bar(stat="identity", position = position_dodge2(preserve = "single")) +
    theme(panel.background = element_rect(fill = "white", colour = "black")) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = age.tag), 
                  width = 0.5, 
                  position= position_dodge(width=0.9)) +
    scale_color_manual("Groups", values = c("Young" = y.color, "Old" = o.color)) +
    scale_fill_manual("Groups", values = c("Young" = y.color, "Old" = o.color)) +
    
    labs(title = gg.MD.title[MDnum], x = "", y = "",colour = "Groups") +   
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
    ylim(c(-0.9, 1.2)) +
    geom_hline(yintercept = 0, color = "black", size = 1) +     
    geom_smooth(data = aggre.ROI.pros, aes(x = as.numeric(ROI_try), y=mean), color = "black", size = 1.3, se = FALSE, method = "lm") +
    geom_smooth(data = aggre.ROI.pros, aes(x = as.numeric(ROI_try), y=mean), color = "black", size = 1.3, se = FALSE, linetype="dashed", method = "lm", formula = y ~ poly(x,2))

}

temp.signal.MD <- ggarrange(gg.MD.signal[[1]],
                            gg.MD.signal[[2]],
                            gg.MD.signal[[3]],
                            gg.MD.signal[[4]],
                            nrow = 1, ncol = 4,
                            common.legend = TRUE, legend = "bottom", 
                            font.label = list(size= 40))

temp.signal.MD.F <- annotate_figure(temp.signal.MD,
                                    top = text_grob("", 
                                                    color = "black", 
                                                    face = "bold", 
                                                    size = 30),
                                    bottom = text_grob("ROIs", 
                                                       color = "black", size = 30),
                                    left = text_grob("Parameter Estimate", color = "black", size = 30, rot = 90)
                                    )

jpeg(file = paste("PE_ROI.jpg"), width = 1500, height = 800)
print(temp.signal.MD.F)
dev.off()

## 
# ggbarplot(tydi.ROI.pre.MD,
#           x = "age.tag",
#           y = "value",
#           color = "age.tag",
#           palette = c("#C6922C","#3A5BA0"),
#           xlab = "Situation", ylab = "Parameter Estimate",
#           size = 1,
#           facet.by = c("ROI_try"),
#           position = position_dodge(0.9),
#           add = c("mean_se"),
#           add.params = list(group = "cond.tag")
#           )

levels(aggre.ROI.MD$cond.tag) <- list("PRO" = 1, "PUR" = 2, "NEU" = 3, "UNC" = 4,
                                      "+300" = 5, "+50" = 6, "+20" = 7, "Same" = 8, 
                                      "-20" = 9, "-50" = 10, "0" = 11)

aggre.ROI.MD$ROI_try <- as.factor(aggre.ROI.MD$ROI_try)

# 
# ggplot(aggre.ROI.MD, aes(x = age.tag, 
#                          y = mean,
#                          fill = age.tag)) + 
#   geom_bar(stat="identity", position = position_dodge2(.9)) +
#   facet_grid(~ cond.tag) +
#   geom_smooth(aes(x = age.tag, y = mean, group = cond.tag),
#               position = position_dodge2(.9), stat = "smooth",
#               color = "black", size = 1.3, se = FALSE, method = "lm") +
#   
#   scale_color_manual("Groups", values = c("Young" = y.color, "Old" = o.color)) +
#   scale_fill_manual("Groups", values = c("Young" = y.color, "Old" = o.color)) +
#   theme(panel.background = element_rect(fill = "white", colour = "black")) +
#   geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = age.tag), 
#                 width = 0.9, 
#                 position = position_dodge2(0.9, padding = 0.8)) +
#   
#   labs(x = "Situations", y = "Parameter Estimate", colour = "Groups") +   
#   theme(plot.title = element_text(hjust = 0.5, face="bold"),
#         title = element_text(size=30),
#         legend.text = element_text(size=30),
#         legend.title = element_text(size=30),
#         axis.text = element_text(size=30),
#         axis.title = element_text(size=30,face="bold"),
#         strip.text.x = element_text(size=20, face="bold"),
#         strip.background = element_rect(colour="black", fill="white")
#   ) +
#   geom_hline(yintercept = 0, color = "black", size = 1) 
# 
#               position = position_dodge2(1.9), color = "black", 
#               size = 1.3, se = FALSE, linetype="dashed", method = "lm", 
#               formula = y ~ poly(x,2))

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
              
              labs(x = "ROIs", y = "Parameter Estimate", colour = "Groups") +   
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

gg.PE.ROI.cond <- ggplot(aggre.ROI.MD, aes(x = cond.tag, 
                         y = mean,
                         fill = age.tag)) + 
  geom_bar(stat="identity", position = position_dodge2(.9)) +
  facet_grid(~ ROI_try) +
  
  scale_color_manual("Groups", values = c("Young" = y.color, "Old" = o.color)) +
  scale_fill_manual("Groups", values = c("Young" = y.color, "Old" = o.color)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = age.tag), 
                width = 0.9, 
                position = position_dodge2(0.9, padding = 0.8)) +
  
  labs(x = "ROIs", y = "Parameter Estimate", colour = "Groups") +   
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        title = element_text(size=30),
        legend.text = element_text(size=30),
        legend.title = element_text(size=30),
        axis.text = element_text(size=10),
        axis.title = element_text(size=30,face="bold"),
        strip.text.x = element_text(size=20, face="bold"),
        strip.background = element_rect(colour="black", fill="white")
  ) +
  geom_hline(yintercept = 0, color = "black", size = 1) 

jpeg(file = paste("gg_PE_ROI_cond.jpg"), width = 1500, height = 800)
print(gg.PE.ROI.cond)
dev.off()
