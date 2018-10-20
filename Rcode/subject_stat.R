setwd("c:/Users/acer/Desktop/PROS/Data/fMRI_PilotSubject_Info/")

library(magrittr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(gtools)
library(gridExtra)
library(plyr)
library(MASS)

subject.info <- read_excel("Subjects_info.xlsx", sheet = 1)

subject.info <- subject.info[-c(1,10,13,30,35,37:39),]

tapply(subject.info$testAge, list(subject.info$`Gender_(M:1_ F:2)`, subject.info$`Group_(Y:1_O:2)`), mean)
tapply(subject.info$testAge, list(subject.info$`Gender_(M:1_ F:2)`, subject.info$`Group_(Y:1_O:2)`), sd)
tapply(subject.info$testAge, subject.info$`Group_(Y:1_O:2)`, mean)
tapply(subject.info$testAge, subject.info$`Group_(Y:1_O:2)`, sd)

tapply(subject.info$`Gender_(M:1_ F:2)`, subject.info$`Group_(Y:1_O:2)`, sum)
tapply(subject.info$`Group_(Y:1_O:2)`, subject.info$`Gender_(M:1_ F:2)`, sum)

table(subject.info$`Gender_(M:1_ F:2)`)
table(subject.info$`Gender_(M:1_ F:2)`, subject.info$`Group_(Y:1_O:2)`)

nona.subinfo <- subject.info[is.na(subject.info$mean_gain) == FALSE,]

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
sub.give <- aggregate(behavior.df$giveM, list(ID =behavior.df$SubjectN, sit = behavior.df$SITtag), mean)
write.csv(sub.give[sub.give$sit==1,], file = sprintf("prosocial.CSV"))

sub.give.id.pro <- c()
for(i in 1:length(nona.subinfo$ID)){
  sub.give.id.pro <- rbind(sub.give.id.pro, sub.give[sub.give$ID==nona.subinfo$ID[i],][1,])
}

nona.subinfo$`Group_(Y:1_O:2)` <- factor(nona.subinfo$`Group_(Y:1_O:2)`)
levels(nona.subinfo$`Group_(Y:1_O:2)`) <- list(Young = "1", Old = "2")

EQ.df <- as.data.frame(nona.subinfo[!is.na(nona.subinfo$EQ),])

total.sub.give <- cbind(sub.give.id.pro, spend = nona.subinfo$mean_spend, gain = nona.subinfo$mean_gain, group = nona.subinfo$`Group_(Y:1_O:2)`, tage = nona.subinfo$testAge)

ggscatter(total.sub.give, x = "gain", y = "x", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Gains", ylab = "Gives",
          title = "Correlation of gives and gains",
          group = "group"
)

ggscatter(total.sub.give, x = "gain", y = "x", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Gains", ylab = "Gives",
          title = "Correlation of give and gains",
          group = "group",
          color = "group",
          facet.by = "group"
)

ggscatter(total.sub.give, x = "tage", y = "gain", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Ages", ylab = "Gains",
          title = "Correlation of ages and gains",
          group = "group",
          color = "group",
          facet.by = "group"
)

sub.Y <- subject.info[subject.info$`Group_(Y:1_O:2)`==1,]
sub.Y.ID <- c(na.omit(sub.Y$ID))
sub.O <- subject.info[subject.info$`Group_(Y:1_O:2)`==2,]
sub.O.ID <- c(na.omit(sub.O$ID))

ordered.corrmerge <- corrmergelist[[1]][order(corrmergelist[[1]]$id),]
nrow(brainsig.moneygive)


##### boxcox transform

try1 <- boxcox(total.sub.give$gain ~ total.sub.give$x)
lambda <- try1$x[which.max(try1$y)]

powerTransform <- function(y, lambda1, lambda2 = NULL, method = "boxcox") {
  
  boxcoxTrans <- function(x, lam1, lam2 = NULL) {
    # if we set lambda2 to zero, it becomes the one parameter transformation
    lam2 <- ifelse(is.null(lam2), 0, lam2)
    
    if (lam1 == 0L) {
      log(y + lam2)
    } else {
      (((y + lam2)^lam1) - 1) / lam1
    }
  }
  
  switch(method
         , boxcox = boxcoxTrans(y, lambda1, lambda2)
         , tukey = y^lambda1
  )
}

# re-run with transformation
m <- lm(total.sub.give$gain ~ total.sub.give$x)
summary(m)
mnew <- lm(powerTransform(total.sub.give$gain, lambda) ~ total.sub.give$x)
summary(mnew)

# QQ-plot
op <- par(pty = "s", mfrow = c(1, 2))
qqnorm(m$residuals); qqline(m$residuals)
qqnorm(mnew$residuals); qqline(mnew$residuals)
par(op)

mnew$model <- cbind(mnew$model, tgroup = total.sub.give$group)
mnew$model <- cbind(mnew$model, age = total.sub.give$tage)

anova(lm(total.sub.give$x ~ tgroup*powerTransform(total.sub.give$gain, lambda), mnew$model))
m.model <- mnew$model

ggscatter(m.model, x= "powerTransform(total.sub.give$gain, lambda)", y = "total.sub.give$x",
          conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean Money Gained (power transformed)", ylab = "Mean Money Given (NTD)",
          title = "Correlation of given and gains",
          ylim = c(0,300), color = "tgroup", size = 5
          ) + geom_smooth(method = "lm", color = "black") +
          theme(plot.title = element_text(hjust = 0.5),
                title = element_text(size=30, face="bold"),
                legend.text = element_text(size=30),
                legend.title = element_text(size=30),
                axis.text = element_text(size=20),
                axis.title = element_text(size=30,face="bold"),
                text = element_text(size=30)
          ) + labs(colour = "Groups") +
          scale_color_manual(values = c("#0075C9","#E5BF21"))
            
ggscatter(mnew$model, x= "powerTransform(total.sub.give$gain, lambda)", y = "total.sub.give$x",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Gains (power transformed)", ylab = "Gives",
          title = "Correlation of gives and gains (boxcox transformed)",
          color = "tgroup",
          group = "tgroup"
          )

ggscatter(mnew$model, x= "age", y = "powerTransform(total.sub.give$gain, lambda)",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Ages", ylab = "Gains (power transformed)",
          title = "Correlation of ages and gains (boxcox transformed)"
)

Ymnew <- mnew$model[mnew$model$tgroup=="Young",]
names(Ymnew) <- c("ptlambda", "gives", "groups", "tgroup")

ggscatter(Ymnew, x= "ptlambda", y = "gives",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Gains (power transformed)", ylab = "Gives",
          title = "Correlation of gives and gains (boxcox transformed)",
          ylim = c(0:200)
)

Omnew <- mnew$model[mnew$model$tgroup=="Old",]
names(Omnew) <- c("ptlambda", "gives", "groups", "tgroup")

ggscatter(Omnew, x= "ptlambda", y = "gives",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Gains (power transformed)", ylab = "Gives",
          title = "Correlation of gives and gains (boxcox transformed)",
          ylim = c(0:300)
)

#### EQ ####

try2 <- boxcox(EQ.df$mean_gain ~ EQ.df$EQ)
EQlambda <- try2$x[which.max(try1$y)]

# re-run with transformation
EQm <- lm(EQ.df$mean_gain ~ EQ.df$EQ)
summary(EQm)
EQmnew <- lm(powerTransform(EQ.df$mean_gain, EQlambda) ~ EQ.df$EQ)
summary(EQmnew)

# QQ-plot
Eop <- par(pty = "s", mfrow = c(1, 2))
qqnorm(EQm$residuals); qqline(EQm$residuals)
qqnorm(EQmnew$residuals); qqline(EQmnew$residuals)
par(op)

ggscatter(EQ.df, x = "mean_gain", y = "EQ",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Gains (original)", ylab = "EQ score",
          title = "Correlation of EQ and gains"
          )

EQmnew$model <- cbind(EQmnew$model, group = EQ.df$`Group`, neugive = EQ.df$NEUgives)
  
ggscatter(EQmnew$model, x = "powerTransform(EQ.df$mean_gain, EQlambda)", y = "EQ.df$EQ",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Gains (power transformed)", ylab = "EQ score",
          title = "Correlation of EQ and gains (boxcox transformed)",
          group = "group",
          color = "group"
)

ggscatter(EQmnew$model, x = "pt_gain", y = "neugive",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Gains (power transformed)", ylab = "Neu given",
          title = "Correlation of neu given and gains (boxcox transformed)",
          group = "group",
          color = "group",
          facet.by = "group"
)

YEQmnew <- EQmnew$model[EQmnew$model$group=="Young",]
names(YEQmnew) <- c("pt", "EQ", "group")
ggscatter(YEQmnew, x = "pt", y = "EQ",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Gains (power transformed)", ylab = "EQ score",
          title = "Correlation of EQ and gains (boxcox transformed)"
)

OEQmnew <- EQmnew$model[EQmnew$model$group=="Old",]
names(OEQmnew) <- c("pt", "EQ", "group")
ggscatter(OEQmnew, x = "pt", y = "EQ",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Gains (power transformed)", ylab = "EQ score",
          title = "Correlation of EQ and gains (boxcox transformed)"
)

ggscatter(EQmnew$model, x = "powerTransform(EQ.df$mean_gain, EQlambda)", y = "EQ.df$EQ",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Gains (power transformed)", ylab = "EQ score",
          title = "Correlation of EQ and gains (boxcox transformed)",
          facet.by = "EQ.df$`Group_(Y:1_O:2)`"
)

#### IRI ####
for(i in 26:29){
  IRI.boxcox <- boxcox(EQ.df$mean_gain ~ EQ.df[,i])
  IRI_lambda <- IRI.boxcox$x[which.max(IRI.boxcox$y)]
  
  # re-run with transformation
  IRIm <- lm(EQ.df$mean_gain ~ EQ.df[,i])
  summary(IRIm)
  IRImnew <- lm(powerTransform(EQ.df$mean_gain, IRI_lambda) ~ EQ.df[,i])
  summary(IRImnew)

  #ggscatter
  print(ggscatter(EQ.df, x = "mean_gain", y = names(EQ.df)[i],
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Gains (original)", ylab = "IRI score",
            title = paste("Correlation of IRI and gains",names(EQ.df)[i])
        )
  )
  
  print(ggscatter(EQ.df, x = "gives", y = names(EQ.df)[i],
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "pearson",
                  xlab = "Gives", ylab = "IRI score",
                  title = paste("Correlation of IRI and gives",names(EQ.df)[i])
  )
  )
  
  print(ggscatter(IRImnew$model, x = "powerTransform(EQ.df$mean_gain, IRI_lambda)", y = "EQ.df[, i]",
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Gains (power transformed)", ylab = "IRI score",
            title = paste("Correlation of IRI and gains (boxcox transformed)",names(EQ.df)[i])
  )
  )
}

#### EQ and Gives
ggscatter(EQ.df, x = "EQ", y = "gives",
          conf.int = TRUE, cor.method = "pearson",
          cor.coef = TRUE,
          xlab = "EQ score", ylab = "Mean Money Given (NTD)", color = "Group",
          size = 5,
          title = paste("Correlation of given and EQ score")
) + theme(plot.title = element_text(hjust = 0.5),
          title = element_text(size=30, face="bold"),
          legend.text = element_text(size=30),
          legend.title = element_text(size=30),
          axis.text = element_text(size=20),
          axis.title = element_text(size=30,face="bold"),
          text = element_text(size=30)) +
  labs(colour = "Groups") +
  scale_color_manual(values = c("#0075C9","#E5BF21")) + 
  geom_smooth(method = "lm", color = "black")

ggscatter(EQ.df, x = "testAge", y = "EQ",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Ages", ylab = "EQ score",
          title = paste("Correlation of ages and EQ")
)

ggscatter(EQ.df, x = "gives", y = "IRI_EC",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Gives", ylab = "IRI_EC score",
          title = paste("Correlation of gives and IRI_EC "),
          facet.by = "EQ.df$`Group_(Y:1_O:2)`"
)

ggscatter(EQ.df, x = "testAge", y = "IRI_EC",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "age", ylab = "IRI_EC score",
          title = paste("Correlation of ages and IRI_EC ")
)

#### gglines ####

colnames(EQ.df)[3] <- "Group"

ggline(EQ.df, x = "Group", y = "IRI_EC",
       add = c("mean_se", "jitter"),
       color = "Group", palette = "jco") +
      stat_compare_means(aes(group = Group), label = "p.signif")

ggline(EQ.df, x = "Group", y = "EQ",
       add = c("mean_se", "jitter"),
       color = "Group", palette = "jco") +
  stat_compare_means(aes(group = Group), label = "p.signif")

names(EQmnew$model)[1] <- "pt_gain"
ggline(EQmnew$model, x = "group", y = "pt_gain",
       add = c("mean_se", "jitter"),
       color = "group", palette = "jco") +
  stat_compare_means(aes(group = group), label = "p.signif")


#### gain and ages ####

gain.boxcox <- boxcox(EQ.df$mean_gain ~ EQ.df$testAge)
gain_lambda <- gain.boxcox$x[which.max(gain.boxcox$y)]

# re-run with transformation
gain.m <- lm(EQ.df$mean_gain ~ EQ.df$testAge)
summary(gain.m)
gain.mnew <- lm(powerTransform(EQ.df$mean_gain, gain_lambda) ~ EQ.df$testAge)
summary(gain.mnew)

ggscatter(EQ.df, x = "testAge", y = "mean_gain",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Ages", ylab = "Gains",
          title = paste("Correlation of ages and gains")
)

ggscatter(gain.mnew$model, x = "EQ.df$testAge", y = "powerTransform(EQ.df$mean_gain, gain_lambda)",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "ages", ylab = "gains",
          title = paste("Correlation of gains and ages")
)

###### Age gives
try.subject.info <- subject.info[-c(42,43),]
ggscatter(try.subject.info, x = "testAge", y = "gives",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Ages", ylab = "gives",
          title = paste("Correlation of ages and gives ")
)

ggscatter(try.subject.info, x = "testAge", y = "mean_gain",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Ages", ylab = "gives",
          title = paste("Correlation of ages and gives ")
)

EQ.df$pro_neu.give <- EQ.df$gives - EQ.df$NEUgives

ggscatter(EQ.df, x = "IRI_EC", y = "pro_neu.give", conf.int = TRUE, 
          cor.method = "pearson",
          add.params = list(group = "Group"),
          cor.coef = TRUE,
          color = "Group",
          xlab = "IRI Empathy Concern Score", ylab = "Mean Money Given (NTD)",size = 5,
          title = paste("Correlation of given and IRI-EC")
)+ theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size=30, face="bold"),
        legend.text = element_text(size=30),
        legend.title = element_text(size=30),
        axis.text = element_text(size=20),
        axis.title = element_text(size=30,face="bold"),
        text = element_text(size=30)) +
  labs(colour = "Groups") +
  scale_color_manual(values = c("#0075C9","#E5BF21")) + 
  geom_smooth(method = "lm", color = "black")


