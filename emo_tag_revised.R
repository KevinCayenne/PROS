setwd("C:/Users/acer/Desktop/PROS/Data/fMRI_PilotData")

## money regulation normalise

library(ggplot2)
library(ggpubr)
library(gtools)
library(gridExtra)

rawdf <- read.csv("behavior.CSV")
T.num <- length(rawdf$SubjectN)/64
ncolrawdf <- ncol(rawdf)
rawdf[,ncolrawdf+1] <- rawdf$RegMtag
rawdf[,ncolrawdf+2] <- rawdf$RegM - rawdf$giveM
colnames(rawdf)[ncol(rawdf)-1] <- c("rev_reg_M")
colnames(rawdf)[ncol(rawdf)] <- c("rev_dist")

for (i in c(2,3,5,6)){
  rawdf$RegMtag[rawdf$RegMtag==i & rawdf$RegM==300] <- 1
  rawdf$RegMtag[rawdf$RegMtag==i & rawdf$RegM==0] <- 7
}
rawdf$RegMtag[rawdf$RegMtag==1 & rawdf$giveM==300] <- 4
rawdf$RegMtag[rawdf$RegMtag==7 & rawdf$giveM==0] <- 4

Yg <- c()
Og <- c()

K <- tapply(rawdf$EmoTag, list(rawdf$SITtag, rawdf$RegMtag, rawdf$SubjectN, rawdf$GroupN), mean)

for(i in c(1:T.num)){
  if(is.na(K[,,,1][,,i][1]) != TRUE){
    Yg <- c(Yg, K[,,,1][,,i])
  }
  if(is.na(K[,,,2][,,i][1]) != TRUE){
    Og <- c(Og, K[,,,2][,,i])
  }
}

#### All ggline emotional section ####

youngnum <- length(Yg)/28
oldnum <- length(Og)/28
all.emo.vector <- c(Yg, Og)

all.emo.group.tag <- as.factor(rep(c("Young","Old"),c((youngnum*28), (oldnum*28))))
all.emo.sit.tag <- as.factor(rep(c("PRO","PUR","NEU","UNC"), length(all.emo.vector)/4))
all.emo.tag <- as.factor(rep(rep(c("300", "+50", "+20", "same", "-20", "-50", "0"), c(4,4,4,4,4,4,4)), Subject.number))

levels(all.emo.sit.tag) <- list(PRO = "PRO", PUR = "PUR", NEU = "NEU", UNC = "UNC")
levels(all.emo.group.tag) <- list(Young = "Young", Old = "Old")
levels(all.emo.tag) <- list(none_give = "0", fifty_less = "-50", twenty_less = "-20", same = "same", twenty_more = "+20", fifty_more = "+50", all_give = "300")

all.emo.dataf <- data.frame(all.emo.vector, all.emo.group.tag, all.emo.sit.tag, all.emo.tag)
is.na(all.emo.vector)
all.emo.dataf[!is.na(all.emo.vector),]

ggline(all.emo.dataf, x = "all.emo.tag", y = "all.emo.vector", add = c("mean_se", "jitter"),
       color = "all.emo.group.tag", palette = "jco", facet.by = "all.emo.sit.tag") +
  labs(title = "Group difference in emotion choices by groups",
       x = "Money regulation type", y = "Emotional rate", colour = "Group") +
  theme(plot.title = element_text(hjust = 0.5, size= 15)) +
  stat_compare_means(aes(group = all.emo.group.tag), label = "p.signif",
                     label.y = 4.5) +
  geom_hline(yintercept = 0)

###
rawdf$rev_dist[rawdf$rev_dist==0]

rawdf[ncol(rawdf)+1] <- rawdf$rev_dist
colnames(rawdf)[ncol(rawdf)] <- c("rev_dist_tag")
#rawdf$rev_dist_tag[rawdf$rev_dist_tag==0] <- 0

iter <- -1
for(i in seq(0,-250,-50)){
  print(iter)
  print(rawdf$rev_dist_tag[rawdf$rev_dist_tag>=i-50 & rawdf$rev_dist_tag < i])
  rawdf$rev_dist_tag[rawdf$rev_dist_tag>=i-50 & rawdf$rev_dist_tag < i] <- iter
  iter <- iter - 1
}
iter <- 1
for(i in seq(0,250,50)){
  rawdf$rev_dist_tag[rawdf$rev_dist_tag>=i & rawdf$rev_dist_tag < i+50] <- iter
  iter <- iter + 1
}
rawdf$rev_dist_tag[rawdf$rev_dist==300] <- 7
rawdf$rev_dist_tag[rawdf$rev_dist==-300] <- -7
rawdf$rev_dist_tag[rawdf$rev_dist==0] <- 0

E <- tapply(rawdf$EmoTag, list(rawdf$SITtag, rawdf$rev_dist_tag, rawdf$SubjectN, rawdf$GroupN), mean)
YgE <- c()
OgE <- c()

for(i in c(1:T.num)){
  if(is.na(E[,,,1][,,i][1,6]) != TRUE){
    YgE <- c(YgE, E[,,,1][,,i])
  }
  if(is.na(E[,,,2][,,i][1,6]) != TRUE){
    OgE <- c(OgE, E[,,,2][,,i])
  }
}

T.E <- c(YgE, OgE)
length(T.E)

#hist(rawdf$rev_dist_tag)

all.emo.group.tag.R <- as.factor(rep(c("Young","Old"),c((youngnum*52), (oldnum*52))))
all.emo.sit.tag.R <- as.factor(rep(c("PRO","PUR","NEU","UNC"), length(T.E)/4))
all.emo.tag.R <- as.factor(rep(rep(c("-7", "-4", "-3", "-2", "-1", "0", "1", "2", "3", "4", "5", "6","7"), c(4,4,4,4,4,4,4,4,4,4,4,4,4)), T.num))

levels(all.emo.sit.tag.R) <- list(PRO = "PRO", PUR = "PUR", NEU = "NEU", UNC = "UNC")
levels(all.emo.group.tag.R) <- list(Young = "Young", Old = "Old")
levels(all.emo.tag.R) <- list("-7" = "-7", "-5" = "-5", "-4" = "-4", "-3" = "-3", "-2" = "-2", "-1" = "-1", "0" = "0", "1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7")

all.emo.dataf.R <- data.frame(T.E, all.emo.group.tag.R, all.emo.sit.tag.R, all.emo.tag.R)
all.emo.dataf.R[!is.na(T.E),]

ggline(all.emo.dataf.R, x = "all.emo.tag.R", y = "T.E", add = c("mean_se", "jitter"),
       color = "all.emo.group.tag.R", palette = "jco", facet.by = "all.emo.sit.tag.R") +
  labs(title = "Group difference in emotion choices by groups",
       x = "Money regulation type", y = "Emotional rate", colour = "Group") +
  theme(plot.title = element_text(hjust = 0.5, size= 15)) +
  stat_compare_means(aes(group = all.emo.group.tag.R), label = "p.signif",
                     label.y = 4.5) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)

