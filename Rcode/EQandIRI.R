setwd("C:/Users/acer/Desktop/PROS/NPT/")
EQreply <- read.csv("EQreply.csv", header = TRUE)

library(magrittr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(gtools)
library(gridExtra)
library(plyr)
library(MASS)
library(tidyr)
library(dplyr)

# EQ #### 

EQreply <- EQreply[,-c(1:4)]
ordered.EQreply <- EQreply[order(EQreply$ID),]

all <- c(1:60)
pos <- c(1, 6, 19, 22, 25, 26, 35, 36, 37, 38, 41, 42, 43, 44, 52, 54, 55, 57, 58, 59, 60)
neg <- c(4, 8, 10, 11, 12, 14, 15,18, 21, 27, 28, 29, 32, 34, 39, 46, 48, 49, 50)
res <- all[-c(sort(c(pos, neg)))]
  
for (i in pos){
  
  ordered.EQreply[i][ordered.EQreply[i] == -1] <- 0
  ordered.EQreply[i][ordered.EQreply[i] == -2] <- 0
}
for (i in neg){
  ordered.EQreply[i][ordered.EQreply[i] == 1] <- 0
  ordered.EQreply[i][ordered.EQreply[i] == 2] <- 0
  ordered.EQreply[i][ordered.EQreply[i] == -1] <- 1
  ordered.EQreply[i][ordered.EQreply[i] == -2] <- 2
}
for (i in res){
  ordered.EQreply[i] <- 0
}

ordered.EQreply$ID <- factor(ordered.EQreply$ID) 

EQdata_long <- gather(ordered.EQreply, item, score, X1:X60, factor_key=TRUE)
EQdata_long <- EQdata_long[order(data_long$ID),]
head(EQdata_long)

tapply(EQdata_long$score, EQdata_long$ID, sum)

# IRI #### 

IRIreply <- read.csv("IRI_reply.csv", header = TRUE)

FS <- c(5,12,16,26)
EC <- c(2,4,8,9,11,14,15,18,20,21,22,25,28)
PD <- c(6,10,13,17,20,24,27)
OT <- c(1,3,7,19,23)
Inv <- c(12,13,14,15,18)

items <- c(1:28)
items[FS] <- "FS"
items[EC] <- "EC"
items[PD] <- "PD"
items[OT] <- "OT"
items <- factor(items)

# invert score
for(i in Inv){
  for(j in 1:nrow(IRIreply)){
    if (IRIreply[,i][j] == 4) {
      IRIreply[,i][j] <- 0
    } else if(IRIreply[,i][j] == 3){
      IRIreply[,i][j] <- 1
    } else if(IRIreply[,i][j] == 1){
      IRIreply[,i][j] <- 3
    } else if(IRIreply[,i][j] == 0){
      IRIreply[,i][j] <- 4
    }
  }
}
IRIreply$ID <- factor(IRIreply$ID) 
IRIreply.long <- gather(IRIreply, item, score, X1:X28, factor_key=TRUE)
IRIreply.long <- IRIreply.long[order(IRIreply.long$ID),]
IRIreply.long <- cbind(IRIreply.long, factors = rep(items, nrow(IRIreply)))

IRIreply.df <- as.data.frame(tapply(IRIreply.long$score, list(IRIreply.long$ID, IRIreply.long$factors), sum))
IRIreply.df
