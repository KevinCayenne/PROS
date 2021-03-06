setwd("c:/Users/acer/Desktop")

library(Matrix)
library(ggplot2)
library(ggpubr)
library(gtools)
library(gridExtra)
library(lme4)
library(tidyr)
library(dplyr)
library(data.table)
library(reshape2)
library(plyr)
library(corrplot)
library(PerformanceAnalytics)



try  <- read.csv("NPTSummary_AllSubj_20180712.csv", header = TRUE)
available.try <- try[try$Available == 1,]
final.try <- available.try[,c(1,3,4,6,22:105,165:174)]

#final.try2 <- transform(final.try2, as.numeric)
final.try2 <- final.try[,-1]
rownames(final.try2) <- final.try[,1]
final.try2 <- sapply(final.try2, as.numeric)

final.old <- final.try2[final.try2$age > 30,]
final.young <- final.try2[final.try2$age < 30,]

final.old <- sapply(final.old, as.numeric)
final.young <- sapply(final.young, as.numeric)

chart.Correlation(final.old,
                  method="pearson",
                  histogram=TRUE,
                  pch=25)

res1 <- cor.mtest(final.old, conf.level = .95)
res2 <- cor.mtest(final.young, conf.level = .95)

corrplot.mixed(cor(final.old), p.mat = res1$p, sig.level = .05, insig = "blank")
corrplot.mixed(cor(final.young), p.mat = res2$p, sig.level = .05, insig = "blank")

a <- cor(final.try2)
melted_cormat <- melt(a)
head(melted_cormat)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(melted_cormat)
upper_tri

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


