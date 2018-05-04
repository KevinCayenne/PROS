setwd("c:/Users/acer/Desktop/PROS/Data/fMRI_PilotSubject_Info/")

library(magrittr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(gtools)
library(gridExtra)
library(plyr)

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

sub.give <- aggregate(behavior.df$giveM, list(ID =behavior.df$SubjectN, sit = behavior.df$SITtag), mean)

sub.give.id.pro <- c()
for(i in 1:length(nona.subinfo$ID)){
  sub.give.id.pro <- rbind(sub.give.id.pro, sub.give[sub.give$ID==nona.subinfo$ID[i],][1,])
}

nona.subinfo$`Group_(Y:1_O:2)` <- factor(nona.subinfo$`Group_(Y:1_O:2)`)
levels(nona.subinfo$`Group_(Y:1_O:2)`) <- list(Young = "1", Old = "2")

total.sub.give <- cbind(sub.give.id.pro, spend = nona.subinfo$mean_spend, gain = nona.subinfo$mean_gain, group = nona.subinfo$`Group_(Y:1_O:2)`)

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
