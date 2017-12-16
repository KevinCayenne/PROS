setwd("c:/Users/acer/Desktop/PROS/Data/fMRI_PilotSubject_Info/")

library(magrittr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(gtools)
library(gridExtra)
library(plyr)

subject.info <- read_excel("Subjects_info.xlsx", sheet = 1)

subject.info <- subject.info[-c(1,10,13),]

tapply(subject.info$Age, list(subject.info$`Gender (M:1, F:2)`, subject.info$`Group (Y:1, O:2)`), mean)
tapply(subject.info$Age, subject.info$`Group (Y:1, O:2)`, mean)
tapply(subject.info$Age, subject.info$`Group (Y:1, O:2)`, sd)

tapply(subject.info$`Gender (M:1, F:2)`, subject.info$`Group (Y:1, O:2)`, sum)
tapply(subject.info$`Group (Y:1, O:2)`, subject.info$`Gender (M:1, F:2)`, sum)

table(subject.info$`Gender (M:1, F:2)`)
table(subject.info$`Gender (M:1, F:2)`, subject.info$`Group (Y:1, O:2)`)