setwd("c:/Users/acer/Desktop/PROS/Data/fMRI_PilotData/ROI/")
ROI_try <- read.csv("ROI.csv", header = T)

young.num <- 16
old.num <- 9
total.numm <- young.num + old.num
cond.num <- 11
timec <- 12

age.tag <- c(rep("Young", young.num*cond.num*timec), rep("Old", old.num*cond.num*timec))

cond.tag <- c()
for(i in 1:cond.num){
  cond.tag <- c(cond.tag, rep(i, young.num*timec))
}
for(i in 1:cond.num){
  cond.tag <- c(cond.tag, rep(i, old.num*timec))
}

tc.tag <- c(rep(rep(0:11, each = young.num), cond.num), rep(rep(0:11, each = old.num), cond.num))

ROI_try <- cbind(ROI_try, age.tag, cond.tag, tc.tag)

pros.sit <- ROI_try[cond.tag == 1 & tc.tag == 5,]

head(pros.sit)

tapply(pros.sit$LH, pros.sit$age.tag, mean)
tapply(pros.sit$Amygdala, pros.sit$age.tag, mean)
