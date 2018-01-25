setwd("c:/Users/acer/Desktop/PROS/Data/fMRI_PilotData/ROI/")
library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)
library(gtools)
library(gridExtra)

ROI_try <- read.csv("ROI.csv", header = T)
ROI_W <- read.csv("ROIW.csv", header = T)

young.num <- 20
old.num <- 9
total.num <- young.num + old.num
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
tydi.ROI <- gather(ROI_try, ROI, value, -age.tag, -cond.tag, -tc.tag)

ROI_W <- cbind(ROI_W, age.tag, cond.tag, tc.tag)
W.ROI <- gather(ROI_W, ROI, value, -age.tag, -cond.tag, -tc.tag)

for (t in 0:2){
  for (i in 1:4){
    
    pros.sit <- ROI_try[cond.tag == i & tc.tag == t,]
    
    # plot(pros.sit$LH ~ pros.sit$age.tag)
    # plot(pros.sit$Amygdala ~ pros.sit$age.tag)
    # plot(pros.sit$IF ~ pros.sit$age.tag)
    
    if (pros.sit$cond.tag[1] == 1){
      pros.m <- total.boxplot[1:total.num,]
    } else if (pros.sit$cond.tag[1] == 2){
      pros.m <- total.boxplot[(total.num+1):(2*total.num),]
    } else if (pros.sit$cond.tag[1] == 3){
      pros.m <- total.boxplot[(2*total.num+1):(3*total.num),]
    } else if (pros.sit$cond.tag[1] == 4){
      pros.m <- total.boxplot[(3*total.num+1):(4*total.num),]
    }
    
    pros.sit <- cbind(pros.sit, pros.m$total.boxplot.mean_money.vector)
    names(pros.sit)[9] <- "tmoney"
    
    png(sprintf("LH_in_cond_%s_timebin_%s.png", i, t), width = 700, height = 500)
    print(ggscatter(pros.sit, x = "tmoney", y = "LH", 
              group = "age.tag", 
              color = "age.tag",
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "mean_money", ylab = "LH",
              title = sprintf("LH_%s_timebin_%s", i, t),
              facet.by = "age.tag") 
    )
    dev.off()
    
    png(sprintf("RH_in_cond_%s_timebin_%s.png", i, t), width = 700, height = 500)
    print(ggscatter(pros.sit, x = "tmoney", y = "RH", 
                    group = "age.tag", 
                    color = "age.tag",
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "mean_money", ylab = "RH",
                    title = sprintf("RH_%s_timebin_%s", i, t),
                    facet.by = "age.tag") 
    )
    dev.off()
    
    png(sprintf("insulaR_in_cond_%s_timebin_%s.png", i, t), width = 700, height = 500)
    print(ggscatter(pros.sit, x = "tmoney", y = "insulaL", 
              group = "age.tag", 
              color = "age.tag",
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "mean_money", ylab = "insulaR",
              title = sprintf("insulaR_%s_timebin_%s", i, t),
              facet.by = "age.tag")
    )
    dev.off()
    
    png(sprintf("insulaR_in_cond_%s_timebin_%s.png", i, t), width = 700, height = 500)
    print(ggscatter(pros.sit, x = "tmoney", y = "insulaR", 
                    group = "age.tag", 
                    color = "age.tag",
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "mean_money", ylab = "insulaR",
                    title = sprintf("InsulaR_%s_timebin_%s", i, t),
                    facet.by = "age.tag")
    )
    dev.off()
    
    png(sprintf("VMPFC_in_cond_%s_timebin_%s.png", i, t), width = 700, height = 500)
    print(ggscatter(pros.sit, x = "tmoney", y = "VMPFC", 
                    group = "age.tag", 
                    color = "age.tag",
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "mean_money", ylab = "VMPFC",
                    title = sprintf("VMPFC_%s_timebin_%s", i, t),
                    facet.by = "age.tag")
    )
    dev.off()
    
    # ggplot(pros.sit, aes(x = pros.m$total.boxplot.mean_money.vector, y = LH, group = age.tag, color = age.tag)) +
    #   geom_point() +
    #   geom_smooth(method = "lm")
    # 
    # ggplot(pros.sit, aes(x = pros.m$total.boxplot.mean_money.vector, y = Amygdala, group = age.tag, color = age.tag)) +
    #   geom_point() +
    #   geom_smooth(method = "lm")
    # 
    # ggplot(pros.sit, aes(x = pros.m$total.boxplot.mean_money.vector, y = IF, group = age.tag, color = age.tag)) +
    #   geom_point() +
    #   geom_smooth(method = "lm")
    
  }
}

# mean.cond <- tapply(ROI_try$VMPFC, list(ROI_try$tc.tag, ROI_try$cond.tag, ROI_try$age.tag), mean)
# old.vmpfc <- as.data.frame(mean.cond[,1:2, 1])
# young.vmpfc <- as.data.frame(mean.cond[,1:2, 2])
# 
# old.vmpfc <- gather(old.vmpfc, sit, value)
# old.vmpfc <- cbind(rep(1:12, 2), old.vmpfc)
# names(old.vmpfc)[1] <- "tc"
# 
# young.vmpfc <- gather(young.vmpfc, sit, value)
# young.vmpfc <- cbind(rep(1:12, 2), young.vmpfc)
# names(young.vmpfc)[1] <- "tc"

for(ROIR in c("motor1", "Avent", "motor2", "V")){
  for(i in 2:3){

    data.tydi <- as.data.frame(tydi.ROI[tydi.ROI$ROI == ROIR & (tydi.ROI$cond.tag == 1 | tydi.ROI$cond.tag == i) ,])
    
    titl <- as.character(data.tydi$ROI[1])
    sitnam <- i
    
    cols <- c("tc.tag", "cond.tag", "age.tag", "ROI")
    data.tydi[cols] <- lapply(data.tydi[cols], factor)
    
    png(sprintf(sprintf("%s_%s.png", titl, sitnam)), width = 1700, height = 800)
      print(ggline(data.tydi, x = "tc.tag", y = "value", add = "mean_se",
            color = "cond.tag", palette = "jco", facet.by = "age.tag") +
            labs(title = sprintf("%s_%s", titl, sitnam)) +
            theme(plot.title = element_text(hjust = 0.5, size= 15))
      )
    dev.off()
  }
}

iter <- 1
for(ROIR in c("motor1", "Avent", "motor2", "V", "Rin", "Lin",	"RPH", "VMPFC")){

    data.tydi <- as.data.frame(W.ROI[W.ROI$ROI == ROIR & (W.ROI$cond.tag == 1) ,])
    data.tydi.2 <- as.data.frame(W.ROI[W.ROI$ROI == ROIR & (W.ROI$cond.tag == 2) ,])
    data.tydi$value <- data.tydi$value - data.tydi.2$value
    
    titl <- as.character(data.tydi$ROI[1])
    tit <- c("-30,-4,50", "-6,-14,50", "-39,8,29", "21,-88,-1", "42,-10,-4", "42,-7,-4", "18,-31,-13", "0,29,-4")
    cols <- c("tc.tag", "cond.tag", "age.tag", "ROI")
    data.tydi[cols] <- lapply(data.tydi[cols], factor)
    levels(data.tydi$age.tag) <- list(Young = "Young", Old = "Old")
    
    png(sprintf(sprintf("%s.png", titl)), width = 800, height = 600)
    print(ggline(data.tydi, x = "tc.tag", y = "value", add = c("mean_se"),
                 color = "age.tag", palette = "jco", size = 1) +
            labs(title = sprintf("%s", tit[iter])) +
            theme(plot.title = element_text(hjust = 0.5, size= 25),
                  legend.text = element_text(size=25),
                  legend.title = element_text(size=25),
                  axis.text = element_text(size=25),
                  axis.title = element_text(size=25,face="bold"),
                  text = element_text(size=25)
                  )+
            labs(x = "Time(TR)", y = "BOLD Signal", 
                 colour = "Situtaion", fill = "Group") +
            geom_smooth(method = 'loess')+
            stat_compare_means(aes(group = age.tag), label = "p.signif", label.y = 3)
    )
    dev.off()
    iter <- iter + 1
}

ROINum <- 5
Pur.Tvalue <- list()
Pur.Tvalue[[1]] <- 0
Neu.Tvalue <- list()
Neu.Tvalue[[1]] <- 0
Pur.p.matrix <- matrix(NA, 11, 5)
Neu.p.matrix <- matrix(NA, 11, 5)

k <- 1
for(ROIR in c("RH", "insulaR", "VMPFC", "insulaL", "LH")){

  for(i in 2:3){
  
    data.tydi <- as.data.frame(tydi.ROI[tydi.ROI$ROI == ROIR & (tydi.ROI$cond.tag == 1 | tydi.ROI$cond.tag == i) ,])
    
    data.tydi.splite <- split(data.tydi, list(data.tydi$cond.tag, data.tydi$age.tag, data.tydi$tc.tag))
    
    counti <- 1
    
    if (i == 2){
      for (t in seq(3,length(data.tydi.splite)-2,4)){
        
        yp.t <- data.tydi.splite[[t]]$value - data.tydi.splite[[t+1]]$value
        op.t <- data.tydi.splite[[t+2]]$value - data.tydi.splite[[t+3]]$value
        
        Pur.Tvalue[[counti]] <- t.test(yp.t, op.t)
        counti <- counti + 1
      }
    } else if(i == 3){
      for (t in seq(3,length(data.tydi.splite)-2,4)){
        
        yn.t <- data.tydi.splite[[t]]$value - data.tydi.splite[[t+1]]$value
        on.t <- data.tydi.splite[[t+2]]$value - data.tydi.splite[[t+3]]$value
        mean(yn.t)
        sem<-sd(yn.t)/sqrt(length(yn.t))
        
        Neu.Tvalue[[counti]] <- t.test(yn.t, on.t)
        counti <- counti + 1
      }
    }
  }
    for(m in 1:11){
      Pur.p.matrix[m,k] <- Pur.Tvalue[[m]]$p.value
      Neu.p.matrix[m,k] <- Neu.Tvalue[[m]]$p.value
    }
    k <- k + 1
}

Pur.p.matrix
Neu.p.matrix
