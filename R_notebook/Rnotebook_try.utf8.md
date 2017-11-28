---
title: "PROS project R Notebook by ZY"
output:
  pdf_document: default
  html_notebook: default
  html_document: default
---
<br/>

+ #### R initialization:

```r
library(knitr)
opts_knit$set(root.dir = "C:/Users/acer/Desktop/PROS/Data/fMRI_PilotData")

library(ggplot2)
library(ggpubr)
```

```
## Loading required package: magrittr
```

```r
library(gtools)
library(gridExtra)
```
+ #### Use `list.files()` function to list all file names into File.list variable:

```r
File.list <- mixedsort(list.files("behaviorD"))
```
+ #### Use `paste()` function to biuld the variables inside the path:

```r
combined = paste("./behaviorD/", File.list, sep="")
```
+ #### Count the length inside the file path (how many files in the directory):

```r
leng = length(combined)
```
+ #### There are 6 files for each subject, divided by six for the number of subjects:

```r
Subject.number =leng/6
```
+ #### Read the contents of the first file (you don't neet to read only the first file, but for simplicity, eliminating the time to define data.frame:


```r
merge.data <- read.csv(file = combined[ 1], header=T, sep=",")
```
+ #### Complete data reading:

```r
for (i in 2:leng){
  new.data = read.csv(file = combined[ i], header=T, sep=",")
  merge.data = rbind(merge.data,new.data)
}

behavior.df <- data.frame(merge.data)
```

---



+ #### Plot the Money giving ggplot:

```r
total.MD.plot <- ggplot() +
        
                 geom_bar(mapping = aes(x = x, y = ALL_Money, fill = Group),
                          stat = 'identity', position = 'dodge', color="black") +
                      
                 labs(title = title.name, x = "Conditions", y = "Unit: dollars") +
                 ylim(c(0,300)) +
                      
                 theme(plot.title = element_text(hjust = 0.5),
                       title = element_text(size=15),
                       legend.text = element_text(size=15),
                       legend.title = element_text(size=15),
                       axis.text = element_text(size=13),
                       axis.title = element_text(size=13,face="bold")) +
                      
                 geom_text(mapping = aes(x = x, y = ALL_Money, group = Group),
                           size = 4, colour = 'black', vjust = -0.5, hjust = .5,
                           label=format(ALL_Money, digits=4),
                           position = position_dodge(.9)) +
                      
                 geom_errorbar(aes(x = x, ymin = ALL_Money, ymax = ALL_Money + ALL_Money.se, group = Group), width= .3,
                              position = position_dodge(.9))
```

<div style="text-align: center">
<img src="/Users/acer/Desktop/PROS/Data/fMRI_PilotData/Average of money giving_pilot_ALL.png"/>
</div>

---


```r
Emo.mean.bySIT <- tapply(behavior.df$EmoTag, list(behavior.df$RegMtag, behavior.df$SITtag), mean)
moneyReg.type <- as.factor(rep(c("300", "+50", "+20", "same", "-20", "-50", "0"),4))
SIT.type <- as.factor(c(rep("prosocial",7),rep("purchase",7),rep("neutral",7),rep("Uncommon",7)))
levels(moneyReg.type) <- list(all_give = "300", fifty_more = "+50", twenty_more = "+20", same = "same", twenty_less = "-20", fifty_less = "-50", none_give = "0")
levels(SIT.type) <- list(prosocial = "prosocial", purchase = "purchase",neutral = "neutral", Uncommon = "Uncommon")

Emo.means <- c(Emo.mean.bySIT[1:28])
Emo.dataframe <- data.frame(Emo.means, SIT.type, moneyReg.type)
Emo.dataframe$moneyReg.type = factor(Emo.dataframe$moneyReg.type, levels = c('none_give','fifty_less','twenty_less','same','twenty_more','fifty_more','all_give'), order = T)
```

+ #### Plot the emotion decision ggplot:

```r
total.emo.plot <- ggplot(data = Emo.dataframe, aes(x = SIT.type, y = Emo.means)) +
        
                  geom_bar(aes(fill = moneyReg.type, group = moneyReg.type),
                           stat = 'identity', position = 'dodge', color="black") +
                        
                  labs(title = sprintf("Emotional degree_All (Old: %d, Young: %d)", oldnum, youngnum),
                       x = "Situations", y = "Mean emotion degree", fill = "money regulation type") +
                        
                  ylim(c(-4, 4)) +
                        
                  theme(plot.title = element_text(hjust = 0.5),
                        title = element_text(size=15),
                        legend.text = element_text(size=12),
                        legend.title = element_text(size=15),
                        axis.text = element_text(size=13),
                        axis.title = element_text(size=13,face="bold")) +
                        
                  geom_text(mapping = aes(x = SIT.type, y = Emo.means, label = "labs", 
                                          group = moneyReg.type),
                            size = 4, colour = 'black', vjust = -0.5, hjust = .5,
                            label=format(Emo.means, digits=2),
                            stat = 'identity',
                            position = position_dodge(width = 0.9))
```
<div style="text-align: center">
<img src="/Users/acer/Desktop/PROS/Data/fMRI_PilotData/Emotional degree_All.png"/>
</div>

---

+ #### Group emotion decision ggplot:

```r
Emo.mean.byGroup <- tapply(behavior.df$EmoTag, list(behavior.df$RegMtag, behavior.df$SITtag, behavior.df$GroupN), mean)
Emo.young.means <- c(Emo.mean.byGroup[1:28])
Emo.old.means <- c(Emo.mean.byGroup[29:56])
Emo.group.dataframe <- data.frame(Emo.young.means, Emo.old.means, SIT.type, moneyReg.type)
Emo.dataframe$moneyReg.type = factor(Emo.dataframe$moneyReg.type, levels = c('none_give','fifty_less','twenty_less','same','twenty_more','fifty_more','all_give'), order = T)

group.emo.y.plot <- ggplot(data = Emo.dataframe, aes(x = SIT.type, y = Emo.young.means)) +
  
                    geom_bar(aes(fill = moneyReg.type, group = moneyReg.type),
                             stat = 'identity', position = 'dodge', color="black") +
  
                    theme(plot.title = element_text(hjust = 0.5),
                          title = element_text(size=15),
                          legend.text = element_text(size=12),
                          legend.title = element_text(size=15),
                          axis.text = element_text(size=13),
                          axis.title = element_text(size=13,face="bold")) +
                    
                    geom_text(mapping = aes(x = SIT.type, y = Emo.young.means, label = "labs", group = moneyReg.type),
                              size = 4, colour = 'black', vjust = -0.5, hjust = .5,
                              label=format(Emo.young.means, digits=2),
                              stat = 'identity',
                              position = position_dodge(width = 0.9)) +
  
                    ylim(c(-4, 3))

group.emo.o.plot <- ggplot(data = Emo.dataframe, aes(x = SIT.type, y = Emo.old.means)) +
                    
                    geom_bar(aes(fill = moneyReg.type, group = moneyReg.type),
                             stat = 'identity', position = 'dodge', color="black") +
  
                    theme(plot.title = element_text(hjust = 0.5),
                          title = element_text(size=15),
                          legend.text = element_text(size=12),
                          legend.title = element_text(size=15),
                          axis.text = element_text(size=13),
                          axis.title = element_text(size=13,face="bold")) +
                    
                    geom_text(mapping = aes(x = SIT.type, y = Emo.old.means, label = "labs", group = moneyReg.type),
                              size = 4, colour = 'black', vjust = -0.5, hjust = .5,
                              label=format(Emo.old.means, digits=2),
                              stat = 'identity',
                              position = position_dodge(width = 0.9)) +
  
                    ylim(c(-4, 3))
```

+ #### Total group plot: 

```r
#### Total group Emo ploting ####
png(sprintf("Total_groupEmo_merge.png"), width = 1400, height = 700)
print(final_plot <- ggarrange(group.emo.y.plot, group.emo.o.plot,
                              ncol = 2, nrow = 1))
dev.off()
```

```
## png 
##   3
```

```r
##### Total MD and Emo merge ploting ####
png(sprintf("Total_merge.png"), width = 1200, height = 700)
print(final_plot <- ggarrange(total.MD.plot, total.emo.plot,
                              ncol = 2, nrow = 1))
dev.off()
```

```
## png 
##   3
```

<div style="text-align: center">
<img src="/Users/acer/Desktop/PROS/Data/fMRI_PilotData/Total_groupEmo_merge.png"/>
</div>

<div style="text-align: center">
<img src="/Users/acer/Desktop/PROS/Data/fMRI_PilotData/Total_merge.png"/>
</div>

---



```r
ALL_T_MD_Y_O 
```

```
##      T.PRO      T.PUR      T.NEU      T.UNC 
## 0.01009079 0.86425105 0.10978628 0.72950662
```

```r
ALL_t_test
```

```
##               T.PRO_PUR    T.PRO_NEU    T.PRO_UNC    T.PUR_NEU   T.PUR_UNC
## ALL_Young_T 0.666147428 0.0015168573 0.0168006096 3.126615e-07 0.000663901
## ALL_Old_T   0.005250345 0.0003946807 0.0003136868 1.030197e-03 0.004872752
##              T.UNC_NEU
## ALL_Young_T 0.06447282
## ALL_Old_T   0.99458725
```

---

+ #### Group MD ggline plot:


```r
#### ggline ####
png(sprintf("Mean money giving ggline by situations.png"), width = 600, height = 600)  
print(ggline(total.boxplot, 
             x = "total.boxplot.sit.vector", y = "total.boxplot.mean_money.vector", add= "mean_se",
      color = "total.boxplot.group.vector", palette = "jco") +
      labs(title = "Group difference in money giving for each situation", 
           x = "Situations", y = "Money (NT dollars)", colour = "Groups") +   
      stat_compare_means(aes(group = total.boxplot.group.vector), label = "p.signif", 
                         label.y = 170) +
      theme(plot.title = element_text(hjust = 0.5, size= 15)))
dev.off()
```

```
## pdf 
##   2
```
<div style="text-align: center">
<img src="/Users/acer/Desktop/PROS/Data/fMRI_PilotData/Mean money giving ggline by situations.png"/>
</div>

---

+ #### Group MD violin plot:

```r
v <- ggviolin(total.boxplot, x = "total.boxplot.sit.vector", y = "total.boxplot.mean_money.vector", 
              color = "total.boxplot.group.vector", palette = "jco",  width = 1.5) +
     labs(title = "Group difference in money giving for each situation", 
          x = "Situations", y = "Money (NT dollars)", colour = "Groups", fill = "Fill") +
     stat_compare_means(aes(group = total.boxplot.group.vector), label = "p.signif", 
                        label.y = 300) +
     theme(plot.title = element_text(hjust = 0.5, size= 15)) +
     ylim(0,300) 

png(sprintf("Mean money giving violin plot by situations.png"), width = 600, height = 600)
ggadd(v, add = c("mean_se", "dotplot"), fill = "total.boxplot.group.vector", position = position_dodge(0.8), binwidth = 6)
```

```
## Warning: position_dodge requires non-overlapping x intervals
```

```
## Warning: Removed 740 rows containing missing values (geom_violin).
```

```r
dev.off()
```

```
## pdf 
##   2
```
<div style="text-align: center">
<img src="/Users/acer/Desktop/PROS/Data/fMRI_PilotData/Mean money giving violin plot by situations.png"/>
</div>
---



```r
TT <- lm(total.boxplot.mean_money.vector ~ total.boxplot.sit.vector * total.boxplot.group.vector, data = total.boxplot)
summary(TT)
```

```
## 
## Call:
## lm(formula = total.boxplot.mean_money.vector ~ total.boxplot.sit.vector * 
##     total.boxplot.group.vector, data = total.boxplot)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -76.181 -18.264  -5.977  13.329 101.758 
## 
## Coefficients:
##                                                           Estimate
## (Intercept)                                                 61.367
## total.boxplot.sit.vectorPUR                                 -6.523
## total.boxplot.sit.vectorNEU                                -53.516
## total.boxplot.sit.vectorUNC                                -39.297
## total.boxplot.group.vectorOld                               72.313
## total.boxplot.sit.vectorPUR:total.boxplot.group.vectorOld  -70.629
## total.boxplot.sit.vectorNEU:total.boxplot.group.vectorOld  -61.970
## total.boxplot.sit.vectorUNC:total.boxplot.group.vectorOld  -76.120
##                                                           Std. Error
## (Intercept)                                                    8.560
## total.boxplot.sit.vectorPUR                                   12.106
## total.boxplot.sit.vectorNEU                                   12.106
## total.boxplot.sit.vectorUNC                                   12.106
## total.boxplot.group.vectorOld                                 14.267
## total.boxplot.sit.vectorPUR:total.boxplot.group.vectorOld     20.177
## total.boxplot.sit.vectorNEU:total.boxplot.group.vectorOld     20.177
## total.boxplot.sit.vectorUNC:total.boxplot.group.vectorOld     20.177
##                                                           t value Pr(>|t|)
## (Intercept)                                                 7.169 1.85e-10
## total.boxplot.sit.vectorPUR                                -0.539 0.591291
## total.boxplot.sit.vectorNEU                                -4.421 2.69e-05
## total.boxplot.sit.vectorUNC                                -3.246 0.001633
## total.boxplot.group.vectorOld                               5.068 2.06e-06
## total.boxplot.sit.vectorPUR:total.boxplot.group.vectorOld  -3.500 0.000718
## total.boxplot.sit.vectorNEU:total.boxplot.group.vectorOld  -3.071 0.002802
## total.boxplot.sit.vectorUNC:total.boxplot.group.vectorOld  -3.773 0.000286
##                                                              
## (Intercept)                                               ***
## total.boxplot.sit.vectorPUR                                  
## total.boxplot.sit.vectorNEU                               ***
## total.boxplot.sit.vectorUNC                               ** 
## total.boxplot.group.vectorOld                             ***
## total.boxplot.sit.vectorPUR:total.boxplot.group.vectorOld ***
## total.boxplot.sit.vectorNEU:total.boxplot.group.vectorOld ** 
## total.boxplot.sit.vectorUNC:total.boxplot.group.vectorOld ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 34.24 on 92 degrees of freedom
## Multiple R-squared:  0.5288,	Adjusted R-squared:  0.4929 
## F-statistic: 14.75 on 7 and 92 DF,  p-value: 9.376e-13
```

---

+ #### Group ED ggline plot:

```r
#### All ggline emotional section ####
all.emo.vector <- as.vector(na.omit(as.vector(tapply(behavior.df$EmoTag, list(behavior.df$SITtag, behavior.df$RegMtag, behavior.df$SubjectN, behavior.df$GroupN), mean))))
all.emo.group.tag <- as.factor(rep(c("Young","Old"),c((youngnum*28), (oldnum*28))))
all.emo.sit.tag <- as.factor(rep(c("PRO","PUR","NEU","UNC"), length(all.emo.vector)/4))
all.emo.tag <- as.factor(rep(rep(c("300", "+50", "+20", "same", "-20", "-50", "0"), c(4,4,4,4,4,4,4)), Subject.number))

levels(all.emo.sit.tag) <- list(PRO = "PRO", PUR = "PUR", NEU = "NEU", UNC = "UNC")
levels(all.emo.group.tag) <- list(Young = "Young", Old = "Old")
levels(all.emo.tag) <- list(none_give = "0", fifty_less = "-50", twenty_less = "-20", same = "same", twenty_more = "+20", fifty_more = "+50", all_give = "300")

all.emo.dataf <- data.frame(all.emo.vector, all.emo.group.tag, all.emo.sit.tag, all.emo.tag)
```


```r
png(sprintf("Mean emotion decision plot by situations.png"), width = 1200, height = 1000)
print(ggline(all.emo.dataf, x = "all.emo.tag", y = "all.emo.vector", add = c("mean_se", "jitter"),
          color = "all.emo.group.tag", palette = "jco", facet.by = "all.emo.sit.tag") +
          labs(title = "Group difference in emotion choices by groups", 
               x = "Money regulation type", y = "Emotional rate", colour = "Group") +   
          theme(plot.title = element_text(hjust = 0.5, size= 15)) +
          stat_compare_means(aes(group = all.emo.group.tag), label = "p.signif", 
                             label.y = 4.5))
dev.off()
```

```
## pdf 
##   2
```
<div style="text-align: center">
<img src="/Users/acer/Desktop/PROS/Data/fMRI_PilotData/Mean emotion decision plot by situations.png"/>
</div>


```r
emlm <- lm(all.emo.vector ~ all.emo.group.tag * all.emo.sit.tag * all.emo.tag , data = all.emo.dataf)
summary(emlm)
```

```
## 
## Call:
## lm(formula = all.emo.vector ~ all.emo.group.tag * all.emo.sit.tag * 
##     all.emo.tag, data = all.emo.dataf)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.5556 -0.7778 -0.1562  0.5938  4.0556 
## 
## Coefficients:
##                                                                  Estimate
## (Intercept)                                                    -5.625e-01
## all.emo.group.tagOld                                           -1.042e-01
## all.emo.sit.tagPUR                                              1.281e+00
## all.emo.sit.tagNEU                                              1.156e+00
## all.emo.sit.tagUNC                                              5.625e-01
## all.emo.tagfifty_less                                           6.875e-01
## all.emo.tagtwenty_less                                          7.500e-01
## all.emo.tagsame                                                 1.094e+00
## all.emo.tagtwenty_more                                          8.438e-01
## all.emo.tagfifty_more                                           7.615e-15
## all.emo.tagall_give                                            -1.313e+00
## all.emo.group.tagOld:all.emo.sit.tagPUR                        -6.701e-01
## all.emo.group.tagOld:all.emo.sit.tagNEU                         1.215e-01
## all.emo.group.tagOld:all.emo.sit.tagUNC                         1.326e+00
## all.emo.group.tagOld:all.emo.tagfifty_less                      5.903e-01
## all.emo.group.tagOld:all.emo.tagtwenty_less                    -1.389e-01
## all.emo.group.tagOld:all.emo.tagsame                            6.562e-01
## all.emo.group.tagOld:all.emo.tagtwenty_more                     9.340e-01
## all.emo.group.tagOld:all.emo.tagfifty_more                      1.167e+00
## all.emo.group.tagOld:all.emo.tagall_give                        9.236e-01
## all.emo.sit.tagPUR:all.emo.tagfifty_less                       -6.250e-02
## all.emo.sit.tagNEU:all.emo.tagfifty_less                       -7.500e-01
## all.emo.sit.tagUNC:all.emo.tagfifty_less                        1.875e-01
## all.emo.sit.tagPUR:all.emo.tagtwenty_less                      -5.313e-01
## all.emo.sit.tagNEU:all.emo.tagtwenty_less                      -9.375e-01
## all.emo.sit.tagUNC:all.emo.tagtwenty_less                       1.875e-01
## all.emo.sit.tagPUR:all.emo.tagsame                             -1.156e+00
## all.emo.sit.tagNEU:all.emo.tagsame                             -9.375e-01
## all.emo.sit.tagUNC:all.emo.tagsame                             -3.281e-01
## all.emo.sit.tagPUR:all.emo.tagtwenty_more                      -2.406e+00
## all.emo.sit.tagNEU:all.emo.tagtwenty_more                      -2.406e+00
## all.emo.sit.tagUNC:all.emo.tagtwenty_more                      -1.281e+00
## all.emo.sit.tagPUR:all.emo.tagfifty_more                       -2.688e+00
## all.emo.sit.tagNEU:all.emo.tagfifty_more                       -2.656e+00
## all.emo.sit.tagUNC:all.emo.tagfifty_more                       -1.969e+00
## all.emo.sit.tagPUR:all.emo.tagall_give                         -2.719e+00
## all.emo.sit.tagNEU:all.emo.tagall_give                         -1.875e+00
## all.emo.sit.tagUNC:all.emo.tagall_give                         -1.281e+00
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagfifty_less  -8.819e-01
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagfifty_less   8.333e-02
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagfifty_less  -9.097e-01
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagtwenty_less  4.757e-01
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagtwenty_less  9.931e-01
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagtwenty_less -4.653e-01
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagsame         4.062e-01
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagsame        -3.125e-01
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagsame        -1.505e+00
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagtwenty_more  2.396e-01
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagtwenty_more -1.493e-01
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagtwenty_more -2.385e+00
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagfifty_more   1.021e+00
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagfifty_more  -3.993e-01
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagfifty_more  -1.476e+00
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagall_give     6.632e-01
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagall_give     9.722e-02
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagall_give    -8.299e-01
##                                                                Std. Error
## (Intercept)                                                     3.048e-01
## all.emo.group.tagOld                                            5.080e-01
## all.emo.sit.tagPUR                                              4.311e-01
## all.emo.sit.tagNEU                                              4.311e-01
## all.emo.sit.tagUNC                                              4.311e-01
## all.emo.tagfifty_less                                           4.311e-01
## all.emo.tagtwenty_less                                          4.311e-01
## all.emo.tagsame                                                 4.311e-01
## all.emo.tagtwenty_more                                          4.311e-01
## all.emo.tagfifty_more                                           4.311e-01
## all.emo.tagall_give                                             4.311e-01
## all.emo.group.tagOld:all.emo.sit.tagPUR                         7.185e-01
## all.emo.group.tagOld:all.emo.sit.tagNEU                         7.185e-01
## all.emo.group.tagOld:all.emo.sit.tagUNC                         7.185e-01
## all.emo.group.tagOld:all.emo.tagfifty_less                      7.185e-01
## all.emo.group.tagOld:all.emo.tagtwenty_less                     7.185e-01
## all.emo.group.tagOld:all.emo.tagsame                            7.185e-01
## all.emo.group.tagOld:all.emo.tagtwenty_more                     7.185e-01
## all.emo.group.tagOld:all.emo.tagfifty_more                      7.185e-01
## all.emo.group.tagOld:all.emo.tagall_give                        7.185e-01
## all.emo.sit.tagPUR:all.emo.tagfifty_less                        6.096e-01
## all.emo.sit.tagNEU:all.emo.tagfifty_less                        6.096e-01
## all.emo.sit.tagUNC:all.emo.tagfifty_less                        6.096e-01
## all.emo.sit.tagPUR:all.emo.tagtwenty_less                       6.096e-01
## all.emo.sit.tagNEU:all.emo.tagtwenty_less                       6.096e-01
## all.emo.sit.tagUNC:all.emo.tagtwenty_less                       6.096e-01
## all.emo.sit.tagPUR:all.emo.tagsame                              6.096e-01
## all.emo.sit.tagNEU:all.emo.tagsame                              6.096e-01
## all.emo.sit.tagUNC:all.emo.tagsame                              6.096e-01
## all.emo.sit.tagPUR:all.emo.tagtwenty_more                       6.096e-01
## all.emo.sit.tagNEU:all.emo.tagtwenty_more                       6.096e-01
## all.emo.sit.tagUNC:all.emo.tagtwenty_more                       6.096e-01
## all.emo.sit.tagPUR:all.emo.tagfifty_more                        6.096e-01
## all.emo.sit.tagNEU:all.emo.tagfifty_more                        6.096e-01
## all.emo.sit.tagUNC:all.emo.tagfifty_more                        6.096e-01
## all.emo.sit.tagPUR:all.emo.tagall_give                          6.096e-01
## all.emo.sit.tagNEU:all.emo.tagall_give                          6.096e-01
## all.emo.sit.tagUNC:all.emo.tagall_give                          6.096e-01
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagfifty_less   1.016e+00
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagfifty_less   1.016e+00
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagfifty_less   1.016e+00
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagtwenty_less  1.016e+00
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagtwenty_less  1.016e+00
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagtwenty_less  1.016e+00
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagsame         1.016e+00
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagsame         1.016e+00
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagsame         1.016e+00
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagtwenty_more  1.016e+00
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagtwenty_more  1.016e+00
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagtwenty_more  1.016e+00
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagfifty_more   1.016e+00
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagfifty_more   1.016e+00
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagfifty_more   1.016e+00
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagall_give     1.016e+00
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagall_give     1.016e+00
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagall_give     1.016e+00
##                                                                t value
## (Intercept)                                                     -1.845
## all.emo.group.tagOld                                            -0.205
## all.emo.sit.tagPUR                                               2.972
## all.emo.sit.tagNEU                                               2.682
## all.emo.sit.tagUNC                                               1.305
## all.emo.tagfifty_less                                            1.595
## all.emo.tagtwenty_less                                           1.740
## all.emo.tagsame                                                  2.537
## all.emo.tagtwenty_more                                           1.957
## all.emo.tagfifty_more                                            0.000
## all.emo.tagall_give                                             -3.045
## all.emo.group.tagOld:all.emo.sit.tagPUR                         -0.933
## all.emo.group.tagOld:all.emo.sit.tagNEU                          0.169
## all.emo.group.tagOld:all.emo.sit.tagUNC                          1.846
## all.emo.group.tagOld:all.emo.tagfifty_less                       0.822
## all.emo.group.tagOld:all.emo.tagtwenty_less                     -0.193
## all.emo.group.tagOld:all.emo.tagsame                             0.913
## all.emo.group.tagOld:all.emo.tagtwenty_more                      1.300
## all.emo.group.tagOld:all.emo.tagfifty_more                       1.624
## all.emo.group.tagOld:all.emo.tagall_give                         1.286
## all.emo.sit.tagPUR:all.emo.tagfifty_less                        -0.103
## all.emo.sit.tagNEU:all.emo.tagfifty_less                        -1.230
## all.emo.sit.tagUNC:all.emo.tagfifty_less                         0.308
## all.emo.sit.tagPUR:all.emo.tagtwenty_less                       -0.871
## all.emo.sit.tagNEU:all.emo.tagtwenty_less                       -1.538
## all.emo.sit.tagUNC:all.emo.tagtwenty_less                        0.308
## all.emo.sit.tagPUR:all.emo.tagsame                              -1.897
## all.emo.sit.tagNEU:all.emo.tagsame                              -1.538
## all.emo.sit.tagUNC:all.emo.tagsame                              -0.538
## all.emo.sit.tagPUR:all.emo.tagtwenty_more                       -3.947
## all.emo.sit.tagNEU:all.emo.tagtwenty_more                       -3.947
## all.emo.sit.tagUNC:all.emo.tagtwenty_more                       -2.102
## all.emo.sit.tagPUR:all.emo.tagfifty_more                        -4.408
## all.emo.sit.tagNEU:all.emo.tagfifty_more                        -4.357
## all.emo.sit.tagUNC:all.emo.tagfifty_more                        -3.229
## all.emo.sit.tagPUR:all.emo.tagall_give                          -4.460
## all.emo.sit.tagNEU:all.emo.tagall_give                          -3.076
## all.emo.sit.tagUNC:all.emo.tagall_give                          -2.102
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagfifty_less   -0.868
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagfifty_less    0.082
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagfifty_less   -0.895
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagtwenty_less   0.468
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagtwenty_less   0.977
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagtwenty_less  -0.458
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagsame          0.400
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagsame         -0.308
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagsame         -1.481
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagtwenty_more   0.236
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagtwenty_more  -0.147
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagtwenty_more  -2.348
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagfifty_more    1.005
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagfifty_more   -0.393
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagfifty_more   -1.452
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagall_give      0.653
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagall_give      0.096
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagall_give     -0.817
##                                                                Pr(>|t|)
## (Intercept)                                                     0.06545
## all.emo.group.tagOld                                            0.83761
## all.emo.sit.tagPUR                                              0.00307
## all.emo.sit.tagNEU                                              0.00750
## all.emo.sit.tagUNC                                              0.19241
## all.emo.tagfifty_less                                           0.11125
## all.emo.tagtwenty_less                                          0.08237
## all.emo.tagsame                                                 0.01141
## all.emo.tagtwenty_more                                          0.05075
## all.emo.tagfifty_more                                           1.00000
## all.emo.tagall_give                                             0.00242
## all.emo.group.tagOld:all.emo.sit.tagPUR                         0.35131
## all.emo.group.tagOld:all.emo.sit.tagNEU                         0.86573
## all.emo.group.tagOld:all.emo.sit.tagUNC                         0.06533
## all.emo.group.tagOld:all.emo.tagfifty_less                      0.41163
## all.emo.group.tagOld:all.emo.tagtwenty_less                     0.84678
## all.emo.group.tagOld:all.emo.tagsame                            0.36138
## all.emo.group.tagOld:all.emo.tagtwenty_more                     0.19406
## all.emo.group.tagOld:all.emo.tagfifty_more                      0.10491
## all.emo.group.tagOld:all.emo.tagall_give                        0.19908
## all.emo.sit.tagPUR:all.emo.tagfifty_less                        0.91838
## all.emo.sit.tagNEU:all.emo.tagfifty_less                        0.21906
## all.emo.sit.tagUNC:all.emo.tagfifty_less                        0.75852
## all.emo.sit.tagPUR:all.emo.tagtwenty_less                       0.38386
## all.emo.sit.tagNEU:all.emo.tagtwenty_less                       0.12460
## all.emo.sit.tagUNC:all.emo.tagtwenty_less                       0.75852
## all.emo.sit.tagPUR:all.emo.tagsame                              0.05833
## all.emo.sit.tagNEU:all.emo.tagsame                              0.12460
## all.emo.sit.tagUNC:all.emo.tagsame                              0.59061
## all.emo.sit.tagPUR:all.emo.tagtwenty_more                      8.79e-05
## all.emo.sit.tagNEU:all.emo.tagtwenty_more                      8.79e-05
## all.emo.sit.tagUNC:all.emo.tagtwenty_more                       0.03597
## all.emo.sit.tagPUR:all.emo.tagfifty_more                       1.22e-05
## all.emo.sit.tagNEU:all.emo.tagfifty_more                       1.53e-05
## all.emo.sit.tagUNC:all.emo.tagfifty_more                        0.00130
## all.emo.sit.tagPUR:all.emo.tagall_give                         9.69e-06
## all.emo.sit.tagNEU:all.emo.tagall_give                          0.00219
## all.emo.sit.tagUNC:all.emo.tagall_give                          0.03597
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagfifty_less   0.38572
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagfifty_less   0.93466
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagfifty_less   0.37095
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagtwenty_less  0.63982
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagtwenty_less  0.32877
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagtwenty_less  0.64717
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagsame         0.68942
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagsame         0.75852
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagsame         0.13899
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagtwenty_more  0.81367
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagtwenty_more  0.88322
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagtwenty_more  0.01919
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagfifty_more   0.31543
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagfifty_more   0.69446
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagfifty_more   0.14689
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagall_give     0.51418
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagall_give     0.92380
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagall_give     0.41439
##                                                                   
## (Intercept)                                                    .  
## all.emo.group.tagOld                                              
## all.emo.sit.tagPUR                                             ** 
## all.emo.sit.tagNEU                                             ** 
## all.emo.sit.tagUNC                                                
## all.emo.tagfifty_less                                             
## all.emo.tagtwenty_less                                         .  
## all.emo.tagsame                                                *  
## all.emo.tagtwenty_more                                         .  
## all.emo.tagfifty_more                                             
## all.emo.tagall_give                                            ** 
## all.emo.group.tagOld:all.emo.sit.tagPUR                           
## all.emo.group.tagOld:all.emo.sit.tagNEU                           
## all.emo.group.tagOld:all.emo.sit.tagUNC                        .  
## all.emo.group.tagOld:all.emo.tagfifty_less                        
## all.emo.group.tagOld:all.emo.tagtwenty_less                       
## all.emo.group.tagOld:all.emo.tagsame                              
## all.emo.group.tagOld:all.emo.tagtwenty_more                       
## all.emo.group.tagOld:all.emo.tagfifty_more                        
## all.emo.group.tagOld:all.emo.tagall_give                          
## all.emo.sit.tagPUR:all.emo.tagfifty_less                          
## all.emo.sit.tagNEU:all.emo.tagfifty_less                          
## all.emo.sit.tagUNC:all.emo.tagfifty_less                          
## all.emo.sit.tagPUR:all.emo.tagtwenty_less                         
## all.emo.sit.tagNEU:all.emo.tagtwenty_less                         
## all.emo.sit.tagUNC:all.emo.tagtwenty_less                         
## all.emo.sit.tagPUR:all.emo.tagsame                             .  
## all.emo.sit.tagNEU:all.emo.tagsame                                
## all.emo.sit.tagUNC:all.emo.tagsame                                
## all.emo.sit.tagPUR:all.emo.tagtwenty_more                      ***
## all.emo.sit.tagNEU:all.emo.tagtwenty_more                      ***
## all.emo.sit.tagUNC:all.emo.tagtwenty_more                      *  
## all.emo.sit.tagPUR:all.emo.tagfifty_more                       ***
## all.emo.sit.tagNEU:all.emo.tagfifty_more                       ***
## all.emo.sit.tagUNC:all.emo.tagfifty_more                       ** 
## all.emo.sit.tagPUR:all.emo.tagall_give                         ***
## all.emo.sit.tagNEU:all.emo.tagall_give                         ** 
## all.emo.sit.tagUNC:all.emo.tagall_give                         *  
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagfifty_less     
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagfifty_less     
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagfifty_less     
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagtwenty_less    
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagtwenty_less    
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagtwenty_less    
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagsame           
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagsame           
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagsame           
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagtwenty_more    
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagtwenty_more    
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagtwenty_more *  
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagfifty_more     
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagfifty_more     
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagfifty_more     
## all.emo.group.tagOld:all.emo.sit.tagPUR:all.emo.tagall_give       
## all.emo.group.tagOld:all.emo.sit.tagNEU:all.emo.tagall_give       
## all.emo.group.tagOld:all.emo.sit.tagUNC:all.emo.tagall_give       
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.219 on 644 degrees of freedom
## Multiple R-squared:  0.5286,	Adjusted R-squared:  0.4884 
## F-statistic: 13.13 on 55 and 644 DF,  p-value: < 2.2e-16
```



