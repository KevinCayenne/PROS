
select.col <- c(10:15)
corr.names <- c("IRI EC Score", "IRI PD Score", "IRI Score", "EQ Score", "Self-report Income (NTD)", "log Self-report Income (NTD)")
MD.cor.scatter <- list()
ttt <- 1

for (i in select.col){
  Y.lm <- summary(lm(new.corrmerge[new.corrmerge$age.tag == 1,][,i] ~ new.corrmerge[new.corrmerge$age.tag == 1,]$mgive))
  O.lm <- summary(lm(new.corrmerge[new.corrmerge$age.tag == 2,][,i] ~ new.corrmerge[new.corrmerge$age.tag == 2,]$mgive))
  All.lm <- summary(lm(new.corrmerge[,i] ~ new.corrmerge$signalvalue))
  
  cor.test.pro.Y <- cor.test(new.corrmerge[new.corrmerge$age.tag == 1,][,i], new.corrmerge[new.corrmerge$age.tag == 1,]$mgive)
  cor.test.pro.O <- cor.test(new.corrmerge[new.corrmerge$age.tag == 2,][,i], new.corrmerge[new.corrmerge$age.tag == 2,]$mgive)
  cor.test.pro <- cor.test(new.corrmerge[,i], new.corrmerge$mgive)
  
  MD.cor.scatter[[ttt]] <- ggscatter(new.corrmerge, x = colnames(new.corrmerge)[i], y = "mgive", 
                                     color = "Groups",
                                     palette = c("#C6922C","#3A5BA0"),
                                     xlab = corr.names[ttt], ylab = "",
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
    annotate(geom="text", x=hjustvalue[1+ttt], y=300, col=c("#C6922C"), 
             label=paste("Young: r =", 
                         round(cor.test.pro.Y$estimate, digit = 3), 
                         addstar(round(cor.test.pro.Y$p.value, digit = 3)),
                         ";£] = ",
                         round(Y.lm$coefficients[2,1], digit = 3),
                         "(",
                         round(Y.lm$coefficients[2,2], digit = 3),
                         ")"), 
             size = size.pro, hjust = 1) +
    annotate(geom="text", x=hjustvalue[1+ttt], y=280, col=c("#3A5BA0"), 
             label=paste("Old: r =", 
                         round(cor.test.pro.O$estimate, digit = 3), 
                         addstar(round(cor.test.pro.O$p.value, digit = 3)),
                         ";£] = ",
                         round(O.lm$coefficients[2,1], digit = 3),
                         "(",
                         round(O.lm$coefficients[2,2], digit = 3),
                         ")"), 
             size = size.pro, hjust = 1) +
    annotate(geom="text", x=hjustvalue[1+ttt], y=260, col="black", 
             label=paste("All: r =", 
                         round(cor.test.pro$estimate, digit = 3), 
                         addstar(round(cor.test.pro$p.value, digit = 3)),
                         ";£] = ",
                         round(All.lm$coefficients[2,1], digit = 3),
                         "(",
                         round(All.lm$coefficients[2,2], digit = 3),
                         ")"), 
             size = size.pro, hjust = 1)
  
  ttt <- ttt + 1
}

temp.corr.K <- ggarrange(MD.cor.scatter[[1]],
                         MD.cor.scatter[[2]],
                         MD.cor.scatter[[3]],
                         MD.cor.scatter[[4]],
                         MD.cor.scatter[[5]],
                         MD.cor.scatter[[6]],
                         nrow = 2, ncol = 3, 
                         labels = c("A", "B", "C", "D", "E", "F"),
                         common.legend = TRUE, legend = "bottom", 
                         font.label = list(size= 30))

temp.corr.P <- annotate_figure(temp.corr.K,
                               left = text_grob("Mean Money Given (NTD)",
                                                color = "black", rot = 90,
                                                size = 50))

jpeg(file = paste("Corr_all.jpg"), width = 2800, height = 1500)
print(temp.corr.P)
dev.off()
