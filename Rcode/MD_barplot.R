library(data.table)
library(devtools)

y.color <- "#C6922C"
o.color <- "#3A5BA0"

setwd("c:/Users/acer/Desktop")

levels(total.boxplot$total.boxplot.sit.vector) <- list(PRO = "PRO", PUR = "PUR", NEU = "NEU", UNC = "UNC")
levels(total.boxplot$total.boxplot.group.vector) <- list(Old = "Old", Young = "Young")

dd.pros <- ddply(total.boxplot, 
                 c("total.boxplot$total.boxplot.sit.vector", "total.boxplot$total.boxplot.group.vector"),
                 summarise,
                 N = length(total.boxplot.mean_money.vector),
                 mean = mean(total.boxplot.mean_money.vector),
                 sd = sd(total.boxplot.mean_money.vector),
                 se = sd / sqrt(N)
                 )

p <- ggplot(dd.pros, aes(x=`total.boxplot$total.boxplot.sit.vector`, 
                         y=mean, 
                         fill=`total.boxplot$total.boxplot.group.vector`)) + 
            geom_bar(stat="identity", position= position_dodge2(preserve = "single")) +
            theme_classic() +
            geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = `total.boxplot$total.boxplot.group.vector`), 
                          width = 0.5, 
                          position= position_dodge(width=0.9)) +
            scale_color_manual("Groups", values = c("Young" = y.color, "Old" = o.color)) +
            scale_fill_manual("Groups", values = c("Young" = y.color, "Old" = o.color)) +
            
            labs(x = "Situations", y = "Mean Money Given (NTD)", colour = "Groups") +   
            theme(plot.title = element_text(hjust = 0.5, face="bold"),
                  title = element_text(size=30),
                  legend.text = element_text(size=30),
                  legend.title = element_text(size=30),
                  axis.text = element_text(size=30),
                  axis.title = element_text(size=30,face="bold")
            ) +
            ylim(c(0,165)) +
            geom_signif(y_position=c(150, 100), xmin=c(0.8, 2.8), xmax=c(1.2, 3.2),
                        annotation=c("**", "**"), textsize=15, tip_length=0)

print(a <- ggplot(inter.total.money, aes(x=inter.tag, y=inter.mean, fill=inter.group)) + 
        geom_bar(position=position_dodge2(), stat="identity",
                 size=.3) +
        theme_classic() +
        geom_errorbar(aes(ymin=inter.mean, ymax=inter.mean+inter.se, color = inter.group),
                      size=.5,    # Thinner lines
                      width=.5,
                      position=position_dodge(.9)
                      ) +
        scale_color_manual("Groups", values = c("Young" = y.color, "Old" = o.color)) +
        scale_fill_manual("Groups", values = c("Young" = y.color, "Old" = o.color)) +
        
        labs(x = "Interaction", y = "Mean Money Given (NTD)", colour = "Groups") +   
        theme(plot.title = element_text(hjust = 0.5, face="bold"),
              title = element_text(size=30),
              legend.text = element_text(size=30),
              legend.title = element_text(size=30),
              axis.text = element_text(size=30),
              axis.title = element_text(size=30,face="bold")
        ) +
        ylim(c(0,165)) +
        geom_signif(y_position=c(150, 100), xmin=c(0.8, 1.8), xmax=c(1.2, 2.2),
                    annotation=c("**", "**"), textsize=15, tip_length=0)
)

png(sprintf("Mean_money_giving_ggline_by_situations.png"), width = 1800, height = 800)
ggarrange(p, a, ncol=2, labels = c("A", "B"), common.legend = TRUE, legend = "right", font.label = list(size= 50))
dev.off()

