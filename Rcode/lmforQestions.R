ddply(EQ.df, 
      c("`Group_(Y:1_O:2)`"),
      summarise,
      N = length(IRI),
      mean = mean(IRI),
      sd = sd(IRI),
      se = sd / sqrt(N)
)

t.test(EQ.df[EQ.df$`Group_(Y:1_O:2)`=="Young",]$IRI, EQ.df[EQ.df$`Group_(Y:1_O:2)`=="Old",]$IRI)

gather.ER <- gather(EQ.df, -condition, -measurement, IRI_EC, IRI_PD, IRI_FS)

options(scipen = 999)

ER.temp.T <- ER.temp[ER.temp$sit.tag == levels(ER.temp$sit.tag)[1],]

Y.lm <- lmer(ER.temp.T[ER.temp.T$age.tag == "Young",]$emo.rate ~ ER.temp.T[ER.temp.T$age.tag == "Young",]$dist + (1|ER.temp.T[ER.temp.T$age.tag == "Young",]$sub.tag))
Y.lm.qua <- lmer(ER.temp.T[ER.temp.T$age.tag == "Young",]$emo.rate ~ poly(ER.temp.T[ER.temp.T$age.tag == "Young",]$dist, 2, raw = TRUE) + (1|ER.temp.T[ER.temp.T$age.tag == "Young",]$sub.tag))
Y.lm.sum <- summary(lmer(ER.temp.T[ER.temp.T$age.tag == "Young",]$emo.rate ~ ER.temp.T[ER.temp.T$age.tag == "Young",]$dist + (1|ER.temp.T[ER.temp.T$age.tag == "Young",]$sub.tag)))
Y.lm.sum.qua <- summary(lmer(ER.temp.T[ER.temp.T$age.tag == "Young",]$emo.rate ~ poly(ER.temp.T[ER.temp.T$age.tag == "Young",]$dist, 2, raw = TRUE) + (1|ER.temp.T[ER.temp.T$age.tag == "Young",]$sub.tag)))
anova(Y.lm, Y.lm.qua)

O.lm <- lmer(ER.temp.T[ER.temp.T$age.tag == "Old",]$emo.rate ~ ER.temp.T[ER.temp.T$age.tag == "Old",]$dist + (1|ER.temp.T[ER.temp.T$age.tag == "Old",]$sub.tag))
O.lm.qua <- lmer(ER.temp.T[ER.temp.T$age.tag == "Old",]$emo.rate ~ poly(ER.temp.T[ER.temp.T$age.tag == "Old",]$dist, 2, raw = TRUE) + (1|ER.temp.T[ER.temp.T$age.tag == "Old",]$sub.tag))
O.lm.sum <- summary(lmer(ER.temp.T[ER.temp.T$age.tag == "Old",]$emo.rate ~ ER.temp.T[ER.temp.T$age.tag == "Old",]$dist + (1|ER.temp.T[ER.temp.T$age.tag == "Old",]$sub.tag)))
O.lm.qua.sum <- summary(lmer(ER.temp.T[ER.temp.T$age.tag == "Old",]$emo.rate ~ poly(ER.temp.T[ER.temp.T$age.tag == "Old",]$dist, 2, raw = TRUE) + (1|ER.temp.T[ER.temp.T$age.tag == "Old",]$sub.tag)))
anova(O.lm, O.lm.qua)

All.lm <- summary(lmer(ER.temp.T$emo.rate ~ ER.temp.T$age.tag*ER.temp.T$dist + (1|ER.temp.T$sub.tag)))

summary(lm(new.corrmerge$PROmPUR ~ new.corrmerge$Groups*new.corrmerge$IRI_EC))
summary(lm(new.corrmerge$PROmPUR ~ new.corrmerge$Groups*new.corrmerge$IRI_PD))
summary(lm(new.corrmerge$PROmPUR ~ new.corrmerge$Groups*new.corrmerge$IRI_FS))
summary(lm(new.corrmerge$PROmPUR ~ new.corrmerge$Groups*new.corrmerge$EQ))
summary(lm(new.corrmerge$PROmPUR ~ new.corrmerge$Groups*new.corrmerge$Income))
summary(lm(new.corrmerge$PROmPUR ~ new.corrmerge$Groups*new.corrmerge$logIncome))
summary(lm(new.corrmerge$PROmPUR ~ new.corrmerge$Groups*new.corrmerge$IRI_EC*new.corrmerge$logIncome))

anova(lm(new.corrmerge$PROmPUR ~ new.corrmerge$Groups*new.corrmerge$logIncome))

summary(lm(new.corrmerge$mgive ~ new.corrmerge$Groups*new.corrmerge$IRI_EC))
summary(lm(new.corrmerge$mgive ~ new.corrmerge$Groups*new.corrmerge$IRI_PD))
summary(lm(new.corrmerge$mgive ~ new.corrmerge$Groups*new.corrmerge$IRI_FS))
summary(lm(new.corrmerge$mgive ~ new.corrmerge$Groups*new.corrmerge$EQ))
summary(lm(new.corrmerge$mgive ~ new.corrmerge$Groups*new.corrmerge$Income))
summary(lm(new.corrmerge$mgive ~ new.corrmerge$Groups*new.corrmerge$logIncome))
summary(lm(new.corrmerge$mgive ~ new.corrmerge$Groups*new.corrmerge$IRI_EC*new.corrmerge$logIncome))
