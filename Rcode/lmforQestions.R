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
anova(lm(gather.ER$`-measurement` ~ gather.ER$`-condition`*gather.ER$`Group_(Y:1_O:2)`))

options(scipen = 999)

ER.temp.T <- ER.temp[ER.temp$sit.tag == levels(ER.temp$sit.tag)[2],]

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

All.lm.qua <- lmer(ER.temp.T$emo.rate ~ ER.temp.T$age.tag*poly(ER.temp.T$dist, 2, raw = TRUE) + (1|ER.temp.T$sub.tag))
All.lm <- lmer(ER.temp.T$emo.rate ~ ER.temp.T$age.tag*ER.temp.T$dist + (1|ER.temp.T$sub.tag))
All.lm.qua.sum <- summary(lmer(ER.temp.T$emo.rate ~ ER.temp.T$age.tag*poly(ER.temp.T$dist, 2, raw = TRUE) + (1|ER.temp.T$sub.tag)))
All.lm.sum <- summary(lmer(ER.temp.T$emo.rate ~ ER.temp.T$age.tag*ER.temp.T$dist + (1|ER.temp.T$sub.tag)))
All.lm

anova(All.lm, All.lm.qua)

summary(lmer(ER.temp.T$emo.rate ~ ER.temp.T$age.tag*poly(ER.temp.T$dist, 2, raw = TRUE) + (1|ER.temp.T$sub.tag)))

ER.temp.propur <- ER.temp[ER.temp$sit.tag %in% c("PRO","PUR"),]
ER.temp.propur.summary <- summary(lmer(ER.temp.propur[ER.temp.propur$age.tag == "Old",]$emo.rate ~ ER.temp.propur[ER.temp.propur$age.tag == "Old",]$sit.tag*poly(ER.temp.propur[ER.temp.propur$age.tag == "Old",]$dist, 2, raw = TRUE) + (1|ER.temp.propur[ER.temp.propur$age.tag == "Old",]$sub.tag)))
ER.temp.propur.summary$coefficients[6,]

summary(lm(new.corrmerge$PROmPUR ~ new.corrmerge$Groups*new.corrmerge$IRI_EC))
summary(lm(new.corrmerge$PROmPUR ~ new.corrmerge$Groups*new.corrmerge$IRI_PD))
summary(lm(new.corrmerge$PROmPUR ~ new.corrmerge$Groups*new.corrmerge$IRI_FS))
summary(lm(new.corrmerge$PROmPUR ~ new.corrmerge$Groups*new.corrmerge$EQ))
summary(lm(new.corrmerge$PROmPUR ~ new.corrmerge$Groups*new.corrmerge$Income))
summary(lm(new.corrmerge$PROmPUR ~ new.corrmerge$Groups*new.corrmerge$logIncome))
summary(lm(new.corrmerge$PROmPUR ~ new.corrmerge$Groups*new.corrmerge$IRI_EC*new.corrmerge$logIncome))

summary(lm(new.corrmerge$PROmNEU ~ new.corrmerge$Groups*new.corrmerge$IRI_EC))
summary(lm(new.corrmerge$PROmNEU ~ new.corrmerge$Groups*new.corrmerge$IRI_PD))
summary(lm(new.corrmerge$PROmNEU ~ new.corrmerge$Groups*new.corrmerge$IRI_FS))
summary(lm(new.corrmerge$PROmNEU ~ new.corrmerge$Groups*new.corrmerge$EQ))
summary(lm(new.corrmerge$PROmNEU ~ new.corrmerge$Groups*new.corrmerge$Income))
summary(lm(new.corrmerge$PROmNEU ~ new.corrmerge$Groups*new.corrmerge$logIncome))
summary(lm(new.corrmerge$PROmNEU ~ new.corrmerge$Groups*new.corrmerge$IRI_EC*new.corrmerge$logIncome))

anova(lm(new.corrmerge$PROmPUR ~ new.corrmerge$Groups*new.corrmerge$logIncome))

summary(lm(new.corrmerge$mgive ~ new.corrmerge$Groups*new.corrmerge$IRI_EC))
summary(lm(new.corrmerge$mgive ~ new.corrmerge$Groups*new.corrmerge$IRI_PD))
summary(lm(new.corrmerge$mgive ~ new.corrmerge$Groups*new.corrmerge$IRI_FS))
summary(lm(new.corrmerge$mgive ~ new.corrmerge$Groups*new.corrmerge$EQ))
summary(lm(new.corrmerge$mgive ~ new.corrmerge$Groups*new.corrmerge$Income))
summary(lm(new.corrmerge$mgive ~ new.corrmerge$Groups*new.corrmerge$logIncome))
summary(lm(new.corrmerge$mgive ~ new.corrmerge$Groups*new.corrmerge$IRI_EC*new.corrmerge$logIncome))

summary(lm(new.corrmerge$Mean_emotion_rating ~ new.corrmerge$Groups*new.corrmerge$IRI_EC))
summary(lm(new.corrmerge$Mean_emotion_rating ~ new.corrmerge$Groups*new.corrmerge$IRI_PD))
summary(lm(new.corrmerge$Mean_emotion_rating ~ new.corrmerge$Groups*new.corrmerge$IRI_FS))
summary(lm(new.corrmerge$Mean_emotion_rating ~ new.corrmerge$Groups*new.corrmerge$EQ))
summary(lm(new.corrmerge$Mean_emotion_rating ~ new.corrmerge$Groups*new.corrmerge$Income))
summary(lm(new.corrmerge$Mean_emotion_rating ~ new.corrmerge$Groups*new.corrmerge$logIncome))

plot(new.corrmerge$mgive ~ new.corrmerge$Groups*new.corrmerge$IRI_EC)

anova(lm(new.corrmerge$IRI ~ new.corrmerge$))
