library(lme4)
library(glmmTMB)
library(emmeans)
library(car)

# GLMM call type
call<-read.csv("alarm call type.csv",header=T)
call$Alarm.Call.Type<-as.factor(call$Alarm.Call.Type)

model<-glmer(Alarm.Call.Type~Height+(1|Site)+(1|Order),family=binomial(logit),data=call)
summary(model)
Anova(model)

# LMM&GLMM acoustic feature
af<-read.csv("acoustic feature.csv",header=T)
a<-subset(af,af$Element=="a")
b<-subset(af,af$Element=="b")
c<-subset(af,af$Element=="c")

apf<-lmer(formula = Peak.Freq..Hz.~Height+(1|Site)+(1|Order),data=a)
summary(apf)
Anova(apf)

alength<-lmer(formula = Length.ms.~Height+(1|Site)+(1|Order),data=a)
summary(alength)
Anova(alength)

bpf<-lmer(formula = Peak.Freq..Hz.~Height+(1|Site),data=b)
summary(bpf)
Anova(bpf)

blength<-lmer(formula = Length.ms.~Height+(1|Site),data=b)
summary(blength)
Anova(blength)

cpf<-lm(formula = Peak.Freq..Hz.~Height,data=c)
summary(cpf)

clength<-lmer(formula = Length.ms.~Height+(1|Site),data=c)
summary(clength)
Anova(clength)

cinterval<-lmer(formula = Interval.of.c.c..ms.~Height+(1|Site),data=c)
summary(cinterval)
Anova(cinterval)

cnumber<-glmer(formula = Number.of.c.note~Height+(1|Site)+(1|Order),family=poisson(link="log"),data=c)
summary(cnumber)
Anova(cnumber)

# Fisher's exact probability test
# Specific behaviour(scan,flee)-Other behaviour
# Control call(2,8)
# Alarm call A(13,0)
# Alarm call B low(14,0)
# Alarm call B high(18,0)

ctrl_A<-matrix(c(2,8,13,0),nrow=2,byrow=T)
fisher.test(ctrl_A)

ctrl_B.low<-matrix(c(2,8,14,0),nrow=2,byrow=T)
fisher.test(ctrl_B.low)

ctrl_B.high<-matrix(c(2,8,18,0),nrow=2,byrow=T)
fisher.test(ctrl_B.high)

# GLM behaviour 
d1<-read.csv("playback.csv",header=T)
d2<-subset(d1,call.type!= 'Ctrl')
d3<-na.exclude(d2)
d2$behaviour<-as.factor(d2$behaviour)

bh<-glm(behaviour~magpie.number+call.type+position,data=d2,family =binomial(link = "logit"))
summary(bh)
Anova(bh)
pairs((emmeans(bh, ~ call.type)),adjust = "fdr")

# LMM latency
l<-lmer(latency~magpie.number+call.type+position+(1|day)+(1|site),data=d3)
summary(l)
Anova(l)

# LMM&GLMM scan,look.up,look.down
s<-lmer(scan~magpie.number+call.type+position+(1|day)+(1|site),data=d3)
summary(s)
Anova(s)
pairs((emmeans(s, ~ call.type)),adjust = "fdr")

lup<-glmmTMB(look.up~magpie.number+call.type+position+(1|day)+(1|site),data=d3,family =nbinom2(link = "log"))
summary(lup)
Anova(lup)
pairs((emmeans(lup, ~ call.type)),adjust = "fdr")

ldown<-glmmTMB(look.down~magpie.number+call.type+position+(1|day)+(1|site),data=d3,family =nbinom2(link = "log"))
summary(ldown)
Anova(ldown)