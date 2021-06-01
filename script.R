setwd("C:/Users/malub/Documents/LAB II")
library(readxl)
bd=read_xlsx("C:/Users/malub/Documents/LAB II/Estatística.xlsx", sheet = "Tipos e códigos EA por natureza")

library(dplyr)
vars=as.data.frame(cbind(bd$Código,bd$GTT,bd$Referência))
vars=vars[-19,]
str(vars)

View(vars)

natureza_evento_1=c()
s=1
i=1
while (i <= length(vars$V3)) {
  j=1
  while (j <= vars$V3[i]) {
    natureza_evento_1[s]=vars$V1[i]
    s=s+1
    j=j+1
  }
  i=i+1
}
natureza_evento_2=c()
s=1
i=1
while (i <= length(vars$V2)) {
  j=1
  while (j <= vars$V2[i]) {
    natureza_evento_2[s]=vars$V1[i]
    s=s+1
    j=j+1
  }
  i=i+1
}

evento = c(natureza_evento_1,natureza_evento_2)

ggt=rep(1,129)
ref=rep(0,176)

metodo=c(ref,ggt)

df_logit=as.data.frame(cbind(metodo,evento))
df_logit$evento=as.factor(df_logit$evento)
df_logit$metodo=as.numeric(df_logit$metodo)
View(df_logit)
logit=glm(metodo~-1+evento, family = binomial(logit), data = df_logit)
library(car)
Anova(logit)
summary(logit)
round(Confint(logit,level=0.95,type="LR",exponentiate = T),2)




ref_e=rep("E",124)
ref_f=rep("F",42)
ref_g=rep("G",3)
ref_h=rep("H",7)
ggt_e=rep("E",64)
ggt_f=rep("F",49)
ggt_g=rep("G",8)
ggt_h=rep("H",8)
ref_fi=rep("F-I",52)
ggt_fi=rep("F-I",65)

ref1=rep(0,228)
ggt1=rep(1,194)

evento2=c(ref_e,ref_f,ref_g,ref_h,ref_fi,ggt_e,ggt_f,ggt_g,ggt_h,ggt_fi)
metodo2=c(ref1,ggt1)

df_logit2=as.data.frame(cbind(metodo2,evento2))
df_logit2$evento2=as.factor(df_logit2$evento2)
df_logit2$metodo2=as.numeric(df_logit2$metodo2)
View(df_logit2)
logit2=glm(metodo2~-1+evento2, family = binomial(logit), data = df_logit2)
Anova(logit2)
summary(logit2)
round(Confint(logit2,level=0.95,type="LR",exponentiate = T),2)

df2=df_logit2
df2=df2[!(df2$evento2=="E"),]
logit3=glm(metodo2~-1+evento2, family = binomial(logit), data = df2)
Anova(logit3)

