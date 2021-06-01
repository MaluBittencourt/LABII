#kappa

vetores=function(banco,vetor){
  lista=list()
  a=levels(vetor)
  a=as.integer(a)
  k=1
  for (i in 1:max(a)) {
    for (j in 1:length(banco)) 
      lista[[k]]=banco[vetor==i,]
      k=k+1
  }
  return(lista)
}
  
vetores2=function(lista){
  lista2=list()
  k=1
  for (i in 1:length(lista)) {
    df=lista[[i]]
    b=length(unique(df$`# paciente`))
    c=length(df$`# paciente`)
    d=abs(b-c)
    e=cbind(df$GTT,df$Ref.)
    
    for (i in (1+nrow(e)):(d+211)) {
      e=rbind(e,c(0,0))
      
    }
    lista2[[k]]=e
    k=k+1
  }
  return(lista2)
}



tabs=function(lista){
  lista3=list()
  k=1
  for (i in 1:length(lista)) {
    df=lista[[i]]
    p=c()
    for (j in 1:nrow(df)) {
      if (df[j,1]==1&df[j,2]==1) {
        p[j]="a"
      } else if (df[j,1]==1&df[j,2]==0) {
        p[j]="b"
      } else if (df[j,1]==0&df[j,2]==1) {
        p[j]="c"
      } else if (df[j,1]==0&df[j,2]==0) {
        p[j]="d"
      }
      
    }
    lista3[[k]]=p
    k=k+1
    
  }
  return(lista3)
}



tabs2=function(lista){
  k=1
  lista4=list()
  for (i in 1:length(lista)) {
    a=0
    b=0
    c=0
    d=0
    df=lista[[i]]
    for (j in 1:length(df)) {
      if (df[j]=="a") {
        a=a+1
      }
      else if (df[j]=="b") {
        b=b+1
      }
      else if (df[j]=="c") {
        c=c+1
      }
      else if (df[j]=="d") {
        d=d+1
      }
    }
    m=matrix(c(a,b,c,d),2,2)
    lista4[[k]]=m
    k=k+1
  }
  return(lista4)
}


setwd("C:/Users/malub/Documents/LAB II")
library(readxl)
bd=read_xlsx("C:/Users/malub/Documents/LAB II/Estatística.xlsx", sheet = "Acurácia Eventos E-I")
bd$`# Evento`=as.factor(bd$`# Evento`)
bd$Classif_natureza=as.factor(bd$Classif_natureza)
bd$`Dano E-1;F-2;G-3;H-4`=as.factor(bd$`Dano E-1;F-2;G-3;H-4`)

library(plyr)

str(bd)

w=vetores(bd,bd$`# Evento`)
z=vetores2(w)
x=tabs(z)
y=tabs2(x)
y

lapply(x, length)

library(epiR)
lapply(y, epi.kappa)

w=vetores(bd,bd$Classif_natureza)
z=vetores2(w)
x=tabs(z)
y=tabs2(x)
y
lapply(w, length)
lapply(z, length)
lapply(x, length)

lapply(y, epi.kappa)

w=vetores(bd,bd$`Dano E-1;F-2;G-3;H-4`)
z=vetores2(w)
x=tabs(z)
y=tabs2(x)
y
lapply(w, length)
lapply(z, length)
lapply(x, length)

lapply(y, epi.kappa)


dano_ei=c()
for (i in 1:length(bd$`Dano E-1;F-2;G-3;H-4`)) {
  if (bd$`Dano E-1;F-2;G-3;H-4`[i]!=0) {
    dano_ei[i]=1
  } 
  else if (bd$`Dano E-1;F-2;G-3;H-4`[i]==0) {
    dano_ei[i]=0
  }
}

dano_fi=c()
for (i in 1:length(bd$`Dano E-1;F-2;G-3;H-4`)) {
  if (bd$`Dano E-1;F-2;G-3;H-4`[i]==0) {
    dano_fi[i]=0
  } 
  else if (bd$`Dano E-1;F-2;G-3;H-4`[i]==1) {
    dano_fi[i]=0
  }
  else if (bd$`Dano E-1;F-2;G-3;H-4`[i]!=1&bd$`Dano E-1;F-2;G-3;H-4`[i]!=0) {
    dano_fi[i]=1
  }
}

bd=cbind(bd,dano_ei,dano_fi)
bd$dano_ei=as.factor(bd$dano_ei)
bd$dano_fi=as.factor(bd$dano_fi)

w=vetores(bd,bd$dano_ei)
z=vetores2(w)
x=tabs(z)
y=tabs2(x)
y
lapply(w, length)
lapply(z, length)
lapply(x, length)

lapply(y, epi.kappa)


w=vetores(bd,bd$dano_fi)
z=vetores2(w)
x=tabs(z)
y=tabs2(x)
y
lapply(w, length)
lapply(z, length)
lapply(x, length)

lapply(y, epi.kappa)


bd=read_xlsx("C:/Users/malub/Documents/LAB II/Estatística.xlsx", sheet = "Indivíduos")

evento_01=function(vetor){
  evento=c()
  for (i in 1:length(vetor)) {
    if (vetor[i]==0) {
      evento[i]=0
    }
    else if (vetor[i]==1) {
      evento[i]=1
    }
    else if (vetor[i]!=0&vetor[i]!=1) {
      evento[i]=1
    }
  }
  return(evento)
}

evento_ei_gtt=evento_01(bd$`Eventos_GTT_E-I`)
evento_fi_gtt=evento_01(bd$`Eventos_GTT_F-I`)
evento_ei_ref=evento_01(bd$`Eventos_Ref_E-I`)
evento_fi_ref=evento_01(bd$`Eventos_Ref_F-I`)

bd=cbind(bd,evento_ei_gtt,evento_fi_gtt,evento_ei_ref,evento_fi_ref)

tb=function(vetor1,vetor2){
  p=c()
  for (i in 1:length(vetor1)) {
    if (vetor1[i]==1&vetor2[i]==1) {
      p[i]="a"
    } else if (vetor1[i]==1&vetor2[i]==0) {
      p[i]="b"
    } else if (vetor1[i]==0&vetor2[i]==1) {
      p[i]="c"
    } else if (vetor1[i]==0&vetor2[i]==0) {
      p[i]="d"
    }
  }
  return(p)
}

tb2=function(vetor){
  a=0
  b=0
  c=0
  d=0
  for (i in 1:length(vetor)) {
    if (vetor[i]=="a") {
      a=a+1
    }
    else if (vetor[i]=="b") {
      b=b+1
    }
    else if (vetor[i]=="c") {
      c=c+1
    }
    else if (vetor[i]=="d") {
      d=d+1
    }
  }
  m=matrix(c(a,b,c,d),2,2)
  return(m)
}

evento_ei=tb(bd$evento_ei_gtt,bd$evento_ei_ref)
evento_fi=tb(bd$evento_fi_gtt,bd$evento_fi_ref)

ei=tb2(evento_ei)
ei
fi=tb2(evento_fi)
fi

epi.kappa(ei)
epi.kappa(fi)


bd=read_xlsx("C:/Users/malub/Documents/LAB II/Estatística.xlsx", sheet = "Eventos")
bd=bd[!is.na(bd$`Local em prontuário`),]
bd$`Local em prontuário`=as.factor(bd$`Local em prontuário`)

vetores3=function(banco,vetor){
  lista=list()
  a=levels(vetor)
  k=1
  for (i in 1:length(a)) {
    for (j in 1:length(banco)) 
      lista[[k]]=banco[vetor==a[i],]
    k=k+1
  }
  return(lista)
}


w=vetores3(bd,bd$`Local em prontuário`)
z=vetores2(w)
x=tabs(z)
y=tabs2(x)
y
lapply(w, length)
lapply(z, length)
lapply(x, length)

lapply(y, epi.kappa)
