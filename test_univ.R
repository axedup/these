### Tests univariés###

# boucle avec glm

#summary(glm(cas~poids,data=cas_temoinsexpoi,family = binomial))


modeltest<-glm(cas~age.f + mopb.f + agegestationnel.f ,family=binomial,data=cas_temoinsexpoi)
modeltest2<-glm(cas~age.f + moyenne_benzene.f2 + poids.f +  agegestationnel.f+ ageenf+parite.f2, family=binomial,data=cas_temoinsexpoi)
summary(modeltest2)


modeltest3<-glm(cas~age.f + moyenne_no2.f2 + poids.f +  agegestationnel.f+ ageenf+parite.f2, family=binomial,data=cas_temoinsexpoi)
summary(modeltest3)


modeltestl<-glm(cas~age.f + moyenne_benzene+ poids.f, family=binomial,data=cas_temoinsexpoi)



modelall<-glm(cas~allaitement, family=binomial,data=cas_temoinsexpoi)

# model<-function(x){
#   with(cas_temoinsexpoi,{
#     model1<-glm(cas~x,family=binomial)
#     p<-round(summary(model1)$coefficient[2,4],3)
#     s<-summary(model1)
#     return(s)
#     return(p)
#   })
# }
# 
# 
# apply(cas_temoinsexpoi[,c("mopb.f2","mopn.f2","poids.f","agegestationnel.f","age.f","moyenne_benzene.f","moyenne_no2.f","gestite.f2","parite.f2","sexe","mopb.f","mopn.f","coeffapgar5mncor.f","moyenne_benzene.f2","moyenne_no2.f2","vb")],2,model)


varlist<-c("poids.f","agegestationnel.f","age.f","moyenne_benzene.f","moyenne_no2.f","gestite.f2","parite.f2","sexe","vb","mopb.f","mopn.f","coeffapgar5mncor.f","moyenne_benzene.f2","moyenne_no2.f2","mopb.f2","mopn.f2")
models <- lapply(varlist, function(x) {
  glm(substitute(cas ~ i, list(i = as.name(x))), data=cas_temoinsexpoi,family=binomial )
})

setwd("C:/Users/Louise/Documents/Desespoir/Bases/resultats")
sink("test-univ.txt")
r<-lapply(models, summary)
r
conf<-lapply(models,confint)
lapply(conf,exp)
sink()


#nbr enfant trop de valeur NA
# pathologie pendant grossesse pas de preuves sûres
# tabac idem
# poids ajusté à l'age gestationnel on pourrait l'exprimer en percentil..



modeltestc<-glm(cas~age.f+ moyenne_benzene.f2 + poids.f + vb+ agegestationnel.f+ ageenf+parite.f2+sexe +coeffapgar5mncor.f,family=binomial,data=cas_temoinsexpoi)
summary(modeltestc)


modeltestcc<-glm(cas~age.f+ moyenne_no2.f2 + poids.f +  vb+ agegestationnel.f+ ageenf+parite.f2+sexe +coeffapgar5mncor.f,family=binomial,data=cas_temoinsexpoi)
summary(modeltestcc)

# et ACP pourquoi pas ?

test<-cas_temoinsexpoi[!is.na(cas_temoinsexpoi$poids)& !is.na(cas_temoinsexpoi$agegestationnel) & !is.na(cas_temoinsexpoi$coeffapgar5mncor) ,c("poids","agegestationnel","coeffapgar5mncor")]
test$poids<-as.numeric(test$poids)
test$coeffapgar5mncor<-as.numeric(test$coeffapgar5mncor)
test$agegestationnel<-as.numeric(test$agegestationnel)

#test$age.f<-as.numeric(test$age.f)

pca<-prcomp(test)
100 * pca$sdev^2 / sum(pca$sdev^2)


cor(test, method = c("pearson"))
# le poids distingue bien les individus suivi de l'age gesta- le coeff apgar à 5 min est de trop possiblement
#♣ pas de corrélation majeure   


###====Calcul des OR====###


varlist<-c("poids.f","agegestationnel.f","age.f","agegestationnel.f2","age.f2","moyenne_benzene.f","moyenne_no2.f","gestite.f2","parite.f2","sexe","vb","mopb.f","mopn.f","coeffapgar5mncor.f","moyenne_benzene.f2","moyenne_no2.f2","mopb.f2","mopn.f2")
models <- lapply(varlist, function(x){
clogit(substitute(cas ~ i+strata(strates), list(i = as.name(x))), data=cas_temoinsexpoi,method ="exact")
})


setwd("C:/Users/Louise/Documents/Desespoir/Bases/resultats")
sink("test-univ.txt")
r<-lapply(models, summary)
r
conf<-lapply(models,confint)
lapply(conf,exp)
sink()



cas_temoinsexpoi$poids.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$poids.f),"NA",cas_temoinsexpoi$poids.f))
cas_temoinsexpoi$agegestationnel.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$agegestationnel.f),"NA",cas_temoinsexpoi$agegestationnel.f))
cas_temoinsexpoi$agegestationnel.f2na<-as.factor(ifelse(is.na(cas_temoinsexpoi$agegestationnel.f2),"NA",cas_temoinsexpoi$agegestationnel.f2))
cas_temoinsexpoi$age.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$age.f),"NA",cas_temoinsexpoi$age.f))
cas_temoinsexpoi$age.f2na<-as.factor(ifelse(is.na(cas_temoinsexpoi$age.f2),"NA",cas_temoinsexpoi$age.f2))
cas_temoinsexpoi$parite.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$parite.f2),"NA",cas_temoinsexpoi$parite.f2))
cas_temoinsexpoi$gestite.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$gestite.f2),"NA",cas_temoinsexpoi$gestite.f2))

cas_temoinsexpoi$sexe.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$sexe),"NA",cas_temoinsexpoi$sexe))
cas_temoinsexpoi$coeffapgar5mncor.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$coeffapgar5mncor.f),"NA",cas_temoinsexpoi$coeffapgar5mncor.f))
cas_temoinsexpoi$vbna<-as.factor(ifelse(is.na(cas_temoinsexpoi$vb),"NA",cas_temoinsexpoi$vb))

cas_temoinsexpoi$allaitementna<-as.factor(ifelse(is.na(cas_temoinsexpoi$allaitement),"NA",cas_temoinsexpoi$allaitement))



cas_temoinsexpoi$poids.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$poids.f),"NA",cas_temoinsexpoi$poids.f))
cas_temoinsexpoi$poids.f3na<-as.factor(ifelse(is.na(cas_temoinsexpoi$poids.f3),"NA",cas_temoinsexpoi$poids.f3))
cas_temoinsexpoi$poids.f4na<-as.factor(ifelse(is.na(cas_temoinsexpoi$poids.f4),"NA",cas_temoinsexpoi$poids.f4))
cas_temoinsexpoi$poids.f5na<-as.factor(ifelse(is.na(cas_temoinsexpoi$poids.f5),"NA",cas_temoinsexpoi$poids.f5))

cas_temoinsexpoi$agegestationnel.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$agegestationnel.f),"NA",cas_temoinsexpoi$agegestationnel.f))
cas_temoinsexpoi$agegestationnel.f2na<-as.factor(ifelse(is.na(cas_temoinsexpoi$agegestationnel.f2),"NA",cas_temoinsexpoi$agegestationnel.f2))

cas_temoinsexpoi$age.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$age.f),"NA",cas_temoinsexpoi$age.f))
cas_temoinsexpoi$age.f2na<-as.factor(ifelse(is.na(cas_temoinsexpoi$age.f2),"NA",cas_temoinsexpoi$age.f2))
cas_temoinsexpoi$age.f3na<-as.factor(ifelse(is.na(cas_temoinsexpoi$age.f3),"NA",cas_temoinsexpoi$age.f3))
cas_temoinsexpoi$age.f4na<-as.factor(ifelse(is.na(cas_temoinsexpoi$age.f4),"NA",cas_temoinsexpoi$age.f4))



cas_temoinsexpoi$parite.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$parite.f2),"NA",cas_temoinsexpoi$parite.f2))
cas_temoinsexpoi$gestite.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$gestite.f2),"NA",cas_temoinsexpoi$gestite.f2))

cas_temoinsexpoi$parite.f3na<-as.factor(ifelse(is.na(cas_temoinsexpoi$parite.f3),"NA",cas_temoinsexpoi$parite.f3))
#cas_temoinsexpoi$gestite.f3na<-as.factor(ifelse(is.na(cas_temoinsexpoi$gestite.f3),"NA",cas_temoinsexpoi$gestite.f3))


cas_temoinsexpoi$sexe.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$sexe),"NA",cas_temoinsexpoi$sexe))
cas_temoinsexpoi$coeffapgar5mncor.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$coeffapgar5mncor.f),"NA",cas_temoinsexpoi$coeffapgar5mncor.f))
cas_temoinsexpoi$vbna<-as.factor(ifelse(is.na(cas_temoinsexpoi$vb),"NA",cas_temoinsexpoi$vb))
cas_temoinsexpoi$taille.f2na<-as.factor(ifelse(is.na(cas_temoinsexpoi$taille.f2),"NA",cas_temoinsexpoi$taille.f2))
cas_temoinsexpoi$taille.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$taille.f),"NA",cas_temoinsexpoi$taille.f))

cas_temoinsexpoi$nbfoetus.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$nbfoetus.f),"NA",cas_temoinsexpoi$nbfoetus.f))


model<-function(x){
  with(cas_temoinsexpoi,{
    model1<-clogit(cas ~ x+strata(cas_temoinsexpoi$strates),method=c("exact"))
    #p<-round(summary(model1)$coefficient[2,4],3)
    s<-summary(model1)
    return(s)
    return(p)
  })
}



cas_temoinsexpoi$cas<-as.numeric(as.character(cas_temoinsexpoi$cas))

jesaispas<-apply(cas_temoinsexpoi[,c("mopb.f2","mopn.f2","poids.fna","agegestationnel.fna","age.f2na","agegestationnel.f2na","age.fna","moyenne_benzene.f","moyenne_no2.f",
                                     "gestite.fna","parite.fna","sexe.fna","mopb.f",
                                     "mopn.f","coeffapgar5mncor.fna","moyenne_benzene.f2","moyenne_no2.f2",
                                     "vbna","most_dep","parite.f3na","forte_expo","forte_expop","age.f3na",
                                     "age.f4na","poids.f3na","poids.f4na","agegestationnel.f2na","agegestationnel.f3",
                                     "agegestationnel.f4","poids.f5na","taille.f2na","nbfoetus.fna","taille.fna")],2,model)
conf<-lapply(jesaispas,function(x){x$conf.int})
p<-lapply(jesaispas,function(x){x$coefficients[,5]})


nbr<-NULL
for (i in c("mopb.f2","mopn.f2","poids.fna","agegestationnel.fna","age.f2na","agegestationnel.f2na","age.fna","moyenne_benzene.f","moyenne_no2.f",
            "gestite.fna","parite.fna","sexe.fna","mopb.f",
            "mopn.f","coeffapgar5mncor.fna","moyenne_benzene.f2","moyenne_no2.f2",
            "vbna","most_dep","parite.f3na","forte_expo","forte_expop","age.f3na",
            "age.f4na","poids.f3na","poids.f4na","agegestationnel.f2na","agegestationnel.f3",
            "agegestationnel.f4","poids.f5na","taille.f2na","nbfoetus.fna","taille.fna")) {
  c<-nlevels(cas_temoinsexpoi[,i])-1
  nbr<-c(nbr,c)
}



legende<-rep(c("mopb.f2","mopn.f2","poids.fna","agegestationnel.fna","age.f2na","agegestationnel.f2na","age.fna","moyenne_benzene.f","moyenne_no2.f",
               "gestite.fna","parite.fna","sexe.fna","mopb.f",
               "mopn.f","coeffapgar5mncor.fna","moyenne_benzene.f2","moyenne_no2.f2",
               "vbna","most_dep","parite.f3na","forte_expo","forte_expop","age.f3na",
               "age.f4na","poids.f3na","poids.f4na","agegestationnel.f2na","agegestationnel.f3",
               "agegestationnel.f4","poids.f5na","taille.f2na","nbfoetus.fna","taille.fna"),nbr)
resultor<-do.call(rbind,conf)
pro<-unlist(p)
resultor<-cbind(legende,resultor,pro)

write.table(resultor,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/test_univarie_toutc.xls")




### modèle finaux 
clogit(cas ~ poids.f +agegestationnel.f+ age.fna + parite.fna+sexe.fna+ coeffapgar5mncor.fna+ most_dep+mopb.f2+strata(cas_temoinsexpoi$strates),data=cas_temoinsexpoi,method=c("exact"))

## modèle avec NA 
clogit(cas ~ poids.f +agegestationnel.f+ age.f + parite.f2+sexe+ coeffapgar5mncor.f+ mopb.f2+ most_dep+strata(cas_temoinsexpoi$strates),data=cas_temoinsexpoi,method=c("exact"))


clogit(cas ~ age.f +strata(cas_temoinsexpoi$strates),data=cas_temoinsexpoi,method=c("exact"))
clogit(cas ~ agegestationnel.f +strata(cas_temoinsexpoi$strates),data=cas_temoinsexpoi,method=c("exact"))
clogit(cas ~ sexe +strata(cas_temoinsexpoi$strates),data=cas_temoinsexpoi,method=c("exact"))
clogit(cas ~ vb +strata(cas_temoinsexpoi$strates),data=cas_temoinsexpoi,method=c("exact"))

clogit(cas ~ moyenne_benzene.f +strata(cas_temoinsexpoi$strates),data=cas_temoinsexpoi,method=c("exact"))


###====Leucémie====###

modell<-function(x){
  with(cas_temoinsexpoi[cas_temoinsexpoi$leucemie=="1",],{
    model1<-clogit(cas ~ x+strata(cas_temoinsexpoi$strates[cas_temoinsexpoi$leucemie=="1"]),method=c("exact"))
    #p<-round(summary(model1)$coefficient[2,4],3)
    s<-summary(model1)
    return(s)
    return(p)
  })
}





jesaispasl<-apply(cas_temoinsexpoi[cas_temoinsexpoi$leucemie=="1",c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession",
                                                                    "niveauetudes","most_dep","parite.fna","parite.f2",
                                                                    "parite.f3na","gestite.fna","gestite.f2","sexe.fna","nbfoetus.f",
                                                                    "agegestationnel.fna","agegestationnel.f2na",
                                                                    "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2",
                                                                    "vbna",
                                                                    "poids.fna","poids.f3na","poids.f4na","poids.f5na","taille.fna","taille.f2na","coeffapgar5mncor.fna",
                                                                    "allaitementna",
                                                                    "moyenne_benzene.f","moyenne_no2.f","mopb.f","mopn.f","forte_expo","forte_expop",
                                                                    "moyenne_benzene.f2","moyenne_no2.f2","mopb.f2","mopn.f2")],2,modell)
conf<-lapply(jesaispasl,function(x){x$conf.int})
p<-lapply(jesaispasl,function(x){x$coefficients[,5]})



nbr<-NULL
for (i in c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession",
            "niveauetudes","most_dep","parite.fna","parite.f2",
            "parite.f3na","gestite.fna","gestite.f2","sexe.fna","nbfoetus.f",
            "agegestationnel.fna","agegestationnel.f2na",
            "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2",
            "vbna",
            "poids.fna","poids.f3na","poids.f4na","poids.f5na","taille.fna","taille.f2na","coeffapgar5mncor.fna",
            "allaitementna","moyenne_benzene.f","moyenne_no2.f","mopb.f","mopn.f","forte_expo","forte_expop",
            "moyenne_benzene.f2","moyenne_no2.f2","mopb.f2","mopn.f2")) {
  c<-length(levels(droplevels(cas_temoinsexpoi[cas_temoinsexpoi$leucemie=="1",i])))-1
  nbr<-c(nbr,c)
}



legende<-rep(c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession",
               "niveauetudes","most_dep","parite.fna","parite.f2",
               "parite.f3na","gestite.fna","gestite.f2","sexe.fna","nbfoetus.f",
               "agegestationnel.fna","agegestationnel.f2na",
               "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2",
               "vbna",
               "poids.fna","poids.f3na","poids.f4na","poids.f5na","taille.fna","taille.f2na","coeffapgar5mncor.fna",
               "allaitementna","moyenne_benzene.f","moyenne_no2.f","mopb.f","mopn.f","forte_expo","forte_expop",
               "moyenne_benzene.f2","moyenne_no2.f2","mopb.f2","mopn.f2"),nbr)
resultor<-do.call(rbind,conf)
#resultor<-cbind(legende,resultor)
pro<-unlist(p)
resultor<-cbind(resultor,pro)
resultor<-round(resultor,2)
resultor<-cbind(legende,resultor)

write.table(resultor,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/test_univarie_leucemie2.xls",sep="\t")


f <- function(d, i){
  d2 <- d[i,]
  model<-clogit(cas ~ age.fna+strata(d2$strates[d2$leucemie=="1"]),data=d2[d2$leucemie=="1",],method=c("exact"))
  return(model$coefficients[4])
}




bootcorr <- boot(cas_temoinsexpoi, f, R=500)
bootcorr
boot.ci(bootcorr, type = "norm")

f <- function(d, i){
  d2 <- d[i,]
model<-glm(cas~agegestationnel.f2na+ ageenf,family=binomial,data=d2[d2$leucemie=="1",])
  return(model$coefficients[3])
}

bootcorr <- boot(cas_temoinsexpoi, f, R=500)
bootcorr
boot.ci(bootcorr, type = "norm")


model1l<-clogit(cas ~ vbna+mopb.f2+strata(cas_temoinsexpoi$strates[cas_temoinsexpoi$leucemie=="1"]),method=c("exact"),data=cas_temoinsexpoi[cas_temoinsexpoi$leucemie=="1",])
summary(model1l)

pA=0.49
pB=0.53
kappa=1
alpha=0.05
beta=0.20
(OR=pA*(1-pB)/pB/(1-pA)) # 2
(nB=(1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))*((qnorm(1-alpha/2)+qnorm(1-beta))/log(OR))^2)
ceiling(nB) # 156
z=log(OR)*sqrt(nB)/sqrt(1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))
(Power=pnorm(z-qnorm(1-alpha/2))+pnorm(-z-qnorm(1-alpha/2)))



### ===================Calcul des effectifs nécessaires============
# formule Bouyer 

#
puissance<-function(p2,or){
 #p2 % d'exposition grp T 
 # or  
  alpha=0.05
  beta=0.20 
odds2 <- p2/(1-p2) 
odds1 <- or*odds2 
p1 <- odds1/(1+odds1) 
n<-(11/10)*((((qnorm(1-alpha/2)+qnorm(1-beta)))^2)/(4*(asin(sqrt(p2))-asin(sqrt(p1)))^2))

#n.for.2p(p1,p2,ratio=10)
return(n)
}

puissance(0.5,1.5)
puissance(0.25,1.5)
puissance(0.20,1.5)
puissance(0.15,1.5)
puissance(0.30,1.3)

puissance(0.5,0.90)
puissance(0.25,1.1)
puissance(0.20,1.05)
puissance(0.15,1.1)
puissance(0.10,1.1)

# N<-NULL
# seq<-seq(0,100,by=1)
# for (i in seq){
#   p2<-0.01*i
#   n<-puissance(p2=p2,1.1)
#   n<-ceiling(n)
#   N<-c(N,n)
# }
# 
# rr_11<-cbind(seq,N,rep(1.1,101))
# 
# or<-c(1,1,1,2)

# on la boucle pour calcul avec tout les pourcentages d'expo
for (j in c(0.7,0.9,1.1,1.2,1.5) ){   # choix des OR 
N<-NULL
seq<-seq(0,100,by=1)
for (i in seq){
  p2<-0.01*i
  n<-puissance(p2=p2,j)
  n<-ceiling(n)
  N<-c(N,n)
}


assign(paste("rr",j,sep=""),cbind(seq,N,rep(j,101)))

}

graph_por<-cbind(rr0.7,rr0.9,rr1.1,rr1.2,rr1.5) # ce graphique est a à ggploter 


ggplot(graph_por, aes(seq, N, colour = j)) + geom_line()


# tenir compte que c'est condi

puissance_bis<-function(p,or){
 #p % groupe T 
  odds2 <- p/(1-p) 
  odds1 <- or*odds2 
  p1 <- odds1/(1+odds1);  p1  
  var<-p*(1-p)
  teta<-(p-p1)/(p*(1-p))
  
  return(((1.960+0.8416)/(teta*sqrt(var)))^2*(11/10))
  
}

puissance_bis(0.15,1.5)
puissance_bis(0.28,1.5)
puissance_bis(0.30,1.5)
puissance_bis(0.32,1.5)

# idem on la boucle 
for (j in c(0.7,0.9,1.1,1.2,1.5) ){
  N<-NULL
  seq<-seq(0,100,by=1)
  for (i in seq){
    p2<-0.01*i
    n<-puissance_bis(p=p2,j)
    n<-ceiling(n)
    N<-c(N,n)
  }
  
  
  assign(paste("rrcondi",j,sep=""),cbind(seq,N,rep(j,101)))
  
}

graph_porcondi<-cbind(rrcondi0.7,rrcondi0.9,rrcondi1.1,rrcondi1.2,rrcondi1.5) # a ggploter 


# p<-(1-B)*p1+B*p2
# num<-((qnorm(1-alpha/2)*(p*(1-p)/B)^(1/2))+(qnorm(1-beta)*(p1*(1-p1)+p2*(1-p2)*(1-B)/B)^(1/2)))^(2)
# denom<-((p1-p2)^(2)*(1-B))
# n<-num/denom
# 
# 
# num<-p*(1+10)
# z<-(qnorm(1-alpha/2)+qnorm(1-beta))^2
# z*num
# 
# denom<-((p1-p2)^2*(1-B)*B)



#### Calcul des effectifs nécessaires avec des corrélations 

or_corr<-function(ratio,or,p,alpha,beta,cor){
num<-p*(1+ratio)^(2)*or+(1-p)*(1+ratio*or)^(2)
den<-p*(1-p)*ratio*or*log(or)^2
phi<-(qnorm(1-alpha/2)+qnorm(1-beta))^2
n<-phi*num/den
#ncas<-n/(1/ratio+1)

ncor=n/(1-cor)
ncas<-ncor/(1/ratio+1)
return(ncas)
}


# pour calculer coeff multiples de corrélation : on met tou les variables en binaire 0/1 mais à remettre en numeric 
cohorte$poids.f5n<-as.numeric(cohorte$poids.f5)
cohorte$poids.f4n<-as.numeric(cohorte$poids.f4)
cohorte$agegestationnel.f4n<-as.numeric(cohorte$agegestationnel.f4)
cohorte$agegestationnel.f3n<-as.numeric(cohorte$agegestationnel.f3)
# +  edi




# on reprend la fonction avec les arc sin et opn multpile avec le coeff de coor

puissance_cor<-function(p2,or,k,cor){
  
  alpha=0.05
  beta=0.20 
  odds2 <- p2/(1-p2) 
  odds1 <- or*odds2 
  p1 <- odds1/(1+odds1) 
  n<-((k+1)/k)*((((qnorm(1-alpha/2)+qnorm(1-beta)))^2)/(4*(asin(sqrt(p2))-asin(sqrt(p1)))^2))
  nt<-n + k*n
  ntcor<-nt/(1-cor)
  nccor<-ntcor/(k+1)
  #n.for.2p(p1,p2,ratio=10)
  return(nccor)
}

  
puissance_cor(0.3,or=1.316,k=10,cor=0.11)  

# on boucle 

for(c in c(0.10,0.15,0.20,0.30)){
for (j in c(0.7,0.9,1.1,1.2,1.5) ){
  N<-NULL
  seq<-seq(0,100,by=1)
  for (i in seq){
    p2<-0.01*i
    n<-puissance_cor(p2=p2,or=j,k=10,cor=c)
    n<-ceiling(n)
    N<-c(N,n)
  }
  
  
  assign(paste("rr",j,c,sep=""),cbind(seq,N,rep(j,101),rep(c,101)))
  
}

}



graph_por<-cbind(rr0.7,rr0.9,rr1.1,rr1.2,rr1.5) # a ggploter 








model2l<-clogit(cas ~ vbna+age.f3na+poids.f4+moyenne_benzene.f2+strata(cas_temoinsexpoi$strates[cas_temoinsexpoi$leucemie=="1"]),method=c("exact"),data=cas_temoinsexpoi[cas_temoinsexpoi$leucemie=="1",])
summary(model2l)

model2lb<-clogit(cas ~ vbna+age.f3na+poids.f4+moyenne_no2.f2+strata(cas_temoinsexpoi$strates[cas_temoinsexpoi$leucemie=="1"]),method=c("exact"),data=cas_temoinsexpoi[cas_temoinsexpoi$leucemie=="1",])
summary(model2lb)



model2l<-clogit(cas ~ vbna+age.f3na+poids.f4+moyenne_benzene.f2+strata(cas_temoinsexpoi$strates[cas_temoinsexpoi$leucemie=="1"]),method=c("exact"),data=cas_temoinsexpoi[cas_temoinsexpoi$leucemie=="1",])
summary(model2l)
model3l<-clogit(cas ~ vbna+age.f3na+mopb.f2+strata(cas_temoinsexpoi$strates[cas_temoinsexpoi$leucemie=="1"]),method=c("exact"),data=cas_temoinsexpoi[cas_temoinsexpoi$leucemie=="1",])
summary(model3l)

model4l<-clogit(cas ~ vbna+age.f3na+forte_expo+strata(cas_temoinsexpoi$strates[cas_temoinsexpoi$leucemie=="1"]),method=c("exact"),data=cas_temoinsexpoi[cas_temoinsexpoi$leucemie=="1",])
summary(model4l)


model4l<-clogit(cas ~ vbna+age.f3na+forte_expo+,method=c("exact"),data=cas_temoinsexpoi[cas_temoinsexpoi$leucemie=="1",])
summary(model4l)

### tableaux croisés

prop.table(table(cas_temoinsexpoi$moyenne_benzene.f[cas_temoinsexpoi$leucemie=="1"],cas_temoinsexpoi$age.f[cas_temoinsexpoi$leucemie=="1"]),1)
prop.table(table(cas_temoinsexpoi$moyenne_benzene.f2[cas_temoinsexpoi$leucemie=="1"],cas_temoinsexpoi$age.f[cas_temoinsexpoi$leucemie=="1"]),1)
                      

prop.table(table(cas_temoinsexpoi$moyenne_benzene.f2[cas_temoinsexpoi$leucemie=="1"],cas_temoinsexpoi$agegestationnel.f[cas_temoinsexpoi$leucemie=="1"]),1)
prop.table(table(cas_temoinsexpoi$age.f[cas_temoinsexpoi$leucemie=="1"],cas_temoinsexpoi$agegestationnel.f2[cas_temoinsexpoi$leucemie=="1"]),2)

prop.table(table(cas_temoinsexpoi$poids.f[cas_temoinsexpoi$leucemie=="1"],cas_temoinsexpoi$age.f2[cas_temoinsexpoi$leucemie=="1"]),2)
prop.table(table(cas_temoinsexpoi$poids.f[cas_temoinsexpoi$leucemie=="1"],cas_temoinsexpoi$moyenne_benzene.f2[cas_temoinsexpoi$leucemie=="1"]),2)


#modeltestleucemie<-glm(cas~age.f2+ forte_expo + poids.f +vb+ agegestationnel.f2+parite.f3+sexe+most_dep ,family=binomial,data=cas_temoinsexpoi[cas_temoinsexpoi$leucemie==1,])
#summary(modeltestleucemie)

#modeltestleucemie2<-glm(cas~age.f+ moyenne_no2.f2 + poids.f +vb+ agegestationnel.f+ ageenf+parite.f2+sexe +most_dep,family=binomial,data=cas_temoinsexpoi[cas_temoinsexpoi$leucemie==1,])
#summary(modeltestleucemie)






###====TC====###



cas_temoinsexpoi$cas<-as.numeric(as.character(cas_temoinsexpoi$cas))
modeltc<-function(x){
  with(cas_temoinsexpoi[cas_temoinsexpoi$tc=="1",],{
    model1<-clogit(cas ~ x+strata(cas_temoinsexpoi$strates[cas_temoinsexpoi$tc=="1"]),method=c("exact"))
    #p<-round(summary(model1)$coefficient[2,4],3)
    s<-summary(model1)
    return(s)
    return(p)
  })
}


jesaispast<-apply(cas_temoinsexpoi[cas_temoinsexpoi$tc=="1",c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession",
                                                               "niveauetudes","most_dep","parite.fna","parite.f2",
                                                               "parite.f3na","gestite.fna","gestite.f2","sexe.fna","nbfoetus.f",
                                                               "agegestationnel.fna","agegestationnel.f2na",
                                                               "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2",
                                                               "vbna",
                                                               "poids.fna","poids.f3na","poids.f4na","poids.f5na","taille.fna","taille.f2na","coeffapgar5mncor.fna",
                                                              "allaitementna","moyenne_benzene.f","moyenne_no2.f","mopb.f","mopn.f","forte_expo","forte_expop",
                                                               "moyenne_benzene.f2","moyenne_no2.f2","mopb.f2","mopn.f2")],2,modeltc)
conf<-lapply(jesaispast,function(x){x$conf.int})
p<-lapply(jesaispast,function(x){x$coefficients[,5]})


nbr<-NULL
for (i in c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession",
            "niveauetudes","most_dep","parite.fna","parite.f2",
            "parite.f3na","gestite.fna","gestite.f2","sexe.fna","nbfoetus.f",
            "agegestationnel.fna","agegestationnel.f2na",
            "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2",
            "vbna",
            "poids.fna","poids.f3na","poids.f4na","poids.f5na","taille.fna","taille.f2na","coeffapgar5mncor.fna",
            "allaitementna","moyenne_benzene.f","moyenne_no2.f","mopb.f","mopn.f","forte_expo","forte_expop",
            "moyenne_benzene.f2","moyenne_no2.f2","mopb.f2","mopn.f2")) {
  c<-length(levels(droplevels(cas_temoinsexpoi[cas_temoinsexpoi$tc=="1",i])))-1
  nbr<-c(nbr,c)
}


legende<-rep(c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession",
               "niveauetudes","most_dep","parite.fna","parite.f2",
               "parite.f3na","gestite.fna","gestite.f2","sexe.fna","nbfoetus.f",
               "agegestationnel.fna","agegestationnel.f2na",
               "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2",
               "vbna",
               "poids.fna","poids.f3na","poids.f4na","poids.f5na","taille.fna","taille.f2na","coeffapgar5mncor.fna",
               "allaitementna","moyenne_benzene.f","moyenne_no2.f","mopb.f","mopn.f","forte_expo","forte_expop",
               "moyenne_benzene.f2","moyenne_no2.f2","mopb.f2","mopn.f2"),nbr)
resultor<-do.call(rbind,conf)
#resultor<-cbind(legende,resultor)
pro<-unlist(p)
resultor<-cbind(resultor,pro)
resultor<-round(resultor,2)
resultor<-cbind(legende,resultor)

write.table(resultor,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/test_univarie_tc_2.xls")


f <- function(d, i){
  d2 <- d[i,]
  model<-clogit(cas ~ age.f3na+strata(d2$strates[d2$tc=="1"]),data=d2[d2$tc=="1",],method=c("exact"))
  return(model$coefficients[1])
}


bootcorr <- boot(cas_temoinsexpoi, f, R=500)
bootcorr
boot.ci(bootcorr, type = "norm")








modeltestc<-clogit(cas~agegestationnel.f4 +age.f3+poids.f3+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tc==1,],method = c("exact"))
summary(modeltestc)


modeltestc<-clogit(cas~ age.f3na+poids.f3+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tc==1,],method = c("exact"))
summary(modeltestc)

modeltestc<-clogit(cas~ age.f3+poids.f3+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tc==1,],method = c("exact"))
summary(modeltestc)





modeltestcv<-clogit(cas~moyenne_no2.f +strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tc==1,],method = c("exact"))
summary(modeltestcv)



modeltestc2<-clogit(cas~agegestationnel.f2 +age.f3na+poids.f+ mopb.f2+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tc==1,],method = c("exact"))
summary(modeltestc2)

modeltestc3<-clogit(cas~agegestationnel.f2 +age.f3na+poids.f3+ mopn.f2+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tc==1,],method = c("exact"))
summary(modeltestc3)

### tableaux croisés
table(cas_temoinsexpoi$age.f[cas_temoinsexpoi$tc=="1"],cas_temoinsexpoi$agegestationnel.f2[cas_temoinsexpoi$tc=="1"])

prop.table(table(cas_temoinsexpoi$age.f[cas_temoinsexpoi$tc=="1"],cas_temoinsexpoi$agegestationnel.f2[cas_temoinsexpoi$tc=="1"]),2)


table(cas_temoinsexpoi$age.f[cas_temoinsexpoi$tc=="1"],cas_temoinsexpoi$agegestationnel.f2[cas_temoinsexpoi$tc=="1"])

prop.table(table(cas_temoinsexpoi$age.f[cas_temoinsexpoi$tc=="1"],cas_temoinsexpoi$agegestationnel.f2[cas_temoinsexpoi$tc=="1"]),2)



table(cas_temoinsexpoi$age.f[cas_temoinsexpoi$tc=="1"],cas_temoinsexpoi$sexe.f[cas_temoinsexpoi$tc=="1"])

prop.table(table(cas_temoinsexpoi$age.f[cas_temoinsexpoi$tc=="1"],cas_temoinsexpoi$sexe.f[cas_temoinsexpoi$tc=="1"]),2)


table(cas_temoinsexpoi$age.f[cas_temoinsexpoi$tc=="1"],cas_temoinsexpoi$most_dep[cas_temoinsexpoi$tc=="1"])

prop.table(table(cas_temoinsexpoi$age.f[cas_temoinsexpoi$tc=="1"],cas_temoinsexpoi$most_dep[cas_temoinsexpoi$tc=="1"]),2)


table(cas_temoinsexpoi$age.f[cas_temoinsexpoi$tc=="1"],cas_temoinsexpoi$moyenne_no2.f[cas_temoinsexpoi$tc=="1"])

prop.table(table(cas_temoinsexpoi$age.f[cas_temoinsexpoi$tc=="1"],cas_temoinsexpoi$moyenne_no2.f[cas_temoinsexpoi$tc=="1"]),2)



prop.table(table(cas_temoinsexpoi$poids.f[cas_temoinsexpoi$tc=="1"],cas_temoinsexpoi$age.f2[cas_temoinsexpoi$tc=="1"]),2)
prop.table(table(cas_temoinsexpoi$poids.f[cas_temoinsexpoi$tc=="1"],cas_temoinsexpoi$moyenne_benzene.f2[cas_temoinsexpoi$tc=="1"]),2)


# testons l'age avec la parite, et le niveau socio démo


modeltestc<-clogit(cas~ age.f3na+parite.f3+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tc==1,],method = c("exact"))
summary(modeltestc)

modeltestc<-clogit(cas~ age.f3na+most_dep+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tc==1,],method = c("exact"))
summary(modeltestc)

# testons le poids avec l'age gesta, voie d'accouchement

modeltestc<-clogit(cas~ poids.f3+agegestationnel.f2na+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tc==1,],method = c("exact"))
summary(modeltestc)


modeltestc<-clogit(cas~ poids.f3+naissancepar.f2+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tc==1,],method = c("exact"))
summary(modeltestc)


modeltestc<-clogit(cas~ poids.f3+taille.f+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tc==1,],method = c("exact"))
summary(modeltestc)


table(cas_temoinsexpoi$poids.f3[cas_temoinsexpoi$tc=="1"],cas_temoinsexpoi$taille.f[cas_temoinsexpoi$tc=="1"])

prop.table(table(cas_temoinsexpoi$poids.f3[cas_temoinsexpoi$tc=="1"],cas_temoinsexpoi$taille.f[cas_temoinsexpoi$tc=="1"]),2)


### expo et poids et gae 


modeltestc<-clogit(cas~ poids.f3+age.f3na+moyenne_no2.f2 +strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tc==1,],method = c("exact"))
summary(modeltestc)

modeltestc<-clogit(cas~ poids.f3+age.f3na+moyenne_benzene.f2 +strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tc==1,],method = c("exact"))
summary(modeltestc)





###====tembryonnaire====###
cas_temoinsexpoi$cas<-as.numeric(as.character(cas_temoinsexpoi$cas))
modelte<-function(x){
  with(cas_temoinsexpoi[cas_temoinsexpoi$tembryonnaire=="1",],{
    model1<-clogit(cas ~ x+strata(cas_temoinsexpoi$strates[cas_temoinsexpoi$tembryonnaire=="1"]),method=c("exact"))
    #p<-round(summary(model1)$coefficient[2,4],3)
    s<-summary(model1)
    return(s)
    return(p)
  })
}


jesaispaste<-apply(cas_temoinsexpoi[cas_temoinsexpoi$tembryonnaire=="1",c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession",
                                                                          "niveauetudes","most_dep","parite.fna","parite.f2",
                                                                          "parite.f3na","gestite.fna","gestite.f2","sexe.fna","nbfoetus.f",
                                                                          "agegestationnel.fna","agegestationnel.f2na",
                                                                          "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2",
                                                                          "vbna",
                                                                          "poids.fna","poids.f3na","poids.f4na","poids.f5na","taille.fna","taille.f2na","coeffapgar5mncor.fna",
                                                                          "allaitementna","moyenne_benzene.f","moyenne_no2.f","mopb.f","mopn.f","forte_expo","forte_expop",
                                                                          "moyenne_benzene.f2","moyenne_no2.f2","mopb.f2","mopn.f2")],2,modelte)
conf<-lapply(jesaispaste,function(x){x$conf.int})
p<-lapply(jesaispaste,function(x){x$coefficients[,5]})

nbr<-NULL
for (i in c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession",
            "niveauetudes","most_dep","parite.fna","parite.f2",
            "parite.f3na","gestite.fna","gestite.f2","sexe.fna","nbfoetus.f",
            "agegestationnel.fna","agegestationnel.f2na",
            "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2",
            "vbna",
            "poids.fna","poids.f3na","poids.f4na","poids.f5na","taille.fna","taille.f2na","coeffapgar5mncor.fna",
            "allaitementna","moyenne_benzene.f","moyenne_no2.f","mopb.f","mopn.f","forte_expo","forte_expop",
            "moyenne_benzene.f2","moyenne_no2.f2","mopb.f2","mopn.f2")) {
  c<-length(levels(droplevels(cas_temoinsexpoi[cas_temoinsexpoi$tembryonnaire=="1",i])))-1
  nbr<-c(nbr,c)
}


legende<-rep(c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession",
               "niveauetudes","most_dep","parite.fna","parite.f2",
               "parite.f3na","gestite.fna","gestite.f2","sexe.fna","nbfoetus.f",
               "agegestationnel.fna","agegestationnel.f2na",
               "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2",
               "vbna",
               "poids.fna","poids.f3na","poids.f4na","poids.f5na","taille.fna","taille.f2na","coeffapgar5mncor.fna",
               "allaitementna","moyenne_benzene.f","moyenne_no2.f","mopb.f","mopn.f","forte_expo","forte_expop",
               "moyenne_benzene.f2","moyenne_no2.f2","mopb.f2","mopn.f2"),nbr)
resultor<-do.call(rbind,conf)
#resultor<-cbind(legende,resultor)
pro<-unlist(p)

resultor<-cbind(resultor,pro)
resultor<-round(resultor,2)
resultor<-cbind(legende,resultor)

write.table(resultor,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/test_univarie_te_2.xls")


modeltestc<-clogit(cas~sexe+agegestationnel.f2na+poids.f4+mopb.f2+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tembryonnaire==1,],method = c("exact"))
summary(modeltestc)

modeltest2<-clogit(cas~sexe+agegestationnel.f2na+poids.f4+mopn.f2+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tembryonnaire==1,],method = c("exact"))
summary(modeltest2)

modeltest3<-clogit(cas~sexe+parite.f3na+agema+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tembryonnaire==1,],method = c("exact"))
summary(modeltest3)

### tableau croisé 

table(cas_temoinsexpoi$age.f[cas_temoinsexpoi$tembryonnaire=="1"],cas_temoinsexpoi$poids.f[cas_temoinsexpoi$tembryonnaire=="1"])

prop.table(table(cas_temoinsexpoi$age.f[cas_temoinsexpoi$tembryonnaire=="1"],cas_temoinsexpoi$poids.f[cas_temoinsexpoi$tembryonnaire=="1"]),2)


modeltestc<-clogit(cas~age.f2na+poids.f3+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tembryonnaire==1,],method = c("exact"))
summary(modeltestc)


modeltestc<-clogit(cas~agegestationnel.f4+poids.f3+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tembryonnaire==1,],method = c("exact"))
summary(modeltestc)
modeltestc<-clogit(cas~taille.fna+poids.f3+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tembryonnaire==1,],method = c("exact"))
summary(modeltestc)


modeltestc<-clogit(cas~naissancepar.f2+poids.f3+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tembryonnaire==1,],method = c("exact"))
summary(modeltestc)


table(cas_temoinsexpoi$age.f[cas_temoinsexpoi$tembryonnaire=="1"],cas_temoinsexpoi$sexe[cas_temoinsexpoi$tembryonnaire=="1"])

prop.table(table(cas_temoinsexpoi$age.f[cas_temoinsexpoi$tembryonnaire=="1"],cas_temoinsexpoi$sexe[cas_temoinsexpoi$tembryonnaire=="1"]),2)

table(cas_temoinsexpoi$parite.f3na[cas_temoinsexpoi$tembryonnaire=="1"],cas_temoinsexpoi$sexe[cas_temoinsexpoi$tembryonnaire=="1"])

prop.table(table(cas_temoinsexpoi$parite.f3na[cas_temoinsexpoi$tembryonnaire=="1"],cas_temoinsexpoi$sexe[cas_temoinsexpoi$tembryonnaire=="1"]),2)

table(cas_temoinsexpoi$parite.f3na[cas_temoinsexpoi$tembryonnaire=="1"],cas_temoinsexpoi$poids.f[cas_temoinsexpoi$tembryonnaire=="1"])
prop.table(table(cas_temoinsexpoi$parite.f3na[cas_temoinsexpoi$tembryonnaire=="1"],cas_temoinsexpoi$poids.f[cas_temoinsexpoi$tembryonnaire=="1"]),2)


table(cas_temoinsexpoi$agegestationnel.f4[cas_temoinsexpoi$tembryonnaire=="1"],cas_temoinsexpoi$poids.f3[cas_temoinsexpoi$tembryonnaire=="1"])
prop.table(table(cas_temoinsexpoi$agegestationnel.f4[cas_temoinsexpoi$tembryonnaire=="1"],cas_temoinsexpoi$poids.f3[cas_temoinsexpoi$tembryonnaire=="1"]),2)

modeltest2<-clogit(cas~sexe+age.f4na+parite.f3+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tembryonnaire==1,],method = c("exact"))
summary(modeltest2)
