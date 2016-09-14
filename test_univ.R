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

jesaispas<-apply(cas_temoinsexpoi[,c("mopb.f2","mopn.f2","poids.fna","agegestationnel.fna","age.f2na","agegestationnel.f2","age.fna","moyenne_benzene.f","moyenne_no2.f","gestite.fna","parite.fna","sexe.fna","mopb.f","mopn.f","coeffapgar5mncor.fna","moyenne_benzene.f2","moyenne_no2.f2","vbna","most_dep","parite.f3","forte_expo","forte_expop")],2,model)
conf<-lapply(jesaispas,function(x){x$conf.int})
p<-lapply(jesaispas,function(x){x$coefficients[,5]})


nbr<-NULL
for (i in c("mopb.f2","mopn.f2","poids.fna","agegestationnel.fna","age.f2na","agegestationnel.f2na","age.fna","moyenne_benzene.f","moyenne_no2.f","gestite.fna","parite.fna","sexe.fna","mopb.f","mopn.f","coeffapgar5mncor.fna","moyenne_benzene.f2","moyenne_no2.f2","vbna","most_dep","parite.f3","forte_expo","forte_expop")) {
  c<-nlevels(cas_temoinsexpoi[,i])-1
  nbr<-c(nbr,c)
}



legende<-rep(c("mopb.f2","mopn.f2","poids.fna","agegestationnel.fna","age.f2na","agegestationnel.f2na","age.fna","moyenne_benzene.f","moyenne_no2.f","gestite.fna","parite.fna","sexe.fna","mopb.f","mopn.f","coeffapgar5mncor.fna","moyenne_benzene.f2","moyenne_no2.f2","vbna","most_dep","parite.f3na","forte_expo","forte_expop"),nbr)
resultor<-do.call(rbind,conf)
pro<-unlist(p)
resultor<-cbind(legende,resultor,pro)

write.table(resultor,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/test_univarie_tp.xls")

cas_temoinsexpoi$poids.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$poids.f),"NA",cas_temoinsexpoi$poids.f))
cas_temoinsexpoi$poids.f3na<-as.factor(ifelse(is.na(cas_temoinsexpoi$poids.f3),"NA",cas_temoinsexpoi$poids.f3))

cas_temoinsexpoi$agegestationnel.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$agegestationnel.f),"NA",cas_temoinsexpoi$agegestationnel.f))
cas_temoinsexpoi$agegestationnel.f2na<-as.factor(ifelse(is.na(cas_temoinsexpoi$agegestationnel.f2),"NA",cas_temoinsexpoi$agegestationnel.f2))
cas_temoinsexpoi$age.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$age.f),"NA",cas_temoinsexpoi$age.f))
cas_temoinsexpoi$age.f2na<-as.factor(ifelse(is.na(cas_temoinsexpoi$age.f2),"NA",cas_temoinsexpoi$age.f2))
cas_temoinsexpoi$age.f3na<-as.factor(ifelse(is.na(cas_temoinsexpoi$age.f3),"NA",cas_temoinsexpoi$age.f3))
cas_temoinsexpoi$parite.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$parite.f2),"NA",cas_temoinsexpoi$parite.f2))
cas_temoinsexpoi$gestite.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$gestite.f2),"NA",cas_temoinsexpoi$gestite.f2))

cas_temoinsexpoi$parite.f3na<-as.factor(ifelse(is.na(cas_temoinsexpoi$parite.f3),"NA",cas_temoinsexpoi$parite.f3))
#cas_temoinsexpoi$gestite.f3na<-as.factor(ifelse(is.na(cas_temoinsexpoi$gestite.f3),"NA",cas_temoinsexpoi$gestite.f3))


cas_temoinsexpoi$sexe.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$sexe),"NA",cas_temoinsexpoi$sexe))
cas_temoinsexpoi$coeffapgar5mncor.fna<-as.factor(ifelse(is.na(cas_temoinsexpoi$coeffapgar5mncor.f),"NA",cas_temoinsexpoi$coeffapgar5mncor.f))
cas_temoinsexpoi$vbna<-as.factor(ifelse(is.na(cas_temoinsexpoi$vb),"NA",cas_temoinsexpoi$vb))


### modèle finaux 
clogit(cas ~ poids.f +agegestationnel.f+ age.fna + parite.fna+sexe.fna+ coeffapgar5mncor.fna+ most_dep+mopb.f2+strata(cas_temoinsexpoi$strates),data=cas_temoinsexpoi,method=c("exact"))

## modèle avec NA 
clogit(cas ~ poids.f +agegestationnel.f+ age.f + parite.f2+sexe+ coeffapgar5mncor.f+ mopb.f2+ most_dep+strata(cas_temoinsexpoi$strates),data=cas_temoinsexpoi,method=c("exact"))


clogit(cas ~ age.f +strata(cas_temoinsexpoi$strates),data=cas_temoinsexpoi,method=c("exact"))
clogit(cas ~ agegestationnel.f +strata(cas_temoinsexpoi$strates),data=cas_temoinsexpoi,method=c("exact"))
clogit(cas ~ sexe +strata(cas_temoinsexpoi$strates),data=cas_temoinsexpoi,method=c("exact"))
clogit(cas ~ vb +strata(cas_temoinsexpoi$strates),data=cas_temoinsexpoi,method=c("exact"))



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





jesaispasl<-apply(cas_temoinsexpoi[cas_temoinsexpoi$leucemie=="1",c("mopb.f2","mopn.f2","poids.fna","agegestationnel.fna","age.f2na","agegestationnel.f2na","age.fna","moyenne_benzene.f","moyenne_no2.f","gestite.fna","parite.fna","sexe.fna","mopb.f","mopn.f","coeffapgar5mncor.fna","moyenne_benzene.f2","moyenne_no2.f2","vbna","most_dep","parite.f3na","forte_expo","forte_expop","age.f3na")],2,modell)
conf<-lapply(jesaispasl,function(x){x$conf.int})
p<-lapply(jesaispasl,function(x){x$coefficients[,5]})



nbr<-NULL
for (i in c("mopb.f2","mopn.f2","poids.fna","agegestationnel.fna","age.f2na","agegestationnel.f2na","age.fna","moyenne_benzene.f","moyenne_no2.f","gestite.fna","parite.fna","sexe.fna","mopb.f","mopn.f","coeffapgar5mncor.fna","moyenne_benzene.f2","moyenne_no2.f2","vbna","most_dep","parite.f3na","forte_expo","forte_expop","age.f3na")) {
  c<-length(levels(droplevels(cas_temoinsexpoi[cas_temoinsexpoi$leucemie=="1",i])))-1
  nbr<-c(nbr,c)
}



legende<-rep(c("mopb.f2","mopn.f2","poids.fna","agegestationnel.fna","age.f2na","agegestationnel.f2na","age.fna","moyenne_benzene.f","moyenne_no2.f","gestite.fna","parite.fna","sexe.fna","mopb.f","mopn.f","coeffapgar5mncor.fna","moyenne_benzene.f2","moyenne_no2.f2","vbna","most_dep","parite.f3na","forte_expo","forte_expop","age.f3na"),nbr)
resultor<-do.call(rbind,conf)
#resultor<-cbind(legende,resultor)
pro<-unlist(p)
resultor<-cbind(legende,resultor,pro)

write.table(resultor,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/test_univarie_leucemie.xls")




model1l<-clogit(cas ~ vbna+age.f3na+mopb.f2+strata(cas_temoinsexpoi$strates[cas_temoinsexpoi$leucemie=="1"]),method=c("exact"),data=cas_temoinsexpoi[cas_temoinsexpoi$leucemie=="1",])
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



model2l<-clogit(cas ~ vbna+age.f3na+moyenne_no2.f2+strata(cas_temoinsexpoi$strates[cas_temoinsexpoi$leucemie=="1"]),method=c("exact"),data=cas_temoinsexpoi[cas_temoinsexpoi$leucemie=="1",])
summary(model2l)


model3l<-clogit(cas ~ vbna+age.f3na+mopb.f2+strata(cas_temoinsexpoi$strates[cas_temoinsexpoi$leucemie=="1"]),method=c("exact"),data=cas_temoinsexpoi[cas_temoinsexpoi$leucemie=="1",])
summary(model3l)

model4l<-clogit(cas ~ vbna+age.f3na+forte_expop+strata(cas_temoinsexpoi$strates[cas_temoinsexpoi$leucemie=="1"]),method=c("exact"),data=cas_temoinsexpoi[cas_temoinsexpoi$leucemie=="1",])
summary(model4l)






#modeltestleucemie<-glm(cas~age.f2+ forte_expo + poids.f +vb+ agegestationnel.f2+parite.f3+sexe+most_dep ,family=binomial,data=cas_temoinsexpoi[cas_temoinsexpoi$leucemie==1,])
#summary(modeltestleucemie)

#modeltestleucemie2<-glm(cas~age.f+ moyenne_no2.f2 + poids.f +vb+ agegestationnel.f+ ageenf+parite.f2+sexe +most_dep,family=binomial,data=cas_temoinsexpoi[cas_temoinsexpoi$leucemie==1,])
#summary(modeltestleucemie)






###====TC====###

modeltc<-function(x){
  with(cas_temoinsexpoi[cas_temoinsexpoi$tc=="1",],{
    model1<-clogit(cas ~ x+strata(cas_temoinsexpoi$strates[cas_temoinsexpoi$tc=="1"]),method=c("exact"))
    #p<-round(summary(model1)$coefficient[2,4],3)
    s<-summary(model1)
    return(s)
    return(p)
  })
}


jesaispast<-apply(cas_temoinsexpoi[cas_temoinsexpoi$tc=="1",c("mopb.f2","mopn.f2","poids.fna","agegestationnel.fna","age.f2na","agegestationnel.f2na","age.fna","moyenne_benzene.f","moyenne_no2.f","gestite.fna","parite.fna","sexe.fna","mopb.f","mopn.f","coeffapgar5mncor.fna","moyenne_benzene.f2","moyenne_no2.f2","vbna","most_dep","parite.f3na","forte_expo","forte_expop")],2,modeltc)
conf<-lapply(jesaispast,function(x){x$conf.int})
p<-lapply(jesaispast,function(x){x$coefficients[,5]})


nbr<-NULL
for (i in c("mopb.f2","mopn.f2","poids.fna","agegestationnel.fna","age.f2na","agegestationnel.f2na","age.fna","moyenne_benzene.f","moyenne_no2.f","gestite.fna","parite.fna","sexe.fna","mopb.f","mopn.f","coeffapgar5mncor.fna","moyenne_benzene.f2","moyenne_no2.f2","vbna","most_dep","parite.f3na","forte_expo","forte_expop")) {
  c<-length(levels(droplevels(cas_temoinsexpoi[cas_temoinsexpoi$leucemie=="1",i])))-1
  nbr<-c(nbr,c)
}


legende<-rep(c("mopb.f2","mopn.f2","poids.fna","agegestationnel.fna","age.f2na","agegestationnel.f2na","age.fna","moyenne_benzene.f","moyenne_no2.f","gestite.fna","parite.fna","sexe.fna","mopb.f","mopn.f","coeffapgar5mncor.fna","moyenne_benzene.f2","moyenne_no2.f2","vbna","most_dep","parite.f3na","forte_expo","forte_expop"),nbr)
resultor<-do.call(rbind,conf)
#resultor<-cbind(legende,resultor)
pro<-unlist(p)
resultor<-cbind(legende,resultor,pro)

write.table(resultor,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/test_univarie_tc.xls")


modeltestc<-clogit(cas~age.f2na +agegestationnel.f2+ poids.f + strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tc==1,],method = c("exact"))
summary(modeltestc)

###====tembryonnaire====###

modelte<-function(x){
  with(cas_temoinsexpoi[cas_temoinsexpoi$tembryonnaire=="1",],{
    model1<-clogit(cas ~ x+strata(cas_temoinsexpoi$strates[cas_temoinsexpoi$tembryonnaire=="1"]),method=c("exact"))
    #p<-round(summary(model1)$coefficient[2,4],3)
    s<-summary(model1)
    return(s)
    return(p)
  })
}


jesaispaste<-apply(cas_temoinsexpoi[cas_temoinsexpoi$tembryonnaire=="1",c("mopb.f2","mopn.f2","poids.fna","agegestationnel.fna","age.f2na","agegestationnel.f2na","age.fna","moyenne_benzene.f","moyenne_no2.f","gestite.fna","parite.fna","sexe.fna","mopb.f","mopn.f","coeffapgar5mncor.fna","moyenne_benzene.f2","moyenne_no2.f2","vbna","most_dep","parite.f3na","forte_expo","forte_expop")],2,modelte)
conf<-lapply(jesaispaste,function(x){x$conf.int})
p<-lapply(jesaispaste,function(x){x$coefficients[,5]})

nbr<-NULL
for (i in c("mopb.f2","mopn.f2","poids.fna","agegestationnel.fna","age.f2na","agegestationnel.f2na","age.fna","moyenne_benzene.f","moyenne_no2.f","gestite.fna","parite.fna","sexe.fna","mopb.f","mopn.f","coeffapgar5mncor.fna","moyenne_benzene.f2","moyenne_no2.f2","vbna","most_dep","parite.f3na","forte_expo","forte_expop")) {
  c<-length(levels(droplevels(cas_temoinsexpoi[cas_temoinsexpoi$tembryonnaire=="1",i])))-1
  nbr<-c(nbr,c)
}


legende<-rep(c("mopb.f2","mopn.f2","poids.fna","agegestationnel.fna","age.f2na","agegestationnel.f2na","age.fna","moyenne_benzene.f","moyenne_no2.f","gestite.fna","parite.fna","sexe.fna","mopb.f","mopn.f","coeffapgar5mncor.fna","moyenne_benzene.f2","moyenne_no2.f2","vbna","most_dep","parite.f3na","forte_expo","forte_expop"),nbr)
resultor<-do.call(rbind,conf)
#resultor<-cbind(legende,resultor)
pro<-unlist(p)
resultor<-cbind(legende,resultor,pro)


write.table(resultor,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/test_univarie_te.xls")


modeltestc<-clogit(cas~sexe+agegestationnel.f2na+poids.f3+mopb.f2+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tembryonnaire==1,],method = c("exact"))
summary(modeltestc)

modeltest2<-clogit(cas~sexe+agegestationnel.f2na+poids.f3+mopn.f2+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tembryonnaire==1,],method = c("exact"))
summary(modeltest2)

modeltest3<-clogit(cas~sexe+agegestationnel.f2na+poids.f3+forte_expop+strata(strates),data=cas_temoinsexpoi[cas_temoinsexpoi$tembryonnaire==1,],method = c("exact"))
summary(modeltest3)
