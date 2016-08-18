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