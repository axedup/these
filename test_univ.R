### Tests univariés###

# boucle avec glm

#summary(glm(cas~poids,data=cas_temoinsexpoi,family = binomial))


modeltest<-glm(cas~age.f + mopb.f + agegestationnel.f ,family=binomial,data=cas_temoinsexpoi)
modeltest2<-glm(cas~age.f + moyenne_benzene.f2 + poids.f +  agegestationnel.f+ ageenf+parite.f2, family=binomial,data=cas_temoinsexpoi)
summary(modeltest2)


modeltest3<-glm(cas~age.f + moyenne_no2.f2 + poids.f +  agegestationnel.f+ ageenf+parite.f2, family=binomial,data=cas_temoinsexpoi)
summary(modeltest3)


modeltestl<-glm(cas~age.f + moyenne_benzene+ poids.f, family=binomial,data=cas_temoinsexpoi)


model<-function(x){
  with(cas_temoinsexpoi,{
    model1<-glm(cas~x,family=binomial)
    p<-round(summary(model1)$coefficient[2,4],3)
    s<-summary(model1)
    return(s)
    return(p)
  })
}


apply(cas_temoinsexpoi[,c("mopb.f2","mopn.f2","poids.f","agegestationnel.f","age.f","moyenne_benzene.f","moyenne_no2.f","gestite.f2","parite.f2","sexe","mopb.f","mopn.f","coeffapgar5mncor.f","moyenne_benzene.f2","moyenne_no2.f2","vb")],2,model)


varlist<-c("poids.f","agegestationnel.f","age.f","moyenne_benzene.f","moyenne_no2.f","gestite.f2","parite.f2","sexe","mopb.f","mopn.f","coeffapgar5mncor.f","moyenne_benzene.f2","moyenne_no2.f2","mopb.f2","mopn.f2")
models <- lapply(varlist, function(x) {
  glm(substitute(cas ~ i, list(i = as.name(x))), data=cas_temoinsexpoi,family=binomial )
})
r<-lapply(models, summary)


conf<-lapply(models,confint)
lapply(conf,exp)










# et ACP pourquoi pas ?

test<-cas_temoinsexpoi[!is.na(cas_temoinsexpoi$poids)& !is.na(cas_temoinsexpoi$age),c("poids.f","agegestationnel.f","age.f","parite.f","expo")]
test$poids.f<-as.numeric(test$poids.f)
test$agegestationnel.f<-as.numeric(test$agegestationnel.f)

test$age.f<-as.numeric(test$age.f)

pca<-prcomp(test)
100 * pca$sdev^2 / sum(pca$sdev^2)
                       