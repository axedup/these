### Tests univariés###

# boucle avec glm


# et ACP pourquoi pas ?

test<-cas_temoinsexpoi[!is.na(cas_temoinsexpoi$poids)& !is.na(cas_temoinsexpoi$age),c("poids.f","agegestationnel.f","age.f","parite.f","expo")]
test$poids.f<-as.numeric(test$poids.f)
test$agegestationnel.f<-as.numeric(test$agegestationnel.f)

test$age.f<-as.numeric(test$age.f)

pca<-prcomp(test)
100 * pca$sdev^2 / sum(pca$sdev^2)
                       