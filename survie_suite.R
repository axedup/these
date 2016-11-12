### Description de la cohorte###

### on va supprimer les décès
cohorte$mprofession<-as.factor(cohorte$mprofession)
cohorte$pprofession<-as.factor(cohorte$pprofession)
cohorte$annee<-year(cohorte$datenaissance)

# après réflexion on ne garde que ceux nés en Ilde France et ceux qui sont nés et résident dans le m département  ! 


cohorte_sd<-cohorte[is.na(cohorte$age_deces_jj) & is.na(cohorte$age_deces_hh) & substr(cohorte$commune_nais_code,1,2) %in% c("75","78","95") ,]
dim(cohorte)
dim(cohorte_sd)

cohorte_sd<-cohorte_sd[! is.na(cohorte_sd$commune_nais_code) & !  is.na(cohorte_sd$commune_famille_code),]

cohorte_sd<-cohorte_sd[substr(cohorte_sd$commune_nais_code,1,2)==substr(cohorte_sd$commune_famille_code,1,2),]
dim(cohorte_sd)


cohorte$Source<-as.factor(cohorte$Source)

nomx<-c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
        "parite.f3","gestite.f","gestite.f2","sexe","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
        "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
        "poids.f","poids.f3","poids.f4","poids.f5","taille.f","taille.f2","coeffapgar5mncor.f","coeffapgar5mn.f2"
)

j<-1
B<-NULL
for (i in cohorte[,c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
                     "parite.f3","gestite.f","gestite.f2","sexe","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
                     "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
                     "poids.f","poids.f3","poids.f4","poids.f5","taille.f","taille.f2","coeffapgar5mncor.f"
                     ,"coeffapgar5mn.f2")
                  ] ){
  b<-test.qual(x=i,y=cohorte$Source,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}


write.table(B,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/cohorte_descript_source.xls",sep="\t")



nomx<-c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
        "parite.f3","gestite.f","gestite.f2","sexe","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
        "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
        "poids.f","poids.f3","poids.f4","poids.f5","taille.f","taille.f2","coeffapgar5mncor.f","coeffapgar5mn.f2"
)

j<-1
B<-NULL
for (i in cohorte[,c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
                     "parite.f3","gestite.f","gestite.f2","sexe","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
                     "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
                     "poids.f","poids.f3","poids.f4","poids.f5","taille.f","taille.f2","coeffapgar5mncor.f"
                     ,"coeffapgar5mn.f2")
                  ] ){
  b<-test.qual(x=i,y=cohorte$cas,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}


test.quant(varquant = c("tailles","poids","perimetre2"),varqual = "cas",nomquant = c("tailles","poids","perimetre"),nomqual = "cas",data=cohorte,savefile=T,
           fichier=)



test.quant(varquant = "tailles",varqual = "cas",nomquant = "tailles",nomqual = "cas",data=cohorte)



nomx<-c("oxygenotherapie", 
        "intubation", "antibiotherapie", "neurologique", "urgence", 
        "anomalie", "polymalformation", "spinabifida", "fente", "atresie", 
        "omphalocele", "reductionmembre", "malformrenale", "hydrocephalie", 
        "malformcard", "trisomie")

j<-1
B<-NULL
for (i in cohorte[,c("oxygenotherapie", 
                     "intubation", "antibiotherapie", "neurologique", "urgence",
                     "anomalie", "polymalformation", "spinabifida", "fente", "atresie", 
                     "omphalocele", "reductionmembre", "malformrenale", "hydrocephalie", 
                     "malformcard", "trisomie")
                  ] ){
  b<-test.qual(x=i,y=cohorte$Source,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}

write.table(B,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/cohorte_descript_source_suite.xls",sep="\t")


nomx<-c("oxygenotherapie", 
        "intubation", "antibiotherapie", "neurologique", "urgence", 
        "anomalie", "polymalformation", "spinabifida", "fente", "atresie", 
        "omphalocele", "reductionmembre", "malformrenale", "hydrocephalie", 
        "malformcard", "trisomie")

j<-1
B<-NULL
for (i in cohorte[,c("oxygenotherapie", 
                     "intubation", "antibiotherapie", "neurologique", "urgence",
                     "anomalie", "polymalformation", "spinabifida", "fente", "atresie", 
                     "omphalocele", "reductionmembre", "malformrenale", "hydrocephalie", 
                     "malformcard", "trisomie")
                  ] ){
  b<-test.qual(x=i,y=cohorte$cas,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}




###====================== leucémie===========================

cohorte_sd_leucemie<-cohorte_sd[cohorte_sd$leucemie==1| (cohorte_sd$cas==0),]
cohorte_sd_leucemie$cas<-as.factor(as.character(cohorte_sd_leucemie$cas))

nomx<-c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
        "parite.f3","gestite.f","gestite.f2","sexe","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
        "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
        "poids.f","poids.f3","poids.f4","poids.f5","taille.f","taille.f2","coeffapgar5mncor.f","coeffapgar5mn.f2"
)

j<-1
B<-NULL
for (i in cohorte_sd_leucemie[,c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
                                 "parite.f3","gestite.f","gestite.f2","sexe","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
                                 "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
                                 "poids.f","poids.f3","poids.f4","poids.f5","taille.f","taille.f2","coeffapgar5mncor.f","coeffapgar5mn.f2"
)
] ){
  b<-test.qual(x=i,y=cohorte_sd_leucemie$cas,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}

write.table(B,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/cohorte_leucemie2.xls",sep="\t")
#☻test.quant(varquant = c("tailles","poids","perimetre2"),varqual = "cas",nomquant = c("tailles","poids","perimetre"),nomqual = "cas",data=cohorte,savefile=T,fichier=)


###

model<-function(x){
  with(cohorte_sd_leucemie,{
    model1<-glm(cas ~x,family="poisson")
    #p<-round(summary(model1)$coefficient[2,4],3)
    s<-summary(model1)$coefficients
    hr<-exp(s[,1])
    conf_inf<-exp(s[,1]+1.960*s[,2])
    conf_sup<-exp(s[,1]-1.960*s[,2])
    s<-cbind(s,hr,conf_sup,conf_inf)
    return(s)
    return(p)
  })
}



cohorte_sd_leucemie$cas<-as.numeric(as.character(cohorte_sd_leucemie$cas))
cohorte_sd_leucemie$coeffapgar5mn.f2na<-as.factor(cohorte_sd_leucemie$coeffapgar5mn.f2na)

 
  jesaispas<-apply(cohorte_sd_leucemie[,c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
                                          "parite.f3na","gestite.fna","gestite.fna","sexe.fna","nbfoetus.fna","agegestationnel.fna",
                                          "agegestationnel.f2na",
                                          "agegestationnel.f3na","agegestationnel.f4na","naissancepar","naissancepar.f2","vbna",
                                          "poids.fna","poids.f3na","poids.f4na","poids.f5na","taille.fna",
                                          "taille.f2na","coeffapgar5mncor.fna","coeffapgar5mn.f2na")
                                       ],2,model)
  #conf<-lapply(jesaispas,function(x){x$conf.int})
  #p<-lapply(jesaispas,function(x){x$coefficients[,5]})
  
  resultor<-do.call(rbind,jesaispas)
  resultor<-resultor[!rownames(resultor)=="(Intercept)",]
  #jesaispas<-apply(cohorte_sd_leucemie[,c("poids.fna","agegestationnel.fna")],2,model)
  
  
  
  
  
  #summary(glm(cas ~parite.f3na,family=binomial(logit),data=cohorte_sd_leucemie))
  #exp(confint(glm(cas ~parite.f3na,family=binomial(logit),data=cohorte_sd_leucemie)))
  
  #summary(glm(cas ~poids.fna,family=binomial(logit),data=cohorte_sd_leucemie))$coefficients->s
  #exp(confint(glm(cas ~poids.fna,family=binomial(logit),data=cohorte_sd_leucemie)))
  
  
  
  nbr<-NULL
  for (i in c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
               "parite.f3na","gestite.fna","gestite.fna","sexe.fna","nbfoetus.fna","agegestationnel.fna",
               "agegestationnel.f2na",
               "agegestationnel.f3na","agegestationnel.f4na","naissancepar","naissancepar.f2","vbna",
               "poids.fna","poids.f3na","poids.f4na","poids.f5na","taille.fna","taille.f2na",
               "coeffapgar5mncor.fna","coeffapgar5mn.f2na")
  ) {
    c<-nlevels(cohorte_sd_leucemie[,i])-1
    nbr<-c(nbr,c)
  }
  
  
  
  legende<-rep(c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
                "parite.f3na","gestite.fna","gestite.fna","sexe.fna","nbfoetus.fna","agegestationnel.fna",
                "agegestationnel.f2na",
                "agegestationnel.f3na","agegestationnel.f4na","naissancepar","naissancepar.f2","vbna",
                "poids.fna","poids.f3na","poids.f4na","poids.f5na","taille.fna","taille.f2na",
                "coeffapgar5mncor.fna","coeffapgar5mn.f2na"),nbr)
  #resultor<-do.call(rbind,conf)
  #pro<-unlist(p)
  
 
  resultor<-round(resultor,2)
  resultor<-cbind(legende,resultor[,c("hr","conf_sup","conf_inf","Pr(>|z|)")])
  
  write.table(resultor,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/test_univarie_leucemiec.xls")

  #glm(cas ~sexe,family=binomial(logit),data=cohorte_sd_leucemie)
  
  poisson_leu<-summary(glm(cas~sexe.fna+agegestationnel.f4na+poids.f3na+parite.f3na+age.f3na, ,family="poisson",data=cohorte_sd_leucemie))$coefficients
  hr<-exp(poisson_leu[,1])
  conf_inf<-exp(poisson_leu[,1]+1.960*poisson_leu[,2])
  conf_sup<-exp(poisson_leu[,1]-1.960*poisson_leu[,2])
  s<-cbind(poisson_leu,hr,conf_sup,conf_inf)
  leucemie_pouisson<-round(s,2)
  
  test<-bigglm(cas_n ~sexe.fna+agegestationnel.f2na+poids.f3na,family=poisson(),data=cohorte_sd_leucemie,chuncksize=1000,maxit=1000)

  
  
  ###==========survie  
  
  cohorte_sd_leucemie$delai<-difftime(cohorte_sd_leucemie$ddp,cohorte_sd_leucemie$datenaissance)
  
  cohorte_sd_leucemie$delai<-as.numeric(cohorte_sd_leucemie$delai)/365
  cohorte_sd_leucemie$cas<-as.numeric(as.character(cohorte_sd_leucemie$cas))
  
  par(mfrow=c(1,1))
  
  modelsurvie<-function(x){
    with(cohorte_sd_leucemie,{
      model1<-coxph( Surv(cohorte_sd_leucemie$delai,cohorte_sd_leucemie$cas)~age.fna,cohorte_sd_leucemie)
      #p<-round(summary(model1)$coefficient[2,4],3)
      s<-summary(model1)$coefficients
      rr<-exp(s[,1])
      conf_inf<-exp(s[,1]+1.960*s[,2])
      conf_sup<-exp(s[,1]-1.960*s[,2])
      s<-cbind(s,rr,conf_sup,conf_inf)
     
      return(s)
      return(p)
    })
  }
  
  
  jesaispass<-apply(cohorte_sd_leucemie[,c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
                                          "parite.f3na","gestite.fna","gestite.fna","sexe.fna","nbfoetus.fna","agegestationnel.fna",
                                          "agegestationnel.f2na",
                                          "agegestationnel.f3na","agegestationnel.f4na","naissancepar","naissancepar.f2","vbna",
                                          "poids.fna","poids.f3na","poids.f4na","poids.f5na","taille.fna",
                                          "taille.f2na","coeffapgar5mncor.fna","coeffapgar5mn.f2na","allaitementna")
                                       ],2,modelsurvie)
  #conf<-lapply(jesaispas,function(x){x$conf.int})
  #p<-lapply(jesaispas,function(x){x$coefficients[,5]})
  
  resultor<-do.call(rbind,jesaispass)
  #resultor<-resultor[!rownames(resultor)=="(Intercept)",]
  #jesaispas<-apply(cohorte_sd_leucemie[,c("poids.fna","agegestationnel.fna")],2,model)
  
 
  nbr<-NULL
  for (i in c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
              "parite.f3na","gestite.fna","gestite.fna","sexe.fna","nbfoetus.fna","agegestationnel.fna",
              "agegestationnel.f2na",
              "agegestationnel.f3na","agegestationnel.f4na","naissancepar","naissancepar.f2","vbna",
              "poids.fna","poids.f3na","poids.f4na","poids.f5na","taille.fna","taille.f2na",
              "coeffapgar5mncor.fna","coeffapgar5mn.f2na","allaitementna")
  ) {
    c<-nlevels(cohorte_sd_leucemie[,i])-1
    nbr<-c(nbr,c)
  }
  
  
  
  legende<-rep(c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
                 "parite.f3na","gestite.fna","gestite.fna","sexe.fna","nbfoetus.fna","agegestationnel.fna",
                 "agegestationnel.f2na",
                 "agegestationnel.f3na","agegestationnel.f4na","naissancepar","naissancepar.f2","vbna",
                 "poids.fna","poids.f3na","poids.f4na","poids.f5na","taille.fna","taille.f2na",
                 "coeffapgar5mncor.fna","coeffapgar5mn.f2na","allaitementna"),nbr)
  #resultor<-do.call(rbind,conf)
  #pro<-unlist(p)
  
  
  resultor<-round(resultor,2)
  resultor<-cbind(legende,resultor[,c("hr","conf_sup","conf_inf","Pr(>|z|)")])
  
  write.table(resultor,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/test_univarie_leucemie_surviec.xls")
  
  
  
  
  
  
  
  
  
  
  
    
  
  
s<- Surv(cohorte_sd_leucemie$delai,cohorte_sd_leucemie$cas)
 
km_ageges<-survfit(s~agegestationnel.f2na,cohorte_sd_leucemie)
plot(km_ageges,ylim=c(0.999,1),mark.time = F)


su<-coxph(s ~agegestationnel.f3na+poids.f3na+ sexe.fna,cohorte_sd_leucemie)
sut<-coxph(s ~poids.fna,cohorte_sd_leucemie)

sut<-survdiff(s ~poids.fna,cohorte_sd_leucemie)
plot(cox.zph(su))

sul<-coxph(s ~ridge(sexe,theta=5),cohorte_sd_leucemie)


glm(cas_n ~agegestationnel.f2na+ ,family="poisson",data=cohorte_sd_leucemie)
