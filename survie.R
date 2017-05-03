### Description de la cohorte###

### on va supprimer les décès
cohorte$mprofession<-as.factor(cohorte$mprofession)
cohorte$pprofession<-as.factor(cohorte$pprofession)
cohorte$annee<-year(cohorte$datenaissance)
cohorte$alcool.f<-ifelse(cohorte$alcool==0 & !is.na(cohorte$alcool),0,NA )
cohorte$alcool.f<-ifelse(!cohorte$alcool==0 & !is.na(cohorte$alcool),1,cohorte$alcool.f )

cohorte$alcool.fna<-ifelse(is.na(cohorte$alcool.f),"NA",cohorte$alcool.f )
cohorte$alcool.fna<-as.factor(cohorte$alcool.fna)
cohorte$alcool.f<-as.factor(cohorte$alcool.f)



cohorte$tabac.f<-ifelse(cohorte$tabac==0 & !is.na(cohorte$tabac),0,NA )
cohorte$tabac.f<-ifelse(!cohorte$tabac==0 & !is.na(cohorte$tabac),1,cohorte$tabac.f )

cohorte$tabac.fna<-ifelse(is.na(cohorte$tabac.f),"NA",cohorte$tabac.f )
cohorte$tabac.fna<-as.factor(cohorte$tabac.fna)
cohorte$tabac.f<-as.factor(cohorte$tabac.f)

cohorte$poids_age.fna<-ifelse(is.na(cohorte$poids_age.f),"NA",cohorte$poids_age.f )


# après réflexion on ne garde que ceux nés en Ilde France et ceux qui sont nés et résident dans le m département  ! 


cohorte_sd<-cohorte[is.na(cohorte$age_deces_jj) & is.na(cohorte$age_deces_hh) & substr(cohorte$commune_nais_code,1,2) %in% c("75","78","95") ,]
dim(cohorte)
dim(cohorte_sd)

cohorte_sd<-cohorte_sd[! is.na(cohorte_sd$commune_nais_code) & !  is.na(cohorte_sd$commune_famille_code),]

cohorte_sd<-cohorte_sd[substr(cohorte_sd$commune_nais_code,1,2)==substr(cohorte_sd$commune_famille_code,1,2),]
dim(cohorte_sd)
cohorte_sd<-cohorte_sd[!(cohorte_sd$cas==1 & cohorte_sd$delai>=5),]

cohorte_sd$Source<-as.factor(cohorte_sd$Source)

nomx<-c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
         "parite.f3","gestite.f","gestite.f2","sexe","tabac.f","alcool.f","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
         "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
         "poids.f","poids.f3","poids.f4","poids.f5","poids_age.f","taille.f","taille.f2","coeffapgar5mncor.f","coeffapgar5mn.f2",
         "allaitement"
)
j<-1
B<-NULL
for (i in cohorte_sd[,c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
                    "parite.f3","gestite.f","gestite.f2","sexe","tabac.f","alcool.f","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
                    "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
                    "poids.f","poids.f3","poids.f4","poids.f5","poids_age.f","taille.f","taille.f2","coeffapgar5mncor.f","coeffapgar5mn.f2",
                    "allaitement"
)
                  ] ){
  b<-test.qual(x=i,y=cohorte_sd$Source,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}


write.table(B,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/cohorte_sd_descript_source.xls",sep="\t")



nomx<-c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
       "parite.f3","gestite.f","gestite.f2","sexe","tabac.f","alcool.f","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
       "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
       "poids.f","poids.f3","poids.f4","poids.f5","poids_age.f","taille.f","taille.f2","coeffapgar5mncor.f","coeffapgar5mn.f2",
       "allaitement"
)

j<-1
B<-NULL
for (i in cohorte_sd[,c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
                     "parite.f3","gestite.f","gestite.f2","sexe","tabac.f","alcool.f","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
                     "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
                     "poids.f","poids.f3","poids.f4","poids.f5","poids_age.f","taille.f","taille.f2","coeffapgar5mncor.f","coeffapgar5mn.f2",
                     "allaitement"
)
                  ] ){
  b<-test.qual(x=i,y=cohorte_sd$cas,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}
write.table(B,file="G:/cohorte_sd_sd_descript_source.xls",sep="\t")



test.quant(varquant = c("tailles","poids","perimetre2"),varqual = "cas",nomquant = c("tailles","poids","perimetre"),nomqual = "cas",data=cohorte_sd,savefile=T,
           fichier=)



test.quant(varquant = "tailles",varqual = "cas",nomquant = "tailles",nomqual = "cas",data=cohorte_sd)



nomx<-c("oxygenotherapie", 
        "intubation", "antibiotherapie", "neurologique", "urgence", 
        "anomalie", "polymalformation", "spinabifida", "fente", "atresie", 
        "omphalocele", "reductionmembre", "malformrenale", "hydrocephalie", 
        "malformcard", "trisomie")

j<-1
B<-NULL
for (i in cohorte_sd[,c("oxygenotherapie", 
                     "intubation", "antibiotherapie", "neurologique", "urgence",
                     "anomalie", "polymalformation", "spinabifida", "fente", "atresie", 
                     "omphalocele", "reductionmembre", "malformrenale", "hydrocephalie", 
                     "malformcard", "trisomie")
                  ] ){
  b<-test.qual(x=i,y=cohorte_sd$Source,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}

write.table(B,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/cohorte_sd_descript_source_suite.xls",sep="\t")


nomx<-c("oxygenotherapie", 
        "intubation", "antibiotherapie", "neurologique", "urgence", 
        "anomalie", "polymalformation", "spinabifida", "fente", "atresie", 
        "omphalocele", "reductionmembre", "malformrenale", "hydrocephalie", 
        "malformcard", "trisomie")

j<-1
B<-NULL
for (i in cohorte_sd[,c("oxygenotherapie", 
                     "intubation", "antibiotherapie", "neurologique", "urgence",
                     "anomalie", "polymalformation", "spinabifida", "fente", "atresie", 
                     "omphalocele", "reductionmembre", "malformrenale", "hydrocephalie", 
                     "malformcard", "trisomie")
                  ] ){
  b<-test.qual(x=i,y=cohorte_sd$cas,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}

write.table(B,file="G:/cohorte_sd_descript_cas_suite.xls",sep="\t")



###====================== leucémie===========================

cohorte_sd_leucemie<-cohorte_sd[cohorte_sd$leucemie==1| (cohorte_sd$cas==0),]
cohorte_sd_leucemie<-cohorte_sd_leucemie[!(cohorte_sd_leucemie$cas==1 & cohorte_sd_leucemie$delai>=5),]
cohorte_sd_leucemie$cas<-as.factor(as.character(cohorte_sd_leucemie$cas))

nomx<-c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
        "parite.f3","gestite.f","gestite.f2","sexe","tabac.f","alcool.f","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
        "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
        "poids.f","poids.f3","poids.f4","poids.f5","poids_age.f","taille.f","taille.f2","coeffapgar5mncor.f","coeffapgar5mn.f2",
        "allaitement"
)

j<-1
B<-NULL
for (i in cohorte_sd_leucemie[,c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
                                 "parite.f3","gestite.f","gestite.f2","sexe","tabac.f","alcool.f","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
                                 "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
                                 "poids.f","poids.f3","poids.f4","poids.f5","poids_age.f","taille.f","taille.f2","coeffapgar5mncor.f","coeffapgar5mn.f2",
                                 "allaitement"
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
                                          "parite.f3na","gestite.fna","gestite.fna","sexe.fna","tabac.f","alcool.f",
                                          "nbfoetus.fna","agegestationnel.fna",
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
               "parite.f3na","gestite.fna","gestite.fna","sexe.fna","tabac.f","alcool.f","nbfoetus.fna","agegestationnel.fna",
               "agegestationnel.f2na",
               "agegestationnel.f3na","agegestationnel.f4na","naissancepar","naissancepar.f2","vbna",
               "poids.fna","poids.f3na","poids.f4na","poids.f5na","taille.fna","taille.f2na",
               "coeffapgar5mncor.fna","coeffapgar5mn.f2na")
  ) {
    c<-nlevels(cohorte_sd_leucemie[,i])-1
    nbr<-c(nbr,c)
  }
  
  
  
  legende<-rep(c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
                "parite.f3na","gestite.fna","gestite.fna","sexe.fna","tabac.f","alcool.f","nbfoetus.fna","agegestationnel.fna",
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

  
  
  ###==========survie===============  
  
  cohorte_sd_leucemie$delai<-difftime(cohorte_sd_leucemie$ddp,cohorte_sd_leucemie$datenaissance)
  
  cohorte_sd_leucemie$delai<-as.numeric(cohorte_sd_leucemie$delai)/365
  cohorte_sd_leucemie$cas<-as.numeric(as.character(cohorte_sd_leucemie$cas))
  
  par(mfrow=c(1,1))
  
  modelsurvie<-function(x){
    with(cohorte_sd_leucemie,{
      model1<-coxph( Surv(cohorte_sd_leucemie$delai,cohorte_sd_leucemie$cas)~x,cohorte_sd_leucemie)
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
                                          "parite.f3na","gestite.fna","gestite.fna","sexe.fna","tabac.f","alcool.f",
                                          "nbfoetus.fna","agegestationnel.fna",
                                          "agegestationnel.f2na",
                                          "agegestationnel.f3na","agegestationnel.f4na","naissancepar","naissancepar.f2","vbna",
                                          "poids.fna","poids.f3na","poids.f4na","poids.f5na","poids_age.fna","taille.fna",
                                          "taille.f2na","coeffapgar5mncor.fna","coeffapgar5mn.f2na","allaitementna")
                                       ],2,modelsurvie)
  #conf<-lapply(jesaispas,function(x){x$conf.int})
  #p<-lapply(jesaispas,function(x){x$coefficients[,5]})
  
  resultor<-do.call(rbind,jesaispass)
  #resultor<-resultor[!rownames(resultor)=="(Intercept)",]
  #jesaispas<-apply(cohorte_sd_leucemie[,c("poids.fna","agegestationnel.fna")],2,model)
  
 
  nbr<-NULL
  for (i in c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
              "parite.f3na","gestite.fna","gestite.fna","sexe.fna","tabac.f","alcool.f"
              ,"nbfoetus.fna","agegestationnel.fna",
              "agegestationnel.f2na",
              "agegestationnel.f3na","agegestationnel.f4na","naissancepar","naissancepar.f2","vbna",
              "poids.fna","poids.f3na","poids.f4na","poids.f5na","poids_age.fna","taille.fna","taille.f2na",
              "coeffapgar5mncor.fna","coeffapgar5mn.f2na","allaitementna")
  ) {
    c<-nlevels(cohorte_sd_leucemie[,i])-1
    nbr<-c(nbr,c)
  }
  
  
  
  legende<-rep(c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
                 "parite.f3na","gestite.fna","gestite.fna","sexe.fna","tabac.f","alcool.f"
                 ,"nbfoetus.fna",
                 "agegestationnel.fna",
                 "agegestationnel.f2na",
                 "agegestationnel.f3na","agegestationnel.f4na","naissancepar","naissancepar.f2","vbna",
                 "poids.fna","poids.f3na","poids.f4na","poids.f5na","poids_age.fna","taille.fna","taille.f2na",
                 "coeffapgar5mncor.fna","coeffapgar5mn.f2na","allaitementna"),nbr)
  #resultor<-do.call(rbind,conf)
  #pro<-unlist(p)
  
  
  resultor<-round(resultor,2)
  resultor<-cbind(legende,resultor[,c("rr","conf_sup","conf_inf","Pr(>|z|)")])
  
  write.table(resultor,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/test_univarie_leucemie_surviec.xls")
  
  
  
  
  numDEpi.default(power = 0.8, theta = 0.9, p = 0.39, rho2 = 0, alpha = 0.05)
  
  for (j in c(0.7,0.9,1.1,1.2,1.5) ){
    N<-NULL
    seq<-seq(0,100,by=1)
    for (i in seq){
      p2<-0.01*i
      n<-numDEpi.default(power = 0.8, theta =j, p = p2, rho2 = 0, alpha = 0.05)
      
      n<-ceiling(n)
      N<-c(N,n)
    }
    assign(paste("HR",j,sep=""),cbind(seq,N,rep(j,101)))
    
  }
  
  graph_sur<-cbind(HR0.7,HR0.9,HR1.1,HR1.2,HR1.5)
  graph_sur<-as.data.frame(graph_sur)
  names(graph_sur)<-c("seq", "N07", "V3", "seq2", "N09", "V6", "seq3", "N11", "V9", "seq4", 
                      "N12", "V12", "seq5", "N15", "V15")
  long_por<-reshape(graph_sur, varying=c("N07","N09","N11","N12","N15"),v.names="eff",timevar="rr",
                    times=c("V3",'V6',"V9","V12","V15"),direction="long"
  )
  long_por$eff<-as.numeric(as.character(long_por$eff))
  long_por$rr<-ifelse(long_por$rr=="V3","0.7",long_por$rr)
  long_por$rr<-ifelse(long_por$rr=="V6","0.9",long_por$rr)
  long_por$rr<-ifelse(long_por$rr=="V9","1.1",long_por$rr)
  long_por$rr<-ifelse(long_por$rr=="V12","1.2",long_por$rr)
  long_por$rr<-ifelse(long_por$rr=="V15","1.5",long_por$rr)
  long_por$hr<-long_por$rr
  
  ggplot(long_por,aes(seq,eff,colour=rr))+geom_line()+scale_x_continuous(name='pourcentage', limits=c(0,50),breaks=seq(0,50,2))+scale_y_continuous(name='effectifs', breaks=seq(0,25000,750),limits=c(0,25000))
  ggplot(long_por,aes(seq,eff,colour=hr))+geom_line()+scale_x_continuous(name='pourcentage', limits=c(10,50),breaks=seq(10,50,2))+scale_y_continuous(name='effectifs', breaks=seq(0,25000,750),limits=c(0,12000))+theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))
  
  
  
  
    
  
  
s<- Surv(cohorte_sd_leucemie$delai,cohorte_sd_leucemie$cas)
 
km_ageges<-survfit(s~agegestationnel.f2na,cohorte_sd_leucemie)
plot(km_ageges,ylim=c(0.999,1),mark.time = F)


su<-coxph(s ~poids.fna,cohorte_sd_leucemie)
# sut<-coxph(s ~poids.fna,cohorte_sd_leucemie)
# 
# sut<-survdiff(s ~poids.fna,cohorte_sd_leucemie)
plot(cox.zph(su)[1,])

sul<-coxph(s ~ridge(sexe,theta=5),cohorte_sd_leucemie)


glm(cas_n ~agegestationnel.f2na+ ,family="poisson",data=cohorte_sd_leucemie)



###====================== TC===========================

cohorte_sd_tc<-cohorte_sd[cohorte_sd$tc==1| (cohorte_sd$cas==0),]
cohorte_sd_tc$cas<-as.factor(as.character(cohorte_sd_tc$cas))

nomx<-c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
        "parite.f3","gestite.f","gestite.f2","sexe","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
        "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
        "poids.f","poids.f3","poids.f4","poids.f5","taille.f","taille.f2","coeffapgar5mncor.f","coeffapgar5mn.f2"
)

j<-1
B<-NULL
for (i in cohorte_sd_tc[,c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
                                 "parite.f3","gestite.f","gestite.f2","sexe","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
                                 "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
                                 "poids.f","poids.f3","poids.f4","poids.f5","taille.f","taille.f2","coeffapgar5mncor.f","coeffapgar5mn.f2"
)
] ){
  b<-test.qual(x=i,y=cohorte_sd_tc$cas,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}

write.table(B,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/cohorte_tc2.xls",sep="\t")
#☻test.quant(varquant = c("tailles","poids","perimetre2"),varqual = "cas",nomquant = c("tailles","poids","perimetre"),nomqual = "cas",data=cohorte,savefile=T,fichier=)


###

model<-function(x){
  with(cohorte_sd_tc,{
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



cohorte_sd_tc$cas<-as.numeric(as.character(cohorte_sd_tc$cas))
cohorte_sd_tc$coeffapgar5mn.f2na<-as.factor(cohorte_sd_tc$coeffapgar5mn.f2na)


jesaispas<-apply(cohorte_sd_tc[,c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
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
#jesaispas<-apply(cohorte_sd_tc[,c("poids.fna","agegestationnel.fna")],2,model)





#summary(glm(cas ~parite.f3na,family=binomial(logit),data=cohorte_sd_tc))
#exp(confint(glm(cas ~parite.f3na,family=binomial(logit),data=cohorte_sd_tc)))

#summary(glm(cas ~poids.fna,family=binomial(logit),data=cohorte_sd_tc))$coefficients->s
#exp(confint(glm(cas ~poids.fna,family=binomial(logit),data=cohorte_sd_tc)))



nbr<-NULL
for (i in c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
            "parite.f3na","gestite.fna","gestite.fna","sexe.fna","nbfoetus.fna","agegestationnel.fna",
            "agegestationnel.f2na",
            "agegestationnel.f3na","agegestationnel.f4na","naissancepar","naissancepar.f2","vbna",
            "poids.fna","poids.f3na","poids.f4na","poids.f5na","taille.fna","taille.f2na",
            "coeffapgar5mncor.fna","coeffapgar5mn.f2na")
) {
  c<-nlevels(cohorte_sd_tc[,i])-1
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

write.table(resultor,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/test_univarie_tcc.xls")

#glm(cas ~sexe,family=binomial(logit),data=cohorte_sd_tc)

poisson_leu<-summary(glm(cas~sexe.fna+agegestationnel.f4na+poids.f3na+parite.f3na+age.f3na, ,family="poisson",data=cohorte_sd_tc))$coefficients
hr<-exp(poisson_leu[,1])
conf_inf<-exp(poisson_leu[,1]+1.960*poisson_leu[,2])
conf_sup<-exp(poisson_leu[,1]-1.960*poisson_leu[,2])
s<-cbind(poisson_leu,hr,conf_sup,conf_inf)
tc_pouisson<-round(s,2)

test<-bigglm(cas_n ~sexe.fna+agegestationnel.f2na+poids.f3na,family=poisson(),data=cohorte_sd_tc,chuncksize=1000,maxit=1000)



###==========survie===============  

cohorte_sd_tc$delai<-difftime(cohorte_sd_tc$ddp,cohorte_sd_tc$datenaissance)

cohorte_sd_tc$delai<-as.numeric(cohorte_sd_tc$delai)/365
cohorte_sd_tc$cas<-as.numeric(as.character(cohorte_sd_tc$cas))

par(mfrow=c(1,1))

modelsurvie<-function(x){
  with(cohorte_sd_tc,{
    model1<-coxph( Surv(cohorte_sd_tc$delai,cohorte_sd_tc$cas)~age.fna,cohorte_sd_tc)
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


jesaispass<-apply(cohorte_sd_tc[,c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
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
#jesaispas<-apply(cohorte_sd_tc[,c("poids.fna","agegestationnel.fna")],2,model)


nbr<-NULL
for (i in c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
            "parite.f3na","gestite.fna","gestite.fna","sexe.fna","nbfoetus.fna","agegestationnel.fna",
            "agegestationnel.f2na",
            "agegestationnel.f3na","agegestationnel.f4na","naissancepar","naissancepar.f2","vbna",
            "poids.fna","poids.f3na","poids.f4na","poids.f5na","taille.fna","taille.f2na",
            "coeffapgar5mncor.fna","coeffapgar5mn.f2na","allaitementna")
) {
  c<-nlevels(cohorte_sd_tc[,i])-1
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

write.table(resultor,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/test_univarie_tc_surviec.xls")


s<- Surv(cohorte_sd_tc$delai,cohorte_sd_tc$cas)

km_ageges<-survfit(s~agegestationnel.f2na,cohorte_sd_tc)
plot(km_ageges,ylim=c(0.999,1),mark.time = F)


su<-coxph(s ~agegestationnel.f3na+poids.f3na+ sexe.fna,cohorte_sd_tc)
sut<-coxph(s ~poids.fna,cohorte_sd_tc)

sut<-survdiff(s ~poids.fna,cohorte_sd_tc)
plot(cox.zph(su))

sul<-coxph(s ~ridge(sexe,theta=5),cohorte_sd_tc)


glm(cas_n ~agegestationnel.f2na+ ,family="poisson",data=cohorte_sd_tc)

###====================== t embryonnaire===========================

cohorte_sd_tembryonnaire<-cohorte_sd[cohorte_sd$tembryonnaire==1| (cohorte_sd$cas==0),]
cohorte_sd_tembryonnaire$cas<-as.factor(as.character(cohorte_sd_tembryonnaire$cas))

nomx<-c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
        "parite.f3","gestite.f","gestite.f2","sexe","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
        "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
        "poids.f","poids.f3","poids.f4","poids.f5","taille.f","taille.f2","coeffapgar5mncor.f","coeffapgar5mn.f2"
)

j<-1
B<-NULL
for (i in cohorte_sd_tembryonnaire[,c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
                                 "parite.f3","gestite.f","gestite.f2","sexe","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
                                 "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
                                 "poids.f","poids.f3","poids.f4","poids.f5","taille.f","taille.f2","coeffapgar5mncor.f","coeffapgar5mn.f2"
)
] ){
  b<-test.qual(x=i,y=cohorte_sd_tembryonnaire$cas,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}

write.table(B,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/cohorte_tembryonnaire2.xls",sep="\t")
#☻test.quant(varquant = c("tailles","poids","perimetre2"),varqual = "cas",nomquant = c("tailles","poids","perimetre"),nomqual = "cas",data=cohorte,savefile=T,fichier=)


###

model<-function(x){
  with(cohorte_sd_tembryonnaire,{
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



cohorte_sd_tembryonnaire$cas<-as.numeric(as.character(cohorte_sd_tembryonnaire$cas))
cohorte_sd_tembryonnaire$coeffapgar5mn.f2na<-as.factor(cohorte_sd_tembryonnaire$coeffapgar5mn.f2na)


jesaispas<-apply(cohorte_sd_tembryonnaire[,c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
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
#jesaispas<-apply(cohorte_sd_tembryonnaire[,c("poids.fna","agegestationnel.fna")],2,model)





#summary(glm(cas ~parite.f3na,family=binomial(logit),data=cohorte_sd_tembryonnaire))
#exp(confint(glm(cas ~parite.f3na,family=binomial(logit),data=cohorte_sd_tembryonnaire)))

#summary(glm(cas ~poids.fna,family=binomial(logit),data=cohorte_sd_tembryonnaire))$coefficients->s
#exp(confint(glm(cas ~poids.fna,family=binomial(logit),data=cohorte_sd_tembryonnaire)))



nbr<-NULL
for (i in c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
            "parite.f3na","gestite.fna","gestite.fna","sexe.fna","nbfoetus.fna","agegestationnel.fna",
            "agegestationnel.f2na",
            "agegestationnel.f3na","agegestationnel.f4na","naissancepar","naissancepar.f2","vbna",
            "poids.fna","poids.f3na","poids.f4na","poids.f5na","taille.fna","taille.f2na",
            "coeffapgar5mncor.fna","coeffapgar5mn.f2na")
) {
  c<-nlevels(cohorte_sd_tembryonnaire[,i])-1
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

write.table(resultor,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/test_univarie_tembryonnairec.xls")

#glm(cas ~sexe,family=binomial(logit),data=cohorte_sd_tembryonnaire)

poisson_leu<-summary(glm(cas~sexe.fna+agegestationnel.f4na+poids.f3na+parite.f3na+age.f3na, ,family="poisson",data=cohorte_sd_tembryonnaire))$coefficients
hr<-exp(poisson_leu[,1])
conf_inf<-exp(poisson_leu[,1]+1.960*poisson_leu[,2])
conf_sup<-exp(poisson_leu[,1]-1.960*poisson_leu[,2])
s<-cbind(poisson_leu,hr,conf_sup,conf_inf)
tembryonnaire_pouisson<-round(s,2)

test<-bigglm(cas_n ~sexe.fna+agegestationnel.f2na+poids.f3na,family=poisson(),data=cohorte_sd_tembryonnaire,chuncksize=1000,maxit=1000)



###==========survie===============  

cohorte_sd_tembryonnaire$delai<-difftime(cohorte_sd_tembryonnaire$ddp,cohorte_sd_tembryonnaire$datenaissance)

cohorte_sd_tembryonnaire$delai<-as.numeric(cohorte_sd_tembryonnaire$delai)/365
cohorte_sd_tembryonnaire$cas<-as.numeric(as.character(cohorte_sd_tembryonnaire$cas))

par(mfrow=c(1,1))

modelsurvie<-function(x){
  with(cohorte_sd_tembryonnaire,{
    model1<-coxph( Surv(cohorte_sd_tembryonnaire$delai,cohorte_sd_tembryonnaire$cas)~age.fna,cohorte_sd_tembryonnaire)
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


jesaispass<-apply(cohorte_sd_tembryonnaire[,c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
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
#jesaispas<-apply(cohorte_sd_tembryonnaire[,c("poids.fna","agegestationnel.fna")],2,model)


nbr<-NULL
for (i in c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
            "parite.f3na","gestite.fna","gestite.fna","sexe.fna","nbfoetus.fna","agegestationnel.fna",
            "agegestationnel.f2na",
            "agegestationnel.f3na","agegestationnel.f4na","naissancepar","naissancepar.f2","vbna",
            "poids.fna","poids.f3na","poids.f4na","poids.f5na","taille.fna","taille.f2na",
            "coeffapgar5mncor.fna","coeffapgar5mn.f2na","allaitementna")
) {
  c<-nlevels(cohorte_sd_tembryonnaire[,i])-1
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

write.table(resultor,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/test_univarie_tembryonnaire_surviec.xls")














s<- Surv(cohorte_sd_leucemie$delai,cohorte_sd_leucemie$cas)

km_ageges<-survfit(s~agegestationnel.f2na,cohorte_sd_leucemie)
plot(km_ageges,ylim=c(0.999,1),mark.time = F)


su<-coxph(s ~agegestationnel.f3na+poids.f3na+ sexe.fna,cohorte_sd_leucemie)
sut<-coxph(s ~poids.fna,cohorte_sd_leucemie)

sut<-survdiff(s ~poids.fna,cohorte_sd_leucemie)
plot(cox.zph(su))

sul<-coxph(s ~ridge(sexe,theta=5),cohorte_sd_leucemie)


glm(cas_n ~agegestationnel.f2na+ ,family="poisson",data=cohorte_sd_leucemie)


###==========survie globale ===============  

cohorte_sd$delai<-difftime(cohorte_sd$ddp,cohorte_sd$datenaissance)

cohorte_sd$delai<-as.numeric(cohorte_sd$delai)/365
cohorte_sd$cas<-as.numeric(as.character(cohorte_sd$cas))

cohorte_sd$poids_age.fna<-as.factor(as.character(cohorte_sd$poids_age.fna), levels = c("2","1","3","NA"))
cohorte_sd$poids_age.f2na<- relevel(cohorte_sd$poids_age.fna, ref = 2)

cohorte_sd$coeffapgar5mn.f2na<- relevel(cohorte_sd$coeffapgar5mn.f2na, ref = 2)

par(mfrow=c(1,1))

modelsurvie<-function(x){
  with(cohorte_sd,{
    model1<-coxph( Surv(cohorte_sd$delai,cohorte_sd$cas)~x,cohorte_sd)
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

modelsurview<-function(x){
  with(cohorte_sd,{
    model1<-coxph( Surv(cohorte_sd$delai,cohorte_sd$cas)~x,cohorte_sd)
    #p<-round(summary(model1)$coefficient[2,4],3)
    s<-summary(model1)$waldtest

    
    return(s)
    return(p)
  })
}
jesaispass<-apply(cohorte_sd[,c("coeffapgar5mn.f2na","poids_age.f2na","age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
                                         "parite.f3na","gestite.fna","gestite.fna","sexe.fna","tabac.f","alcool.f",
                                         "nbfoetus.fna","agegestationnel.fna",
                                         "agegestationnel.f2na",
                                         "agegestationnel.f3na","agegestationnel.f4na","naissancepar","naissancepar.f2","vbna",
                                         "poids.fna","poids.f3na","poids.f4na","poids.f5na","poids_age.fna","taille.fna",
                                         "taille.f2na","coeffapgar5mncor.fna","coeffapgar5mn.f2na","allaitementna")
                                      ],2,modelsurvie)
#conf<-lapply(jesaispas,function(x){x$conf.int})
#p<-lapply(jesaispas,function(x){x$coefficients[,5]})

resultor<-do.call(rbind,jesaispass)
#resultor<-resultor[!rownames(resultor)=="(Intercept)",]
#jesaispas<-apply(cohorte_sd[,c("poids.fna","agegestationnel.fna")],2,model)


nbr<-NULL
for (i in c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
            "parite.f3na","gestite.fna","gestite.fna","sexe.fna","tabac.f","alcool.f"
            ,"nbfoetus.fna","agegestationnel.fna",
            "agegestationnel.f2na",
            "agegestationnel.f3na","agegestationnel.f4na","naissancepar","naissancepar.f2","vbna",
            "poids.fna","poids.f3na","poids.f4na","poids.f5na","poids_age.fna","taille.fna","taille.f2na",
            "coeffapgar5mncor.fna","coeffapgar5mn.f2na","allaitementna")
) {
  c<-nlevels(cohorte_sd[,i])-1
  nbr<-c(nbr,c)
}



legende<-rep(c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
               "parite.f3na","gestite.fna","gestite.fna","sexe.fna","tabac.f","alcool.f"
               ,"nbfoetus.fna",
               "agegestationnel.fna",
               "agegestationnel.f2na",
               "agegestationnel.f3na","agegestationnel.f4na","naissancepar","naissancepar.f2","vbna",
               "poids.fna","poids.f3na","poids.f4na","poids.f5na","poids_age.fna","taille.fna","taille.f2na",
               "coeffapgar5mncor.fna","coeffapgar5mn.f2na","allaitementna"),nbr)
#resultor<-do.call(rbind,conf)
#pro<-unlist(p)


resultor<-round(resultor,2)
resultor<-cbind(legende,resultor[,c("rr","conf_sup","conf_inf","Pr(>|z|)")])

write.table(resultor,file="G:/test_univarie_tc_surviec2.xls")




cohorte_sd$poids.f5<-as.factor(ifelse(cohorte_sd$poids < 4000,0,1))

cohorte_sd$poids.f5na<-ifelse(is.na(ccohorte_sd$poids.f5),"NA",cohorte_sd$poids.f5)
s<- Surv(cohorte_sd$delai,cohorte_sd$cas)

jesaispass<-apply(cohorte_sd[,c("poids.f5na","poids.f5",'agegestationnel.f4','agegestationnel.f4na',
                                'age.f2',"age.f2na")
                             ],2,modelsurvie)

jesaispassw<-apply(cohorte_sd[,c("poids.f5na","poids.f5",'agegestationnel.f4','agegestationnel.f4na',
                                'age.f2',"age.f2na")
                             ],2,modelsurview)


su<-coxph(s ~ sexe.fna,cohorte_sd)
plot(cox.zph(su))
su
coxph(s ~ sexe,cohorte_sd)


sut<-coxph(s ~poids.fna,cohorte_sd)
plot(cox.zph(sut))
plot(cox.zph(sut)[2])
plot(cox.zph(sut)[1])
plot(cox.zph(sut)[3])
plot(cox.zph(sut)[4])
sut

sut<-coxph(s ~poids.f,cohorte_sd)

sut<-coxph(s ~poids.f5na,cohorte_sd)
plot(cox.zph(sut))
plot(cox.zph(sut)[2])
plot(cox.zph(sut)[1])
plot(cox.zph(sut)[3])
plot(cox.zph(sut)[4])
sut







su<-coxph(s ~ agegestationnel.fna,cohorte_sd)
plot(cox.zph(su))
plot(cox.zph(su)[2])
plot(cox.zph(su)[1])
plot(cox.zph(su)[3])
plot(cox.zph(su)[4])
su


coxph(s ~ agegestationnel.f,cohorte_sd)
su<-coxph(s ~ age.fna,cohorte_sd)
plot(cox.zph(su))
plot(cox.zph(su)[2])
plot(cox.zph(su)[1])
plot(cox.zph(su)[3])
plot(cox.zph(su)[4])
su
coxph(s ~ age.f,cohorte_sd)



su<-coxph(s ~ parite.f3na,cohorte_sd)
plot(cox.zph(su))
plot(cox.zph(su)[2])
plot(cox.zph(su)[1])
plot(cox.zph(su)[3])
plot(cox.zph(su)[4])
su

coxph(s ~ parite.f3,cohorte_sd)



su<-coxph(s ~ poids_age.f2na,cohorte_sd)
plot(cox.zph(su))
plot(cox.zph(su)[2])
plot(cox.zph(su)[1])
plot(cox.zph(su)[3])
plot(cox.zph(su)[4])
su
coxph(s ~ poids_age.f,cohorte_sd)



su<-coxph(s ~ vbna,cohorte_sd)
plot(cox.zph(su)[1])
su

coxph(s ~ vb,cohorte_sd)



su<-coxph(s ~ coeffapgar5mn.f2na,cohorte_sd)
plot(cox.zph(su))
su
coxph(s ~ coeffapgar5mn.f2,cohorte_sd)
modelsurvie<-function(x){
  with(cohorte_sd_leucemie,{
    model1<-coxph( Surv(cohorte_sd_leucemie$delai,cohorte_sd_leucemie$cas)~x,cohorte_sd_leucemie)
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

modelsurview<-function(x){
  with(cohorte_sd_leucemie,{
    model1<-coxph( Surv(cohorte_sd_leucemie$delai,cohorte_sd_leucemie$cas)~x,cohorte_sd_leucemie)
    #p<-round(summary(model1)$coefficient[2,4],3)
    s<-summary(model1)$waldtest
    
    
    return(s)
    return(p)
  })
}


cohorte_sd_leucemie$poids.f5<-as.factor(ifelse(cohorte_sd_leucemie$poids < 4000,0,1))
cohorte_sd_leucemie$poids.f5na<-ifelse(is.na(ccohorte_sd_leucemie$poids.f5),"NA",cohorte_sd_leucemie$poids.f5)

table(cohorte_sd_leucemie$poids.f5,cohorte_sd_leucemie$cas)




jesaispass<-apply(cohorte_sd_leucemie[,c("poids.f5na","poids.f5",'agegestationnel.f4','agegestationnel.f4na',
                                'age.f2',"age.f2na")
                             ],2,modelsurvie)

jesaispassw<-apply(cohorte_sd_leucemie[,c("poids.f5na","poids.f5",'agegestationnel.f4','agegestationnel.f4na',
                                 'age.f2',"age.f2na")
                              ],2,modelsurview)
par(mfrow=c(2,2))
s<- Surv(cohorte_sd_leucemie$delai,cohorte_sd_leucemie$cas)


su<-coxph(s ~ sexe.fna,cohorte_sd_leucemie)
plot(cox.zph(su))
su
coxph(s ~ sexe,cohorte_sd_leucemie)


sut<-coxph(s ~poids.f5na,cohorte_sd_leucemie)

sut<-coxph(s ~poids.fna,cohorte_sd_leucemie)
plot(cox.zph(sut)[2])
plot(cox.zph(sut)[1])
plot(cox.zph(sut)[3])
plot(cox.zph(sut)[4])
sut
coxph(s ~poids.f,cohorte_sd_leucemie)


su<-coxph(s ~ agegestationnel.fna,cohorte_sd_leucemie)
plot(cox.zph(su))
plot(cox.zph(su)[2])
plot(cox.zph(su)[1])
plot(cox.zph(su)[3])
plot(cox.zph(su)[4])
su
coxph(s ~ agegestationnel.f,cohorte_sd_leucemie)


su<-coxph(s ~ age.fna,cohorte_sd_leucemie)
plot(cox.zph(su))
plot(cox.zph(su)[2])
plot(cox.zph(su)[1])
plot(cox.zph(su)[3])
plot(cox.zph(su)[4])
su
coxph(s ~ age.f,cohorte_sd_leucemie)
su<-coxph(s ~ parite.f3na,cohorte_sd_leucemie)
plot(cox.zph(su))
plot(cox.zph(su))
plot(cox.zph(su)[2])
plot(cox.zph(su)[1])
plot(cox.zph(su)[3])
plot(cox.zph(su)[4])
su
coxph(s ~ parite.f3,cohorte_sd_leucemie)


su<-coxph(s ~ poids_age.f2na,cohorte_sd_leucemie)
plot(cox.zph(su))
su
coxph(s ~ poids_age.f,cohorte_sd_leucemie)



su<-coxph(s ~ vbna,cohorte_sd_leucemie)
plot(cox.zph(su))
su

coxph(s ~ vb,cohorte_sd_leucemie)


su<-coxph(s ~ coeffapgar5mn.f2na,cohorte_sd_leucemie)
plot(cox.zph(su))
su
coxph(s ~ coeffapgar5mn.f2,cohorte_sd_leucemie)
