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

cohorte_sd<-cohorte_sd[!cohorte_sd$commune_nais_code %in% ("") & !cohorte_sd$commune_famille_code  %in% (""),]

cohorte_sd<-cohorte_sd[substr(cohorte_sd$commune_nais_code,1,2)==substr(cohorte_sd$commune_famille_code,1,2),]
dim(cohorte_sd)


cohorte$Source<-as.factor(cohorte$Source)

nomx<-c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
        "parite.f3","gestite.f","gestite.f2","sexe","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
        "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
        "poids.f","poids.f3","poids.f4","poids.f5","poids_age.f","taille.f","taille.f2","coeffapgar5mncor.f","coeffapgar5mn.f2"
)

j<-1
B<-NULL
for (i in cohorte[,c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
                     "parite.f3","gestite.f","gestite.f2","sexe","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
                     "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
                     "poids.f","poids.f3","poids.f4","poids.f5","poids_age.f","taille.f","taille.f2","coeffapgar5mncor.f"
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
        "poids.f","poids.f3","poids.f4","poids.f5","poids_age.f","taille.f","taille.f2","coeffapgar5mncor.f","coeffapgar5mn.f2"
       )

j<-1
B<-NULL
for (i in cohorte[,c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
                     "parite.f3","gestite.f","gestite.f2","sexe","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
                     "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
                     "poids.f","poids.f3","poids.f4","poids.f5","poids_age.f","taille.f","taille.f2","coeffapgar5mncor.f"
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


nomx<-c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
        "parite.f3","gestite.f","gestite.f2","sexe","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
        "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
        "poids.f","poids.f3","poids.f4","poids.f5","poids_age.f","taille.f","taille.f2","coeffapgar5mncor.f","coeffapgar5mn.f2"
)

j<-1
B<-NULL
for (i in cohorte_sd_leucemie[,c("age.f","age.f2","age.f3","age.f4","mprofession","pprofession","niveauetudes","parite.f","parite.f2",
                                 "parite.f3","gestite.f","gestite.f2","sexe","nbfoetus.f","agegestationnel.f","agegestationnel.f2",
                                 "agegestationnel.f3","agegestationnel.f4","naissancepar","naissancepar.f2","vb",
                                 "poids.f","poids.f3","poids.f4","poids.f5","poids_age.f","taille.f","taille.f2","coeffapgar5mncor.f","coeffapgar5mn.f2"
)
                  ] ){
  b<-test.qual(x=i,y=cohorte_sd_leucemie$cas,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}

write.table(B,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/cohorte_leucemie.xls",sep="\t")
test.quant(varquant = c("tailles","poids","perimetre2"),varqual = "cas",nomquant = c("tailles","poids","perimetre"),nomqual = "cas",data=cohorte,savefile=T,
           fichier=)


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


c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
  "parite.f3na","gestite.fna","gestite.f2na","sexe.fna","nbfoetus.fna","agegestationnel.fna",
  "agegestationnel.f2na",
  "agegestationnel.f3na","agegestationnel.f4na","naissancepar","naissancepar.f2","vbna",
  "poids.fna","poids.f3na","poids.f4na","poids.f5na","poids_age.fna","taille.fna","taille.f2na","coeffapgar5mncor.fna","coeffapgar5mn.f2na"

  
  
  jesaispas<-apply(cohorte_sd_leucemie[,c("age.fna","age.f2na","age.f3na","age.f4na","mprofession","pprofession","niveauetudes","parite.f2","parite.fna",
                                          "parite.f3na","gestite.fna","gestite.fna","sexe.fna","nbfoetus.fna","agegestationnel.fna",
                                          "agegestationnel.f2na",
                                          "agegestationnel.f3na","agegestationnel.f4na","naissancepar","naissancepar.f2","vbna",
                                          "poids.fna","poids.f3na","poids.f4na","poids.f5na","poids_age.fna",
                                          "taille.fna","taille.f2na","coeffapgar5mncor.fna","coeffapgar5mn.f2na")
                                          ],2,model)
#conf<-lapply(jesaispas,function(x){x$conf.int})
p<-lapply(jesaispas,function(x){x$coefficients[,5]})


jesaispas<-apply(cohorte_sd_leucemie[,c("poids.fna","agegestationnel.fna")],2,model)





#summary(glm(cas ~parite.f3na,family=binomial(logit),data=cohorte_sd_leucemie))
#exp(confint(glm(cas ~parite.f3na,family=binomial(logit),data=cohorte_sd_leucemie)))

#summary(glm(cas ~poids.fna,family=binomial(logit),data=cohorte_sd_leucemie))$coefficients->s
#exp(confint(glm(cas ~poids.fna,family=binomial(logit),data=cohorte_sd_leucemie)))



nbr<-NULL
for (i in c("poids.fna","agegestationnel.fna","age.f2na","agegestationnel.f2na","age.fna",
            "gestite.fna","parite.fna","sexe.fna",,"coeffapgar5mncor.fna",
            "vbna","most_dep","parite.f3na","forte_expo","forte_expop","age.f3na",
            "age.f4na","poids.f3na","poids.f4na","agegestationnel.f2na","agegestationnel.f3",
            "agegestationnel.f4","poids.f5na","taille.f2na","nbfoetus.fna","taille.fna")) {
  c<-nlevels(cohorte_sd_leucemie[,i])-1
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
+vbna+agegestationnel.f4na+age.f2na

glm(cas ~sexe,family=binomial(logit),data=cohorte_sd_leucemie)
glm(cas_n ~sexe.fna+agegestationnel.f2na+poids.f3na, ,family="poisson",data=cohorte_sd_leucemie)
test<-bigglm(cas_n ~sexe.fna+agegestationnel.f2na+poids.f3na,family=poisson(),data=cohorte_sd_leucemie,chuncksize=1000,maxit=1000)
