### Description de la cohorte###

### on va supprimer les décès
cohorte$mprofession<-as.factor(cohorte$mprofession)
cohorte$pprofession<-as.factor(cohorte$pprofession)
cohorte$annee<-year(cohorte$datenaissance)

# après réflexion on ne garde que ceux nés en Ilde France ! 


cohorte_sd<-cohorte[is.na(cohorte$age_deces_jj) & is.na(cohorte$age_deces_hh) & substr(cohorte$commune_nais_code,1,2) %in% c("75","78","95") ,]
dim(cohorte)
dim(cohorte_sd)

cohorte$Source<-as.factor(cohorte$Source)

nomx<-c("sexe","niveauetudes","parite.f","parite.f2","parite.f3","gestite.f","gestite.f2",
        "naissancepar","naissancepar.f2","vb","presentation","debuttravail","age.f","age.f2","age.f3","age.f4",
        
        "agegestationnel.f","agegestationnel.f2","agegestationnel.f3","agegestationnel.f4",
        "coeffapgar5mncor.f", "coeffapgar5mn.f2", "poids.f","poids.f3","poids.f4",
        "poids.f5","taille.f","taille.f2","nbfoetus.f")

j<-1
B<-NULL
for (i in cohorte[,c("sexe","niveauetudes","parite.f","parite.f2","parite.f3","gestite.f","gestite.f2",
                              "naissancepar","naissancepar.f2","vb","presentation","debuttravail","age.f","age.f2","age.f3","age.f4",
                     
                  "agegestationnel.f","agegestationnel.f2","agegestationnel.f3","agegestationnel.f4",
                              "coeffapgar5mncor.f","coeffapgar5mn.f2", "poids.f","poids.f3","poids.f4",
                              "poids.f5","taille.f","taille.f2","nbfoetus.f")
                           ] ){
  b<-test.qual(x=i,y=cohorte$Source,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}



nomx<-c("sexe","niveauetudes","parite.f","parite.f2","parite.f3","gestite.f","gestite.f2",
        "naissancepar","naissancepar.f2","vb","presentation","debuttravail","age.f","age.f2","age.f3","age.f4",
        
        "agegestationnel.f","agegestationnel.f2","agegestationnel.f3","agegestationnel.f4",
        "coeffapgar5mncor.f", "coeffapgar5mn.f2", "poids.f","poids.f3","poids.f4",
        "poids.f5","taille.f","taille.f2","nbfoetus.f")

j<-1
B<-NULL
for (i in cohorte[,c("sexe","niveauetudes","parite.f","parite.f2","parite.f3","gestite.f","gestite.f2",
                     "naissancepar","naissancepar.f2","vb","presentation","debuttravail","age.f","age.f2","age.f3","age.f4",
                     
                     "agegestationnel.f","agegestationnel.f2","agegestationnel.f3","agegestationnel.f4",
                     "coeffapgar5mncor.f","coeffapgar5mn.f2", "poids.f","poids.f3","poids.f4",
                     "poids.f5","taille.f","taille.f2","nbfoetus.f")
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

cohorte_sd_leucemie<-u[u$leucemie==1| (u$cas==0),]


nomx<-c("sexe","niveauetudes","mprofession","pprofession","parite.f","parite.f2","parite.f3","gestite.f","gestite.f2",
        "naissancepar","naissancepar.f2","vb","presentation","debuttravail","age.f","age.f2","age.f3","age.f4",
        
        "agegestationnel.f","agegestationnel.f2","agegestationnel.f3","agegestationnel.f4",
        "coeffapgar5mncor.f", "coeffapgar5mn.f2", "poids.f","poids.f3","poids.f4",
        "poids.f5","taille.f","taille.f2","nbfoetus.f")

j<-1
B<-NULL
for (i in cohorte_sd_leucemie[,c("sexe","niveauetudes","mprofession","pprofession","parite.f","parite.f2","parite.f3","gestite.f","gestite.f2",
                     "naissancepar","naissancepar.f2","vb","presentation","debuttravail","age.f","age.f2","age.f3","age.f4",
                     
                     "agegestationnel.f","agegestationnel.f2","agegestationnel.f3","agegestationnel.f4",
                     "coeffapgar5mncor.f","coeffapgar5mn.f2", "poids.f","poids.f3","poids.f4",
                     "poids.f5","taille.f","taille.f2","nbfoetus.f")
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
    model1<-glm(cas ~x,family=binomial(logit))
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

jesaispas<-apply(cohorte_sd_leucemie[,c("poids.fna","agegestationnel.fna","age.fna",
                                        "parite.f3na","sexe.fna",
                                        "coeffapgar5mncor.f2na",
                                        "vbna","age.f3na",
                                        "age.f4na","poids.f3na","poids.f4na"
                                        ,"poids.f5na","taille.f2na","nbfoetus.f2na","taille.fna")],2,model)
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
