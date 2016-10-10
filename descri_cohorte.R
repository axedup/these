### Description de la cohorte###


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

  
}
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

