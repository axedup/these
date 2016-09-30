###===Description===###

libelle<-read.csv2("C:\\Users\\Louise\\Documents\\Desespoir\\Bases\\AFF_XY.csv",stringsAsFactors = FALSE)
res_geo$MA___NumMalade<-as.character(res_geo$MA___NumMalade)
affectation<-merge(res_geo[,c("AFF","MA___NumMalade","IRIS","IRIS_2012","iris_diff","TOPO_IRIS","cp")],cas_temoinsexpoi[,c("numunique","cas")],by.x="MA___NumMalade",by.y="numunique")

affectation<-affectation[,c("AFF","MA___NumMalade","IRIS","IRIS_2012","iris_diff","cp")]
names(affectation)<-c("AFF","Référence.Enfant","IRIS","IRIS_2012","iris_diff","result_postcode")
res_geo_95_suite$IRIS<-as.factor(res_geo_95_suite@data$IRIS)
affectation<-rbind(affectation,res_geo_95_suite@data[c("Référence.Enfant","AFF","IRIS","IRIS_2012","iris_diff","result_postcode")])

dim(affectation)
prop.table(table(affectation$AFF))



libelle$AFF<-libelle$Nom_Index
libelle$AFF<-gsub(pattern = "(^[0-9]{1})([0-9]{2})",replacement="\\2",x=libelle$AFF)
libelle$AFF<-gsub(pattern = "(^[0]{1})([0-9]{1,})",replacement="\\2",x=libelle$AFF)
affectation<-merge(affectation,libelle,by="AFF")
affectation$lib<-as.factor(affectation$Lib_Affectation)

summary(affectation)

### géocoadge
quali(x=c("lib"),nomx=c("Qualité du géocodage"), data=affectation,RAPPORT=F,SAVEFILE=T,ordonner=c(FALSE), numerique=c(TRUE), seq=list(c(19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)),chemin="C:/Users/Louise/Documents/Desespoir/Bases/resultats/",fichier="geo")

###qualité du géorcodage selon la Source

affectation$Source<-substr(affectation$result_postcode,1,2)
affectation$Source<-ifelse(is.na(affectation$Source),"95",affectation$Source)

geocod_dep<-affectation %>%
  group_by(Source) %>%
  do(data.frame(n=table(.$lib,exclude=NULL),pour=c(round(prop.table(table(.$lib))*100,1),0)))

geocod_dep<-as.data.frame(geocod_dep)
geocod_dep$Source<-as.factor(geocod_dep$Source)


w <- reshape(geocod_dep, 
             timevar = "Source",
             idvar = c("n.Var1"),
  direction = "wide")


g<-affectation %>%
  do(data.frame(n=table(.$lib,exclude=NULL),pour=c(round(prop.table(table(.$lib))*100,1),0)))

w<-merge(w,g,by="n.Var1")



write.table(w[c(7,3,4,2,5,6,8,9,15,11,12,10,13,14,16,17,1,18),],file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/geocodager.xls",sep="\t")

### iris

affectation$codage_iris<-ifelse( grepl("^[0-9]{9}",x=affectation$IRIS) & affectation$IRIS_2012=="-1"  & !is.na(affectation$IRIS_2012)& affectation$iris_diff==0,1,0)
affectation$codage_iris<-ifelse( grepl("^[0-9]{9}",x=affectation$IRIS) & is.na(affectation$IRIS_2012) & affectation$iris_diff==1 & affectation$AFF %in% c("0","1","10","11"),2,affectation$codage_iris)
affectation$codage_iris<-ifelse( grepl("^[0-9]{9}",x=affectation$IRIS) & affectation$IRIS_2012=="-1" & !is.na(affectation$IRIS_2012) & affectation$iris_diff==1 ,3,affectation$codage_iris)
affectation$codage_iris<-ifelse( !grepl("^[0-9]{9}",x=affectation$IRIS) & affectation$AFF %in% c("0","1","10","11"),4,affectation$codage_iris)

affectation$codage_iris<-factor(affectation$codage_iris,labels=c("Attribution à l'IRIS base impossible et géolocalisation imprécise","Attribution à l'IRIS base correcte","IRIS base supprimé et géolocalisation correcte","Discordance","IRIS base non affecté mais géolocalisation correcte"))
table(affectation$codage_iris)


quali(x=c("codage_iris"),nomx=c("IRIS"), data=affectation,RAPPORT=F,SAVEFILE=F,ordonner=c(FALSE), numerique=c(TRUE), seq=list(c(19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)),chemin="C:/Users/Louise/Documents/Desespoir/Bases/resultats/",fichier="IRIS")


iris_dep<-affectation %>%
  group_by(Source) %>%
  do(data.frame(n=table(.$codage_iris,exclude=NULL),pour=c(round(prop.table(table(.$codage_iris))*100,1),0)))
iris_dep<-as.data.frame(iris_dep)

wt <- reshape(iris_dep, 
             timevar = "Source",
             idvar = c("n.Var1"),
             direction = "wide")


gt<-affectation %>%
  do(data.frame(n=table(.$codage_iris,exclude=NULL),pour=c(round(prop.table(table(.$codage_iris))*100,1),0)))

wt<-merge(wt,gt,by="n.Var1")


write.table(wt[c(2,3,5,4,1,6),],file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/irisr.xls",sep="\t")


### quel département 

cas_temoinsexpoi$Source<-as.factor(cas_temoinsexpoi$Source)





### caractéristiques périnatales 




cas_temoinsexpoi$parite.f3<-cas_temoinsexpoi$parite.f2
cas_temoinsexpoi$parite.f3<-ifelse(cas_temoinsexpoi$parite >2,"3 et +",cas_temoinsexpoi$parite.f2)
table(cas_temoinsexpoi$parite.f3)

cas_temoinsexpoi$parite.f3<-as.factor(cas_temoinsexpoi$parite.f3)

cas_temoinsexpoi$tailles<-cas_temoinsexpoi$tailles/10

cas_temoinsexpoi$naissancepar<-factor(cas_temoinsexpoi$naissancepar,labels=c("vbni","vbi","cesar prog","cesar urg","cesar sp"))
cas_temoinsexpoi$naissancepar.f2<-cas_temoinsexpoi$naissancepar
levels(cas_temoinsexpoi$naissancepar.f2)<-c("vbni","vbi" ,"cesar" ,"cesar","cesar") 
table(cas_temoinsexpoi$naissancepar.f2)

cas_temoinsexpoi$vb<-as.factor(ifelse(cas_temoinsexpoi$naissancepar.f %in% c("vbi","vbni") & !is.na(cas_temoinsexpoi$naissancepar.f2),"vb",cas_temoinsexpoi$naissancepar.f2))

quantifb(xc=c("age","agegestationnel","tailles","poids","perimetre2"),nomx=c("age en an","terme","taille en cm","poids en g","perimetre en cm"), data=cas_temoinsexpoi,RAPPORT=F,SAVEFILE=T,chemin="C:/Users/Louise/Documents/Desespoir/Bases/resultats/",fichier="peri")
test.quant(varquant=c("age","agegestationnel","tailles","poids","perimetre2"),nomquant=c("age en an","terme","taille en cm","poids en g","perimetre en cm"), varqual=c("cas"),nomqual=c("cas"),data=cas_temoinsexpoi,RAPPORT=F,SAVEFILE=T,chemin="C:/Users/Louise/Documents/Desespoir/Bases/resultats/",fichier="peric")



#quali(x=c("sexe","niveauetudes","parite.f","parite.f2","gestite.f","gestite.f2","naissancepar","naissancepar.f2","coeffapgar1mn","coeffapgar5mn"),nomx=c("sexe","etudes","parite","parite.f2","gestite","gestite.f","mode d'accouchement","voie basse ou césar","Apgar 1 min","Apgar 5 min"), data=cas_temoinsexpoi,RAPPORT=F,SAVEFILE=F,chemin=NULL)

#desc.qual2(x=c("sexe","niveauetudes","parite.f","parite.f2","gestite.f","gestite.f2","naissancepar","naissancepar.f2"),y=cas_temoinsexpoi$cas,nomx=c("sexe","etudes","parite","parite.f2","gestite","gestite.f","mode d'accouchement","voie basse ou césar"), data=cas_temoins_parisexpoi,RAPPORT=F,SAVEFILE=T,chemin="C:/Users/Louise/Documents/Desespoir/Bases/resultats/")


nomx<-c("sexe","etudes","parite","parite.f2","gestite","gestite.f","mode d'accouchement","voie basse ou césar","voie basse")
j<-1
B<-NULL
for (i in cas_temoinsexpoi[,c("sexe","niveauetudes","parite.f","parite.f2","gestite.f","gestite.f2","naissancepar","naissancepar.f2","vb")] ){
  b<-test.qual(x=i,y=cas_temoinsexpoi$cas,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}

write.table(B,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/pericc.xls",sep="\t")

nomx<-c("sexe","etudes","parite","parite.f2","gestite","gestite.f","mode d'accouchement","voie basse ou césar","voie basse")
j<-1
B<-NULL
for (i in cas_temoinsexpoi[,c("sexe","niveauetudes","parite.f","parite.f2","gestite.f","gestite.f2","naissancepar","naissancepar.f2","vb")] ){
  b<-test.qual(x=i,y=cas_temoinsexpoi$Source,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}

write.table(B,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/perics.xls",sep="\t")


cas_temoinsexpoi$extraction<-as.factor(cas_temoinsexpoi$extraction)

nomx<-c("sexe","etudes","parite","parite.f2","gestite","gestite.f","mode d'accouchement","voie basse ou césar","voie basse")
j<-1
B<-NULL
for (i in cas_temoinsexpoi[,c("sexe","niveauetudes","parite.f","parite.f2","gestite.f","gestite.f2","naissancepar","naissancepar.f2","vb")] ){
  b<-test.qual(x=i,y=cas_temoinsexpoi$extraction,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}

write.table(B,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/perice.xls",sep="\t")



### par type de cancer 

nomx<-c("sexe","etudes","parite","parite.f2","gestite","gestite.f","mode d'accouchement","voie basse ou césar","voie basse")
j<-1
B<-NULL
for (i in cas_temoinsexpoi[cas_temoinsexpoi$leucemie==1,c("sexe","niveauetudes","parite.f","parite.f2","gestite.f","gestite.f2","naissancepar","naissancepar.f2","vb")] ){
  b<-test.qual(x=i,y=cas_temoinsexpoi$cas[cas_temoinsexpoi$leucemie==1],nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}

cas_temoinsexpoi$leucemie<-as.factor(cas_temoinsexpoi$leucemie)
cas_temoinsexpoi$cas<-as.factor(cas_temoinsexpoi$cas)

nomx<-c("sexe","etudes","parite","parite.f2","parite.f3","gestite","gestite.f","mode d'accouchement","voie basse ou césar",
        "voie basse","age.f","poids.f","agegestationnel.f","coeffapgar5mncor.f","age.f3","age.f4","poids.f3","age.f2","poids.f4",
        "agegestationnel.f2","agegestationnel.f3","agegestationnel.f4","poids.f5","taille.f","taille.f2","nbfoetus.f")
j<-1
B<-NULL
for (i in cas_temoinsexpoi[cas_temoinsexpoi$leucemie==1,c("sexe","niveauetudes","parite.f","parite.f2","parite.f3","gestite.f","gestite.f2",
                                                          "naissancepar","naissancepar.f2","vb","age.f","poids.f","agegestationnel.f",
                                                          "coeffapgar5mncor.f","age.f3","age.f4","poids.f3","age.f2","poids.f4","agegestationnel.f2",
                                                          "agegestationnel.f3","agegestationnel.f4","poids.f5","taille.f","taille.f2","nbfoetus.f")] ){
  b<-test.qual(x=i,y=cas_temoinsexpoi$cas[cas_temoinsexpoi$leucemie==1],nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}


write.table(B,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/peri_leucemie.xls",sep="\t")


nomx<-c("sexe","etudes","parite","parite.f2","parite.f3","gestite","gestite.f","mode d'accouchement","voie basse ou césar",
        "voie basse","age.f","poids.f","agegestationnel.f","coeffapgar5mncor.f","age.f3","age.f4","poids.f3","age.f2","poids.f4",
        "agegestationnel.f2","agegestationnel.f3","agegestationnel.f4","poids.f5","taille.f","taille.f2","nbfoetus.f")
j<-1
B<-NULL
for (i in cas_temoinsexpoi[cas_temoinsexpoi$tembryonnaire==1,c("sexe","niveauetudes","parite.f","parite.f2","parite.f3","gestite.f","gestite.f2",
                                                               "naissancepar","naissancepar.f2","vb","age.f","poids.f","agegestationnel.f",
                                                               "coeffapgar5mncor.f","age.f3","age.f4","poids.f3","age.f2","poids.f4","agegestationnel.f2",
                                                               "agegestationnel.f3","agegestationnel.f4","poids.f5","taille.f","taille.f2","nbfoetus.f")] ){
  b<-test.qual(x=i,y=cas_temoinsexpoi$cas[cas_temoinsexpoi$tembryonnaire==1],nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}  


write.table(B,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/peri_tembr.xls",sep="\t")


nomx<-c("sexe","etudes","parite","parite.f2","parite.f3","gestite","gestite.f","mode d'accouchement","voie basse ou césar","voie basse","age.f","age.f2","age.f3","poids.f","poids.f3","agegestationnel.f","agegestationnel.f2","coeffapgar5mncor.f")
  j<-1
  B<-NULL
  for (i in cas_temoinsexpoi[cas_temoinsexpoi$tc==1,c("sexe","niveauetudes","parite.f","parite.f2","parite.f3","gestite.f","gestite.f2","naissancepar","naissancepar.f2","vb","age.f","age.f2","age.f3","poids.f","poids.f3","agegestationnel.f","agegestationnel.f2","coeffapgar5mncor.f")] ){
    b<-test.qual(x=i,y=cas_temoinsexpoi$cas[cas_temoinsexpoi$tc==1],nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
    B<-rbind(B,b)
    j<-j+1
  }   
    
  write.table(B,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/peri_tc.xls",sep="\t")










### on va faire les catégories pour age mater, poids terme...;
ggplot(cas_temoinsexpoi,aes(age))+geom_freqpoly()
cas_temoinsexpoi$age.f<-cut(cas_temoinsexpoi$age,breaks=c(0,25,30,35,40,80),include.lowest = T,right = F)

table(cas_temoinsexpoi$age.f,cas_temoinsexpoi$age)
table(cas_temoinsexpoi$age.f)
# class de ref 30 -35 
cas_temoinsexpoi$age.f<-reorder(cas_temoinsexpoi$age.f,new.order=c(3,1,2,4,5))
cas_temoinsexpoi$age.f<-relevel(cas_temoinsexpoi$age.f,ref="[30,35)")
table(cas_temoinsexpoi$age.f)

cas_temoinsexpoi$age.f2<-cut(cas_temoinsexpoi$age,breaks=c(0,35,80),include.lowest = T,right = F)
cas_temoinsexpoi$age.f3<-cut(cas_temoinsexpoi$age,breaks=c(0,27,80),include.lowest = T,right = F)
cas_temoinsexpoi$age.f4<-as.factor(ifelse(cas_temoinsexpoi$age < summary(cas_temoinsexpoi$age)["Median"],0,1)) # le découpage précédent est trop concentré sur les petites classes


table(cas_temoinsexpoi$age.f2,cas_temoinsexpoi$age)
table(cas_temoinsexpoi$age.f2)
# class de ref 30 -35 
#cas_temoinsexpoi$age.f2<-reorder(cas_temoinsexpoi$age.f,new.order=c(3,1,2,4,5))
cas_temoinsexpoi$age.f2<-relevel(cas_temoinsexpoi$age.f2,ref="[0,35)")
table(cas_temoinsexpoi$age.f2)






ggplot(cas_temoinsexpoi,aes(poids))+geom_freqpoly()
cas_temoinsexpoi$poids.f<-cut(cas_temoinsexpoi$poids,breaks=c(0,2500,3000,3500,4000,8000),include.lowest = T,right = F)
table(cas_temoinsexpoi$poids)
table(cas_temoinsexpoi$poids.f)
cas_temoinsexpoi$poids.f<-reorder(cas_temoinsexpoi$poids.f,new.order=c(3,1,2,4,5))
cas_temoinsexpoi$poids.f<-relevel(cas_temoinsexpoi$poids.f,ref="[3e+03,3.5e+03)")
table(cas_temoinsexpoi$poids.f)

cas_temoinsexpoi$poids.f3<-cut(cas_temoinsexpoi$poids,breaks=c(0,2500,8000),include.lowest = T,right = F)
table(cas_temoinsexpoi$poids.f3)
cas_temoinsexpoi$poids.f4<-as.factor(ifelse(cas_temoinsexpoi$poids < summary(cas_temoinsexpoi$poids)["Median"],0,1)) # le découpage précédent est trop concentré sur les petites classes
cas_temoinsexpoi$poids.f5<-as.factor(ifelse(cas_temoinsexpoi$poids < 3000,0,1)) # le découpage précédent est trop concentré sur les petites classes




plot(cas_temoinsexpoi$agegestationnel,cas_temoinsexpoi$poids)

ggplot(cas_temoinsexpoi,aes(agegestationnel))+geom_freqpoly() # l'age gesta sert plus de varaiables d'ajustement donc les classes sont peut être peu importantes
cas_temoinsexpoi$agegestationnel.f<-cut(cas_temoinsexpoi$agegestationnel,breaks=c(22,35,38,40,45),include.lowest = T,right = F)
table(cas_temoinsexpoi$agegestationnel.f) # voir pour 40 ou 41

cas_temoinsexpoi$agegestationnel.f<-reorder(cas_temoinsexpoi$agegestationnel.f,new.order=c(3,1,2,4))
table(cas_temoinsexpoi$agegestationnel.f)
cas_temoinsexpoi$agegestationnel.f<-relevel(cas_temoinsexpoi$agegestationnel.f,ref="[38,40)")
table(cas_temoinsexpoi$agegestationnel.f)


cas_temoinsexpoi$agegestationnel.f2<-cut(cas_temoinsexpoi$agegestationnel,breaks=c(22,38,40,45),include.lowest = T,right = F)
table(cas_temoinsexpoi$agegestationnel.f2) # voir pour 40 ou 41

cas_temoinsexpoi$agegestationnel.f3<-cut(cas_temoinsexpoi$agegestationnel,breaks=c(22,40,45),include.lowest = T,right = F)
table(cas_temoinsexpoi$agegestationnel.f3) # voir pour 40 ou 41

cas_temoinsexpoi$agegestationnel.f4<-cut(cas_temoinsexpoi$agegestationnel,breaks=c(22,38,45),include.lowest = T,right = F)
table(cas_temoinsexpoi$agegestationnel.f4) 

#cas_temoinsexpoi$agegestationnel.f<-reorder(cas_temoinsexpoi$agegestationnel.f,new.order=c(3,1,2,4))
table(cas_temoinsexpoi$agegestationnel.f)
table(cas_temoinsexpoi$agegestationnel.f2)


cas_temoinsexpoi$taille.f<-cut(cas_temoinsexpoi$tailles,breaks=c(min(cas_temoinsexpoi$tailles,na.rm=T),
                                                                                  summary(cas_temoinsexpoi$tailles)["1st Qu."], 
                                                                                  summary(cas_temoinsexpoi$tailles)["Median"],
                                                                                  summary(cas_temoinsexpoi$tailles)["3rd Qu."],
                                                                                  max(cas_temoinsexpoi$tailles,na.rm=T)+1), right=F,include.lowest=T)


cas_temoinsexpoi$taille.f2<-as.factor(ifelse(cas_temoinsexpoi$tailles < summary(cas_temoinsexpoi$tailles)["Median"],0,1))
cas_temoinsexpoi$nbfoetus.f<-as.factor(cas_temoinsexpoi$nbfoetus)


nomx<-c("age.f","poids.f","agegestationnel.f","coeffapgar5mncor.f")
j<-1
B<-NULL
for (i in cas_temoinsexpoi[,c("age.f","poids.f","agegestationnel.f","coeffapgar5mncor.f")] ){
  b<-test.qual(x=i,y=cas_temoinsexpoi$cas,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}

write.table(B,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/perisuitec.xls",sep="\t")

nomx<-c("age.f","poids.f","agegestationnel.f","coeffapgar5mncor.f")
j<-1
B<-NULL
for (i in cas_temoinsexpoi[,c("age.f","poids.f","agegestationnel.f","coeffapgar5mncor.f")] ){
  b<-test.qual(x=i,y=cas_temoinsexpoi$Source,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}

write.table(B,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/perisuites.xls",sep="\t")

nomx<-c("age.f","poids.f","agegestationnel.f","coeffapgar5mncor.f")
j<-1
B<-NULL
for (i in cas_temoinsexpoi[,c("age.f","poids.f","agegestationnel.f","coeffapgar5mncor.f")] ){
  b<-test.qual(x=i,y=cas_temoinsexpoi$extraction,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}

write.table(B,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/perisuitee.xls",sep="\t")






### par type de cancer
nomx<-c("age.f","poids.f","agegestationnel.f","coeffapgar5mncor.f")
j<-1
B<-NULL
for (i in cas_temoinsexpoi[cas_temoinsexpoi$leucemie==1,c("age.f","poids.f","agegestationnel.f","coeffapgar5mncor.f")] ){
  b<-test.qual(x=i,y=cas_temoinsexpoi$cas[cas_temoinsexpoi$leucemie==1],nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}










### les patients tombent dans combien d'iris 

iris_resume<-cas_temoinsexpoi %>%
  group_by(DCOMIRIS) %>%
  summarise (niris=n()) %>%
  do(data.frame(.$DCOMIRIS,.$niris,Source=substr(.$DCOMIRIS,1,2)))

write.table(iris_resume,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/repartition_iris.xls",sep="\t")
length(unique(cas_temoinsexpoi$DCOMIRIS))


long_unique<-function(x){length(unique(x))}

iris_resume2<-iris_resume %>%
  group_by(Source) %>%
  summarise (npts=sum(..niris),
             nbriris=long_unique(..DCOMIRIS),
             moy_pts=mean(..niris),
             max_pts=max(..niris))


write.table(iris_resume2,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/repartition_iris_resume.xls",sep="\t")



### données distance de l'expo

summary(cas_temoinsexpoi$distance_iris)
summary(cas_temoinsexpoi$distance_moy_pts)
s<-tapply(cas_temoinsexpoi$distance_iris,INDEX=cas_temoinsexpoi$Source,FUN=summary)
s<-do.call(rbind,s)

write.table(s,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/distance_dep.xls",sep="\t")



### nombre de points par IRIS pour l'expo

summary(cas_temoinsexpoi$np)
s<-tapply(cas_temoinsexpoi$np,INDEX=cas_temoinsexpoi$Source,FUN=summary)
s<-do.call(rbind,s)

write.table(s,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/nbr_pts.xls",sep="\t")

###☺ données expositions

total<-summary(cas_temoinsexpoi$moyenne_benzene[cas_temoinsexpoi$imp==1])
s<-tapply(cas_temoinsexpoi$moyenne_benzene[cas_temoinsexpoi$imp==1],INDEX=cas_temoinsexpoi$Source[cas_temoinsexpoi$imp==1],FUN=summary)
s<-do.call(rbind,s)
s<-rbind(s,total)


write.table(s,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/benzene_r_si.xls",sep="\t")

total<-summary(cas_temoinsexpoi$mopb)
s<-tapply(cas_temoinsexpoi$mopb,INDEX=cas_temoinsexpoi$Source,FUN=summary)
s<-do.call(rbind,s)
s<-rbind(s,total)


write.table(s,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/benzene_r_p.xls",sep="\t")





total<-summary(cas_temoinsexpoi$moyenne_no2[cas_temoinsexpoi$imp==1])
s<-tapply(cas_temoinsexpoi$moyenne_no2[cas_temoinsexpoi$imp==1],INDEX=cas_temoinsexpoi$Source[cas_temoinsexpoi$imp==1],FUN=summary)
s<-do.call(rbind,s)
s<-rbind(s,total)


write.table(s,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/no2_r_si.xls",sep="\t")






cas_temoinsexpoi$moyenne_benzene.f<-cut(cas_temoinsexpoi$moyenne_benzene,breaks=c(min(cas_temoinsexpoi$moyenne_benzene),
                                                                                  summary(cas_temoinsexpoi$moyenne_benzene)["1st Qu."], 
                                                                                  summary(cas_temoinsexpoi$moyenne_benzene)["Median"],
                                                                                  summary(cas_temoinsexpoi$moyenne_benzene)["3rd Qu."],
                                                                                  max(cas_temoinsexpoi$moyenne_benzene)+1), right=F,include.lowest=T)
table(cas_temoinsexpoi$moyenne_benzene.f,exclude = NULL)
summary(cas_temoinsexpoi$mopb)

cas_temoinsexpoi$moyenne_benzene.f2<-ifelse(cas_temoinsexpoi$moyenne_benzene < summary(cas_temoinsexpoi$moyenne_benzene)["Median"],0,1) # le découpage précédent est trop concentré sur les petites classes
cas_temoinsexpoi$moyenne_benzene.f2<-as.factor(cas_temoinsexpoi$moyenne_benzene.f2)
cas_temoinsexpoi$moyenne_benzene.f3<-ifelse(cas_temoinsexpoi$moyenne_benzene <2,0,1) # le découpage précédent est trop concentré sur les petites classes
cas_temoinsexpoi$moyenne_benzene.f3<-as.factor(cas_temoinsexpoi$moyenne_benzene.f3)




cas_temoinsexpoi$mopb.f<-cut(cas_temoinsexpoi$mopb,breaks=c(min(cas_temoinsexpoi$mopb),
                                                                                  summary(cas_temoinsexpoi$mopb)["1st Qu."], 
                                                                                  summary(cas_temoinsexpoi$mopb)["Median"],
                                                                                  summary(cas_temoinsexpoi$mopb)["3rd Qu."],
                                                                                  max(cas_temoinsexpoi$mopb)+1), right=F,include.lowest=T)

cas_temoinsexpoi$mopb.f2<-ifelse(cas_temoinsexpoi$mopb < summary(cas_temoinsexpoi$mopb)["Median"] ,0,1) # le découpage précédent est trop concentré sur les petites classes
cas_temoinsexpoi$mopb.f2<-as.factor(cas_temoinsexpoi$mopb.f2)
cas_temoinsexpoi$mopb.f3<-ifelse(cas_temoinsexpoi$mopb <2,0,1)
cas_temoinsexpoi$mopb.f3<-as.factor(cas_temoinsexpoi$mopb.f3)


summary(cas_temoinsexpoi$moyenne_no2)

cas_temoinsexpoi$moyenne_no2.f<-cut(cas_temoinsexpoi$moyenne_no2,breaks=c(min(cas_temoinsexpoi$moyenne_no2),
                                                                       summary(cas_temoinsexpoi$moyenne_no2)["1st Qu."], 
                                                                       summary(cas_temoinsexpoi$moyenne_no2)["Median"],
                                                                       summary(cas_temoinsexpoi$moyenne_no2)["3rd Qu."],
                                                                       max(cas_temoinsexpoi$moyenne_no2)+1), right=F,include.lowest=T)


cas_temoinsexpoi$moyenne_no2.f2<-ifelse(cas_temoinsexpoi$moyenne_no2 <  summary(cas_temoinsexpoi$moyenne_no2)["Median"],0,1)# le no2 est mieux réparti


cas_temoinsexpoi$moyenne_no2.f2<-as.factor(cas_temoinsexpoi$moyenne_no2.f2)
summary(cas_temoinsexpoi$mopn)
cas_temoinsexpoi$mopn.f<-cut(cas_temoinsexpoi$mopn,breaks=c(min(cas_temoinsexpoi$mopn),
                                                                          summary(cas_temoinsexpoi$mopn)["1st Qu."], 
                                                                          summary(cas_temoinsexpoi$mopn)["Median"],
                                                                          summary(cas_temoinsexpoi$mopn)["3rd Qu."],
                                                            max(cas_temoinsexpoi$mopn)+1), right=F,include.lowest=T)

cas_temoinsexpoi$mopn.f2<-ifelse(cas_temoinsexpoi$mopn < summary(cas_temoinsexpoi$mopn)["Median"],0,1)                                                            
cas_temoinsexpoi$mopn.f2<-as.factor(cas_temoinsexpoi$mopn.f2)          
   
summary(cas_temoinsexpoi$mopn.f)
table(cas_temoinsexpoi$moyenne_no2.f)
table(cas_temoinsexpoi$moyenne_no2.f2)
summary(cas_temoinsexpoi$moyenne_benzene.f)
summary(cas_temoinsexpoi$mopb.f)


cas_temoinsexpoi$forte_expo<-ifelse(cas_temoinsexpoi$moyenne_no2.f2==1 & cas_temoinsexpoi$moyenne_benzene.f2==1,1,0 )
cas_temoinsexpoi$forte_expo<-ifelse(cas_temoinsexpoi$moyenne_no2.f2==1 & cas_temoinsexpoi$moyenne_benzene.f2==0,2,cas_temoinsexpoi$forte_expo)
cas_temoinsexpoi$forte_expo<-ifelse(cas_temoinsexpoi$moyenne_no2.f2==0 & cas_temoinsexpoi$moyenne_benzene.f2==1,2,cas_temoinsexpoi$forte_expo)
cas_temoinsexpoi$forte_expo<-as.factor(cas_temoinsexpoi$forte_expo)

cas_temoinsexpoi$forte_expop<-ifelse(cas_temoinsexpoi$mopn.f2==1 & cas_temoinsexpoi$mopb.f2==1,1,0 )
cas_temoinsexpoi$forte_expop<-ifelse(cas_temoinsexpoi$mopn.f2==1 & cas_temoinsexpoi$mopb.f2==0,2,cas_temoinsexpoi$forte_expop)
cas_temoinsexpoi$forte_expop<-ifelse(cas_temoinsexpoi$mopn.f2==0 & cas_temoinsexpoi$mopb.f2==1,2,cas_temoinsexpoi$forte_expop)
cas_temoinsexpoi$forte_expop<-as.factor(cas_temoinsexpoi$forte_expop)


 ### c'est pas parfait mais c'est les problèmes d'intervalles ouverts/ fermé. 

nomx<-c("moyenne_benzene.f","moyenne_benzene.f2","moyenne_benzene.f3",
        "mopb.f","mopb.f2","mopb.f3","moyenne_no2.f","moyenne_no2.f2","mopn.f","mopn.f2","forte_expo","forte_expop")
j<-1
B<-NULL
for (i in c("moyenne_benzene.f","moyenne_benzene.f2", "moyenne_benzene.f3",
            "mopb.f","mopb.f2","mopb.f3","moyenne_no2.f","moyenne_no2.f2","mopn.f","mopn.f2","forte_expo","forte_expop"
           )){
  b<-test.qual(x=cas_temoinsexpoi[,i],y=cas_temoinsexpoi$cas,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}

write.table(B,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/expoc4.xls",sep="\t")










nomx<-c("moyenne_benzene.f","moyenne_benzene.f2","moyenne_benzene.f3",
        "mopb.f","mopb.f2","mopb.f3","moyenne_no2.f","moyenne_no2.f2","mopn.f","mopn.f2","forte_expo","forte_expop")
j<-1
B<-NULL
for (i in c("moyenne_benzene.f","moyenne_benzene.f2", "moyenne_benzene.f3",
            "mopb.f","mopb.f2","mopb.f3","moyenne_no2.f","moyenne_no2.f2","mopn.f","mopn.f2","forte_expo","forte_expop"
)){
  b<-test.qual(x=cas_temoinsexpoi[cas_temoinsexpoi$leucemie==1,i],y=cas_temoinsexpoi$cas[cas_temoinsexpoi$leucemie==1],nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}

write.table(B,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/expo_leucemie.xls",sep="\t")





### création de la varaible age enfant pour ajustement car on a apparié sur l'age (le département bof ) 

str(cas_temoinsexpoi$datenaissance)
repere<-as.Date("2017-01-01")

cas_temoinsexpoi$ageenf<-round(difftime(repere,cas_temoinsexpoi$datenaissance)/365,0) ## on a pris 2017 donc c'est normal d'avoir des âges sup


### Apgar à 5 min : 2 études tendrait à montrer un surisque 
# y a des valeur abrettantes avec les apgar à 0 : sont  à 10 à 1 min... et aucun geste technique, 3 echos, pas de prematurité poids ok 
# y a des valeurs aberrantes avec les apagar à 1 : 1 apgar à 10 à 1 min, les deux autres accouchement voie basse ni transfert ni geste technique , pas de polymorfations, test auditif et recherche ag hbs 
# 0 gestes techniques 

### on met ça en NA 

cas_temoinsexpoi$coeffapgar5mncor<-as.character(ifelse(cas_temoinsexpoi$coeffapgar5mn %in% c("0","1"),NA,as.character(cas_temoinsexpoi$coeffapgar5mn)))
table(cas_temoinsexpoi$coeffapgar5mncor,exclude=NULL)
cas_temoinsexpoi$coeffapgar5mncor.f <-ifelse(cas_temoinsexpoi$coeffapgar5mncor %in% c("9","10"),"9-10",cas_temoinsexpoi$coeffapgar5mncor)
cas_temoinsexpoi$coeffapgar5mncor.f <-ifelse(!cas_temoinsexpoi$coeffapgar5mncor %in% c("9","10") & !is.na(cas_temoinsexpoi$coeffapgar5mncor)," 9<",cas_temoinsexpoi$coeffapgar5mncor.f)
table(cas_temoinsexpoi$coeffapgar5mncor.f,exclude=NULL)
cas_temoinsexpoi$coeffapgar5mncor.f <-as.factor(cas_temoinsexpoi$coeffapgar5mncor.f)
cas_temoinsexpoi$coeffapgar5mncor.f <- reorder(cas_temoinsexpoi$coeffapgar5mncor.f,new.order = c(2,1))
table(cas_temoinsexpoi$coeffapgar5mncor.f,exclude=NULL)


### Les Cartes 

fdc<-aus2
fdc@data <- data.frame(fdc@data,resume_expo[match(fdc@data[, "DCOMIRIS"],resume_expo[, "DCOMIRIS"]), ])
#head(comm@data)


### moyenne benzène 
# Découpage du temps de trajet en 5 classes via la méthodes des quantiles : idenfication des bornes (breaks, ou brks)
classTemps <- classIntervals(fdc@data$moyenne_benzene, 5, style = "quantile")
# Choix d'une palette de couleur pour les 5 catégories
palette <- brewer.pal(n = 5, name = "YlOrRd")

fdc@data$moyenne_benzenet<-as.character(cut(fdc@data$moyenne_benzene, breaks = classTemps$brks, labels = palette, include.lowest = TRUE))

legende <- as.character(levels(cut(fdc@data$moyenne_benzene, breaks = classTemps$brks, include.lowest = TRUE, right = FALSE)))

#iris
plot(fdc,col=fdc@data$moyenne_benzenet)
legend("bottomright",legend=legende,fill=palette,cex=0.3,pt.cex=5)

# patientsParis
plot(fdc[substr(fdc@data$DEPCOM,1,2) %in% c("75"),],col=fdc@data$moyenne_benzenet)
plot(dist_qgis_spa_l93,add=T,col="green",type="p")

plot(fdc[substr(fdc@data$DEPCOM,1,2) %in% c("95","93","78","92"),],col=fdc@data$moyenne_benzenet)
plot(dist_qgis_spa_l93,add=T,col="green",type="p")



### moyenne no2
# Découpage du temps de trajet en 5 classes via la méthodes des quantiles : idenfication des bornes (breaks, ou brks)
classTempse <- classIntervals(fdc@data$moyenne_no2, 5, style = "quantile")
# Choix d'une palette de couleur pour les 5 catégories
palette <- brewer.pal(n = 5, name = "Blues")

fdc@data$moyenne_no2t<-as.character(cut(fdc@data$moyenne_no2, breaks = classTempse$brks, labels = palette, include.lowest = TRUE))
legenden <- as.character(levels(cut(fdc@data$moyenne_no2, breaks = classTempse$brks, include.lowest = TRUE, right = FALSE)))


#iris
plot(fdc,col=fdc@data$moyenne_no2t)
legend("bottomright",legend=legenden,fill=palette,cex=0.3,pt.cex=5)


# patientsParis
plot(fdc[substr(fdc@data$DEPCOM,1,2) %in% c("75"),],col=fdc@data$moyenne_no2t)
plot(dist_qgis_spa_l93,add=T,col="green",type="p")

plot(fdc[substr(fdc@data$DEPCOM,1,2) %in% c("95","93","78","92"),],col=fdc@data$moyenne_no2t)
plot(dist_qgis_spa_l93,add=T,col="green",type="p")


# EDI ()


quantileedi<-function(x){quantile(x,probs=c(0,0.20,0.40,0.60,0.80,1))}
quantileedi(cas_temoinsexpoi$edi07)
write.table(quantileedi(cas_temoinsexpoi$edi07),file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/edi.xls",sep="\t")

s<-tapply(cas_temoinsexpoi$edi07,INDEX=cas_temoinsexpoi$Source,FUN=quantileedi)
s<-do.call(rbind,s)

write.table(s,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/edi_r.xls",sep="\t")

cas_temoinsexpoi$most_dep<-ifelse(cas_temoinsexpoi$edi07 >= quantile(cas_temoinsexpoi$edi07,probs=c(0,0.20,0.40,0.60,0.80,1))[4],1,0)
cas_temoinsexpoi$most_dep<-as.factor(cas_temoinsexpoi$most_dep)


edit<-cas_temoinsexpoi %>%
  group_by(Source) %>%
  do(data.frame(n=table(.$most_dep,exclude=NULL),pour=c(round(prop.table(table(.$most_dep))*100,1),0)))



### carte edi 

fdcedi<-aus2
fdcedi@data <- merge(fdcedi@data,cas_temoinsexpoi[,c("DCOMIRIS","edi07")],by="DCOMIRIS",all.x=T)
#fdcedi@data<-fdcedi@data[!is.na(fdcedi@data$edi07),]
#fdcedi@data<-unique(fdcedi@data)

#head(comm@data)


### EDI
# Découpage du temps de trajet en 5 classes via la méthodes des quantiles : idenfication des bornes (breaks, ou brks)
classTemps <- classIntervals(fdcedi@data$edi07, 5, style = "quantile")
# Choix d'une palette de couleur pour les 5 catégories
palette <- brewer.pal(n = 5, name = "YlOrRd")

fdcedi@data$edi07c<-as.character(cut(fdcedi@data$edi07, breaks = classTemps$brks, labels = palette, include.lowest = TRUE))

legende <- as.character(levels(cut(fdcedi@data$edi07, breaks = classTemps$brks, include.lowest = TRUE, right = FALSE)))

#iris toute l'idf
plot(fdcedi,col=fdcedi@data$edi07c)
legend("bottomright",legend=legende,fill=palette,cex=0.3,pt.cex=5)


plot(fdcedi[substr(fdcedi@data$DCOMIRIS,1,2) %in% c("75"),],col=fdcedi@data$edi07c)
plot(dist_qgis_spa_l93,add=T,col="green",type="p")

plot(fdcedi[substr(fdcedi@data$DCOMIRIS,1,2) %in% c("95"),],col=fdcedi@data$edi07c)
plot(dist_qgis_spa_l93,add=T,col="green",type="p")


### type histologiques

histo<- cas_temoinsexpoi[cas_temoinsexpoi$cas=="1",]%>%
  group_by(Source) %>%
  do(data.frame(n=table(.$TU__FamilleDiagcor,exclude=NULL),pour=c(round(prop.table(table(.$TU__FamilleDiagcor))*100,1),0)))

histo<-as.data.frame(histo)
histo$Source<-as.factor(histo$Source)


w <- reshape(histo, 
             timevar = "Source",
             idvar = c("n.Var1"),
             direction = "wide")


g<-cas_temoinsexpoi[cas_temoinsexpoi$cas=="1",] %>%
  do(data.frame(n=table(.$TU__FamilleDiagcor,exclude=NULL),pour=c(round(prop.table(table(.$TU__FamilleDiagcor))*100,1),0)))

w<-merge(w,g,by="n.Var1")



write.table(w,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/type_grandr.xls",sep="\t")



histo<- cas_temoinsexpoi[cas_temoinsexpoi$cas=="1",]%>%
  group_by(Source) %>%
  do(data.frame(n=table(.$TU__ICDO3MorphoLibelle_zcor,exclude=NULL),pour=c(round(prop.table(table(.$TU__ICDO3MorphoLibelle_zcor))*100,1),0)))

histo<-as.data.frame(histo)
histo$Source<-as.factor(histo$Source)


w <- reshape(histo, 
             timevar = "Source",
             idvar = c("n.Var1"),
             direction = "wide")


g<-cas_temoinsexpoi[cas_temoinsexpoi$cas=="1",] %>%
  do(data.frame(n=table(.$TU__ICDO3MorphoLibelle_zcor,exclude=NULL),pour=c(round(prop.table(table(.$TU__ICDO3MorphoLibelle_zcor))*100,1),0)))

w<-merge(w,g,by="n.Var1")



write.table(w,file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/type_detr.xls",sep="\t")






cas_temoinsexpoi$oxygenotherapie<-as.factor(cas_temoinsexpoi$oxygenotherapie)
cas_temoinsexpoi$intubation<-as.factor(cas_temoinsexpoi$intubation)
cas_temoinsexpoi$antibiotherapie<-as.factor(cas_temoinsexpoi$antibiotherapie)
cas_temoinsexpoi$neurologique<-as.factor(cas_temoinsexpoi$neurologique)
cas_temoinsexpoi$urgence<-as.factor(cas_temoinsexpoi$urgence)
cas_temoinsexpoi$autre_patho<-as.factor(cas_temoinsexpoi$autre_patho)
cas_temoinsexpoi$anomalie<-as.factor(cas_temoinsexpoi$anomalie)
cas_temoinsexpoi$polymalformation<-as.factor(cas_temoinsexpoi$polymalformation)
cas_temoinsexpoi$spinabifida<-as.factor(cas_temoinsexpoi$spinabifida)
cas_temoinsexpoi$fente<-as.factor(cas_temoinsexpoi$fente)
cas_temoinsexpoi$atresie<-as.factor(cas_temoinsexpoi$atresie)
cas_temoinsexpoi$omphalocele<-as.factor(cas_temoinsexpoi$omphalocele)
cas_temoinsexpoi$reductionmembre<-as.factor(cas_temoinsexpoi$reductionmembre)
cas_temoinsexpoi$malformrenale<-as.factor(cas_temoinsexpoi$malformrenale)
cas_temoinsexpoi$hydrocephalie<-as.factor(cas_temoinsexpoi$hydrocephalie)
cas_temoinsexpoi$malformcard<-as.factor(cas_temoinsexpoi$malformcard)
cas_temoinsexpoi$trisomie<-as.factor(cas_temoinsexpoi$trisomie)
cas_temoinsexpoi$autrez<-as.factor(cas_temoinsexpoi$autrez)
cas_temoinsexpoi$gestes_techniques<-as.factor(cas_temoinsexpoi$gestes_techniques)

nomx<-c("trisomie","malformcard","hydrocephalie","malformrenale","reductionmembre")
j<-1
B<-NULL
for (i in cas_temoinsexpoi[,c("trisomie","malformcard","hydrocephalie","malformrenale","reductionmembre")] ){
    b<-test.qual(x=i,y=cas_temoinsexpoi$cas,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
   B<-rbind(B,b)
   j<-j+1
  }
