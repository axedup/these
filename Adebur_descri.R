###===Description===###

libelle<-read.csv2("C:\\Users\\Louise\\Documents\\Desespoir\\Bases\\AFF_XY.csv",stringsAsFactors = FALSE)
res_geo$MA___NumMalade<-as.character(res_geo$MA___NumMalade)
affectation<-merge(res_geo[,c("AFF","MA___NumMalade","IRIS","IRIS_2012","iris_diff")],cas_temoinsexpoi[,c("numunique","cas")],by.x="MA___NumMalade",by.y="numunique")

affectation<-affectation[,c("AFF","MA___NumMalade","IRIS","IRIS_2012","iris_diff")]
names(affectation)<-c("AFF","Référence.Enfant","IRIS","IRIS_2012","iris_diff")
res_geo_95_suite$IRIS<-as.factor(res_geo_95_suite$IRIS)
affectation<-rbind(affectation,res_geo_95_suite[c("Référence.Enfant","AFF","IRIS","IRIS_2012","iris_diff")])

dim(affectation)
prop.table(table(affectation$AFF))



libelle$AFF<-libelle$Nom_Index
libelle$AFF<-gsub(pattern = "(^[0-9]{1})([0-9]{2})",replacement="\\2",x=libelle$AFF)
libelle$AFF<-gsub(pattern = "(^[0]{1})([0-9]{1,})",replacement="\\2",x=libelle$AFF)
affectation<-merge(affectation,libelle,by="AFF")
affectation$lib<-as.factor(affectation$Lib_Affectation)

summary(affectation)

### géocoadge
quali(x=c("lib"),nomx=c("Qualité du géocodage"), data=affectation,RAPPORT=F,SAVEFILE=F,ordonner=c(FALSE), numerique=c(TRUE), seq=list(c(19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)),chemin="C:/Users/Louise/Documents/Desespoir/Bases/resultats/",fichier="geo")

### iris

affectation$codage_iris<-ifelse( grepl("^[0-9]{9}",x=affectation$IRIS) & affectation$IRIS_2012=="-1"  & !is.na(affectation$IRIS_2012)& affectation$iris_diff==0,1,0)
affectation$codage_iris<-ifelse( grepl("^[0-9]{9}",x=affectation$IRIS) & is.na(affectation$IRIS_2012) & affectation$iris_diff==1 & affectation$AFF %in% c("0","1","10","11"),2,affectation$codage_iris)
affectation$codage_iris<-ifelse( grepl("^[0-9]{9}",x=affectation$IRIS) & affectation$IRIS_2012=="-1" & !is.na(affectation$IRIS_2012) & affectation$iris_diff==1 ,3,affectation$codage_iris)
affectation$codage_iris<-ifelse( !grepl("^[0-9]{9}",x=affectation$IRIS) & affectation$AFF %in% c("0","1","10","11"),4,affectation$codage_iris)

affectation$codage_iris<-factor(affectation$codage_iris,labels=c("Attribution à l'IRIS base impossible et géolocalisation imprécise","Attribution à l'IRIS base correcte","IRIS base supprimé et géolocalisation correcte","Discordance","IRIS base non affecté mais géolocalisation correcte"))
table(affectation$codage_iris)


quali(x=c("codage_iris"),nomx=c("IRIS"), data=affectation,RAPPORT=F,SAVEFILE=F,ordonner=c(FALSE), numerique=c(TRUE), seq=list(c(19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)),chemin="C:/Users/Louise/Documents/Desespoir/Bases/resultats/",fichier="IRIS")


### caractéristiques périnatales 

cas_temoinsexpoi$tailles<-cas_temoinsexpoi$tailles/10


quantifb(xc=c("age","agegestationnel","tailles","poids","perimetre2"),nomx=c("age en an","terme","taille en cm","poids en g","perimetre en cm"), data=cas_temoinsexpoi,RAPPORT=F,SAVEFILE=T,chemin="C:/Users/Louise/Documents/Desespoir/Bases/resultats/",fichier="peri")
test.quant(varquant=c("age","agegestationnel","tailles","poids","perimetre2"),nomquant=c("age en an","terme","taille en cm","poids en g","perimetre en cm"), varqual=c("cas"),nomqual=c("cas"),data=cas_temoinsexpoi,RAPPORT=F,SAVEFILE=T,chemin="C:/Users/Louise/Documents/Desespoir/Bases/resultats/",fichier="peric")



#quali(x=c("sexe","niveauetudes","parite.f","parite.f2","gestite.f","gestite.f2","naissancepar","naissancepar.f2","coeffapgar1mn","coeffapgar5mn"),nomx=c("sexe","etudes","parite","parite.f2","gestite","gestite.f","mode d'accouchement","voie basse ou césar","Apgar 1 min","Apgar 5 min"), data=cas_temoinsexpoi,RAPPORT=F,SAVEFILE=F,chemin=NULL)

#desc.qual2(x=c("sexe","niveauetudes","parite.f","parite.f2","gestite.f","gestite.f2","naissancepar","naissancepar.f2"),y=cas_temoinsexpoi$cas,nomx=c("sexe","etudes","parite","parite.f2","gestite","gestite.f","mode d'accouchement","voie basse ou césar"), data=cas_temoins_parisexpoi,RAPPORT=F,SAVEFILE=T,chemin="C:/Users/Louise/Documents/Desespoir/Bases/resultats/")


nomx<-c("sexe","etudes","parite","parite.f2","gestite","gestite.f","mode d'accouchement","voie basse ou césar")
j<-1
B<-NULL
for (i in cas_temoins_parishbis[,c("sexe","niveauetudes","parite.f","parite.f2","gestite.f","gestite.f2","naissancepar","naissancepar.f2")] ){
  b<-test.qual(x=i,y=cas_temoins_parishbis$cas,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
  B<-rbind(B,b)
  j<-j+1
}

write.table(B,sep="/t",file="C:/Users/Louise/Documents/Desespoir/Bases/resultats/pericc.xls")


### on va faire les catégories pour age mater, poids terme...;
ggplot(cas_temoinsexpoi,aes(age))+geom_freqpoly()
cas_temoinsexpoi$age.f<-cut(cas_temoinsexpoi$age,breaks=c(0,25,30,35,40,80),include.lowest = T,right = F)

table(cas_temoinsexpoi$age.f,cas_temoinsexpoi$age)
table(cas_temoinsexpoi$age.f)


ggplot(cas_temoinsexpoi,aes(poids))+geom_freqpoly()
cas_temoinsexpoi$poids.f<-cut(cas_temoinsexpoi$poids,breaks=c(0,2500,3000,3500,4000,8000),include.lowest = T,right = F)
table(cas_temoinsexpoi$poids)
table(cas_temoinsexpoi$poids.f)

plot(cas_temoinsexpoi$agegestationnel,cas_temoinsexpoi$poids)

ggplot(cas_temoinsexpoi,aes(agegestationnel))+geom_freqpoly() # l'age gesta sert plus de varaiables d'ajustement donc les classes sont peut être peu importantes
cas_temoinsexpoi$agegestationnel.f<-cut(cas_temoinsexpoi$agegestationnel,breaks=c(22,35,38,40,45),include.lowest = T,right = F)
table(cas_temoinsexpoi$agegestationnel.f) # voir pour 40 ou 41

git commit

###☺ données expositions

summary(cas_temoinsexpoi$moyenne_benzene)
cas_temoinsexpoi$moyenne_benzene.f<-cut(cas_temoinsexpoi$moyenne_benzene,breaks=c(min(cas_temoinsexpoi$moyenne_benzene),
                                                                                  summary(cas_temoinsexpoi$moyenne_benzene)["1st Qu."], 
                                                                                  summary(cas_temoinsexpoi$moyenne_benzene)["Median"],
                                                                                  summary(cas_temoinsexpoi$moyenne_benzene)["3rd Qu."],
                                                                                  max(cas_temoinsexpoi$moyenne_benzene)+1), right=F,include.lowest=T)
table(cas_temoinsexpoi$moyenne_benzene.f,exclude = NULL)
summary(cas_temoinsexpoi$mopb)
cas_temoinsexpoi$mopb.f<-cut(cas_temoinsexpoi$mopb,breaks=c(min(cas_temoinsexpoi$mopb),
                                                                                  summary(cas_temoinsexpoi$mopb)["1st Qu."], 
                                                                                  summary(cas_temoinsexpoi$mopb)["Median"],
                                                                                  summary(cas_temoinsexpoi$mopb)["3rd Qu."],
                                                                                  max(cas_temoinsexpoi$mopb)+1), right=F,include.lowest=T)

summary(cas_temoinsexpoi$moyenne_no2)
cas_temoinsexpoi$moyenne_no2.f<-cut(cas_temoinsexpoi$moyenne_no2,breaks=c(min(cas_temoinsexpoi$moyenne_no2),
                                                                       summary(cas_temoinsexpoi$moyenne_no2)["1st Qu."], 
                                                                       summary(cas_temoinsexpoi$moyenne_no2)["Median"],
                                                                       summary(cas_temoinsexpoi$moyenne_no2)["3rd Qu."],
                                                                       max(cas_temoinsexpoi$moyenne_no2)+1), right=F,include.lowest=T)

summary(cas_temoinsexpoi$mopn)
cas_temoinsexpoi$mopn.f<-cut(cas_temoinsexpoi$mopn,breaks=c(min(cas_temoinsexpoi$mopn),
                                                                          summary(cas_temoinsexpoi$mopn)["1st Qu."], 
                                                                          summary(cas_temoinsexpoi$mopn)["Median"],
                                                                          summary(cas_temoinsexpoi$mopn)["3rd Qu."],
                                                                          max(cas_temoinsexpoi$mopn)+1), right=F,include.lowest=T)
summary(cas_temoinsexpoi$mopn.f)
summary(cas_temoinsexpoi$moyenne_no2.f)
summary(cas_temoinsexpoi$moyenne_benzene.f)
summary(cas_temoinsexpoi$mopb.f)
 ### c'est pas parfait mais c'est les problèmes d'intervalles ouverts/ fermé. 


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
