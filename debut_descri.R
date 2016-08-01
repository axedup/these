###===Description===###

libelle<-read.csv2("C:\\Users\\Louise\\Documents\\Desespoir\\Bases\\AFF_XY.csv",stringsAsFactors = FALSE)
res_geo$MA___NumMalade<-as.character(res_geo$MA___NumMalade)
affectation<-merge(res_geo[,c("AFF","MA___NumMalade")],cas_temoinsexpoi[,c("numunique","cas")],by.x="MA___NumMalade",by.y="numunique")

affectation<-affectation[,c("AFF","MA___NumMalade")]
names(affectation)<-c("AFF","Référence.Enfant")
affectation<-rbind(affectation,res_geo_95_suite[c("Référence.Enfant","AFF")])

dim(affectation)
prop.table(table(affectation$AFF))



libelle$AFF<-libelle$Nom_Index
libelle$AFF<-gsub(pattern = "(^[0-9]{1})([0-9]{2})",replacement="\\2",x=libelle$AFF)
libelle$AFF<-gsub(pattern = "(^[0]{1})([0-9]{1,})",replacement="\\2",x=libelle$AFF)
affectation<-merge(affectation,libelle,by="AFF")
affectation$lib<-as.factor(affectation$Lib_Affectation)

summary(affectation)

quali(x=c("lib"),nomx=c("Qualité du géocodage"), data=affectation,RAPPORT=F,SAVEFILE=T,ordonner=c(TRUE), seq=list(c(19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)),chemin="C:\\Users\\Louise\\Documents\\Desespoir\\Bases\\resultats\\")
