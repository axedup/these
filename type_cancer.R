type_cancer<-read.csv2(file = "L:/20160823_Liste_cas_IDF_1013.csv")
cor_cas_paris<-read.csv(file="C:/Users/Louise/Documents/Desespoir/Bases/correspondance_cas_paris.csv")


type_cancer<-merge(type_cancer,cor_cas_paris,by.x="MA___NumMalade",by.y="V1",all.x=TRUE)
dim(type_cancer)

type_cancer$numunique<-ifelse(!is.na(type_cancer$V2),type_cancer$V2,type_cancer$MA___NumMalade)

anapath<-merge(cas_temoinsexpoi,type_cancer,by="numunique",all.x=T)

sink("Repartition.txt")
table(anapath$TU__FamilleDiag,anapath$cas,exclude=NULL)
sink()


sink("Repartitiond.txt")
table(anapath$TU__ICDO3MorphoLibelle_z,anapath$cas,exclude=NULL)
sink()