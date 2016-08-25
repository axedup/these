type_cancer<-read.csv2(file = "L:/20160823_Liste_cas_IDF_1013.csv",na.strings ="")
cor_cas_paris<-read.csv(file="C:/Users/Louise/Documents/Desespoir/Bases/correspondance_cas_paris.csv")

#cas_temoinsexpoi$extraction<-ifelse(cas_temoinsexpoi$Source=="75",75,95)
type_cancer<-merge(type_cancer,cor_cas_paris,by.x="MA___NumMalade",by.y="V1",all.x=TRUE)
dim(type_cancer)

type_cancer$numunique<-ifelse(!is.na(type_cancer$V2),type_cancer$V2,type_cancer$MA___NumMalade)

anapath<-merge(cas_temoinsexpoi,type_cancer,by="numunique",all.x=T)
anapath$TU__DiagEnClair_Auto<-as.character(anapath$TU__DiagEnClair_Auto)
anapath$TU__DiagEnClair_Auto<-ifelse(is.na(anapath$TU__DiagEnClair_Auto),"nondeter",anapath$TU__DiagEnClair_Auto)

anapath$TU__FamilleDiag<-as.character(anapath$TU__FamilleDiag)
anapath$TU__FamilleDiag<-ifelse(is.na(anapath$TU__FamilleDiag),"nondeter",anapath$TU__FamilleDiag)

anapath$TU__ICDO3MorphoLibelle_z<-as.character(anapath$TU__ICDO3MorphoLibelle_z)
anapath$TU__ICDO3MorphoLibelle_z<-ifelse(is.na(anapath$TU__ICDO3MorphoLibelle_z),"nondeter",anapath$TU__ICDO3MorphoLibelle_z)


final<-NULL
anapath$grandtype<-NA
anapath$categories<-NA
anapath$libelle<-NA
for (i in anapath$numunique[anapath$cas=="1"]){
  
  j<-anapath$datenaissance[anapath$numunique==i]
  ex<- anapath[anapath$datenaissance==j & anapath$cas=="1" & anapath$numunique==i ,"extraction"]
  acompl<-  anapath[anapath$datenaissance==j & anapath$extraction==ex,]
  cas<-acompl[acompl$cas=="1" & is.na(acompl$grandtype),]
  cas<-cas[1,]
  cas_num<-cas$numunique
  temoins<-acompl[acompl$cas=="0" & is.na(acompl$grandtype),]
  temoins<-temoins[1:10,]
  temoins_num<-temoins$numunique
  ct<-rbind(cas,temoins)
  
  anapath[anapath$datenaissance==j & anapath$extraction==ex & anapath$numunique==cas_num,"grandtype"]<-ct$TU__DiagEnClair_Auto[ct$cas=="1"]
  anapath[anapath$datenaissance==j & anapath$extraction==ex & anapath$numunique %in% temoins_num,"grandtype"]<-rep(ct$TU__DiagEnClair_Auto[ct$cas=="1"],10)
  
  
  ct$categories<-rep(ct$TU__FamilleDiag[ct$cas=="1"],11)
  ct$grandtype<-rep(ct$TU__DiagEnClair_Auto[ct$cas=="1"],11)
  ct$libelle<-rep(ct$TU__ICDO3MorphoLibelle_z[ct$cas=="1"],11)
  final<-rbind(final,ct)
}


final$leucemie<-ifelse(final$categories %in% c("LA","LAL","LAM"),1,0)
final$tembryonnaire<-ifelse(grepl(pattern="blastome|latome",x=final$libelle),1,0)
final$tc<-ifelse(final$categories %in% c("SNC","TC"),1,0)

#sauvbaseparis95->cas_temoinsexpoi
cas_temoinsexpoi<-final




sink("Repartition.txt")
table(anapath$TU__FamilleDiag,anapath$cas,exclude=NULL)
sink()


sink("Repartitiond.txt")
table(anapath$TU__ICDO3MorphoLibelle_z,anapath$cas,exclude=NULL)
sink()