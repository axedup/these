type_cancer<-read.csv2(file = "L:/20160825_Liste_cas_IDF_1013.csv",na.strings ="")
type_cancer$datenaissance<-as.character(type_cancer$MA__DateNaissance)
type_cancer$datenaissance<-as.Date(type_cancer$datenaissance,format="%d/%m/%Y")


type_cancer$datediag<-as.character(type_cancer$TU__DateDiag)
type_cancer$datediag<-as.Date(type_cancer$datediag,format="%d/%m/%Y")

type_cancer$sexe<-type_cancer$MA__Sexe
type_cancer$cp_naissance<-type_cancer$Departement

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


anapath$TU__FamilleDiagcor<-NA
anapath$TU__FamilleDiagcor<-ifelse(!is.na(anapath$TU__DiagEnClair_Saisie), ifelse(anapath$TU__DiagEnClair_Saisie %in% c("HEPATOBASTOME","HEPATOBLASTOME","HEPATO CARCINOME"),"FOIE",anapath$TU__FamilleDiagcor),anapath$TU__FamilleDiagcor)
anapath$TU__FamilleDiagcor<-ifelse(!is.na(anapath$TU__DiagEnClair_Saisie) & grepl(pattern="LAL|LAM|LEUCEMIE",x=anapath$TU__DiagEnClair_Saisie)
                                   ,"LA",anapath$TU__FamilleDiagcor)
anapath$TU__FamilleDiagcor<-ifelse(!is.na(anapath$TU__DiagEnClair_Saisie) & grepl(pattern="ETMR|ASTROCYTOME|EPENDYMOME|GLIOME|PINEALOBLASTOME|CEREBRALE|GLIOBLASTOME|MEDULLOMYOBLASTOME",x=anapath$TU__DiagEnClair_Saisie)
                                   ,"TC",anapath$TU__FamilleDiagcor)
anapath$TU__FamilleDiagcor<-ifelse(!is.na(anapath$TU__DiagEnClair_Saisie) & grepl(pattern="RETINOBLASTOME",x=anapath$TU__DiagEnClair_Saisie)
                                   ,"RETINO",anapath$TU__FamilleDiagcor)
anapath$TU__FamilleDiagcor<-ifelse(!is.na(anapath$TU__DiagEnClair_Saisie) & grepl(pattern="NEPHROBLASTOME|NEPHROME",x=anapath$TU__DiagEnClair_Saisie)
                                   ,"REIN",anapath$TU__FamilleDiagcor)
anapath$TU__FamilleDiagcor<-ifelse(!is.na(anapath$TU__DiagEnClair_Saisie) & grepl(pattern="TERATOME|KYSTE DERMOIDE|GERMINALE|GRANULOSA",x=anapath$TU__DiagEnClair_Saisie)
                                   ,"TGM",anapath$TU__FamilleDiagcor)
anapath$TU__FamilleDiagcor<-ifelse(!is.na(anapath$TU__DiagEnClair_Saisie) & grepl(pattern="LYMPHOME",x=anapath$TU__DiagEnClair_Saisie)
                                   ,"LNH",anapath$TU__FamilleDiagcor)
anapath$TU__FamilleDiagcor<-ifelse(!is.na(anapath$TU__DiagEnClair_Saisie) & grepl(pattern="NEUROBLASTOME|NEUROBLASTIQUE",x=anapath$TU__DiagEnClair_Saisie)
                                   ,"SNS",anapath$TU__FamilleDiagcor)
anapath$TU__FamilleDiagcor<-ifelse(!is.na(anapath$TU__DiagEnClair_Saisie) & grepl(pattern="EWING",x=anapath$TU__DiagEnClair_Saisie)
                                   ,"OS",anapath$TU__FamilleDiagcor)


anapath$TU__FamilleDiagcor<-ifelse(anapath$TU__FamilleDiag=="nondeter",anapath$TU__FamilleDiagcor,anapath$TU__FamilleDiag)
anapath$TU__FamilleDiagcor<-ifelse(is.na(anapath$TU__FamilleDiagcor),"nondeter",anapath$TU__FamilleDiagcor)

anapath$TU__ICDO3MorphoLibelle_z<-as.character(anapath$TU__ICDO3MorphoLibelle_z)
anapath$TU__ICDO3MorphoLibelle_z<-ifelse(is.na(anapath$TU__ICDO3MorphoLibelle_z),"nondeter",anapath$TU__ICDO3MorphoLibelle_z)

anapath$TU__ICDO3MorphoLibelle_zcor<-NA
anapath$TU__ICDO3MorphoLibelle_zcor<-ifelse(anapath$TU__ICDO3MorphoLibelle_z=="nondeter",anapath$TU__DiagEnClair_Saisie,anapath$TU__ICDO3MorphoLibelle_z)
anapath$TU__ICDO3MorphoLibelle_zcor<-ifelse(is.na(anapath$TU__ICDO3MorphoLibelle_zcor),"nondeter",anapath$TU__ICDO3MorphoLibelle_zcor)




final<-NULL
anapath$strates<-NA
anapath$grandtype<-NA
anapath$categories<-NA
anapath$libelle<-NA
anapath$categoriescor<-NA
anapath$libellecor<-NA
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
  ct$categoriescor<-rep(ct$TU__FamilleDiagcor[ct$cas=="1"],11)
  ct$libellecor<-rep(ct$TU__ICDO3MorphoLibelle_zcor[ct$cas=="1"],11)
  ct$strates<-rep(ct$numunique[ct$cas=="1"],11)
  
  final<-rbind(final,ct)
}


final$leucemie<-ifelse(final$categoriescor %in% c("LA","LAL","LAM"),1,0)
final$tembryonnaire<-ifelse(grepl(pattern="blastome|latome",x=final$libellecor),1,0)
final$tc<-ifelse(final$categoriescor %in% c("TC"),1,0)

sauvbaseparis95->cas_temoinsexpoi


cas_temoinsexpoi<-final




sink("Repartition.txt")
table(anapath$TU__FamilleDiagcor,anapath$cas,exclude=NULL)
sink()


sink("Repartitiond.txt")
table(anapath$TU__ICDO3MorphoLibelle_z,anapath$cas,exclude=NULL)
sink()