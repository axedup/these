data_paris$cas<-ifelse(data_paris$numt %in% cas_paris$numt,1,0)
table(data_paris$cas)


data_1013$cas<-ifelse(data_1013$numt %in% c(cas_95$numt,cas_78$numt),1,0)
table(data_1013$cas)

cohorte<-rbind(data_paris,data_1013[!data_1013$Source %in% c("75","91","92","93","94","77"),])

# on en perd car y a ceux qui ne résident pas a Pairs ou Val de marne ou Yvelines
cohorte$cp_naissance<-substr(cohorte$commune_nais_code,1,2)
table(cohorte$cas)
dim(cohorte)


cohorte<-merge(cohorte,cas_paris[,c("numt","MA___NumMalade")],by="numt",all.x=T,suffixes=c("",".75"))
dim(cohorte)
cohorte$MA___NumMalade.75<-cohorte$MA___NumMalade
table(cohorte$MA___NumMalade.75)

cohorte<-merge(cohorte,cas_95[,c("numt","MA___NumMalade")],by="numt",all.x=T,suffixes=c("",".95"))
cohorte<-merge(cohorte,cas_78[,c("numt","MA___NumMalade")],by="numt",all.x=T,suffixes=c("",".78"))
dim(cohorte)


cohorte$MA___NumMalade<-NA
cohorte$MA___NumMalade<-ifelse(!is.na(cohorte$MA___NumMalade.75),cohorte$MA___NumMalade.75,cohorte$MA___NumMalade)
cohorte$MA___NumMalade<-ifelse(!is.na(cohorte$MA___NumMalade.95),cohorte$MA___NumMalade.95,cohorte$MA___NumMalade)
cohorte$MA___NumMalade<-ifelse(!is.na(cohorte$MA___NumMalade.78),cohorte$MA___NumMalade.78,cohorte$MA___NumMalade)
table(cohorte$MA___NumMalade)


cohorte<-merge(cohorte,type_cancer[,c("MA___NumMalade", "MA__DateNaissance", "MA__EtatCivilVerifie", 
                                    "MA__Nom", "MA__Prenom", "MA__NomJFMere", "MA__Sexe", "MA__Sexe_MF_z", 
                                    "MA__LieuNaiss_INSEE_z", "MA__LieuNaissCom", "MA__LieuNaissDept", 
                                    "TU___NumTumeur", "TU___NumTumeur_HS_z", "TU__AnneeRegistre", 
                                    "TU__DateDiag", "TU__DateNaissance_z", "TU__Exclusion", "TU__FamilleDiag", 
                                    "TU__DiagEnClair_Auto", "TU__ICDO3MorphoCode_z", "TU__ICDO3MorphoLibelle_z", 
                                    "TU__ICDO3TopoCode_z", "TU__ICDO3TopoLibelle_z", "TU__ICCCGroupeSousGroupe_z", 
                                    "TU__ICCCGroupe", "TU__ICCCSousGroupe", "TU__DiagEnClair_Saisie", 
                                    "CodeInseeValide", "CodePostal", "Departement", "LibelleCommune", 
                                    "MAJ_JGR", "R_gion", "verifcom", "X.x", "V2.x", "numunique")],by="MA___NumMalade",all.x=T)
dim(cohorte)
cohorte$TU__DiagEnClair_Auto<-as.character(cohorte$TU__DiagEnClair_Auto)
cohorte$TU__DiagEnClair_Auto<-ifelse(is.na(cohorte$TU__DiagEnClair_Auto),"nondeter",cohorte$TU__DiagEnClair_Auto)

cohorte$TU__FamilleDiag<-as.character(cohorte$TU__FamilleDiag)
cohorte$TU__FamilleDiag<-ifelse(is.na(cohorte$TU__FamilleDiag),"nondeter",cohorte$TU__FamilleDiag)


cohorte$TU__FamilleDiagcor<-NA
cohorte$TU__FamilleDiagcor<-ifelse(!is.na(cohorte$TU__DiagEnClair_Saisie), ifelse(cohorte$TU__DiagEnClair_Saisie %in% c("HEPATOBASTOME","HEPATOBLASTOME","HEPATO CARCINOME"),"FOIE",cohorte$TU__FamilleDiagcor),cohorte$TU__FamilleDiagcor)
cohorte$TU__FamilleDiagcor<-ifelse(!is.na(cohorte$TU__DiagEnClair_Saisie) & grepl(pattern="LAL|LAM|LEUCEMIE",x=cohorte$TU__DiagEnClair_Saisie)
                                   ,"LA",cohorte$TU__FamilleDiagcor)
cohorte$TU__FamilleDiagcor<-ifelse(!is.na(cohorte$TU__DiagEnClair_Saisie) & grepl(pattern="ETMR|ASTROCYTOME|EPENDYMOME|GLIOME|PINEALOBLASTOME|CEREBRALE|GLIOBLASTOME|MEDULLOMYOBLASTOME",x=cohorte$TU__DiagEnClair_Saisie)
                                   ,"TC",cohorte$TU__FamilleDiagcor)
cohorte$TU__FamilleDiagcor<-ifelse(!is.na(cohorte$TU__DiagEnClair_Saisie) & grepl(pattern="RETINOBLASTOME",x=cohorte$TU__DiagEnClair_Saisie)
                                   ,"RETINO",cohorte$TU__FamilleDiagcor)
cohorte$TU__FamilleDiagcor<-ifelse(!is.na(cohorte$TU__DiagEnClair_Saisie) & grepl(pattern="NEPHROBLASTOME|NEPHROME",x=cohorte$TU__DiagEnClair_Saisie)
                                   ,"REIN",cohorte$TU__FamilleDiagcor)
cohorte$TU__FamilleDiagcor<-ifelse(!is.na(cohorte$TU__DiagEnClair_Saisie) & grepl(pattern="TERATOME|KYSTE DERMOIDE|GERMINALE|GRANULOSA",x=cohorte$TU__DiagEnClair_Saisie)
                                   ,"TGM",cohorte$TU__FamilleDiagcor)
cohorte$TU__FamilleDiagcor<-ifelse(!is.na(cohorte$TU__DiagEnClair_Saisie) & grepl(pattern="LYMPHOME",x=cohorte$TU__DiagEnClair_Saisie)
                                   ,"LNH",cohorte$TU__FamilleDiagcor)
cohorte$TU__FamilleDiagcor<-ifelse(!is.na(cohorte$TU__DiagEnClair_Saisie) & grepl(pattern="NEUROBLASTOME|NEUROBLASTIQUE",x=cohorte$TU__DiagEnClair_Saisie)
                                   ,"SNS",cohorte$TU__FamilleDiagcor)
cohorte$TU__FamilleDiagcor<-ifelse(!is.na(cohorte$TU__DiagEnClair_Saisie) & grepl(pattern="EWING",x=cohorte$TU__DiagEnClair_Saisie)
                                   ,"OS",cohorte$TU__FamilleDiagcor)


cohorte$TU__FamilleDiagcor<-ifelse(cohorte$TU__FamilleDiag=="nondeter",cohorte$TU__FamilleDiagcor,cohorte$TU__FamilleDiag)
cohorte$TU__FamilleDiagcor<-ifelse(is.na(cohorte$TU__FamilleDiagcor),"nondeter",cohorte$TU__FamilleDiagcor)

cohorte$TU__ICDO3MorphoLibelle_z<-as.character(cohorte$TU__ICDO3MorphoLibelle_z)
cohorte$TU__ICDO3MorphoLibelle_z<-ifelse(is.na(cohorte$TU__ICDO3MorphoLibelle_z),"nondeter",cohorte$TU__ICDO3MorphoLibelle_z)

cohorte$TU__ICDO3MorphoLibelle_zcor<-NA
cohorte$TU__ICDO3MorphoLibelle_zcor<-ifelse(cohorte$TU__ICDO3MorphoLibelle_z=="nondeter",cohorte$TU__DiagEnClair_Saisie,cohorte$TU__ICDO3MorphoLibelle_z)
cohorte$TU__ICDO3MorphoLibelle_zcor<-ifelse(is.na(cohorte$TU__ICDO3MorphoLibelle_zcor),"nondeter",cohorte$TU__ICDO3MorphoLibelle_zcor)


cohorte$leucemie<-ifelse(cohorte$TU__FamilleDiagcor %in% c("LA","LAL","LAM"),1,0)
cohorte$tembryonnaire<-ifelse(grepl(pattern="blastome|latome",x=cohorte$TU__ICDO3MorphoLibelle_zcor),1,0)
cohorte$tc<-ifelse(cohorte$TU__FamilleDiagcor %in% c("TC"),1,0)


table(cohorte$taille)

cohorte$tailles<-gsub(pattern="(^[0-9]{2})\\.([0-9]{1})$", replacement="\\1\\2",x=as.character(cohorte$taille))
cohorte$tailles<-gsub(pattern="(^[0-9]{2})\\.([0-9]{1})([0-9]{1,})$", replacement="\\1\\2",x=as.character(cohorte$taille))
cohorte$tailles<-gsub(pattern="(^[0-9]{2})$", replacement="\\10",x=as.character(cohorte$tailles))
cohorte$tailles<-ifelse(cohorte$tailles=="48.5999984741211","486",cohorte$tailles)
cohorte$tailles<-ifelse(cohorte$tailles=="0",NA,cohorte$tailles)     
cohorte$tailles<-ifelse(cohorte$tailles=="5",NA,cohorte$tailles)     
cohorte$tailles<-gsub(pattern="(^[0-9]{2})\\.([0-9]{1})$", replacement="\\1\\2",x=as.character(cohorte$tailles))
cohorte$tailles<-as.numeric(cohorte$tailles)
table(cohorte$tailles)
summary(cohorte$tailles)
### corection incohérance selon
cohorte$tailles<-ifelse(cohorte$tailles >750,NA,cohorte$tailles)
#♦cohorte$tailles<-ifelse(cohorte$tailles <25,NA,cohorte$tailles)
cohorte$tailles<-as.numeric(as.character(cohorte$tailles))


cohorte$niveauetudes<-as.factor(as.character(cohorte$niveauetudes)) 
cohorte$sexe<-as.factor(as.character(cohorte$sexe)) 

### vérifier gestité 
table(cohorte$gestite)
cohorte$gestite.f<-as.factor(as.character(cohorte$gestite)) 
cohorte$gestite.f2<-ifelse(cohorte$gestite >= 4,"4 et +", cohorte$gestite) 
cohorte$gestite.f2<-as.factor(cohorte$gestite.f2) 

table(cohorte$parite)
cohorte$parite<-ifelse(cohorte$parite > 25,NA,cohorte$parite)
cohorte$parite.f<-as.factor(as.character(cohorte$parite)) 
cohorte$parite.f2<-ifelse(cohorte$parite >= 4,"4 et +", cohorte$parite) 
cohorte$parite.f2<-as.factor(cohorte$parite.f2) 

cohorte$naissancepar<-factor(cohorte$naissancepar,labels=c("vbni","vbi","cesar prog","cesar urg","cesar sp"))
cohorte$naissancepar.f2<-cohorte$naissancepar
levels(cohorte$naissancepar.f2)<-c("vbni","vbi" ,"cesar" ,"cesar","cesar") 
table(cohorte$naissancepar.f2)


table(cohorte$datenaissance_mere,exclude=NULL)
table(cohorte$datenaissance_mere,is.na(as.Date(as.character(cohorte$datenaissance_mere),format="%d/%m/%Y")),exclude=NULL)

cohorte$datenaissance_meres<-gsub(pattern="(^[0-9]{4})\\-([0-9]{2})\\-([0-9]{2})$", replacement="\\3\\/\\2\\/\\1",x=cohorte$datenaissance_mere)

table(cohorte$datenaissance_mere,exclude=NULL)
table(cohorte$datenaissance_mere,is.na(as.Date(as.character(cohorte$datenaissance_mere),format="%d/%m/%Y")),exclude=NULL)

table(is.na(as.Date(as.character(cohorte$datenaissance_meres),format="%d/%m/%Y")),exclude=NULL)

cohorte$datenaissance_meres<-as.Date(as.character(cohorte$datenaissance_meres),format="%d/%m/%Y")
table(cohorte$datenaissance,exclude=NULL)
table(cohorte$datenaissance_meres,exclude=NULL)
cohorte$age<-round(difftime(cohorte$datenaissance,cohorte$datenaissance_meres)/365,0)
cohorte$age<-as.numeric(cohorte$age)
summary(cohorte$age)
cohorte$age<-ifelse(cohorte$age >55,NA,cohorte$age)
cohorte$age<-ifelse(cohorte$age <13,NA,cohorte$age)
summary(cohorte$age)

cohorte$cas<-as.factor(cohorte$cas)

dim(cohorte)
table(cohorte$cas)

table(cohortebis$datenaissance)
table(cohortebis$perimetre)

cohorte$perimetre2<-as.character(cohorte$perimetre)
table(cohorte$perimetre,exclude=NULL)
table(cohorte$perimetre2)
cohorte$perimetre2<-gsub(pattern="(^[0-9]{2})\\.([0-9]{1})$", replacement="\\1\\2",x=as.character(cohorte$perimetre2))
cohorte$perimetre2<-gsub(pattern="(^[0-9]{2})\\.([0-9]{1})([0-9]{1,})$", replacement="\\1\\2",x=as.character(cohorte$perimetre2))
cohorte$perimetre2<-gsub(pattern="(^[0-9]{2})$", replacement="\\10",x=as.character(cohorte$perimetre2))
cohorte$perimetre2<-ifelse(cohorte$perimetre=="0",NA,cohorte$perimetre2)     
cohorte$perimetre2<-ifelse(cohorte$perimetre=="3.3",NA,cohorte$perimetre2)     
cohorte$perimetre2<-gsub(pattern="(^[0-9]{2})\\.([0-9]{1})$", replacement="\\1\\2",x=as.character(cohorte$perimetre2))
cohorte$perimetre2<-as.numeric(cohorte$perimetre2)
table(cohorte$perimetre2)




cohorte$parite.f3<-cohorte$parite.f2
cohorte$parite.f3<-ifelse(cohorte$parite >2,"3 et +",cohorte$parite.f2)
table(cohorte$parite.f3)

cohorte$parite.f3<-as.factor(cohorte$parite.f3)

cohorte$tailles<-cohorte$tailles/10

cohorte$vb<-as.factor(ifelse(cohorte$naissancepar.f %in% c("vbi","vbni") & !is.na(cohorte$naissancepar.f2),"vb",cohorte$naissancepar.f2))
cohorte$presentation<-as.factor(cohorte$presentation)
cohorte$debuttravail<-as.factor(cohorte$debuttravail)

### on va faire les catégories pour age mater, poids terme...;
#ggplot(cohorte,aes(age))+geom_freqpoly()
cohorte$age.f<-cut(cohorte$age,breaks=c(0,25,30,35,40,80),include.lowest = T,right = F)

table(cohorte$age.f,cohorte$age)
table(cohorte$age.f)
# class de ref 30 -35 
cohorte$age.f<-reorder(cohorte$age.f,new.order=c(3,1,2,4,5))
cohorte$age.f<-relevel(cohorte$age.f,ref="[30,35)")
table(cohorte$age.f)

cohorte$age.f2<-cut(cohorte$age,breaks=c(0,35,80),include.lowest = T,right = F)
cohorte$age.f3<-cut(cohorte$age,breaks=c(0,27,80),include.lowest = T,right = F)
cohorte$age.f4<-as.factor(ifelse(cohorte$age < summary(cohorte$age)["Median"],0,1)) # le découpage précédent est trop concentré sur les petites classes


table(cohorte$age.f2,cohorte$age)
table(cohorte$age.f2)
# class de ref 30 -35 
#cohorte$age.f2<-reorder(cohorte$age.f,new.order=c(3,1,2,4,5))
cohorte$age.f2<-relevel(cohorte$age.f2,ref="[0,35)")
table(cohorte$age.f2)






#ggplot(cohorte,aes(poids))+geom_freqpoly()

summary(cohorte$poids)
# les poids inférieurs a 500 ne sont pas viables mais pourquoi les supprimer?
# 6000 g on vire 

cohorte$poids.f<-cut(cohorte$poids,breaks=c(0,2500,3000,3500,4000,8000),include.lowest = T,right = F)
table(cohorte$poids)
table(cohorte$poids.f)
cohorte$poids.f<-reorder(cohorte$poids.f,new.order=c(3,1,2,4,5))
cohorte$poids.f<-relevel(cohorte$poids.f,ref="[3e+03,3.5e+03)")
table(cohorte$poids.f)

cohorte$poids.f3<-cut(cohorte$poids,breaks=c(0,2500,8000),include.lowest = T,right = F)
table(cohorte$poids.f3)
cohorte$poids.f4<-as.factor(ifelse(cohorte$poids < summary(cohorte$poids)["Median"],0,1)) # le découpage précédent est trop concentré sur les petites classes
cohorte$poids.f5<-as.factor(ifelse(cohorte$poids < 3000,0,1)) # le découpage précédent est trop concentré sur les petites classes




plot(cohorte$agegestationnel,cohorte$poids)

str(cohorte$agegestationnel)
cohorte$agegestationnel<-ifelse(cohorte$agegestationnel>45,NA,cohorte$agegestationnel)
ggplot(cohorte,aes(agegestationnel))+geom_freqpoly() # l'age gesta sert plus de varaiables d'ajustement donc les classes sont peut être peu importantes
cohorte$agegestationnel.f<-cut(cohorte$agegestationnel,breaks=c(22,35,38,40,45),include.lowest = T,right = F)
table(cohorte$agegestationnel.f) # voir pour 40 ou 41

cohorte$agegestationnel.f<-reorder(cohorte$agegestationnel.f,new.order=c(3,1,2,4))
table(cohorte$agegestationnel.f)
cohorte$agegestationnel.f<-relevel(cohorte$agegestationnel.f,ref="[38,40)")
table(cohorte$agegestationnel.f)


cohorte$agegestationnel.f2<-cut(cohorte$agegestationnel,breaks=c(22,38,40,45),include.lowest = T,right = F)
table(cohorte$agegestationnel.f2) # voir pour 40 ou 41

cohorte$agegestationnel.f3<-cut(cohorte$agegestationnel,breaks=c(22,40,45),include.lowest = T,right = F)
table(cohorte$agegestationnel.f3) # voir pour 40 ou 41

cohorte$agegestationnel.f4<-cut(cohorte$agegestationnel,breaks=c(22,38,45),include.lowest = T,right = F)
table(cohorte$agegestationnel.f4) 

#cohorte$agegestationnel.f<-reorder(cohorte$agegestationnel.f,new.order=c(3,1,2,4))
table(cohorte$agegestationnel.f)
table(cohorte$agegestationnel.f2)


cohorte$taille.f<-cut(cohorte$tailles,breaks=c(min(cohorte$tailles,na.rm=T),
                                                                 summary(cohorte$tailles)["1st Qu."], 
                                                                 summary(cohorte$tailles)["Median"],
                                                                 summary(cohorte$tailles)["3rd Qu."],
                                                                 max(cohorte$tailles,na.rm=T)+1), right=F,include.lowest=T)


cohorte$taille.f2<-as.factor(ifelse(cohorte$tailles < summary(cohorte$tailles)["Median"],0,1))

#remplacement valeurs manquantes

cohorte$nbfoetus<-as.numeric(as.character(cohorte$nbfoetus))
cohorte$nbfoetus<-ifelse(is.na(cohorte$nbfoetus),0,cohorte$nbfoetus)
cohorte$nbfoetus.f<-as.factor(cohorte$nbfoetus)
cohorte$nbfoetus.f2<-ifelse(!is.na(cohorte$nbfoetus) & cohorte$nbfoetus>1,2,cohorte$nbfoetus)
cohorte$nbfoetus.f2<-as.factor(cohorte$nbfoetus.f2)


cohorte$poids.fna<-as.factor(ifelse(is.na(cohorte$poids.f),"NA",cohorte$poids.f))
cohorte$agegestationnel.fna<-as.factor(ifelse(is.na(cohorte$agegestationnel.f),"NA",cohorte$agegestationnel.f))
cohorte$agegestationnel.f2na<-as.factor(ifelse(is.na(cohorte$agegestationnel.f2),"NA",cohorte$agegestationnel.f2))
cohorte$agegestationnel.f3na<-as.factor(ifelse(is.na(cohorte$agegestationnel.f3),"NA",cohorte$agegestationnel.f3))
cohorte$agegestationnel.f4na<-as.factor(ifelse(is.na(cohorte$agegestationnel.f4),"NA",cohorte$agegestationnel.f4))


cohorte$age.fna<-as.factor(ifelse(is.na(cohorte$age.f),"NA",cohorte$age.f))
cohorte$age.f2na<-as.factor(ifelse(is.na(cohorte$age.f2),"NA",cohorte$age.f2))
cohorte$parite.fna<-as.factor(ifelse(is.na(cohorte$parite.f2),"NA",cohorte$parite.f2))
cohorte$gestite.fna<-as.factor(ifelse(is.na(cohorte$gestite.f2),"NA",cohorte$gestite.f2))

cohorte$sexe.fna<-as.factor(ifelse(is.na(cohorte$sexe),"NA",cohorte$sexe))

cohorte$vbna<-as.factor(ifelse(is.na(cohorte$vb),"NA",cohorte$vb))



cohorte$poids.fna<-as.factor(ifelse(is.na(cohorte$poids.f),"NA",cohorte$poids.f))
cohorte$poids.f3na<-as.factor(ifelse(is.na(cohorte$poids.f3),"NA",cohorte$poids.f3))
cohorte$poids.f4na<-as.factor(ifelse(is.na(cohorte$poids.f4),"NA",cohorte$poids.f4))
cohorte$poids.f5na<-as.factor(ifelse(is.na(cohorte$poids.f5),"NA",cohorte$poids.f5))

cohorte$agegestationnel.fna<-as.factor(ifelse(is.na(cohorte$agegestationnel.f),"NA",cohorte$agegestationnel.f))
cohorte$agegestationnel.f2na<-as.factor(ifelse(is.na(cohorte$agegestationnel.f2),"NA",cohorte$agegestationnel.f2))
cohorte$agegestationnel.f3na<-as.factor(ifelse(is.na(cohorte$agegestationnel.f3),"NA",cohorte$agegestationnel.f3))
cohorte$agegestationnel.f4na<-as.factor(ifelse(is.na(cohorte$agegestationnel.f4),"NA",cohorte$agegestationnel.f4))



cohorte$age.fna<-as.factor(ifelse(is.na(cohorte$age.f),"NA",cohorte$age.f))
cohorte$age.f2na<-as.factor(ifelse(is.na(cohorte$age.f2),"NA",cohorte$age.f2))
cohorte$age.f3na<-as.factor(ifelse(is.na(cohorte$age.f3),"NA",cohorte$age.f3))
cohorte$age.f4na<-as.factor(ifelse(is.na(cohorte$age.f4),"NA",cohorte$age.f4))



cohorte$parite.fna<-as.factor(ifelse(is.na(cohorte$parite.f2),"NA",cohorte$parite.f2))
cohorte$gestite.fna<-as.factor(ifelse(is.na(cohorte$gestite.f2),"NA",cohorte$gestite.f2))

cohorte$parite.f3na<-as.factor(ifelse(is.na(cohorte$parite.f3),"NA",cohorte$parite.f3))
#cohorte$gestite.f3na<-as.factor(ifelse(is.na(cohorte$gestite.f3),"NA",cohorte$gestite.f3))


cohorte$sexe.fna<-as.factor(ifelse(is.na(cohorte$sexe),"NA",cohorte$sexe))

cohorte$coeffapgar5mncor<-as.numeric(as.character(cohorte$coeffapgar5mn))
cohorte$coeffapgar5mncor<-ifelse(cohorte$coeffapgar5mncor >10,NA,cohorte$coeffapgar5mncor)
cohorte$coeffapgar5mncor.f<-as.factor(cohorte$coeffapgar5mncor)

cohorte$coeffapgar5mncor.fna<-as.factor(ifelse(is.na(cohorte$coeffapgar5mncor.f),"NA",cohorte$coeffapgar5mncor.f))
cohorte$coeffapgar5mn.f2<-ifelse(cohorte$coeffapgar5mn %in% c("9","10") ,"9-10","<9")
cohorte$coeffapgar5mn.f2<-ifelse(is.na(cohorte$coeffapgar5mn) ,NA,cohorte$coeffapgar5mn.f2)
cohorte$coeffapgar5mn.f2<-as.factor(cohorte$coeffapgar5mn.f2)
table(cohorte$coeffapgar5mn.f2)

cohorte$coeffapgar5mn.f2na<-ifelse(cohorte$coeffapgar5mn %in% c("9","10") ,"9-10","<9")
cohorte$coeffapgar5mn.f2na<-ifelse(is.na(cohorte$coeffapgar5mn) ,"NA",cohorte$coeffapgar5mn.f2na)

cohorte$coeffapgar5mn.fna<-as.factor(cohorte$coeffapgar5mn.f2na)
table(cohorte$coeffapgar5mn.f2na)

cohorte$coeffapgar1mn<-as.factor(cohorte$coeffapgar1mn)
cohorte$coeffapgar5mn<-as.factor(cohorte$coeffapgar5mn)
table(cohorte$coeffapgar5mn,exclude = NULL)


cohorte$vbna<-as.factor(ifelse(is.na(cohorte$vb),"NA",cohorte$vb))
cohorte$taille.f2na<-as.factor(ifelse(is.na(cohorte$taille.f2),"NA",cohorte$taille.f2))
cohorte$taille.fna<-as.factor(ifelse(is.na(cohorte$taille.f),"NA",cohorte$taille.f))

cohorte$nbfoetus.fna<-as.factor(ifelse(is.na(cohorte$nbfoetus.f),"NA",cohorte$nbfoetus.f))
cohorte$nbfoetus.f2na<-as.factor(ifelse(is.na(cohorte$nbfoetus.f2),"NA",cohorte$nbfoetus.f2))

cohorte$oxygenotherapie<-ifelse(is.na(cohorte$oxygenotherapie),0,cohorte$oxygenotherapie)
cohorte$oxygenotherapie<-as.factor(cohorte$oxygenotherapie)
cohorte$intubation<-ifelse(is.na(cohorte$intubation),0,cohorte$intubation)
cohorte$intubation<-as.factor(cohorte$intubation)

cohorte$antibiotherapie<-ifelse(is.na(cohorte$antibiotherapie),0,cohorte$antibiotherapie)
cohorte$antibiotherapie<-as.factor(cohorte$antibiotherapie)

cohorte$neurologique<-ifelse(is.na(cohorte$neurologique),0,cohorte$neurologique)
cohorte$neurologique<-as.factor(cohorte$neurologique)

cohorte$urgence<-ifelse(is.na(cohorte$urgence),0,cohorte$urgence)
cohorte$urgence<-as.factor(cohorte$urgence)
cohorte$autre_patho<-as.factor(cohorte$autre_patho)





cohorte$polymalformation<-ifelse(is.na(cohorte$polymalformation),0,cohorte$polymalformation)

cohorte$spinabifida<-ifelse(is.na(cohorte$spinabifida),0,cohorte$spinabifida)

cohorte$fente<-ifelse(is.na(cohorte$fente),0,cohorte$fente)

cohorte$atresie<-ifelse(is.na(cohorte$atresie),0,cohorte$atresie)

cohorte$omphalocele<-ifelse(is.na(cohorte$omphalocele),0,cohorte$omphalocele)

cohorte$reductionmembre<-ifelse(is.na(cohorte$reductionmembre),0,cohorte$reductionmembre)

cohorte$malformrenale<-ifelse(is.na(cohorte$malformrenale),0,cohorte$malformrenale)

cohorte$hydrocephalie<-ifelse(is.na(cohorte$hydrocephalie),0,cohorte$hydrocephalie)

cohorte$malformcard<-ifelse(is.na(cohorte$malformcard),0,cohorte$malformcard)

cohorte$malformrenale<-ifelse(is.na(cohorte$malformrenale),0,cohorte$malformrenale)
cohorte$trisomie<-ifelse(is.na(cohorte$trisomie),0,cohorte$trisomie)


cohorte$autreznum<-ifelse(cohorte$autrez=="",0,1)
table(cohorte$autreznum)
cohorte$autrez<-as.factor(cohorte$autrez)



cohorte$summ<-cohorte$polymalformation+cohorte$spinabifida+cohorte$fente+cohorte$atresie+
                  cohorte$omphalocele+cohorte$reductionmembre+cohorte$malformcard+
                  cohorte$malformrenale+cohorte$trisomie+cohorte$autreznum
table(cohorte$summ)

cohorte$polymalformation<-as.factor(cohorte$polymalformation)
cohorte$spinabifida<-as.factor(cohorte$spinabifida)
cohorte$fente<-as.factor(cohorte$fente)
cohorte$atresie<-as.factor(cohorte$atresie)
cohorte$omphalocele<-as.factor(cohorte$omphalocele)
cohorte$reductionmembre<-as.factor(cohorte$reductionmembre)
cohorte$malformrenale<-as.factor(cohorte$malformrenale)
cohorte$hydrocephalie<-as.factor(cohorte$hydrocephalie)
cohorte$malformcard<-as.factor(cohorte$malformcard)
cohorte$trisomie<-as.factor(cohorte$trisomie)
cohorte$autrez<-as.factor(cohorte$autrez)


cohorte$anomaliena<-ifelse(is.na(cohorte$anomalie),0,cohorte$anomalie)
cohorte$anomaliena<-ifelse(cohorte$summ>1,1,cohorte$anomaliena)
cohorte$anomaliena<-ifelse(cohorte$anomalie==1 & !is.na(cohorte$anomalie),1,cohorte$anomaliena)
cohorte$anomalie<-as.factor(cohorte$anomalie)
table(cohorte$anomalie)
table(cohorte$anomaliena)


cohorte$gestes_techniques<-ifelse(is.na(cohorte$gestes_techniques),0,cohorte$gestes_techniques)
cohorte$gestes_techniques<-as.factor(cohorte$gestes_techniques)

table(is.na(cohorte$nbnesavant37),cohorte$Source)
table(is.na(cohorte$nbnesmoins2500),cohorte$Source)
table(is.na(cohorte$nbmortne),cohorte$Source)
table(is.na(cohorte$nbdecedeav28j),cohorte$Source)
table(is.na(cohorte$cesarienne),cohorte$Source)

cohorte$nbnesavant37<-ifelse(is.na(cohorte$nbnesavant37),0,cohorte$nbnesavant37)
cohorte$nbnesmoins2500<-ifelse(is.na(cohorte$nbnesmoins2500),0,cohorte$nbnesmoins2500)
cohorte$nbmortne<-ifelse(is.na(cohorte$nbmortne),0,cohorte$nbmortne)
cohorte$nbdecedeav28j<-ifelse(is.na(cohorte$nbdecedeav28j),0,cohorte$nbdecedeav28j)
cohorte$cesarienne<-ifelse(is.na(cohorte$cesarienne),0,cohorte$cesarienne)

table(cohorte$pathologieg,exclude=NULL)


cohorte$pathologiegna<-ifelse(is.na(cohorte$pathologieg),0,cohorte$pathologieg)
cohorte$pathologiegna<-ifelse((cohorte$pathologiegna==0 & !is.na(cohorte$pathologiegna) &
                                 cohorte$preeclampsie==1 & !is.na(cohorte$preeclampsie))|                                      
                                                                   
                                (cohorte$pathologiegna==0 & !is.na(cohorte$pathologiegna) &
                                   cohorte$hta==1 & !is.na(cohorte$hta))|
                                
                                (cohorte$pathologiegna==0 & !is.na(cohorte$pathologiegna) &
                                   cohorte$diabete==1 & !is.na(cohorte$diabete))
                                ,0,cohorte$pathologieg)
cohorte$pathologiegna<-ifelse( cohorte$pathologieg==1 &!is.na(cohorte$pathologieg),1,cohorte$pathologieg)

table(cohorte$pathologiegna,cohorte$pathologieg)

cohorte$preeclampsiena<-ifelse(is.na(cohorte$preeclampsie),0,cohorte$preeclampsie)
table(cohorte$preeclampsiena,cohorte$preeclampsie)
cohorte$htana<-ifelse(is.na(cohorte$hta) ,0,cohorte$hta)
table(cohorte$htana,cohorte$hta)
cohorte$diabetena<-ifelse(is.na(cohorte$diabete) ,0,cohorte$diabete)



quali(x=c("parite.f2","gestite.f2","nbfoetus.f","agegestationnel.f","naissancepar.f2","vb","poids.f","taille.f","coeffapgar5mn","coeffapgar5mn.f2","age.f","trisomie","malformcard","hydrocephalie","malformrenale","reductionmembre"),nomx=c("parite.f2","gestite.f2","nbfoetus.f","agegestationnel.f","naissancepar.f2","vb","poids.f","taille.f","coeffapgar5mn","coeffapgar5mn.f","age.f","trisomie","malformcard","hydrocephalie","malformrenale","reductionmembre"), data=cohorte,RAPPORT=F,SAVEFILE=F,ordonner=c(FALSE,FALSE,FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                                                                                                                                                                                                                                                                                                                                                                                                        FALSE, FALSE, FALSE, FALSE, FALSE), numerique=c(FALSE,FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                        FALSE, FALSE, FALSE, FALSE, FALSE,FALSE), seq=list(c(19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)),chemin="C:/Users/Louise/Documents/Desespoir/Bases/resultats/",fichier="IRIS")

### création date de point ###

cohorte$ddp<-NA
cohorte$ddp<-ifelse(cohorte$cas==1 & !is.na(cohorte$cas),as.character(cohorte$TU__DateDiag),cohorte$ddp)
cohorte$date_deces<-cohorte$datenaissance+cohorte$age_deces_jj
cohorte$ddp<-ifelse(!is.na(cohorte$age_deces_jj),as.character(cohorte$date_deces),cohorte$ddp)
cohorte$ddp<-ifelse(!is.na(cohorte$age_deces_hh),as.character(cohorte$datenaissance),cohorte$ddp)
cohorte$date_suivi<-ymd(cohorte$datenaissance)+ years(5)
cohorte$ddp<-ifelse(is.na(cohorte$age_deces_hh) & is.na(cohorte$age_deces_jj),
                    as.character(cohorte$date_suivi),cohorte$ddp)
cohorte$ddp<-as.Date(cohorte$ddp)
cohorte$ddp<-ifelse(cohorte$ddp>="2016-10-04",17077.96,cohorte$ddp )
table(cohorte$ddp)
cohorte$ddp<-as.Date(cohorte$ddp,origin="1970-01-01")
table(cohorte$ddp)


