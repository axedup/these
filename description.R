###==========Description variables et création de la ase finale de Paris car il y vait parfois plus de 10 témoins par cas===========####

cas_temoins_parish$tailles<-gsub(pattern="(^[0-9]{2})\\.([0-9]{1})$", replacement="\\1\\2",x=as.character(cas_temoins_parish$taille))
cas_temoins_parish$tailles<-gsub(pattern="(^[0-9]{2})$", replacement="\\10",x=as.character(cas_temoins_parish$tailles))
cas_temoins_parish$tailles<-ifelse(cas_temoins_parish$tailles=="48.5999984741211","486",cas_temoins_parish$tailles)
cas_temoins_parish$tailles<-ifelse(cas_temoins_parish$tailles=="0",NA,cas_temoins_parish$tailles)                                   
cas_temoins_parish$tailles<-as.numeric(cas_temoins_parish$tailles)
cas_temoins_parish$niveauetudes<-as.factor(as.character(cas_temoins_parish$niveauetudes)) 
cas_temoins_parish$sexe<-as.factor(as.character(cas_temoins_parish$sexe)) 
cas_temoins_parish$gestite.f<-as.factor(as.character(cas_temoins_parish$gestite)) 
cas_temoins_parish$gestite.f2<-ifelse(cas_temoins_parish$gestite >= 4,"4 et +", cas_temoins_parish$gestite) 
cas_temoins_parish$gestite.f2<-as.factor(cas_temoins_parish$gestite.f2) 


cas_temoins_parish$parite.f<-as.factor(as.character(cas_temoins_parish$parite)) 
cas_temoins_parish$parite.f2<-ifelse(cas_temoins_parish$parite >= 4,"4 et +", cas_temoins_parish$parite) 
cas_temoins_parish$parite.f2<-as.factor(cas_temoins_parish$parite.f2) 

cas_temoins_parish$naissancepar<-factor(cas_temoins_parish$naissancepar,labels=c("vbni","vbi","cesar prog","cesar urg","cesar sp"))
cas_temoins_parish$naissancepar.f2<-cas_temoins_parish$naissancepar
levels(cas_temoins_parish$naissancepar.f2)<-c("vbni","vbi" ,"cesar" ,"cesar","cesar") 
table(cas_temoins_parish$naissancepar.f2)


table(cas_temoins_parish$datenaissance_mere,exclude=NULL)
table(cas_temoins_parish$datenaissance_mere,is.na(as.Date(as.character(cas_temoins_parish$datenaissance_mere),format="%d/%m/%Y")),exclude=NULL)

#cas_temoins_parish$datenaissance_meres<-gsub(pattern="(^[0-9]{4})\\-([0-9]{2})\\-([0-9]{2})$", replacement="\\3\\/\\2\\/\\1",x=cas_temoins_parish$datenaissance_mere)

table(cas_temoins_parishbis$datenaissance_mere,exclude=NULL)
table(cas_temoins_parish$datenaissance_mere,is.na(as.Date(as.character(cas_temoins_parish$datenaissance_mere),format="%d/%m/%Y")),exclude=NULL)

table(is.na(as.Date(as.character(cas_temoins_parish$datenaissance_meres),format="%d/%m/%Y")),exclude=NULL)

#cas_temoins_parish$datenaissance_meres<-as.Date(as.character(cas_temoins_parish$datenaissance_meres),format="%d/%m/%Y")
table(cas_temoins_parishbis$datenaissance,exclude=NULL)
cas_temoins_parish$age<-round(difftime(cas_temoins_parish$datenaissance,cas_temoins_parish$datenaissance_meres)/365,0)
cas_temoins_parish$age<-as.numeric(cas_temoins_parish$age)

cas_temoins_parish$cas<-as.factor(cas_temoins_parish$cas)

cas_temoins_parish$coeffapgar1mn<-as.factor(cas_temoins_parish$coeffapgar1mn)
cas_temoins_parish$coeffapgar5mn<-as.factor(cas_temoins_parish$coeffapgar5mn)




# tabec trop de NA suposer que NA =0 parait etre exagéré ça farait 95 % de non fumeuses alors que c'est plutôt autour de 20%#


temoins_paris_inter<-cas_temoins_parish[cas_temoins_parish$cas==0,]
temoins_paris_inter<-cas_temoins_parish[order(cas_temoins_parish$datenaissance),]
temoins_paris_inter$compteur<-NULL
temoins_paris_inter$compteur<-unlist(lapply(table(temoins_paris_inter$datenaissance),function(x){1:x}))



set.seed(456)
temoins_paris_inter<-temoins_paris_inter[sample(1:dim(temoins_paris_inter)[1]),]

temoins_paris_inter1<-temoins_paris_inter[temoins_paris_inter$compteur %in% 1:10,]
temoins_paris_inter2<-temoins_paris_inter[temoins_paris_inter$compteur %in% 11:20 & (temoins_paris_inter$datenaissance=="2011-10-30"|
                                                                                       temoins_paris_inter$datenaissance=="2011-04-06"|
                                                                                       temoins_paris_inter$datenaissance=="2012-07-13"|
                                                                                       temoins_paris_inter$datenaissance=="2012-04-26"|
                                                                                       temoins_paris_inter$datenaissance=="2012-06-25"),]
table(temoins_paris_inter1$datenaissance)
table(temoins_paris_inter2$datenaissance)

temoins_paris_inter1$compteur<-NULL
temoins_paris_inter2$compteur<-NULL

cas_temoins_parishbis<-rbind(cas_temoins_parish[cas_temoins_parish$cas==1,],temoins_paris_inter1,temoins_paris_inter2)
as.Date(c("2012-11-01","2011-09-27"))
cas_temoins_parishbis<-cas_temoins_parishbis[!cas_temoins_parishbis$datenaissance %in% as.Date(c("2012-11-01","2011-09-27")),]# virer les cas et les témoins avec les mauvaises date de naissance

dim(cas_temoins_parishbis)
table(cas_temoins_parishbis$cas)

table(cas_temoins_parishbis$datenaissance)
table(cas_temoins_parishbis$perimetre)

cas_temoins_parishbis$perimetre2<-as.character(cas_temoins_parishbis$perimetre)
table(cas_temoins_parishbis$perimetre,exclude=NULL)
table(cas_temoins_parishbis$perimetre2)
cas_temoins_parishbis$perimetre2<-gsub(pattern="^([0-9]{2})([0-9]{1})$",replacement = "\\1\\.\\2",x=cas_temoins_parishbis$perimetre2)
table(cas_temoins_parishbis$perimetre2)
cas_temoins_parishbis$perimetre2<-as.numeric(cas_temoins_parishbis$perimetre2)
table(cas_temoins_parishbis$perimetre2)
cas_temoins_parishbis$perimetre2<-ifelse(cas_temoins_parishbis$perimetre2==0,NA,cas_temoins_parishbis$perimetre2)
table(cas_temoins_parishbis$perimetre2,exclude=NULL)
cas_temoins_parishbis$perimetre<-cas_temoins_parishbis$perimetre2
cas_temoins_parishbis$gestite.f<-as.factor(as.character(cas_temoins_parishbis$gestite)) 
cas_temoins_parishbis$gestite.f2<-ifelse(cas_temoins_parishbis$gestite >= 4,"4 et +", cas_temoins_parishbis$gestite) 
cas_temoins_parishbis$gestite.f2<-as.factor(cas_temoins_parishbis$gestite.f2) 


cas_temoins_parishbis$parite.f<-as.factor(as.character(cas_temoins_parishbis$parite)) 
cas_temoins_parishbis$parite.f2<-ifelse(cas_temoins_parishbis$parite >= 4,"4 et +", cas_temoins_parishbis$parite) 
cas_temoins_parishbis$parite.f2<-as.factor(cas_temoins_parishbis$parite.f2) 

cas_temoins_parishbis$naissancepar<-factor(cas_temoins_parishbis$naissancepar,labels=c("vbni","vbi","cesar prog","cesar urg","cesar sp"))
cas_temoins_parishbis$naissancepar.f2<-cas_temoins_parishbis$naissancepar
levels(cas_temoins_parishbis$naissancepar.f2)<-c("vbni","vbi" ,"cesar" ,"cesar","cesar") 
table(cas_temoins_parishbis$naissancepar.f2)

cas_temoins_parishbis$niveauetudes<-as.factor(as.character(cas_temoins_parishbis$niveauetudes)) 

cas_temoins_parishbis$sexe<-as.factor(as.character(cas_temoins_parishbis$sexe)) 

cas_temoins_parishbis$cas<-as.factor(cas_temoins_parishbis$cas)

cas_temoins_parishbis$coeffapgar1mn<-as.factor(cas_temoins_parishbis$coeffapgar1mn)
cas_temoins_parishbis$coeffapgar5mn<-as.factor(cas_temoins_parishbis$coeffapgar5mn)

cas_temoins_parishbis$datenaissance_meres<-gsub(pattern="(^[0-9]{4})\\-([0-9]{2})\\-([0-9]{2})$", replacement="\\3\\/\\2\\/\\1",x=cas_temoins_parishbis$datenaissance_mere)

table(cas_temoins_parishbis$datenaissance_mere,exclude=NULL)
table(cas_temoins_parishbis$datenaissance_mere,is.na(as.Date(as.character(cas_temoins_parishbis$datenaissance_mere),format="%d/%m/%Y")),exclude=NULL)

table(is.na(as.Date(as.character(cas_temoins_parishbis$datenaissance_meres),format="%d/%m/%Y")),exclude=NULL)

cas_temoins_parishbis$datenaissance_meres<-as.Date(as.character(cas_temoins_parishbis$datenaissance_meres),format="%d/%m/%Y")
table(cas_temoins_parishbis$datenaissance,exclude=NULL)
cas_temoins_parishbis$age<-round(difftime(cas_temoins_parishbis$datenaissance,cas_temoins_parishbis$datenaissance_meres)/365,0)
cas_temoins_parishbis$age<-as.numeric(cas_temoins_parishbis$age)
table(cas_temoins_parishbis$age,exclude=NULL)





quantifb(xc=c("age","agegestationnel","taille","poids","perimetre"),nomx=c("age en an","terme","taille en cm","poids en g","perimetre en cm"), data=cas_temoins_parishbis,RAPPORT=F,SAVEFILE=F,chemin=NULL)
test.quant(varquant=c("age","agegestationnel","tailles","poids","perimetre"),nomquant=c("age en an","terme","taille en cm","poids en g","perimetre en cm"), varqual=c("cas"),nomqual=c("cas"),data=cas_temoins_parishbis,RAPPORT=F,SAVEFILE=F,chemin=NULL)



quali(x=c("sexe","niveauetudes","parite.f","parite.f2","gestite.f","gestite.f2","naissancepar","naissancepar.f2","coeffapgar1mn","coeffapgar5mn"),nomx=c("sexe","etudes","parite","parite.f2","gestite","gestite.f","mode d'accouchement","voie basse ou césar","Apgar 1 min","Apgar 5 min"), data=cas_temoins_parishbis,RAPPORT=F,SAVEFILE=F,chemin=NULL)
desc.qual2(x=c("sexe","niveauetudes","parite.f","parite.f2","gestite.f","gestite.f2","naissancepar","naissancepar.f2"),y=cas_temoins_parishbis$cas,nomx=c("sexe","etudes","parite","parite.f2","gestite","gestite.f","mode d'accouchement","voie basse ou césar"), data=cas_temoins_parishbis,RAPPORT=F,SAVEFILE=F,chemin=NULL)
nomx<-c("sexe","etudes","parite","parite.f2","gestite","gestite.f","mode d'accouchement","voie basse ou césar")
j<-1
B<-NULL
for (i in cas_temoins_parishbis[,c("sexe","niveauetudes","parite.f","parite.f2","gestite.f","gestite.f2","naissancepar","naissancepar.f2")] ){
b<-test.qual(x=i,y=cas_temoins_parishbis$cas,nomx[j],test=T,RAPPORT=F,SAVEFILE=F,chemin=NULL)
B<-rbind(B,b)
j<-j+1
}


### pour les 95###

cas_temoins_95_help$tailles<-gsub(pattern="(^[0-9]{2})\\.([0-9]{1})$", replacement="\\1\\2",x=as.character(cas_temoins_95_help$taille))
cas_temoins_95_help$tailles<-gsub(pattern="(^[0-9]{2})$", replacement="\\10",x=as.character(cas_temoins_95_help$tailles))
cas_temoins_95_help$tailles<-ifelse(cas_temoins_95_help$tailles=="48.5999984741211","486",cas_temoins_95_help$tailles)
cas_temoins_95_help$tailles<-ifelse(cas_temoins_95_help$tailles=="0",NA,cas_temoins_95_help$tailles)                                   
cas_temoins_95_help$tailles<-as.numeric(cas_temoins_95_help$tailles)
cas_temoins_95_help$niveauetudes<-as.factor(as.character(cas_temoins_95_help$niveauetudes)) 
cas_temoins_95_help$sexe<-as.factor(as.character(cas_temoins_95_help$sexe)) 
cas_temoins_95_help$gestite.f<-as.factor(as.character(cas_temoins_95_help$gestite)) 
cas_temoins_95_help$gestite.f2<-ifelse(cas_temoins_95_help$gestite >= 4,"4 et +", cas_temoins_95_help$gestite) 
cas_temoins_95_help$gestite.f2<-as.factor(cas_temoins_95_help$gestite.f2) 


cas_temoins_95_help$parite.f<-as.factor(as.character(cas_temoins_95_help$parite)) 
cas_temoins_95_help$parite.f2<-ifelse(cas_temoins_95_help$parite >= 4,"4 et +", cas_temoins_95_help$parite) 
cas_temoins_95_help$parite.f2<-as.factor(cas_temoins_95_help$parite.f2) 

cas_temoins_95_help$naissancepar<-factor(cas_temoins_95_help$naissancepar,labels=c("vbni","vbi","cesar prog","cesar urg"))
cas_temoins_95_help$naissancepar.f2<-cas_temoins_95_help$naissancepar
levels(cas_temoins_95_help$naissancepar.f2)<-c("vbni","vbi" ,"cesar" ,"cesar","cesar") 
table(cas_temoins_95_help$naissancepar.f2)


table(cas_temoins_95_help$datenaissance_mere,exclude=NULL)
table(cas_temoins_95_help$datenaissance_mere,is.na(as.Date(as.character(cas_temoins_95_help$datenaissance_mere),format="%d/%m/%Y")),exclude=NULL)

#cas_temoins_95_help$datenaissance_meres<-gsub(pattern="(^[0-9]{4})\\-([0-9]{2})\\-([0-9]{2})$", replacement="\\3\\/\\2\\/\\1",x=cas_temoins_95_help$datenaissance_mere)

table(cas_temoins_95_helpbis$datenaissance_mere,exclude=NULL)
table(cas_temoins_95_help$datenaissance_mere,is.na(as.Date(as.character(cas_temoins_95_help$datenaissance_mere),format="%d/%m/%Y")),exclude=NULL)

table(is.na(as.Date(as.character(cas_temoins_95_help$datenaissance_meres),format="%d/%m/%Y")),exclude=NULL)

#cas_temoins_95_help$datenaissance_meres<-as.Date(as.character(cas_temoins_95_help$datenaissance_meres),format="%d/%m/%Y")
table(cas_temoins_95_helpbis$datenaissance,exclude=NULL)
cas_temoins_95_help$datenaissance_meres<-cas_temoins_95_help$datenaissance_mere

cas_temoins_95_help$age<-round(difftime(cas_temoins_95_help$datenaissance,cas_temoins_95_help$datenaissance_mere)/365,0)
cas_temoins_95_help$age<-as.numeric(cas_temoins_95_help$age)

cas_temoins_95_help$cas<-as.factor(cas_temoins_95_help$cas)

cas_temoins_95_help$coeffapgar1mn<-as.factor(cas_temoins_95_help$coeffapgar1mn)
cas_temoins_95_help$coeffapgar5mn<-as.factor(cas_temoins_95_help$coeffapgar5mn)

cas_temoins_95_help$perimetre2<-as.character(cas_temoins_95_help$perimetre)
table(cas_temoins_95_help$perimetre,exclude=NULL)
table(cas_temoins_95_help$perimetre2)
cas_temoins_95_help$perimetre2<-gsub(pattern="^([0-9]{2})([0-9]{1})$",replacement = "\\1\\.\\2",x=cas_temoins_95_help$perimetre2)
table(cas_temoins_95_help$perimetre2)
cas_temoins_95_help$perimetre2<-as.numeric(cas_temoins_95_help$perimetre2)
table(cas_temoins_95_help$perimetre2)
cas_temoins_95_help$perimetre2<-ifelse(cas_temoins_95_help$perimetre2==0,NA,cas_temoins_95_help$perimetre2)
table(cas_temoins_95_help$perimetre2,exclude=NULL)





