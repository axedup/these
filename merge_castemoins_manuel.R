# suite fusion 

### cas :

# garder les dates de naissance mater corrigées, les poids

names(cas_93)[which(!names(cas_93) %in% names(temoins_2011) )]
dput(names(cas_93)[which(!names(cas_93) %in% names(temoins_2011) )])
cas_93_trier<-cas_93
cas_93_trier<-cas_93_trier[,!names(cas_93_trier) %in% c("numt", "X.2", "X.1", "TU__DateDiag", "TU__AnneeRegistre", 
                                                                 "TU__AgeDiag", "taille.c", "poids2", "poids.c",  "MA__Prenom", "MA__Nom", "MA__LieuNaissDept", 
                                                                 "MA__LieuNaissCom", "MA__LieuNaiss_INSEE_z", "MA___NumMalade", "ddn_mater2", "poids2.c","MA__DateNaissance")]
dput(names(cas_93_trier))

# ça ne devrait pas être nécessaire
names(cas_93_trier)<-c("code_p","prevoie", "urgence", "typetransfert", "trisomie", "transfert", 
                          "testauditif", "taille", "tabac", "spinabifida", "Source", 
                          "rupturemembrane", "retardcroissance", "resultaudit", "resultatrecherche", 
                          "reductionmembre", "rechaghbs", "rangnaissance", "rachieanesthesie", 
                          "voirie", "presentation", "prepaccouchement", "premconsult", 
                          "preeclampsie", "pprofession", "polymalformation", "poids", 
                          "perimetre", "pathologieg", "parite", "pactivite", "oxygenotherapie", 
                          "omphalocele", "num_v", "nom_v", "niveauetudes", "neurologique", 
                          "nbnesmoins2500", "nbnesavant37", "nbmortne", "nbjourhopital", 
                          "nbfoetus", "nbenfant", "nbecho", "nbdecedeav28j", "naissancepar", 
                          "mprofession", "motifcesariennem", "motifcesariennef", "mois_naiss_m", 
                          "mois_acc", "menacepremature", "maternite_nom", "maternite_code", 
                          "malformrenale", "malformcard", "mactivite", "sexe", 
                          "lieutransfert_nom", "lieutransfert_code", "inutero", "intubation", 
                          "infotransfert", "hypertension", "hydrocephalie", "hta", "hepatitebvacc", 
                          "hepatitebinj", "gestite", "gestes_techniques", "fente", "echomorpho", 
                          "diabete", "dep_nais", "dep", "debuttravail", "ddnMest", "ddnest", 
                          "datenaissance_mere", "num", "commune_nais_code", "commune_famille_code", 
                          "coeffapgar5mn", "coeffapgar1mn", "clartenucale", "cesarienne", 
                          "bcg", "autrez", "autre_patho", "aucune_analgesie", "atresie", 
                          "antibiotherapie", "anomalie", "analgesieperi", "analgesiegene", 
                          "analgesieautr", "an_naiss_m", "an_acc", "allaitement", "alcool", 
                          "agegestationnel", "age_deces_jj", "age_deces_hh", "datenaissance", 
                          "cas", "tbis", "num_rue", "numunique")
# num

dim(cas_93_trier)
dim(temoins_2012)



cas_93_trier<-cas_93_trier[,order(colnames(cas_93_trier)),]
dim(cas_93_trier)

### arg vÃ©rifier le type de varaible
str(temoins_93_1013h)
str(cas_93_trier)

cas_93_trier$numunique<-as.character(cas_93_trier$numunique)


### témoins 


temoins_93_1013h<-rbind(temoins_93_10,temoins_93_11,temoins_93_12,temoins_93_13)


# variables extraction a ajouter 

###  fusion cas et témoins
# bien vérifier la nature des variables
cas_temoins_93h<-rbind(temoins_93_1013h,cas_93_trier)
dim(cas_temoins_93h)
table(cas_temoins_93h$cas)

#93_geoloco<- cas_temoins_93h[,c("numunique","num_rue","prevoie","voirie","nom_v","code_p","cas")]
#93_geoloco$codep<-ifelse(nchar(93_geoloco$code_p)==2 & !is.na(93_geoloco$code_p),paste0("750", 93_geoloco$code_p),93_geoloco$code_p)

#cas_temoins_93h$commune<-"93"
write.csv(93_geoloco,"F:/cas_temoins_93.csv")



# num_v :nulle
# poids, (taille), date de naissance : prendre celle des relevés


# on ordonne les colonnes par ordre croissant



cas_temoins_93h$tailles<-gsub(pattern="(^[0-9]{2})\\.([0-9]{1})$", replacement="\\1\\2",x=as.character(cas_temoins_93h$taille))
cas_temoins_93h$tailles<-gsub(pattern="(^[0-9]{2})$", replacement="\\10",x=as.character(cas_temoins_93h$tailles))
cas_temoins_93h$tailles<-ifelse(cas_temoins_93h$tailles=="48.5999984741211","486",cas_temoins_93h$tailles)
cas_temoins_93h$tailles<-ifelse(cas_temoins_93h$tailles=="0",NA,cas_temoins_93h$tailles)                                   
cas_temoins_93h$tailles<-as.numeric(cas_temoins_93h$tailles)
cas_temoins_93h$niveauetudes<-as.factor(as.character(cas_temoins_93h$niveauetudes)) 
cas_temoins_93h$sexe<-as.factor(as.character(cas_temoins_93h$sexe)) 
cas_temoins_93h$gestite.f<-as.factor(as.character(cas_temoins_93h$gestite)) 
cas_temoins_93h$gestite.f2<-ifelse(cas_temoins_93h$gestite >= 4,"4 et +", cas_temoins_93h$gestite) 
cas_temoins_93h$gestite.f2<-as.factor(cas_temoins_93h$gestite.f2) 


cas_temoins_93h$parite.f<-as.factor(as.character(cas_temoins_93h$parite)) 
cas_temoins_93h$parite.f2<-ifelse(cas_temoins_93h$parite >= 4,"4 et +", cas_temoins_93h$parite) 
cas_temoins_93h$parite.f2<-as.factor(cas_temoins_93h$parite.f2) 

cas_temoins_93h$naissancepar<-factor(cas_temoins_93h$naissancepar,labels=c("vbni","vbi","cesar prog","cesar urg","cesar sp"))
cas_temoins_93h$naissancepar.f2<-cas_temoins_93h$naissancepar
levels(cas_temoins_93h$naissancepar.f2)<-c("vbni","vbi" ,"cesar" ,"cesar","cesar") 
table(cas_temoins_93h$naissancepar.f2)


table(cas_temoins_93h$datenaissance_mere,exclude=NULL)
table(cas_temoins_93h$datenaissance_mere,is.na(as.Date(as.character(cas_temoins_93h$datenaissance_mere),format="%d/%m/%Y")),exclude=NULL)

#cas_temoins_93h$datenaissance_meres<-gsub(pattern="(^[0-9]{4})\\-([0-9]{2})\\-([0-9]{2})$", replacement="\\3\\/\\2\\/\\1",x=cas_temoins_93h$datenaissance_mere)

table(cas_temoins_93hbis$datenaissance_mere,exclude=NULL)
table(cas_temoins_93h$datenaissance_mere,is.na(as.Date(as.character(cas_temoins_93h$datenaissance_mere),format="%d/%m/%Y")),exclude=NULL)

table(is.na(as.Date(as.character(cas_temoins_93h$datenaissance_meres),format="%d/%m/%Y")),exclude=NULL)

#cas_temoins_93h$datenaissance_meres<-as.Date(as.character(cas_temoins_93h$datenaissance_meres),format="%d/%m/%Y")
table(cas_temoins_93hbis$datenaissance,exclude=NULL)
cas_temoins_93h$age<-round(difftime(cas_temoins_93h$datenaissance,cas_temoins_93h$datenaissance_meres)/365,0)
cas_temoins_93h$age<-as.numeric(cas_temoins_93h$age)

cas_temoins_93h$cas<-as.factor(cas_temoins_93h$cas)

cas_temoins_93h$coeffapgar1mn<-as.factor(cas_temoins_93h$coeffapgar1mn)
cas_temoins_93h$coeffapgar5mn<-as.factor(cas_temoins_93h$coeffapgar5mn)

# il faudra faire fusionner avec cas_temoins_parisexpoi_trier,cas_temoins_95expoi_trier après avoir attribuer les expos 


