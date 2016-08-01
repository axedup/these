################FUSION DES BASES###################

### Paris
names(cas_paris)
dim(cas_paris)

dim(temoins_2010help) #╔ prendre les date de naissance mater relevée
names(temoins_2010help) #○ ok pour datenaissance_mere



dim(temoins_2011) #╔ prendre les date de naissance mater relevée
names(temoins_2011) #○ ok pour datenaissance_mere

dim(temoins_2012help) #╔ prendre les date de naissance mater relevée
names(temoins_2012help) #○ ok pour datenaissance_mere

names(temoins_2013)
dim(temoins_2013)

names(temoins_2011)[!which(names(temoins_2011) %in% names(temoins_2012) )] # ok 
names(temoins_2013)[which(!names(temoins_2013) %in% names(temoins_2011) )] # non rue bis à virer en 2013 ? 

temoins_2013_trier<-temoins_2013[!names(temoins_2013) %in% c("nom.ruebis")]
dim(temoins_2013_trier)
names(temoins_2010help)[which(!names(temoins_2010help) %in% names(temoins_2011) )]
names(temoins_2011)[which(!names(temoins_2011) %in% names(temoins_2010help) )] # ya les noms qui ont changé entre 2010 et 2011 etc....
# j'i fait une erreur en fusionnant j'ai pas virer voirie, prévoie, num de rue de essai alors qu'il sont vides... 

table(temoins_2010help$num_v.x,exclude=NULL)
table(temoins_2010help$voirie.x,exclude=NULL)
table(temoins_2010help$code_p.x,exclude=NULL)
table(temoins_2010help$prevoie.x,exclude=NULL)

temoins_2010_trierh<-temoins_2010help[,!names(temoins_2010help) %in% c("num_v.x", "voirie.x", "nom_v.x", "code_p.x", "prevoie.x")]

dim(temoins_2010_trierh)


temoins_2010_trierh<-temoins_2010_trierh[,order(colnames(temoins_2010_trierh))]
temoins_2011_trier<-temoins_2011[,order(colnames(temoins_2011))]
temoins_2012_trierh<-temoins_2012help[,order(colnames(temoins_2012help))]
temoins_2013_trier<-temoins_2013_trier[,order(colnames(temoins_2013_trier))]

colnames(temoins_2010_trierh)<-colnames(temoins_2012_trierh)

temoins_paris_1013h<-rbind(temoins_2010_trierh,temoins_2011_trier,temoins_2012_trierh,temoins_2013_trier)
dim(temoins_paris_1013h)


names(cas_paris)[which(!names(cas_paris) %in% names(temoins_2011) )]
dput(names(cas_paris)[which(!names(cas_paris) %in% names(temoins_2011) )])

# numt servait au moment de la correction des fautes inutiles
#"MA___NumMalade" remplacé par un autre identifiant 
# date idage pas utili mais a garder quand meêm dans la base ds cas
# on garde ddn.mater et datenaissance
# les taille.c ou .r sont les même c'est juste que les certif sont remplis bizarres avec des cm ou des mm 
# MA_sexe a renommer en sexe
# poids.c poids.r c'est les mêmes on garde celui de notre choix
# poids2 à virer 
# x,x1,x2 a virer (x je sais pas ce que c'est)
# MA_prenom nom dateNaissance
à virer 


names(cas_paris)[which(!names(cas_paris) %in% names(temoins_2011) )]
dput(names(cas_paris)[which(!names(cas_paris) %in% names(temoins_2011) )])
cas_paris_trier<-cas_paris
cas_paris_trier<-cas_paris_trier[,!names(cas_paris_trier) %in% c("numt", "X.2", "X.1", "TU__DateDiag", "TU__AnneeRegistre", 
                                                                 
                                                                 "TU__AgeDiag", "taille.c", "poids2", "poids.c",  "MA__Prenom", "MA__Nom", "MA__LieuNaissDept", 
                                                                 "MA__LieuNaissCom", "MA__LieuNaiss_INSEE_z", "MA___NumMalade", "ddn_mater2", "poids2.c","MA__DateNaissance")]
dput(names(cas_paris_trier))
                                                                                    
names(cas_paris_trier)<-c("code_p","prevoie", "urgence", "typetransfert", "trisomie", "transfert", 
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

dim(cas_paris_trier)
dim(temoins_2012)

### rajouter une collonne commune "Paris#

cas_paris_trier<-cas_paris_trier[,order(colnames(cas_paris_trier)),]
dim(cas_paris_trier)

### arg vérifier le type de varaible
str(temoins_paris_1013h)
str(cas_paris_trier)

cas_paris_trier$numunique<-as.character(cas_paris_trier$numunique)

cas_temoins_parish<-rbind(temoins_paris_1013h,cas_paris_trier)
dim(cas_temoins_parish)
table(cas_temoins_parish$cas)

paris_geoloco<- cas_temoins_parish[,c("numunique","num_rue","prevoie","voirie","nom_v","code_p","cas")]
paris_geoloco$codep<-ifelse(nchar(paris_geoloco$code_p)==2 & !is.na(paris_geoloco$code_p),paste0("750", paris_geoloco$code_p),paris_geoloco$code_p)

cas_temoins_parish$commune<-"Paris"
write.csv(paris_geoloco,"F:/cas_temoins_paris.csv")


### pour le 95 reloader la base temoins F:/temoins_95_geocoderv3.csv


### 95

# dans l'idéal retomber à 109 variables...


dim(cas_95)
names(cas_95)
names(cas_95)[!names(cas_95) %in% names(cas_temoins_parish)]

# numt on vire ?  (regarder quel fichier j'ai envoyé : on garde MA numt comme ref enfant )

# jumeau on vire
# ad : numéro rue
# resses # tout es réuni 
# date de naisance enfant et mater rgarder si différence
# "X.y"                   "X.x"                   "X.2"                   "X.1" a virer
# tu_datediag et tu_année de registre a virer 
# tu age_diag 
# rajouter 2 colones voie et prevoi
# ma sexe à renommer en sexe
# ddn.mater2 à virer
# on garde les date mater relevé
# cp code_p


table(cas_95$MA__DateNaissance,exclude=NULL)
table(cas_95$datenaissance,exclude=NULL)

table(cas_95$MA__DateNaissance==cas_95$datenaissance)
cas_95$MA__DateNaissance[!cas_95$MA__DateNaissance==cas_95$datenaissance]
cas_95$datenaissance[!cas_95$MA__DateNaissance==cas_95$datenaissance]
table(cas_95$MA__DateNaissance==cas_95$datenaissance) # ya une erreu de date donc j'aurais un peu tendance à considérer que le cs8 c'est mieux... 

dim(cas_95)
cas_95_trier<-cas_95
cas_95_trier<-cas_95_trier[,!names(cas_95_trier) %in% c( "poids","nom_v","numt","X.y", "X.x", "X.2", "X.1", "TU__DateDiag", "TU__AnneeRegistre", "TU__AgeDiag", "taille.y",  "poids2", "poids.y",  "MA__LieuNaissDept", "MA__LieuNaissCom", "MA__LieuNaiss_INSEE_z", "MA__DateNaissance", "Jumeau", "ddn_mater2", "ddn.mater.y", "poids2.c", "ddn_mater2.c","taille.x","datenaissance_mere","nom.rue")]
dim(cas_95_trier)


names(cas_95_trier)[!names(cas_95_trier) %in% names(cas_temoins_parish)]
names(cas_temoins_parish)[!names(cas_temoins_parish) %in% names(cas_95_trier)]

cas_95_trier[1,]
dput(names(cas_95_trier))
colnames(cas_95_trier)<-c( "voirie", "urgence", "typetransfert", "trisomie", "transfert", 
                     "testauditif", "taille", "tabac", "spinabifida", "Source", "rupturemembrane", 
                     "retardcroissance", "resultaudit", "resultatrecherche", "nom_v", 
                     "reductionmembre", "rechaghbs", "rangnaissance", "rachieanesthesie", 
                     "prevoie", "presentation", "prepaccouchement", "premconsult", 
                     "preeclampsie", "pprofession", "polymalformation",
                     "poids", "perimetre", "pathologieg", "parite", "pactivite", "oxygenotherapie", 
                     "omphalocele", "num", "niveauetudes", "neurologique", 
                     "nbnesmoins2500", "nbnesavant37", "nbmortne", "nbjourhopital", 
                     "nbfoetus", "nbenfant", "nbecho", "nbdecedeav28j", "naissancepar", 
                     "mprofession", "motifcesariennem", "motifcesariennef", "mois_naiss_m", 
                     "mois_acc", "menacepremature", "maternite_nom", "maternite_code", 
                     "malformrenale", "malformcard", "mactivite", "sexe","numunique", "lieutransfert_nom", 
                     "lieutransfert_code", "inutero", "intubation", "infotransfert", 
                     "hypertension", "hydrocephalie", "hta", "hepatitebvacc", "hepatitebinj", 
                     "gestite", "gestes_techniques", "fente", "echomorpho", "diabete", 
                     "dep_nais", "dep", "debuttravail", "ddnMest", "ddnest", 
                     "datenaissance_mere", "code_p", "commune_nais_code", "commune_famille_code", 
                     "commune", "coeffapgar5mn", "coeffapgar1mn", "clartenucale", 
                     "cesarienne", "bcg", "autrez", "autre_patho", "aucune_analgesie", 
                     "atresie", "antibiotherapie", "anomalie", "analgesieperi", "analgesiegene", 
                     "analgesieautr", "an_naiss_m", "an_acc", "allaitement", "alcool", 
                     "agegestationnel", "age_deces_jj", "age_deces_hh", "num_v", "datenaissance", 
                     "cas", "tbis", "num_rue")
dim(cas_95_trier)

sort(names(cas_95_trier)) 

cas_95_trier$num_v
cas_95_trier$ad
cas_95_trier$num_rue
sort(names(cas_temoins_parish))


cas_95_trier<-cas_95_trier[,order(colnames(cas_95_trier))]
dim(cas_95_trier)
cas_95_trier[1,]


### temoins le numunique c'est la ref enfant


dim(temoins_95help)
names(temoins_95help)
names(temoins_95help)[!names(temoins_95help) %in% names(cas_temoins_paris)]
names(cas_temoins_paris)[!names(cas_temoins_paris) %in% names(temoins_95help)]


# numt on vire ?  (regarder quel fichier j'ai envoyé : on garde MA numt comme ref enfant )

# apgar a virer 
# ad : numéro rue
# resses # tout es réuni 
# date de naisance enfant et mater rgarder si différence
# "X.y"                   "X.x"                   "X.2"                   "X.1" a virer
# tu_datediag et tu_année de registre a virer 
# tu age_diag 
# rajouter 2 colones voie et prevoi
# ma sexe à renommer en sexe
# ddn.mater2 à virer
# on garde les date mater relevé
# cp code_p




#table(cas_95$MA__DateNaissance==cas_95$datenaissance)
#cas_95$MA__DateNaissance[!cas_95$MA__DateNaissance==cas_95$datenaissance]
#cas_95$datenaissance[!cas_95$MA__DateNaissance==cas_95$datenaissance]
#table(cas_95$MA__DateNaissance==cas_95$datenaissance) # ya une erreu de date donc j'aurais un peu tendance à considérer que le cs8 c'est mieux... 


temoins_95_trier<-temoins_95helpp
temoins_95_trier<-temoins_95_trier[,!names(temoins_95_trier) %in% c("X", "Taille.en.cm", "poids2", "Poids.en.gr", 
                                                                    "Mère.Date.de.naissance",  
                                                                    "Ligne2", "Extension.numéro", "Enfant.Date.de.naissance", 
                                                                    "ddn_mater2", "comp",  "Code.postal","Apgar.à.5.mn","Apgar.à.1.mn","Commune.insee"  )]
dim(temoins_95_trier)


names(temoins_95_trier)[!names(temoins_95_trier) %in% names(cas_temoins_paris)]
names(cas_temoins_paris)[!names(cas_temoins_paris) %in% names(temoins_95_trier)]

names(temoins_95_trier)<-c("urgence", "typetransfert", "trisomie", "transfert", "testauditif", 
                           "taille", "tabac", "spinabifida", "Source", "sexe", "rupturemembrane", 
                           "retardcroissance", "resultaudit", "resultatrecherche", "numunique", 
                           "reductionmembre", "rechaghbs", "rangnaissance", "rachieanesthesie", 
                           "presentation", "prepaccouchement", "premconsult", "preeclampsie", 
                           "pprofession", "polymalformation", "poids", "perimetre", "pathologieg", 
                           "parite", "pactivite", "oxygenotherapie", "omphalocele", "num",
                           "num_rue", "niveauetudes", "neurologique", "nbnesmoins2500", 
                           "nbnesavant37", "nbmortne", "nbjourhopital", "nbfoetus", "nbenfant", 
                           "nbecho", "nbdecedeav28j", "naissancepar", "mprofession", "motifcesariennem", 
                           "motifcesariennef", "mois_naiss_m", "mois_acc", "menacepremature", 
                           "maternite_nom", "maternite_code", "malformrenale", "malformcard", 
                           "mactivite", "commune", "nom_v", "lieutransfert_nom", "lieutransfert_code", 
                           "inutero", "intubation", "infotransfert", "hypertension", "hydrocephalie", 
                           "hta", "hepatitebvacc", "hepatitebinj", "gestite", "gestes_techniques", 
                           "fente", "echomorpho", "diabete", "dep_nais", "dep", "debuttravail", 
                           "ddnMest", "ddnest", "datenaissance_mere", "datenaissance", "commune_nais_code", 
                           "commune_famille_code", "coeffapgar5mn", "coeffapgar1mn", 
                           "code_p", "clartenucale", "cesarienne", "bcg", "autrez", "autre_patho", 
                           "aucune_analgesie", "atresie", "antibiotherapie", "anomalie", 
                           "analgesieperi", "analgesiegene", "analgesieautr", "an_naiss_m", 
                           "an_acc", "allaitement", "alcool", "agegestationnel", "age_deces_jj", 
                           "age_deces_hh", "cas", "tbis")



dim(temoins_95_trier)
names(cas_temoins_parish)[!names(cas_temoins_parish) %in% names(temoins_95_trier)]

temoins_95_trier$num_v<-NA
temoins_95_trier$prevoie<-NA
temoins_95_trier$voirie<-NA

sort(names(temoins_95_trier))



temoins_95_trier<-temoins_95_trier[,order(colnames(temoins_95_trier))]
temoins_95_trier[1,]


summary(cas_95_trier)
summary(temoins_95_trier)

CORR<-as.vector(NULL)
for (i in names(cas_95_trier)){
  corr<-!class(cas_95_trier[,i])==class(temoins_95_trier[,i])
  CORR<-c(CORR,corr)
}

lapply(cas_95_trier[,CORR],class)  
lapply(temoins_95_trier[,CORR],class)    


temoins_95_trier$age_deces_jj <-as.integer(as.character(temoins_95_trier$age_deces_jj))
temoins_95_trier$an_acc<-as.integer(as.character(temoins_95_trier$an_acc))
temoins_95_trier$an_naiss_m<-as.integer(as.character(temoins_95_trier$an_naiss_m))
temoins_95_trier$autre_patho<-as.character(temoins_95_trier$autre_patho)
temoins_95_trier$autrez<-as.character(temoins_95_trier$autrez)
temoins_95_trier$commune<-as.character(temoins_95_trier$commune)
temoins_95_trier$commune_nais_code<-as.character(temoins_95_trier$commune_nais_code)
temoins_95_trier$datenaissance<-as.character(temoins_95_trier$datenaissance)
temoins_95_trier$datenaissance<-as.Date(temoins_95_trier$datenaissance,format="%Y-%m-%d")

temoins_95_trier$datenaissance_mere<-as.character(temoins_95_trier$datenaissance_mere)
cas_95_trier$datenaissance_mere<-as.Date(cas_95_trier$datenaissance_mere,format="%d/%m/%Y")
cas_95_trier$datenaissance_mere<-as.character(cas_95_trier$datenaissance_mere)


temoins_95_trier$ddnest<-as.character(temoins_95_trier$ddnest)
cas_95_trier$ddnest<-as.character(cas_95_trier$ddnest)
temoins_95_trier$ddnMest<-as.character(temoins_95_trier$ddnMest)
cas_95_trier$ddnMest<-as.character(cas_95_trier$ddnMest)

temoins_95_trier$dep<-as.integer(as.character(temoins_95_trier$dep))
temoins_95_trier$dep_nais<-as.character(temoins_95_trier$dep_nais)
temoins_95_trier$hydrocephalie<-as.integer(as.character(temoins_95_trier$hydrocephalie))
temoins_95_trier$infotransfert<-as.character(temoins_95_trier$infotransfert)
temoins_95_trier$lieutransfert_code<-as.character(temoins_95_trier$lieutransfert_code)
temoins_95_trier$lieutransfert_nom<-as.character(temoins_95_trier$lieutransfert_nom)
temoins_95_trier$malformcard<-as.integer(as.character(temoins_95_trier$malformcard))
temoins_95_trier$maternite_code<-as.character(temoins_95_trier$maternite_code)
temoins_95_trier$maternite_nom<-as.character(temoins_95_trier$maternite_nom)
temoins_95_trier$mois_acc<-as.integer(as.character(temoins_95_trier$mois_acc))
temoins_95_trier$mois_naiss_m<-as.integer(as.character(temoins_95_trier$mois_naiss_m))
temoins_95_trier$nom_v<-as.character(temoins_95_trier$nom_v)
temoins_95_trier$num<-as.character(temoins_95_trier$num)
temoins_95_trier$num_rue<-as.character(temoins_95_trier$num_rue)
temoins_95_trier$num_v<-as.character(temoins_95_trier$num_v)

temoins_95_trier$numunique<-as.character(temoins_95_trier$numunique)
cas_95_trier$numunique<-as.character(cas_95_trier$numunique)
temoins_95_trier$trisomie<-as.integer(as.character(temoins_95_trier$trisomie))


CORR<-as.vector(NULL)
for (i in names(cas_95_trier)){
  corr<-!class(cas_95_trier[,i])==class(temoins_95_trier[,i])
  CORR<-c(CORR,corr)
}
lapply(temoins_95_trier[,CORR],class)  
# ya plus qu'a ordonner les noms, vérifier la nature  des variables et faire fusionner 

cas_temoins_95_help<-rbind(cas_95_trier,temoins_95_trier)
dim(cas_temoins_95_help)

table(cas_temoins_95_help$datenaissance)
table(cas_temoins_95_help$datenaissance_mere)

### marge final 75 et 95 : a revoir a virer 

sort(names(cas_temoins_95_help))
sort(names(cas_temoins_parish))


CORR<-as.vector(NULL)
for (i in names(cas_temoins_95_help)){
  corr<-!class(cas_temoins_95[,i])==class(cas_temoins_parish[,i])
  CORR<-c(CORR,corr)
}
lapply(cas_temoins_95[,CORR],class)  

CORR<-as.vector(NULL)
for (i in names(cas_temoins_parish)){
  corr<-!class(cas_temoins_95[,i])==class(cas_temoins_parish[,i])
  CORR<-c(CORR,corr)
}


lapply(cas_temoins_parish[,CORR],class)  

cas_temoins_95$code_p<-as.character(cas_temoins_95$code_p)
table(cas_temoins_parish$datenaissance_mere,exclude=NULL)
table(cas_temoins_parish$datenaissance_mere,is.na(as.Date(as.character(cas_temoins_parish$datenaissance_mere),format="%d/%m/%Y")),exclude=NULL)

cas_temoins_parish$datenaissance_mere<-gsub(pattern="(^[0-9]{4})\\-([0-9]{2})\\-([0-9]{2})$",
                                             replacement="\\3\\/\\2\\/\\1",x=cas_temoins_parish$datenaissance_mere)

table(cas_temoins_parish$datenaissance_mere,exclude=NULL)


table(is.na(as.Date(as.character(cas_temoins_parish$datenaissance_meres),format="%d/%m/%Y")),exclude=NULL)

table(cas_temoins_parish$datenaissance_mere)
cas_temoins_parish$datenaissance_mere<-as.Date(as.character(cas_temoins_parish$datenaissance_mere),format="%d/%m/%Y")
cas_temoins_parish$datenaissance_meres<-NULL

cas_temoins_95$prevoie<-as.character(cas_temoins_95$prevoie)
cas_temoins_95$voirie<-as.character(cas_temoins_95$voirie)
cas_temoins_parish$num<-as.character(cas_temoins_parish$num)
cas_temoins_95$ddnest<-as.Date(cas_temoins_95$ddnest)
cas_temoins_95$ddnMest<-as.Date(cas_temoins_95$ddnMest)

cas_temoins_95_trier<-cas_temoins_95
cas_temoins_parish_trier<-cas_temoins_parish


cas_temoins_95_trier<-cas_temoins_95_trier[,order(colnames(cas_temoins_95_trier))]
cas_temoins_parish_trier<-cas_temoins_parish_trier[,order(colnames(cas_temoins_parish_trier))]


cas_temoins<-rbind(cas_temoins_parish_trier,cas_temoins_95_trier)
