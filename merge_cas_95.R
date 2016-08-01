### Merging cas 95 2010-2013###

cas_95 <- read.csv("F:/Extraits CD95.csv", encoding="UTF-8", sep=";", stringsAsFactors=FALSE,na.strings = "")
cas_95 <- read.csv("C:/Users/Louise/Documents/Desespoir/Bases/Extraits CD95.csv", encoding="UTF-8", sep=";", stringsAsFactors=FALSE,na.strings = "")

cas_idf <- read.csv("C:/Users/Louise/Documents/Desespoir/Bases/Liste_cas_IDF_compl4crypte.csv", sep=";", quote="", stringsAsFactors=FALSE,na.strings = "")
cas_idf <- read.csv("F:/Liste_cas_IDF_compl4crypte.csv", sep=";", quote="", stringsAsFactors=FALSE,na.strings = "")



dim(cas_95) #53 idf 
cas_95
cas_95<-cas_95[!is.na(cas_95$resses),]
cas_95<-cas_95[!substr(cas_95$cp,1,2)=="60",]
dim(cas_95) #47 


str(cas_95$MA___NumMalade)
str(cas_idf$MA___NumMalade)

cas_95<-merge(cas_95,cas_idf,by="MA___NumMalade",suffixes=c(".x",".y"))
table(cas_95$MA__DateNaissance,exclude=NULL)

cas_95$MA__DateNaissance<-as.Date(as.character(cas_95$MA__DateNaissance), "%d/%m/%Y")
table(cas_95$MA__DateNaissance,exclude=NULL)
#cas_95$MA__DateNaissance<-as.Date(as.character(cas_95$MA__DateNaissance), "%d/%m/%Y")
dim(cas_95)

dim(cas_95) #47 idf 
table(cas_95$MA__DateNaissance)



# !!!! faut virer l'oise 

cas_95<-cas_95[!substr(cas_95$cp,1,2)=="60",]
dim(cas_95)
cas_95<-cas_95[substr(cas_95$cp,1,2) %in% c("75","91","92","93","94","95","77","78"),]

cas_95<-cas_95[!is.na(cas_95$resses),]
#cas_95$MA__DateNaissance<-as.Date(as.character(cas_95$MA__DateNaissance), "%d/%m/%Y")
#cas_95$MA__DateNaissance<-as.Date(as.character(cas_95$MA__DateNaissance), "%d/%m/%Y")
dim(cas_95)


### base ors 2010-2013 
# les bases ors ont les mêmes nombre de variables seuls les noms (au tiret pres) différent 
data_2010_95<-data_2010[data_2010$Source=="95" & !is.na(data_2010$Source),] 
data_2011_95<-data_2011[data_2011$Source=="95" & !is.na(data_2011$Source),] 
data_2012_95<-data_2012[data_2012$Source=="95" & !is.na(data_2012$Source),] 
data_2013_95<-data_2013[data_2013$Source=="95" & !is.na(data_2013$Source),] 


which(!names(data_2010_95) %in% names(data_2011_95))
which(!names(data_2011_95) %in% names(data_2012_95))
which(!names(data_2012_95) %in% names(data_2013_95))
which(!names(data_2010_95) %in% names(data_2012_95))

names(data_2010)<-names(data_2012)
data_1013<-rbind(data_2010,data_2011,data_2012,data_2013)



### merge
# 
# pas normal de galérer sur les ifelse....
# 
# cas_95$ddn_mater2<-ifelse(is.na(cas_95$ddn.mater.x),"13/13/1313",cas_95$ddn.mater.x)
# cas_95$ddn_mater2<-ifelse(cas_95$ddn_mater2=="nc ","13/13/1313",cas_95$ddn_mater2)
# cas_95$ddn_mater2<-ifelse(cas_95$ddn_mater2=="nc","13/13/1313",cas_95$ddn_mater2)
# table(cas_95$ddn_mater2,exclude=NULL)

# je préfère comme ça 
cas_95$ddn_mater2<-ifelse(is.na(cas_95$ddn.mater.x) | cas_95$ddn.mater.x=="nc "|cas_95$ddn.mater.x=="nc" ,"13/12/1913",cas_95$ddn.mater.x)
cas_95$ddn_mater2<-as.Date(cas_95$ddn_mater2,format="%d/%m/%Y")
table(cas_95$ddn_mater2,exclude=NULL)

table(cas_95$poids.x,exclude=NULL)
cas_95$poids2<-ifelse(is.na(cas_95$poids.x),40000,cas_95$poids.x)
table(cas_95$poids2,exclude=NULL)
str(cas_95$poids2)
#cas_95$MA__DateNaissance<-as.Date(cas_95$MA__DateNaissance,format="%d/%m/%Y")
table(cas_95$MA__DateNaissance,exclude=NULL)

cas_95<-cas_95[order(cas_95$MA__DateNaissance,cas_95$ddn_mater2),]

data_1013$ddn_mater2<-as.character(data_1013$datenaissance_mere)
table(data_1013$ddn_mater2,exclude=NULL)
table(is.na(data_1013$ddn_mater2))
data_1013$ddn_mater2<-ifelse(is.na(data_1013$ddn_mater2),"1913-12-13",data_1013$ddn_mater2)
table(is.na(data_1013$ddn_mater2))
table(data_1013$ddn_mater2)
data_1013$ddn_mater2<-as.Date(data_1013$ddn_mater2,format="%Y-%m-%d")
table(is.na(data_1013$ddn_mater2))
table(data_1013$ddn_mater2)
str(data_1013$ddn_mater2)

table(is.na(data_1013$poids))
data_1013$poids2<-as.character(data_1013$poids)
data_1013$poids2<-ifelse(is.na(data_1013$poids2),"40000",data_1013$poids2)
data_1013$poids2<-as.numeric(data_1013$poids2)
table(is.na(data_1013$poids2))
str(data_1013$poids2)

data_1013<-data_1013[order(data_1013$datenaissance,data_1013$ddn_mater2),]
data_1013$numt<-1:dim(data_1013)[1]


# même codage du sexe. 

str(cas_95$MA__Sexe)
str(data_1013$sexe)
test_95<-merge(cas_95[,],data_1013,by.x=c("MA__DateNaissance","MA__Sexe","ddn_mater2","poids2"),by.y=c("datenaissance","sexe","ddn_mater2","poids2"),suffixes=c(".r",".c") )
test_95<-test_95[,order(colnames(test_95),decreasing=TRUE)]

table(test$MA__LieuNaissCom %in% test$commune_famille_code)

length(test_95$MA___NumMalade)
length(unique(test_95$MA___NumMalade))

dim(test_95)


test_95$MA___NumMalade<-as.character(test_95$MA___NumMalade)
table(test_95$MA___NumMalade)
table(test_95$datenaissance) 

table(nom$Référence.Enfant)

test_95$taille.x<-(as.numeric(gsub(pattern = "^([0-9]{2})\\.([0-9]*)|^([0-9]{2})\\/\\,([0-9]*)",x=test_95$taille.x,replacement="\\1\\.\\2")))
test_95$taille<-as.numeric(gsub(pattern = "^([0-9]{2})([0-9]*)",x=test_95$taille,replacement="\\1\\.\\2"))



dim(test_95)

verif_enf<-test_95$MA___NumMalade[duplicated(test_95$MA___NumMalade)]


doublons<-test_95[test_95$MA___NumMalade %in% verif_enf,]
dim(doublons)

dim(doublons[!doublons$taille==doublons$taille.x,])
#ceux_a_g<-doublons[doublons$commune_famille_code==doublons$Commune.insee,]
#dim(ceux_a_g)
#table(ceux_a_g$R?f?rence.Enfant)
#verif_enf

# ok y a un doublons

agarder<-test_95[test_95$numt==15654,]
test_95<-test_95[!test_95$MA___NumMalade %in% verif_enf,]
dim(test_95)
test_95<-rbind(test_95,agarder)

dim(test_95)

head(test_95)

table(test_95$MA__DateNaissance)
dim(test_95)
table(test_95$datenaissance) 
length(test_95$MA___NumMalade)
length(unique(test_95$MA___NumMalade))
table(test_95$MA___NumMalade)















####
length(which(!cas_95$MA___NumMalade %in% test_95$MA___NumMalade ))
cas_95[which(!cas_95$MA___NumMalade %in% test_95$MA___NumMalade ),]
dim(cas_95[which(!cas_95$MA___NumMalade %in% test_95$MA___NumMalade ),])



controle_1<-data_1013[data_1013$datenaissance=="2010-01-30" ,c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
controle_1<-controle_1[controle_1$sexe==2 & !is.na(controle_1$sexe),]
controle_1<-controle_1[order(c(controle_1$ddn_mater2,controle_1$poids2)),] 
controle_1 # 150676 erreur sur la date de naissance mater 1977 au lieu de 1997 ! 




controle_1<-data_1013[data_1013$datenaissance=="2010-01-31",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
controle_1<-controle_1[controle_1$sexe==2 & !is.na(controle_1$sexe),]
controle_1<-controle_1[order(controle_1$ddn_mater2,controle_1$poids2),] 
controle_1 #12515 erreur sur le poids

#####
controle_1<-data_1013[data_1013$datenaissance=="2010-10-04" & !is.na(data_1013$datenaissance) ,c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
controle_1<-controle_1[controle_1$sexe==1 &!is.na(controle_1$sexe),]
controle_1<-controle_1[order(controle_1$ddn_mater2,controle_1$poids2),] 
controle_1
controle_1[controle_1$poids2==40000,]
controle_1 # manquante 


v<-data_1013[data_1013$poids2==40000 ,c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
v<-v[is.na(v$taille) ,]
v<-v[v$ddn_mater2=="1989-12-24" ,]
v

controle_1<-data_1013[data_1013$poids2==40000 & data_1013$Source==95,c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
controle_1[controle_1$ddn_mater2=="1979-12-25",]

controle_1


##########
controle_1<-data_1013[data_1013$datenaissance=="2010-07-19" & !is.na(data_1013$datenaissance),c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code","maternite_nom")]
controle_1<-controle_1[controle_1$sexe==1 &!is.na(controle_1$sexe),]
controle_1<-controle_1[order(controle_1$ddn_mater2,controle_1$poids2),] # manquante ++++
controle_1
controle_1[controle_1$poids2==3300,]
controle_1[controle_1$taille==510,] ### manquante


controle_1<-data_1013[data_1013$poids2==3300 & data_1013$Source==95,c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
controle_1[controle_1$ddn_mater2=="1981-06-14",]
#controle_1<-controle_1[controle_1$taille==50.5 |controle_1$taille==505.0,]
controle_1

### 382343 erreur date de naissance  n?s en 2012 et pas 2010



#####################
controle_1<-data_1013[data_1013$datenaissance=="2012-01-24 ",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code","maternite_nom")]
controle_1<-controle_1[controle_1$sexe==2 &!is.na(controle_1$sexe),]

controle_1

controle_1<-controle_1[order(c(controle_1$ddn_mater2,controle_1$poids2)),] 
controle_1[controle_1$taille=="50.5",] # manquante


v<-data_1013[data_1013$poids2==3020 & !is.na(data_1013$poids2) ,c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code","Source")]
v<-v[ v$taille=="50.5" & !is.na(v$taille) ,]
v<-v[v$ddn_mater2=="1979-12-24" ,] # manquante

# pas retrouv? 


controle_1<-data_1013[data_1013$poids2==3020 & data_1013$Source==95,c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
controle_1[controle_1$ddn_mater2=="1979-06-01",]
controle_1<-controle_1[controle_1$taille==50.5 |controle_1$taille==505.0,]
controle_1

controle_1<-data_1013[data_1013$poids2==3300 & data_1013$Source==95,c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
controle_1[controle_1$ddn_mater2=="1981-06-14",]
#controle_1<-controle_1[controle_1$taille==50.5 |controle_1$taille==505.0,]
controle_1

### 382343 erreur date de naissance mater n?s en 2012 et pas 2010




####################
controle_1<-data_1013[data_1013$datenaissance=="2012-05-08",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
controle_1<-controle_1[controle_1$sexe==2 &!is.na(controle_1$sexe),]
controle_1<-controle_1[order(controle_1$ddn_mater2,controle_1$poids2),] 
controle_1[controle_1$poids2==3250,]
controle_1[controle_1$poids2==2440,]

# retrouv? 
controle_1<-data_1013[data_1013$poids2==3250 & data_1013$Source==95,c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
controle_1<-controle_1[controle_1$taille==51.0 |controle_1$taille==510.0,]
controle_1
# 349452


# pas retrouv? 
controle_1<-data_1013[data_1013$poids2==2440 & data_1013$Source==95,c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
controle_1<-controle_1[controle_1$taille==46.0 |controle_1$taille==460.0,]
controle_1


############ Fusion des corrections 
### !!!! c'est pas un merge et r ordonne selon son propre ordre 

flu<-data_1013[data_1013$numt %in% c(12162,12515,382343,349452),!names(data_1013) %in% c("datenaissance","sexe","ddn_mater2","poids2")]
flu<-flu[match(c(12162,12515,382343,349452),flu$numt),]

g<-cbind(cas_95[which(cas_95$MA___NumMalade %in% c(200906,131053,129812,206384)),],flu)
g

str(cas_95$MA___NumMalade)
g<-g[,order(colnames(g),decreasing=TRUE)]
test_95<-test_95[,order(colnames(test_95),decreasing=TRUE)]
names(g)<-names(test_95)


str(test_95)
fait_95<-rbind(test_95,g) ## faut vériifer la nature des VA : seul taille.x a un pb 


fait_95<-merge(fait_95,data_1013[,c("numt","datenaissance","poids2","ddn_mater2")], by="numt",suffixes=c("",".c")) # pas de pb sur les date de naissance des enfants
names(fait_95)
fait_95<-fait_95[,!names(fait_95) %in% c("MA__Nom","MA__Prenom")]

## patientes 20104 fusionne 2 fois ? cause du poids il faut garder l'observation avec une taille de 49 cm virer l'observ numt 15653 
dim(fait_95)

fait_95<-fait_95[!fait_95$numt %in% c("15653"),]

length(fait_95$MA___NumMalade)
length(unique(fait_95$MA___NumMalade))


length(test_95$MA___NumMalade)
length(unique(test_95$MA___NumMalade))

head(fait_95)
table(fait_95$datenaissance)


fait_95[!fait_95$MA__DateNaissance==fait_95$datenaissance,]

# 44 cas en val d'oise (exclusion des cas n?s en idf mais dont l'adresse ? la r?sidence et dans le 60/ un cas est problématique registre nés en IDF cs8 dans l'oise)
cas_95<-fait_95
dim(cas_95)

#cas_95_complet<-cas_95

cas_95<-fait_95
cas_95$cas<-1
cas_95$tbis<-FALSE
cas_95$num_rue<-cas_95$ad
cas_95$voie<-NA
cas_95$prevoie<-NA
cas_95$nom_v<-NA


write.csv(cas_95[,c("MA___NumMalade","ddn_mater2","MA__Sexe","poids.x","taille.x","MA__DateNaissance","num_rue","resses","commune","cp")],"F:/cas_95_geocoderv2.csv")
write.csv(fait_95,"F:/cas_95.csv")
