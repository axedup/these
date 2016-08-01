### Merging cas paris 2010-2013###

cas_paris<- read.csv("~/Desespoir/Bases/Liste_cas_IDF_compl4crypte.csv.gpg.out", sep=";", quote="", stringsAsFactors=FALSE,na.strings = "")
cas_paris<-cas_paris[!is.na(cas_paris$prevoie),]
cas_paris$MA__DateNaissance<-as.Date(as.character(cas_paris$MA__DateNaissance), "%d/%m/%Y")
str(cas_paris$MA__DateNaissance)
#cas_paris$MA__DateNaissance<-as.Date(as.character(cas_paris$MA__DateNaissance), "%d/%m/%Y")
dim(cas_paris)# 94 cas au 21/06 
head(cas_paris)

### base ors 2010-2013 
# les bases ors ont les mêmes nombre de variables seuls les noms (au tiret pres) différent 
which(!names(data_2010_paris) %in% names(data_2011_paris))
which(!names(data_2011_paris) %in% names(data_2012_paris))
which(!names(data_2012_paris) %in% names(data_2013_paris))
which(!names(data_2010_paris) %in% names(data_2012_paris))

names(data_2010_paris)<-names(data_2012_paris) # euh un sort avant c'est mieux 
data_paris<-rbind(data_2010_paris,data_2011_paris,data_2012_paris,data_2013_paris)
names(data_2010_paris) %in% names(data_2011_paris)
names(data_2011_paris)
names(data_2012_paris)
names(data_2013_paris)




### merge
# je vais virer les 3 premiers car pas bien rempli
# pas normal de galérer sur les ifelse....

cas_paris$ddn_mater2<-ifelse(is.na(cas_paris$ddn.mater),"13/13/1313",cas_paris$ddn.mater)
cas_paris$ddn_mater2<-ifelse(cas_paris$ddn_mater2=="nc ","13/13/1313",cas_paris$ddn_mater2)
cas_paris$ddn_mater2<-ifelse(cas_paris$ddn_mater2=="nc","13/13/1313",cas_paris$ddn_mater2)
table(cas_paris$ddn_mater2,exclude=NULL)

# je préfère comme ça 
cas_paris$ddn_mater2<-ifelse(is.na(cas_paris$ddn.mater) | cas_paris$ddn.mater=="nc "|cas_paris$ddn.mater=="nc" ,"13/12/1913",cas_paris$ddn.mater)
cas_paris$ddn_mater2<-as.Date(cas_paris$ddn_mater2,format="%d/%m/%Y")
table(cas_paris$ddn_mater2,exclude=NULL)

table(cas_paris$poids,exclude=NULL)
cas_paris$poids2<-ifelse(is.na(cas_paris$poids),40000,cas_paris$poids)
table(cas_paris$poids2,exclude=NULL)
str(cas_paris$poids2)
#cas_paris$MA__DateNaissance<-as.Date(cas_paris$MA__DateNaissance,format="%d/%m/%Y")
table(cas_paris$MA__DateNaissance,exclude=NULL)

cas_paris<-cas_paris[order(cas_paris$MA__DateNaissance,cas_paris$ddn_mater2),]

data_paris$ddn_mater2<-as.character(data_paris$datenaissance_mere)
table(data_paris$ddn_mater2,exclude=NULL)
table(is.na(data_paris$ddn_mater2))
data_paris$ddn_mater2<-ifelse(is.na(data_paris$ddn_mater2),"1913-12-13",data_paris$ddn_mater2)
table(is.na(data_paris$ddn_mater2))
table(data_paris$ddn_mater2)
data_paris$ddn_mater2<-as.Date(data_paris$ddn_mater2,format="%Y-%m-%d")
table(is.na(data_paris$ddn_mater2)
table(data_paris$ddn_mater2)

table(is.na(data_paris$poids))
data_paris$poids2<-as.character(data_paris$poids)
data_paris$poids2<-ifelse(is.na(data_paris$poids2),"40000",data_paris$poids2)
data_paris$poids2<-as.numeric(data_paris$poids2)
table(is.na(data_paris$poids2))
str(data_paris$poids2)

data_paris<-data_paris[order(data_paris$datenaissance,data_paris$ddn_mater2),]
data_paris$numt<-1:dim(data_paris)[1]


#même codage de sexe 
test<-merge(cas_paris[-3,],data_paris,by.x=c("MA__DateNaissance","MA__Sexe","ddn_mater2","poids2"),by.y=c("datenaissance","sexe","ddn_mater2","poids2"),suffixes=c(".r",".c") )
test<-test[,order(colnames(test),decreasing=TRUE)]

table(test$MA__LieuNaissCom %in% test$commune_famille_code)

dim(test)
length(test$MA___NumMalade)
length(unique(test$MA___NumMalade)) ### pas de gag !!!!!!!


cas_paris[which(!cas_paris$MA___NumMalade %in% test$MA___NumMalade ),]

controle_1<-data_paris[data_paris$datenaissance=="2010-02-09",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
controle_1<-controle_1[controle_1$sexe==1,]
controle_1<-controle_1[order(c(controle_1$ddn_mater2,controle_1$poids2)),] 
controle_1 # 3148




controle_1<-data_paris[data_paris$datenaissance=="2010-10-19",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
controle_1<-controle_1[controle_1$sexe==2,]
controle_1<-controle_1[order(c(controle_1$ddn_mater2,controle_1$poids2)),] 
controle_1 # 23512


controle_1<-data_paris[data_paris$datenaissance=="2011-10-30",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
controle_1<-controle_1[controle_1$sexe==1,]
#controle_1<-controle_1[order(c(controle_1$ddn_mater2,controle_1$poids2)),] 
controle_1[controle_1$poids2==3620,]
controle_1 # trouvé 53014

controle_1<-data_paris[data_paris$datenaissance=="2011-09-27",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
controle_1<-controle_1[controle_1$sexe==1,]
#controle_1<-controle_1[order(c(controle_1$ddn_mater2,controle_1$poids2)),] # manquante ++++
controle_1
controle_1[controle_1$poids2==3150,]



v<-data_paris[data_paris$poids2==3150,c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
v[v$taille==480 ,]
v[v$ddn_mater2=="1913-12-13" ,]## erreur date de naissance il est né le 2011-08-27. #48008


controle_1<-data_paris[data_paris$datenaissance=="2012-07-01",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
controle_1<-controle_1[controle_1$sexe==2,]
#controle_1<-controle_1[order(c(controle_1$ddn_mater2,controle_1$poids2)),] # trouve 71292 erreur ddn mater 
controle_1

controle_1<-data_paris[data_paris$datenaissance=="2012-02-07",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
controle_1<-controle_1[controle_1$sexe==2,]
#controle_1<-controle_1[order(c(controle_1$ddn_mater2,controle_1$poids2)),] # trouve 60478
controle_1


controle_1<-data_paris[data_paris$datenaissance=="2010-11-06",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
controle_1<-controle_1[controle_1$sexe==2,]
controle_1[controle_1$poids2==2435,]
#controle_1<-controle_1[order(c(controle_1$ddn_mater2,controle_1$poids2)),] 
controle_1 #25073

controle_1<-data_paris[data_paris$datenaissance=="2012-11-01",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]

controle_1[controle_1$poids2==4200,]
controle_1[controle_1$taille==52.5,]
#controle_1<-controle_1[order(c(controle_1$ddn_mater2,controle_1$poids2)),] m nquante  #78315 pb date de naissance enfant
controle_1 


v<-data_paris[data_paris$poids2==4200,c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
v[v$taille==52.5 ,]


controle_1<-data_paris[data_paris$datenaissance=="2012-06-25",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code","commune_famille_code")]

controle_1[controle_1$taille==46.0,]
#controle_1<-controle_1[order(c(controle_1$ddn_mater2,controle_1$poids2)),]
controle_1 #70799



############ Fusion des corrections 
### !!!! c'est pas un merge et r ordonne selon son propre ordre 




g<-cbind(cas_paris[which(!cas_paris$MA___NumMalade %in% test$MA___NumMalade ),],data_paris[data_paris$numt %in% c(3148,25073,78315,70799,23512,53014,48008,71292,60478),!names(data_paris) %in% c("datenaissance","sexe","ddn_mater2","poids2")])

g[,c("MA___NumMalade","numt")]

g<-g[,order(colnames(g),decreasing=TRUE)]
names(g)<-names(test)
fait<-rbind(test,g)
fait<-merge(fait,data_paris[,c("numt","datenaissance","poids2")], by="numt",suffixes=c("",".c")) # pour corrige les gags d'eeureur de ddn dans le registe 
names(fait)
dim(fait)


cas_paris<-fait
cas_paris$cas<-1
cas_paris$tbis<-grepl(pattern="^b",cas_paris$num)
cas_paris$num_rue<-gsub(pattern="^b*([0-9].*)",replace="\\1",cas_paris$num)

table(cas_paris$num_rue,cas_paris$num)




### cr?ation num?ro unique 


set.seed(8521)# a faire tourner a chaque add
add<-sample(seq(from =1, to =dim(cas_paris)[1], by = 1), size = dim(cas_paris)[1], replace = FALSE)

cas_paris$numunique<-cas_paris$MA___NumMalade*add


### choix de la date de naissance mater : ddn.mater car on a recup des date de naisance mater dans les dossier sauf pour les 3 premiers erreur de saisie - un doute 
# aptientes entre le 1 et le cas du m?me mois 


cas_paris$ddn.mater[cas_paris$datenaissance=="2010-11-06"]<-"1980-03-08"
cas_paris$datenaissance_mere[cas_paris$datenaissance=="2010-11-06"]

## EUH LA C42TAIT PAS A FAIRE#
cas_paris$ddn.mater[cas_paris$datenaissance=="2012-06-25"]<-c(NA,"1970-01-06")
cas_paris$datenaissance_mere[cas_paris$datenaissance=="2012-06-25"]
cas_paris$ddn.mater[cas_paris$datenaissance=="2012-06-25"]


cas_paris$ddn.mater[cas_paris$datenaissance=="2012-10-01"]<-c("1975-10-26")
cas_paris$ddn.mater[cas_paris$datenaissance=="2012-10-01"]
cas_paris$datenaissance_mere[cas_paris$datenaissance=="2012-10-01"]


write.csv(cas_paris,"F:/cas_paris.csv")
write.csv(cbind(cas_paris$MA___NumMalade,cas_paris$numunique),"~/Desespoir/Bases/correspondance_cas_paris.csv")
write.csv(cas_paris[,c("MA___NumMalade","datenaissance_mere","MA__Sexe","poids.c","taille.c","MA__DateNaissance","num_rue","prevoie","voie","nom.rue")],"F:/cas_paris_geocoder.csv")

