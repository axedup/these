
rm(ls=lista)
rm(ls=listu)
rm(ls=essai)
rm(ls=essai2)
rm(ls=data_2010)


### merge
# 
names(temoins_95t)
head(table(temoins_95t$Ligne4))
table(temoins_95t$codep)

# je prÃ©fÃ¨re comme Ã§a 
temoins_95t$Mère.Date.de.naissance<-as.character(temoins_95t$Mère.Date.de.naissance)
table(temoins_95t$Mère.Date.de.naissance,exclude=NULL)
temoins_95t$ddn_mater2<-ifelse(is.na(temoins_95t$Mère.Date.de.naissance) | temoins_95t$Mère.Date.de.naissance==""|temoins_95t$Mère.Date.de.naissance=="nc" ,"13/12/1913",temoins_95t$Mère.Date.de.naissance)
temoins_95t$ddn_mater2<-as.Date(temoins_95t$ddn_mater2,format="%d/%m/%Y")
table(temoins_95t$ddn_mater2,exclude=NULL)

table(temoins_95t$Poids.en.gr,exclude=NULL)
temoins_95t$Poids.en.gr<-as.character(temoins_95t$Poids.en.gr)
table(temoins_95t$Poids.en.gr,exclude=NULL)
temoins_95t$Poids.en.gr<-gsub(" ","",temoins_95t$Poids.en.gr)
temoins_95t$poids2<-ifelse(temoins_95t$Poids.en.gr=="",40000,temoins_95t$Poids.en.gr)

temoins_95t$poids2<-as.numeric(temoins_95t$poids2)

table(temoins_95t$poids2,exclude=NULL)
str(temoins_95t$poids2)
#temoins_95t$MA__DateNaissance<-as.Date(temoins_95t$MA__DateNaissance,format="%d/%m/%Y")
table(temoins_95t$datenaissance,exclude=NULL)
str(temoins_95t$datenaissance,exclude=NULL)
temoins_95t<-temoins_95t[order(temoins_95t$datenaissance,temoins_95t$ddn_mater2),]

data_1013$ddn_mater2<-as.character(data_1013$datenaissance_mere)
table(data_1013$ddn_mater2,exclude=NULL)
table(is.na(data_1013$ddn_mater2))
data_1013$ddn_mater2<-ifelse(is.na(data_1013$ddn_mater2),"1913-12-13",data_1013$ddn_mater2)
table(is.na(data_1013$ddn_mater2))
table(data_1013$ddn_mater2)
head(table(data_1013$ddn_mater2))
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

test_95<-merge(temoins_95t[,],data_1013,by.x=c("datenaissance","ddn_mater2","poids2"),by.y=c("datenaissance","ddn_mater2","poids2"),suffixes=c(".r",".c") )
test_95<-test_95[,order(colnames(test_95),decreasing=TRUE)]
dim(test_95)
table(test_95$datenaissance) ### ARG MERGATION 2 FOIS


### merge 2 fois : vérification 

test_95$Référence.Enfant<-as.character(test_95$Référence.Enfant)
table(test_95$Référence.Enfant)
table(test_95$datenaissance) 



test_95$Taille.en.cm<-as.numeric(gsub(pattern = "^([0-9]{2})\\.([0-9]*)",x=test_95$Taille.en.cm,replacement="\\1\\.\\2"))
test_95$taille<-as.numeric(gsub(pattern = "^([0-9]{2})([0-9]*)",x=test_95$taille,replacement="\\1\\.\\2"))



dim(test_95)

verif_enf<-test_95$Référence.Enfant[duplicated(test_95$Référence.Enfant)]


doublons<-test_95[test_95$Référence.Enfant %in% verif_enf,]
dim(doublons)

#dim(doublons[!doublons$taille==doublons$Taille.en.cm,])
ceux_a_g<-doublons[doublons$commune_famille_code==doublons$Commune.insee,]
dim(ceux_a_g)
table(ceux_a_g$Référence.Enfant)
verif_enf

# ok 

test_95<-test_95[!test_95$Référence.Enfant %in% verif_enf,]
dim(test_95)
test_95<-rbind(test_95,ceux_a_g)

table(test_95$datenaissance) 



dim(test_95)
table(test_95$datenaissance) 
length(test_95$Référence.Enfant)
length(unique(test_95$Référence.Enfant))


### récupérons alors bien 10 témoins par cas : a répéter si nécessessaire (si un cas a 8 témoins par exemple) 


ft<-10-table(test_95$datenaissance) 
ft

ft<-ifelse(ft<0,0,ft)
ft<-rep(names(table(test_95$datenaissance)),ft)
ft

dim(test_95)
date1<-names(table(test_95$datenaissance))[table(test_95$datenaissance) < 10]
date1
ds<-temoins_95[which(!temoins_95$Référence.Enfant %in% test_95$Référence.Enfant ),]

ds<-ds[ds$datenaissance %in% date1,]
set.seed(55556)
ds<-ds[sample(nrow(ds)),]

GS<-NULL
for(i in unique(ft)){
  tt<-ds[ds$datenaissance==i,]
  gs<-tt[1 :table(ft)[names(table(ft))==i],]
  GS<-rbind(GS,gs)
}

# je prÃ©fÃ¨re comme Ã§a 
GS$Mère.Date.de.naissance<-as.character(GS$Mère.Date.de.naissance)
table(GS$Mère.Date.de.naissance,exclude=NULL)
GS$ddn_mater2<-ifelse(is.na(GS$Mère.Date.de.naissance) | GS$Mère.Date.de.naissance==""|GS$Mère.Date.de.naissance=="nc" ,"13/12/1913",GS$Mère.Date.de.naissance)
GS$ddn_mater2<-as.Date(GS$ddn_mater2,format="%d/%m/%Y")
table(GS$ddn_mater2,exclude=NULL)

table(GS$Poids.en.gr,exclude=NULL)
GS$Poids.en.gr<-as.character(GS$Poids.en.gr)
table(GS$Poids.en.gr,exclude=NULL)
GS$Poids.en.gr<-gsub(" ","",GS$Poids.en.gr)
GS$poids2<-ifelse(GS$Poids.en.gr=="",40000,GS$Poids.en.gr)

GS$poids2<-as.numeric(GS$poids2)

table(GS$poids2,exclude=NULL)
str(GS$poids2)

str(GS$datenaissance,exclude=NULL)
GS$datenaissance<-as.Date(GS$datenaissance,format="%Y-%m-%d")
table(GS$datenaissance,exclude=NULL)
str(GS$datenaissance,exclude=NULL)
GS<-GS[order(GS$datenaissance,GS$ddn_mater2),]



test_95suite<-merge(GS[,],data_1013,by.x=c("datenaissance","ddn_mater2","poids2"),by.y=c("datenaissance","ddn_mater2","poids2"),suffixes=c(".r",".c") )
test_95suite$Référence.Enfant<-as.character(test_95suite$Référence.Enfant)
str(ft)
dim(test_95suite)
table(test_95suite$datenaissance)
table(test_95suite$Référence.Enfant)
length(test_95suite$Référence.Enfant)
length(unique(test_95suite$Référence.Enfant))


dim(test_95)
table(test_95$datenaissance)
test_95<-rbind(test_95,test_95suite)

dim(test_95)
table(test_95$datenaissance) 
length(test_95$Référence.Enfant)
length(unique(test_95$Référence.Enfant))

test_95[duplicated(test_95$Référence.Enfant),]


### on recommence pour les éventuells qui n'a 10 par cas


ft<-10-table(test_95$datenaissance) 
ft<-ifelse(ft<0,0,ft)
ft<-rep(names(table(test_95$datenaissance)),ft)
ft

dim(test_95)
date1<-names(table(test_95$datenaissance))[table(test_95$datenaissance) < 10]
date1
ds<-temoins_95[which(!temoins_95$Référence.Enfant %in% test_95$Référence.Enfant ),]

ds<-ds[ds$datenaissance %in% date1,]
set.seed(5555750)
ds<-ds[sample(nrow(ds)),]

GS<-NULL
for(i in unique(ft)){
  tt<-ds[ds$datenaissance==i,]
  gs<-tt[1 :table(ft)[names(table(ft))==i],]
  GS<-rbind(GS,gs)
}


GS$Mère.Date.de.naissance<-as.character(GS$Mère.Date.de.naissance)
table(GS$Mère.Date.de.naissance,exclude=NULL)
GS$ddn_mater2<-ifelse(is.na(GS$Mère.Date.de.naissance) | GS$Mère.Date.de.naissance==""|GS$Mère.Date.de.naissance=="nc" ,"13/12/1913",GS$Mère.Date.de.naissance)
GS$ddn_mater2<-as.Date(GS$ddn_mater2,format="%d/%m/%Y")
table(GS$ddn_mater2,exclude=NULL)

table(GS$Poids.en.gr,exclude=NULL)
GS$Poids.en.gr<-as.character(GS$Poids.en.gr)
table(GS$Poids.en.gr,exclude=NULL)
GS$Poids.en.gr<-gsub(" ","",GS$Poids.en.gr)
GS$poids2<-ifelse(GS$Poids.en.gr=="",40000,GS$Poids.en.gr)

GS$poids2<-as.numeric(GS$poids2)

table(GS$poids2,exclude=NULL)
str(GS$poids2)

str(GS$datenaissance,exclude=NULL)
GS$datenaissance<-as.Date(GS$datenaissance,format="%Y-%m-%d")
table(GS$datenaissance,exclude=NULL)
str(GS$datenaissance,exclude=NULL)
GS<-GS[order(GS$datenaissance,GS$ddn_mater2),]



test_95suite<-merge(GS[,],data_1013,by.x=c("datenaissance","ddn_mater2","poids2"),by.y=c("datenaissance","ddn_mater2","poids2"),suffixes=c(".r",".c") )
test_95suite$Référence.Enfant<-as.character(test_95suite$Référence.Enfant)
str(ft)
dim(test_95suite)
table(test_95suite$datenaissance)
table(test_95suite$Référence.Enfant)
length(test_95suite$Référence.Enfant)



dim(test_95)
table(test_95$datenaissance)
test_95<-rbind(test_95,test_95suite)

dim(test_95)
table(test_95$datenaissance) 
length(test_95$Référence.Enfant)
length(unique(test_95$Référence.Enfant))

test_95[duplicated(test_95$Référence.Enfant),]


## vérification que les témoins n'ont pas une adresse parisienne (risque de doublons)  
table(test_95$Commune.insee,exclude=NULL) #!!!! # Y a aucun témoins de Paris




fait_95[!fait_95$MA__DateNaissance==fait_95$datenaissance,]
# 44 cas en val d'oise (exclusion des cas nés en idf mais dont l'adresse à la résidence et dans le 60)




### controle que aucun témoins n'est en fait un cas 

table(test_95$numt %in% fait_95$numt)
# 4 témoins sont des cas


avirer<-test_95[test_95$numt %in% fait_95$numt,]
test_95<-test_95[!test_95$numt %in% fait_95$numt,]
dim(test_95)

table(test_95$datenaissance)


ft<-10-table(test_95$datenaissance) 
ft<-ifelse(ft<0,0,ft)
ft<-rep(names(table(test_95$datenaissance)),ft)
ft<-as.Date(ft,format="%d/%m/%Y")
ft
ft<-as.character(ft)
ft

dim(test_95)
date1<-names(table(test_95$datenaissance))[table(test_95$datenaissance) < 10]
date1
ds<-temoins_95[which(!temoins_95$Référence.Enfant %in% test_95$Référence.Enfant ),]

str(date1)
date1<-as.Date(date1,format="%d/%m/%Y")
date1
date1<-as.character(date1)
date1
str(ds$datenaissance)

ds<-ds[ds$datenaissance %in% date1,]


dim(ds)
set.seed(5555759)
ds<-ds[sample(nrow(ds)),]

GS<-NULL
for(i in unique(ft)){
  tt<-ds[ds$datenaissance==i,]
  gs<-tt[1 :table(ft)[names(table(ft))==i],]
  GS<-rbind(GS,gs)
}

# je prÃ©fÃ¨re comme Ã§a 

str(GS$Mère.Date.de.naissance)
GS$Mère.Date.de.naissance<-as.character(GS$Mère.Date.de.naissance)
table(GS$Mère.Date.de.naissance,exclude=NULL)
GS$ddn_mater2<-ifelse(is.na(GS$Mère.Date.de.naissance) | GS$Mère.Date.de.naissance==""|GS$Mère.Date.de.naissance=="nc" ,"13/12/1913",GS$Mère.Date.de.naissance)
GS$ddn_mater2<-as.Date(GS$ddn_mater2,format="%d/%m/%Y")
table(GS$ddn_mater2,exclude=NULL)

table(GS$Poids.en.gr,exclude=NULL)
GS$Poids.en.gr<-as.character(GS$Poids.en.gr)
table(GS$Poids.en.gr,exclude=NULL)
GS$Poids.en.gr<-gsub(" ","",GS$Poids.en.gr)
GS$poids2<-ifelse(GS$Poids.en.gr=="",40000,GS$Poids.en.gr)

GS$poids2<-as.numeric(GS$poids2)

table(GS$poids2,exclude=NULL)
str(GS$poids2)

str(GS$datenaissance,exclude=NULL)
GS$datenaissance<-as.Date(GS$datenaissance,format="%Y-%m-%d")
table(GS$datenaissance,exclude=NULL)
str(GS$datenaissance,exclude=NULL)
GS<-GS[order(GS$datenaissance,GS$ddn_mater2),]



test_95suite<-merge(GS[,],data_1013,by.x=c("datenaissance","ddn_mater2","poids2"),by.y=c("datenaissance","ddn_mater2","poids2"),suffixes=c(".r",".c") )
test_95suite$Référence.Enfant<-as.character(test_95suite$Référence.Enfant)
str(ft)
dim(test_95suite)
table(test_95suite$datenaissance)
table(test_95suite$datenaissance_mere)
table(test_95suite$Référence.Enfant)
length(unique(test_95suite$Référence.Enfant))
test_95suite<-test_95suite[,order(colnames(test_95suite),decreasing=TRUE)]


dim(test_95)
dim(test_95suite)

test_95$codep<-as.character(test_95$Code.postal)
test_95<-test_95[,order(colnames(test_95),decreasing=TRUE)]
names(test_95)
test_95<-test_95[,-1]
test_95$datenaissance<-as.character(test_95$datenaissance)
test_95$datenaissance<-as.Date(test_95$datenaissance,format="%d/%m/%Y")
test_95$datenaissance_mere<-as.Date(test_95$datenaissance_mere,format="%d/%m/%Y")
test_95$ddn_mater2<-as.Date(test_95$ddn_mater2,format="%d/%m/%Y")


table(test_95$datenaissance)
test_95<-rbind(test_95,test_95suite)

dim(test_95)
table(test_95$ddn_mater2,exclude = NULL) 
length(test_95$Référence.Enfant)
length(unique(test_95$Référence.Enfant))

test_95[duplicated(test_95$Référence.Enfant),] # pas de doublons 

table(test_95$Commune.insee,exclude=NULL)


table(test_95$Commune.insee,exclude=NULL) #!!!! # Y a aucun témoins de Paris

#### vérification du lieu de naissance 

table(test_95$maternite_code,exclude=NULL) # 3 dans le 60, 1 dans le 76, 2 NA a virer 
table(test_95$maternite_code,test_95$maternite_nom,exclude=NULL)
dim(test_95)

test_95<-test_95[!is.na(test_95$maternite_code),]
dim(test_95)
test_95<-test_95[grepl(pattern="^75|^91|^92|^93|^94|^95|^77|^78",test_95$maternite_code),]
dim(test_95)

table(test_95$datenaissance) 


ft<-10-table(test_95$datenaissance) 
ft<-ifelse(ft<0,0,ft)
ft<-rep(names(table(test_95$datenaissance)),ft)
ft

dim(test_95)
date1<-names(table(test_95$datenaissance))[table(test_95$datenaissance) < 10]
date1
ds<-temoins_95[which(!temoins_95$Référence.Enfant %in% test_95$Référence.Enfant ),]

ds<-ds[ds$datenaissance %in% date1,]
set.seed(5555750)
ds<-ds[sample(nrow(ds)),]

GS<-NULL
for(i in unique(ft)){
  tt<-ds[ds$datenaissance==i,]
  gs<-tt[1 :table(ft)[names(table(ft))==i],]
  GS<-rbind(GS,gs)
}

#### !!!! Faudrait pas faire ça mais y a eu gag ###########


GS<-ds[ds$Référence.Enfant %in% c("H_ENFANT1000004650","H_ENFANT1000007396","H_ENFANT1000017685","H_ENFANT110000000859","H_ENFANT110000004910","H_ENFANT110000016667"),]
# je prÃ©fÃ¨re comme Ã§a 
GS$Mère.Date.de.naissance<-as.character(GS$Mère.Date.de.naissance)
table(GS$Mère.Date.de.naissance,exclude=NULL)
GS$ddn_mater2<-ifelse(is.na(GS$Mère.Date.de.naissance) | GS$Mère.Date.de.naissance==""|GS$Mère.Date.de.naissance=="nc" ,"13/12/1913",GS$Mère.Date.de.naissance)
GS$ddn_mater2<-as.Date(GS$ddn_mater2,format="%d/%m/%Y")
table(GS$ddn_mater2,exclude=NULL)

table(GS$Poids.en.gr,exclude=NULL)
GS$Poids.en.gr<-as.character(GS$Poids.en.gr)
table(GS$Poids.en.gr,exclude=NULL)
GS$Poids.en.gr<-gsub(" ","",GS$Poids.en.gr)
GS$poids2<-ifelse(GS$Poids.en.gr=="",40000,GS$Poids.en.gr)

GS$poids2<-as.numeric(GS$poids2)

table(GS$poids2,exclude=NULL)
str(GS$poids2)

str(GS$datenaissance,exclude=NULL)
GS$datenaissance<-as.Date(GS$datenaissance,format="%Y-%m-%d")
table(GS$datenaissance,exclude=NULL)
str(GS$datenaissance,exclude=NULL)
GS<-GS[order(GS$datenaissance,GS$ddn_mater2),]



test_95suite<-merge(GS[,],data_1013,by.x=c("datenaissance","ddn_mater2","poids2"),by.y=c("datenaissance","ddn_mater2","poids2"),suffixes=c(".r",".c") )
test_95suite$Référence.Enfant<-as.character(test_95suite$Référence.Enfant)
str(ft)
dim(test_95suite)
table(test_95suite$datenaissance)
table(test_95suite$Référence.Enfant)
length(test_95suite$Référence.Enfant)
test_95suite$datenaissance<-as.character(test_95suite$datenaissance)
test_95$datenaissance<-as.character(test_95$datenaissance)



dim(test_95)
table(test_95$datenaissance)
test_95<-rbind(test_95[,names(test_95)%in% names(test_95suite)],test_95suite)

dim(test_95)
table(test_95$datenaissance) 
length(test_95$Référence.Enfant) # pas de doublonns 
length(unique(test_95$Référence.Enfant))# pas de doublonns 

test_95[duplicated(test_95$Référence.Enfant),] # pas de doublons

table(test_95$Commune.insee,exclude=NULL) # pas de parisien ni de provinciaux 
table(test_95$maternite_code,exclude=NULL) # que des naissances en idf 

table(test_95$numt %in% fait_95$numt) # pas de cas 

#table(cas_temoins_95_help$numunique %in% fait_95$numt)


####X FIN 



temoins_95helpp[,!names(temoins_95helpp) %in% c("X","cas","tbis")]->test_95
test_95$datenaissance<-as.Date(test_95$datenaissance,format="%Y-%m-%d")
test_95$datenaissance_mere<-as.Date(test_95$datenaissance_mere,format="%Y-%m-%d")

fait_95[!fait_95$MA__DateNaissance==fait_95$datenaissance,]



temoins_95helpp<-test_95
temoins_95helpp$cas<-0
temoins_95helpp$tbis<-FALSE
#temoins_95$help$num_rue<-temoins_95help$Numéro.dans.la.rue

table(temoins_95$num_rue,temoins_95$Numéro.dans.la.rue)












write.csv(temoins_95help,"F:/temoins_95_geocoderv3.csv")

#####

write.csv(temoins_95_geocoderv3[,c("Référence.Enfant","Numéro.dans.la.rue","Ligne4","Localité","Code.postal")],"F:/temoins_95_geocoderv4.csv")


### 440 temoins pour 44 cas 

write.csv(test_95,"F:temoins.95.csv")
