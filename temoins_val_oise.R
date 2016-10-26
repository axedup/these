library(gdata)

temoins_951_2<-read.csv2("F:/temoins951_2.csv",header=TRUE) # c'est le fichier temoins.95.xls qu'il faut séparer en 2 fichiers csv car deux onglets 
head(temoins_951_2)

temoins_952_2<-read.csv2("F:/temoins952_2.csv",header=TRUE)
head(temoins_952_2)

temoins_95<-rbind(temoins_951_2,temoins_952_2)
names(temoins_95)
table(temoins_95$Ligne4)
dim(temoins_95)
temoins_95<-temoins_95[!temoins_95$Ligne4=="" ,] # on prend les témoins avec adresses
temoins_95<-temoins_95[!temoins_95$Ligne4==" " ,]
dim(temoins_95)
head(table(temoins_95$Ligne4))

temoins_95$codep<-as.character(temoins_95$Code.postal)
temoins_95<-temoins_95[nchar(temoins_95$codep)=="5",]
dim(temoins_95)

dim(temoins_95[grepl(pattern="^75|^91|^92|^93|^94|^95|^77|^78",temoins_95$codep),]) # on prend les témoins résidant en idf 
temoins_95<-temoins_95[grepl(pattern="^75|^91|^92|^93|^94|^95|^77|^78",temoins_95$codep),]
dim(temoins_95)

temoins_95$datenaissance<-as.character(temoins_95$Enfant.Date.de.naissance)
table(temoins_95$datenaissance)
temoins_95$datenaissance<-as.Date(temoins_95$datenaissance,format="%d/%m/%Y")
temoins_95$datenaissance<-as.character(temoins_95$datenaissance)

table(temoins_95$datenaissance)
str(temoins_95$datenaissance)

temoins_95<-temoins_95[order(temoins_95$datenaissance),]
temoins_95$comp<-unlist(lapply(table(temoins_95$datenaissance),function(x){1:x}))


### base témoins_95#######
fait_95$datenaissance # attention 2 cas ont la même date de naissance


tsort<-function(x){
set.seed(123)
sequ<-sample(seq(from =1, to =x, by = 1), size = 10, replace = FALSE)
}

lista<-lapply(table(temoins_95$datenaissance),tsort)


tsorta<-function(x){
ba<-temoins_95[temoins_95$datenaissance==x,]
ba<-ba[ba$comp %in% lista[[x]],]
}

listu<-lapply(unique(temoins_95$datenaissance),tsorta)
temoins_95_ts<-do.call(rbind,listu)


temoins_95_ts$datenaissance<-as.character(temoins_95_ts$datenaissance)
temoins_95_ts$datenaissance<-as.Date(temoins_95_ts$datenaissance,format="%Y-%m-%d")
str(temoins_95_ts$datenaissance)

str(unique(fait_95$datenaissance))
temoins_95t<-temoins_95_ts[temoins_95_ts$datenaissance %in% fait_95$datenaissance,] ### ouf c'est bon c'est la bonne date (erreur précédente entre 2010 et 2012)
dim(temoins_95t)


table(temoins_95t$datenaissance)
unique(temoins_95t$datenaissance)


vu<-temoins_95t$comp[temoins_95t$datenaissance=="2010-02-08"]
vu<-vu+1
dbl<-temoins_95[temoins_95$datenaissance=="2010-02-08" & temoins_95$comp %in% vu , ]

temoins_95t<-rbind(temoins_95t,dbl)
dim(temoins_95t)


### on merge le secours### 


temoins_95$datenaissance<-as.character(temoins_95$datenaissance)
temoins_95$datenaissance<-as.Date(temoins_95$datenaissance,format="%Y-%m-%d")
table(temoins_95$datenaissance)
str(temoins_95$datenaissance)



temoins_ds<-temoins_95[!temoins_95$Référence.Enfant %in% temoins_95t$Référence.Enfant & temoins_95$datenaissance %in% ds$datenaissance,]
dim(temoins_ds)




temoins_ds$Mère.Date.de.naissance<-as.character(temoins_ds$Mère.Date.de.naissance)
table(temoins_ds$Mère.Date.de.naissance,exclude=NULL)
temoins_ds$ddn_mater2<-ifelse(is.na(temoins_ds$Mère.Date.de.naissance) | temoins_ds$Mère.Date.de.naissance==""|temoins_ds$Mère.Date.de.naissance=="nc" ,"13/12/1913",temoins_ds$Mère.Date.de.naissance)
temoins_ds$ddn_mater2<-as.Date(temoins_ds$ddn_mater2,format="%d/%m/%Y")
table(temoins_ds$ddn_mater2,exclude=NULL)

table(temoins_ds$Poids.en.gr,exclude=NULL)
temoins_ds$Poids.en.gr<-as.character(temoins_ds$Poids.en.gr)
table(temoins_ds$Poids.en.gr,exclude=NULL)

temoins_ds$poids2<-ifelse(temoins_ds$Poids.en.gr=="",40000,temoins_ds$Poids.en.gr)
temoins_ds$poids2<-gsub(" ","",temoins_ds$poids2)
temoins_ds$poids2<-as.numeric(temoins_ds$poids2)

table(temoins_ds$poids2,exclude=NULL)
str(temoins_ds$poids2)
#temoins_ds$MA__DateNaissance<-as.Date(temoins_ds$MA__DateNaissance,format="%d/%m/%Y")
table(temoins_ds$datenaissance,exclude=NULL)
str(temoins_ds$datenaissance,exclude=NULL)
temoins_ds<-temoins_ds[order(temoins_ds$datenaissance,temoins_ds$ddn_mater2),]



test_95_ds<-merge(temoins_ds[,],data_1013,by.x=c("datenaissance","ddn_mater2","poids2"),by.y=c("datenaissance","ddn_mater2","poids2"),suffixes=c(".r",".c") )
test_95_ds<-test_95_ds[,order(colnames(test_95_ds),decreasing=TRUE)]

set.seed(456)
test_95_ds<-test_95_ds[sample(nrow(test_95_ds)),]
secours_1<-test_95_ds[match(ds$datenaissance,test_95_ds$datenaissance),]

date_2<-secours_1$datenaissance[duplicated(secours_1)]
secours_1<-secours_1[!duplicated(secours_1),]
patients_1<-secours_1$Référence.Enfant


secours_2<-test_95_ds[!test_95_ds$Référence.Enfant %in% patients_1,]
secours_2<-secours_2[match(date_2,test_95_ds$datenaissance),]
