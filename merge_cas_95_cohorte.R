cas_78 <- read.csv("~/Desespoir/Bases/cas_78.csv", sep=";")


cas_78$datenaissance_mere<-as.character(cas_78$datenaissance_mere)
cas_78$ddn_mater2<-ifelse(is.na(cas_78$datenaissance_mere) ,"13/12/1913",cas_78$datenaissance_mere)
cas_78$ddn_mater2<-as.Date(cas_78$ddn_mater2,format="%d/%m/%Y")
table(cas_78$ddn_mater2,exclude=NULL)

table(cas_78$poids,exclude=NULL)
cas_78$poids2<-ifelse(is.na(cas_78$poids),40000,cas_78$poids)
cas_78$poids2<-as.numeric(cas_78$poids2)
table(cas_78$poids2,exclude=NULL)
str(cas_78$poids2)
cas_78$datenaissance<-as.Date(cas_78$datenaissance,format="%d/%m/%Y")
table(cas_78$sexe)
cas_78$sexe<-ifelse(cas_78$sexe=="M",1,2)
table(cas_78$sexe)


cas_78<-cas_78[order(cas_78$datenaissance,cas_78$ddn_mater2),]




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
test_78<-merge(cas_78[,],data_1013,by.x=c("datenaissance","sexe_2","ddn_mater2","poids2"),by.y=c("datenaissance","sexe","ddn_mater2","poids2"),suffixes=c(".r",".c") )
dim(test_78)

a_check<-anti_join(x =cas_78,y=data_1013,by=c("datenaissance"="datenaissance","sexe_2"="sexe","ddn_mater2","poids2") )


test_78_c<-merge(a_check,data_1013[,c("datenaissance","poids2","ddn_mater2","poids2","taille")],by.x=c("datenaissance","poids2"),by.y=c("datenaissance","poids2"),suffixes=c(".r",".c") )

table(test_78_c$datenaissance)

# seul un seul cas est récupérable 

cas_78$datenaissance_mere<-ifelse(cas_78$datenaissance_mere=="28/02/1974","25/02/1974",cas_78$datenaissance_mere)
cas_78$ddn_mater2<-ifelse(is.na(cas_78$datenaissance_mere) ,"13/12/1913",cas_78$datenaissance_mere)
cas_78$ddn_mater2<-as.Date(cas_78$ddn_mater2,format="%d/%m/%Y")
table(cas_78$ddn_mater2,exclude=NULL)


test_78<-merge(cas_78[,c("datenaissance","sexe","ddn_mater2","poids2")],data_1013,by.x=c("datenaissance","sexe","ddn_mater2","poids2"),by.y=c("datenaissance","sexe","ddn_mater2","poids2"),suffixes=c(".r",".c") )
dim(test_78)


cas_78<-test_78
type_cancer$MA__LieuNaiss_INSEE_z<-as.character(type_cancer$MA__LieuNaiss_INSEE_z)
cas_78<-merge(cas_78,type_cancer[type_cancer$MA__LieuNaissDept=="78", c("MA___NumMalade","datenaissance","MA__Sexe","MA__LieuNaiss_INSEE_z")],
              by.x=c("datenaissance","sexe"  ),
              by.y=c("datenaissance","MA__Sexe"))
              
dim(cas_78)


cas_78[cas_78$datenaissance=="2010-01-19" ,c("MA__LieuNaiss_INSEE_z","commune_nais_code","numt")]
### comme prévu y a des gag sur les codes postaux donc faut bien garder les numéros 

cas_78<-cas_78[!(cas_78$numt==7361 & cas_78$MA__LieuNaiss_INSEE_z==78361),]
dim(cas_78)
cas_78<-cas_78[!(cas_78$numt==7628 & cas_78$MA__LieuNaiss_INSEE_z==78158),]
dim(cas_78)


#cas_78[cas_78$datenaissance=="2013-02-27" ,c("MA__LieuNaiss_INSEE_z","commune_nais_code")]

              
"CodePostal"
"CodePostal"
,"commune_nais_code"        
