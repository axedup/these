temoins_p1_2013_na <- read.csv2("F:/temoins_p2013v10_1506.csv", stringsAsFactors=FALSE)
temoins_p1_2013_na<-temoins_p1_2013_na[!is.na(temoins_p1_2013_na$voirie),]


names(temoins_p1_2013_na)

# fichier non anonymisÃ© _na donc je vire les noms
temoins_p1_2013<- temoins_p1_2013_na[,c( "num", "datenaissance", "sexe", "poids", "datenaissance_mere", 
"num_v", "voirie", "prevoie", "nom_v", "code_p")]


temoins_2013<-temoins_p1_2013
temoins_2013<-temoins_p1_2013[!is.na(temoins_p1_2013$voirie),]
names(temoins_2013)
table(temoins_2013$datenaissance)
table(temoins_2013$datenaissance,is.na(temoins_2013$datenaissance_mere))




essai_m<-merge(essai_2013,temoins_2013,by="num")
essai_m[!essai_m$poids.x==essai_m$poids.y,]
#essai_m$datenaissance.y<-as.Date(as.character(essai_m$datenaissance.y), "%d/%m/%Y")
#essai_m$datenaissance_mere.y<-as.Date(as.character(essai_m$datenaissance_mere.y), "%d/%m/%Y")

#essai_m[!essai_m$datenaissance.x==essai_m$datenaissance.y,]
#essai_m[!essai_m$datenaissance_mere.x==essai_m$datenaissance_mere.y,]

table(essai_m$maternite_code,exclude=NULL)
essai_m<-essai_m[substr(essai_m$maternite_code,1,2) %in% c(75,91,92,93,94,95,77,78),]
table(essai_m$maternite_code,exclude=NULL)
table(essai_m$maternite_code,essai_m$datenaissance.x,exclude=NULL)
table(essai_m$datenaissance.y,is.na(essai_m$datenaissance_mere.y))
table(essai_m$datenaissance.x)

str(essai_2013$num)
str(temoins_2013$num)
temoins_2013<-merge(essai_2013[,!names(essai_2013) %in% c("datenaissance_mere","num_v","voirie", "nom_v", "code_p", "prevoie")],temoins_2013[,c( "num","num_v","voirie", "prevoie", "nom_v", "code_p","datenaissance_mere")],by="num")


dim(temoins_2013)
dim(temoins_p1_2013)

temoins_2013<-temoins_2013[substr(temoins_2013$maternite_code,1,2) %in% c(75,91,92,93,94,95,77,78),]


table(temoins_2013$num)
table(temoins_2013$datenaissance)
table(temoins_2013$datenaissance,!is.na(temoins_2013$datenaissance_mere))
table(temoins_2013$maternite_code)
dim(temoins_2013)# 113 témoins nés en idf et résident à Paris




temoins_2013$cas<-0
temoins_2013$tbis<-grepl(pattern="^b",temoins_2013$num_v)
temoins_2013$num_rue<-gsub(pattern="^b*([0-9].*)",replace="\\1",temoins_2013$num_v)

temoins_2013$numunique<-paste0("2013-",temoins_2013$num)
table(temoins_2013$num_rue,temoins_2013$num_v)

write.csv(temoins_2013,"F:/temoins_2013.csv")
write.csv(temoins_2013[,c("numunique","datenaissance_mere","sexe","poids","taille","datenaissance","num_rue","voirie","prevoie","nom_v")],"F:/temoins_2013_geocoder.csv")

