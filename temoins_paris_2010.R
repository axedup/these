temoins_p1_2010_na <- read.csv2("F:/Temoins_2010.p1.csv", stringsAsFactors=FALSE)
temoins_p2_2010_na  <- read.csv2("F:/temoins_p_1406suitev12.csv", stringsAsFactors=FALSE)


names(temoins_p1_2010_na)
names(temoins_p2_2010_na)
# fichier non anonymisÃ© _na donc je vire les noms
temoins_p1_2010<- temoins_p1_2010_na[,names(temoins_p1_2010_na) %in% c("num","datenaissance","sexe","poids", "datenaissance_mere" ,"num_v", "voirie" ,"prevoie" ,"nom_v", "code_p")]

temoins_p2_2010<-temoins_p2_2010_na[,names(temoins_p2_2010_na) %in% c("num","datenaissance","sexe","poids", "datenaissance_mere" ,"num_v" ,"voirie" ,"prevoie", "nom_v", "code_p")]
head(temoins_p1_2010)
summary(temoins_p1_2010)
head(temoins_p2_2010)
summary(temoins_p2_2010)
# controle 10 tÃ©moins 
table(temoins_p1_2010$datenaissance)
table(temoins_p2_2010$datenaissance)
names(temoins_p1_2010)
names(temoins_p2_2010)

temoins_2010fr<-rbind(temoins_p1_2010,temoins_p2_2010)
temoins_2010fr<-temoins_2010fr[!is.na(temoins_2010fr$voirie),]

names(temoins_2010fr)
dim(temoins_2010fr)
temoins_2010fr
table(temoins_2010fr$datenaissance)
table(temoins_2010fr$datenaissance,is.na(temoins_2010fr$datenaissance_mere))

# essai c'est data_2010_paris
essai_m<-merge(essai,temoins_2010fr,by="num")
dim(essai_m)
essai_m[!essai_m$poids.x==essai_m$poids.y,]
essai_m$datenaissance.y<-as.Date(as.character(essai_m$datenaissance.y), "%d/%m/%Y")
essai_m$datenaissance_mere.y<-as.Date(as.character(essai_m$datenaissance_mere.y), "%d/%m/%Y")

essai_m$datenaissance.x
essai_m$datenaissance.y
essai_m[!essai_m$datenaissance.x==essai_m$datenaissance.y,]
essai_m[!essai_m$datenaissance_mere.x==essai_m$datenaissance_mere.y,]
essai_m<-essai_m[substr(essai_m$maternite_code,1,2) %in% c(75,91,92,93,94,95,77,78),]
dim(essai_m)
table(essai_m$maternite_code,exclude=NULL) # 2 a virer

table(essai_m$maternite_code,exclude=NULL)
table(essai_m$maternite_code,essai_m$datenaissance.x,exclude=NULL)
table(essai_m$datenaissance.y,is.na(essai_m$datenaissance_mere.y))
table(essai_m$datenaissance.y)
str(essai$num)


###??? attention au merge vérifier que c'est bien la même nature de variable on sait jamais ! 

str(temoins_2010fr$num)
temoins_2010help<-merge(essai[ ,!names(essai) %in% c("datenaissance_mere")],temoins_2010fr[,c("datenaissance_mere" ,"num_v","voirie", "prevoie", "nom_v", "code_p","num")],by="num")
temoins_2010help<-temoins_2010help[substr(temoins_2010help$maternite_code,1,2) %in% c(75,91,92,93,94,95,77,78),]

table(essai_m$datenaissance.x)
table(temoins_2010help$datenaissance)
table(temoins_2010help$maternite_code)
table(temoins_2010help$datenaissance_mere)
table(temoins_2010help$datenaissance,is.na(temoins_2010$datenaissance_mere))
table(temoins_2010help$num)
length(unique(temoins_2010help$num))
dim(temoins_2010help)
head(temoins_2010help)

temoins_2010help$cas<-0
temoins_2010help$tbis<-grepl(pattern="^b",temoins_2010help$num_v.y)
temoins_2010help$num_rue<-gsub(pattern="^b*([0-9].*)",replace="\\1",temoins_2010help$num_v.y)
temoins_2010help$numunique<-paste0("2010-",temoins_2010help$num)
table(temoins_2010help$num_rue,temoins_2010help$num_v.y)

write.csv(temoins_2010,"F:/temoins_2010.csv")
write.csv(temoins_2010[,c("numunique","datenaissance_mere","sexe","poids","taille","datenaissance","num_rue","voirie.y","prevoie.y","nom_v.y")],"F:/temoins_2010_geocoder.csv")






















write.csv(temoins_2010,"F:/temoins_2010_ageocod.csv")
write.csv(temoins_2011,"F:/temoins_2011.csv")
write.csv(temoins_2012,"F:/temoins_2012.csv")
write.csv(temoins_2013,"F:/temoins_2013.csv")


table(temoins_2011$datenaissance)

temoins_2010$num_v
