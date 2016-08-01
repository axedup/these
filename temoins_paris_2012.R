temoins_p1_2012_na <- read.csv2("C:/Users/Louise/Documents/Desespoir/Bases/temoins_p2012v22_1506_v2.csv",stringsAsFactors=FALSE)
### le gag 
temoins_p1_2012_na<-temoins_p1_2012_na[!is.na(temoins_p1_2012_na$voirie),]


table(temoins_p1_2012_na$datenaissance)
table(temoins_p1_2012_na$datenaissance,is.na(temoins_p1_2012_na$datenaissance_mere))

names(temoins_p1_2012_na)

# fichier non anonymisÃ© _na donc je vire les noms
temoins_p1_2012<- temoins_p1_2012_na[,c( "num", "datenaissance", "sexe", "poids", "datenaissance_mere", 
"num_v", "voirie", "prevoie", "nom_v", "code_p")]
dim(temoins_p1_2012)
head(temoins_p1_2012)

temoins_2012fr<-temoins_p1_2012
temoins_2012fr<-temoins_p1_2012[!is.na(temoins_p1_2012$voirie),]
names(temoins_2012fr)
dim(temoins_2012fr)
table(temoins_2012fr$datenaissance)
table(temoins_2012fr$datenaissance,is.na(temoins_2012fr$datenaissance_mere))

essai_m<-merge(essai_2012,temoins_2012fr,by="num")
essai_m[!essai_m$poids.x==essai_m$poids.y,]
#essai_m$datenaissance.y<-as.Date(as.character(essai_m$datenaissance.y), "%d/%m/%Y")
#essai_m$datenaissance_mere.y<-as.Date(as.character(essai_m$datenaissance_mere.y), "%d/%m/%Y")
dim(essai_m)
#essai_m[!essai_m$datenaissance.x==essai_m$datenaissance.y,]
#essai_m[!essai_m$datenaissance_mere.x==essai_m$datenaissance_mere.y,]

table(essai_m$maternite_code,exclude=NULL) # 1 a virer
essai_m<-essai_m[substr(essai_m$maternite_code,1,2) %in% c(75,91,92,93,94,95,77,78),]
table(essai_m$maternite_code,exclude=NULL)
table(essai_m$maternite_code,essai_m$datenaissance.x,exclude=NULL)
table(essai_m$datenaissance.y,is.na(essai_m$datenaissance_mere.y))
table(essai_m$datenaissance.x)


str(temoins_2012fr$num)
temoins_2012help<-merge(essai_2012[,!names(essai_2012) %in% c("datenaissance_mere","num_v","voirie", "nom_v", "code_p", "prevoie")],temoins_2012fr[,c( "num","num_v","voirie", "prevoie", "nom_v", "code_p","datenaissance_mere")],by="num")
dim(temoins_2012help)
dim(temoins_p1_2012)
temoins_2012help<-temoins_2012help[substr(temoins_2012help$maternite_code,1,2) %in% c(75,91,92,93,94,95,77,78),]

table(temoins_2012help$maternite_code)
table(temoins_2012help$num)
table(temoins_2012help$datenaissance)
table(temoins_2012help$datenaissance,!is.na(temoins_2012$datenaissance_mere))
length(unique(temoins_2012help$num)) ### y  a les doublons identifiés par Fabienne et corrigés mais pas virés
dim(temoins_2012help) #242 +10  témoins  nés en idf résident à paris


temoins_2012help<-temoins_2012help[!duplicated(temoins_2012help$num),]
dim(temoins_2012help) 
length(unique(temoins_2012help$num)) # on a viré les doublons 
table(temoins_2012help$datenaissance)



temoins_2012help$cas<-0
temoins_2012help$tbis<-grepl(pattern="^b",temoins_2012help$num_v)
temoins_2012help$num_rue<-gsub(pattern="^b*([0-9].*)",replace="\\1",temoins_2012help$num_v)
temoins_2012help$numunique<-paste0("2012-",temoins_2012help$num)

table(temoins_2012help$num_rue,temoins_2012help$num_v)

write.csv(temoins_2012,"F:/temoins_2012.csv")
write.csv(temoins_2012[,c("numunique","datenaissance_mere","sexe","poids","taille","datenaissance","num_rue","voirie","prevoie","nom_v")],"F:/temoins_2012_geocoder.csv")



