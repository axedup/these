temoins_p1_2011_na  <- read.csv2("C:/Users/Louise/Documents/Desespoir/Bases/temoins_p2011v7_1506.csv", stringsAsFactors=FALSE,na.strings = "NA")
temoins_p2_2011_na  <- read.csv2("C:/Users/Louise/Documents/Desespoir/Bases/temoins_p2011v24_1506.csv", stringsAsFactors=FALSE,na.strings = "NA")
temoins_p3_2011_na  <- read.csv2("F:/temoins_p2011v25_1506.csv", stringsAsFactors=FALSE,na.strings = "NA")
  
C:/Users/Louise/Documents/Desespoir/Bases

#read.csv("E:/temoins_p2011v24_1506.xls", sep="", stringsAsFactors=FALSE)


names(temoins_p1_2011_na)
names(temoins_p2_2011_na)

# fichier non anonymisÃ© _na donc je vire les noms
temoins_p1_2011<- temoins_p1_2011_na[,c("X", "num", "datenaissance", "sexe", "poids", "datenaissance_mere", 
"num_v", "voirie", "prevoie", "nom_v", "code_p")]
temoins_p2_2011<- temoins_p2_2011_na[,c("X", "num", "datenaissance", "sexe", "poids", "datenaissance_mere", 
"num_v", "voirie", "prevoie", "nom_v", "code_p")]
temoins_p3_2011<- temoins_p3_2011_na[,c("X", "num", "datenaissance", "sexe", "poids", "datenaissance_mere", 
                                        "num_v", "voirie", "prevoie", "nom_v", "code_p")]
head(temoins_p1_2011)
head(temoins_p2_2011)
head(temoins_p3_2011)
names(temoins_p2_2011)
names(temoins_p1_2011)
table(temoins_p1_2011$datenaissance)
str(temoins_p1_2011$datenaissance)
table(temoins_p2_2011$datenaissance)
table(temoins_p3_2011$datenaissance)
temoins_p1_2011$datenaissance<-as.Date(as.character(temoins_p1_2011$datenaissance), "%d/%m/%Y")
temoins_p2_2011$datenaissance<-as.Date(as.character(temoins_p2_2011$datenaissance), "%d/%m/%Y")
temoins_p3_2011$datenaissance<-as.Date(as.character(temoins_p3_2011$datenaissance), "%d/%m/%Y")
table(temoins_p1_2011$datenaissance)
table(temoins_p2_2011$datenaissance)
table(temoins_p3_2011$datenaissance)
str(temoins_p2_2011$datenaissance)




temoins_2011_y<-rbind(temoins_p1_2011,temoins_p2_2011,temoins_p3_2011)
temoins_2011_y<-temoins_2011_y[!is.na(temoins_2011_y$voirie),]


names(temoins_2011)
dim(temoins_2011)

head(temoins_2011)
table(temoins_2011$datenaissance)

table(temoins_2011$datenaissance,is.na(temoins_2011$datenaissance_mere))

essai_m<-merge(essai_2011,temoins_2011_y,by="num")
essai_m[!essai_m$poids.x==essai_m$poids.y,]
#essai_m$datenaissance.y<-as.Date(as.character(essai_m$datenaissance.y), "%d/%m/%Y")
#essai_m$datenaissance_mere.y<-as.Date(as.character(essai_m$datenaissance_mere.y), "%d/%m/%Y")

#essai_m[!essai_m$datenaissance.x==essai_m$datenaissance.y,]
#essai_m[!essai_m$datenaissance_mere.x==essai_m$datenaissance_mere.y,]

table(essai_m$maternite_code,exclude=NULL) # 3 a virer
essai_m<-essai_m[substr(essai_m$maternite_code,1,2) %in% c(75,91,92,93,94,95,77,78),]
table(essai_m$maternite_code,exclude=NULL)
table(essai_m$maternite_code,essai_m$datenaissance.x,exclude=NULL)
table(essai_m$datenaissance.y,is.na(essai_m$datenaissance_mere.y))
table(essai_m$datenaissance.x)

temoins_2011<-merge(essai_2011[,!names(essai_2011) %in% c("datenaissance_mere","num_v","voirie", "nom_v", "code_p", "prevoie")],temoins_2011[,c( "num","num_v","voirie", "prevoie", "nom_v", "code_p","datenaissance_mere")],by="num")
temoins_2011<-temoins_2011[substr(temoins_2011$maternite_code,1,2) %in% c(75,91,92,93,94,95,77,78),]

dim(temoins_2011)
dim(essai_m)
table(temoins_2011$num)
table(temoins_2011$datenaissance)
table(temoins_2011$datenaissance,!is.na(temoins_2011$datenaissance_mere))
table(temoins_2011$maternite_code,exclude=NULL)





temoins_2011$cas<-0
temoins_2011$tbis<-grepl(pattern="^b",temoins_2011$num_v)
temoins_2011$num_rue<-gsub(pattern="^b*([0-9].*)",replace="\\1",temoins_2011$num_v)
temoins_2011$numunique<-paste0("2011-",temoins_2011$num)

write.csv(temoins_2011,"F:/temoins_2011.csv")
write.csv(temoins_2011[,c("numunique","datenaissance_mere","sexe","poids","taille","datenaissance","num_rue","voirie","prevoie","nom_v")],"F:/temoins_2011_geocoder.csv")


### pourquoi ç afusionne d fois ? 
library(dplyr)
temoins_2011[temoins_2011$num==28232,]
temoins_2011[temoins_2011$num==27978,]
temoins_2011[temoins_2011$num==16341,]
temoins_2011_2<-temoins_2011 %>%
    group_by(num) %>%
 	 mutate(ni=n())

temoins_2011_2[temoins_2011_2$ni==2,c("num","datenaissance")]
