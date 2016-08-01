
voies_paris <- read.csv("G:/CS8/voiesactuellesparis2012-1.csv", encoding="UTF-8", sep=";")
adr<-read.csv2("E:/ddn_paris.csv",header=T,na.strings="")

adr$MA__DateNaissance<-as.Date(as.character(adr$MA__DateNaissance), "%d/%m/%Y")
ddn_paris<-adr$MA__DateNaissance[!is.na(adr$prevoie)]


#data_2010_paris_paris<-data_2010_paris[substr(data_2010_paris$maternite_code,1,2)==75 & !is.na(data_2010_paris$maternite_code),]

essai<-data_2010_paris

#essai<-essai[order(c(as.Date(essai$datenaissance,format="%Y-%m-%d"),essai$sexe,as.Date(essai$datenaissance_mere,format="%Y-%m-%d"))),]

essai<-essai[order(c(as.Date(essai$datenaissance,format="%Y-%m-%d"))),]
essai<-essai[order(essai$sexe),]
essai<-essai[order(c(essai$poids)),]
essai<-essai[order(c(as.Date(essai$datenaissance_mere,format="%Y-%m-%d"))),]

#write.table(essai,file="G:/CS8/base_acompleter_paris.xls")
table(essai$maternite_code)

essai$num_v<-NA
essai$num_v<-as.character(essai$num_v)

essai$voirie<-NA
essai$voirie<-as.character(essai$voirie)

essai$nom_v<-NA
essai$nom_v<-as.character(essai$nom_v)

essai$code_p<-NA
essai$code_p<-as.character(essai$code_p)

essai$prevoie<-NA
essai$prevoie<-as.character(essai$prevoie)


essai$num<-1 : dim(essai)[1]

TAV<-data.frame(NULL)

requete<-function(x){

ta<-essai[essai$datenaissance==x,c("num","datenaissance","sexe","poids","datenaissance_mere","num_v","voirie","prevoie","nom_v","code_p")]
te<-fix(ta)
TAC<-rbind(TAV,te)
assign("TAV", TAC, envir=globalenv())
}


annee<-format(ddn_paris, format = "%Y")
ddn_paris_2010<-ddn_paris[format(ddn_paris, format = "%Y")=="2010"]

ddn<-as.list(ddn_paris_2010[1:5]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv1<-TA
sauv2<-TA


ddn<-as.list(ddn_paris_2010[6:10]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv3<-TA
write.table(sauv3,file="E:/temoins_p_1306.xls")


ddn<-as.list(ddn_paris_2010[11:12]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv4<-TA
write.table(sauv4,file="E:/temoins_p_1306v2.xls")

ddn<-as.list(ddn_paris_2010[13:15]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv5<-TAV
write.table(sauv5,file="E:/temoins_p_1306suitev3.xls")


ddn<-as.list(ddn_paris_2010[16:18]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv6<-TAV
write.table(sauv6,file="E:/temoins_p_1306suitev4.xls")


ddn<-as.list(ddn_paris_2010[19:20]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv7<-TAV
write.table(sauv7,file="E:/temoins_p_1306suitev5.xls")

ddn<-as.list(ddn_paris_2010[21:22]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv8<-TAV
write.table(sauv8,file="E:/temoins_p_1306suitev6.xls")

ddn<-as.list(ddn_paris_2010[23:24]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv9<-TAV
write.table(sauv9,file="E:/temoins_p_1306suitev7.xls")

ddn<-as.list(ddn_paris_2010[25:26]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv10<-TAV
write.table(sauv10,file="E:/temoins_p_1306suitev8.xls")

ddn<-as.list(ddn_paris_2010[27:27]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv11<-TAV
write.table(sauv11,file="E:/temoins_p_1306suitev9.xls")

ddn<-as.list(ddn_paris_2010[28:28]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv12<-TAV
write.table(sauv12,file="E:/temoins_p_1306suitev10.xls")


ddn<-as.list(ddn_paris_2010[29:30]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv13<-TAV
write.table(sauv13,file="E:/temoins_p_1406suitev11.xls")

ddn<-as.list(ddn_paris_2010[31:32]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv14<-TAV
write.table(sauv14,file="E:/temoins_p_1406suitev12 .xls")









###


essai<-merge(essai,TA,by="num",all.x=T,suffixes = c("",".def"))
essai$adresse<-paste(essai$num_v.def,essai$voirie.def,essai$prevoie.def,essai$nom_v.def,sep="")

# controle avenue,rues###



####



esai<-essai[!is.na(essai$nom_v.def),]
erreur<-which(!esai$nom_v.def %in% voies_paris$nomvoie)


RUES<-NULL
for(i in erreur){

w<-agrep(esai$nom_v.def[i], voies_paris$nomvoie, max.distance = 0.1, costs = NULL,
         ignore.case = TRUE, fixed = TRUE, useBytes = FALSE)
rues<-c(esai$nom_v.def[i],as.character(voies_paris$nomvoie[as.numeric(as.character(w))]))
rues<-c(rues,rep("",10-length(rues)))
RUES<-rbind(RUES,rues)
}





#match(esai$nom_v.def,voies_paris$nomvoie)

#return(essait)
#assign("essai", essait, envir=globalenv())





