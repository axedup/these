
voies_paris <- read.csv("G:/CS8/voiesactuellesparis2013-1.csv", encoding="UTF-8", sep=";")
adr<-read.csv2("E:/ddn_paris.csv",header=T,na.strings="")

adr$MA__DateNaissance<-as.Date(as.character(adr$MA__DateNaissance), "%d/%m/%Y")
ddn_paris<-adr$MA__DateNaissance[!is.na(adr$prevoie)]


#data_2010_paris_paris<-data_2010_paris[substr(data_2010_paris$maternite_code,1,2)==75 & !is.na(data_2010_paris$maternite_code),]

essai_2013<-data_2013_paris

#essai_2013<-essai_2013[order(c(as.Date(essai_2013$datenaissance,format="%Y-%m-%d"),essai_2013$sexe,as.Date(essai_2013$datenaissance_mere,format="%Y-%m-%d"))),]

essai_2013<-essai_2013[order(c(as.Date(essai_2013$datenaissance,format="%Y-%m-%d"))),]
essai_2013<-essai_2013[order(essai_2013$sexe),]
essai_2013<-essai_2013[order(c(essai_2013$poids)),]
essai_2013<-essai_2013[order(c(as.Date(essai_2013$datenaissance_mere,format="%Y-%m-%d"))),]

#write.table(essai_2013,file="G:/CS8/base_acompleter_paris.xls")
table(essai_2013$maternite_code)

essai_2013$num_v<-NA
essai_2013$num_v<-as.character(essai_2013$num_v)

essai_2013$voirie<-NA
essai_2013$voirie<-as.character(essai_2013$voirie)

essai_2013$nom_v<-NA
essai_2013$nom_v<-as.character(essai_2013$nom_v)

essai_2013$code_p<-NA
essai_2013$code_p<-as.character(essai_2013$code_p)

essai_2013$prevoie<-NA
essai_2013$prevoie<-as.character(essai_2013$prevoie)


essai_2013$num<-1 : dim(essai_2013)[1]

TAVISS<-data.frame(NULL)

requete<-function(x){

ta<-essai_2013[essai_2013$datenaissance==x,c("num","datenaissance","sexe","poids","datenaissance_mere","num_v","voirie","prevoie","nom_v","code_p")]
te<-fix(ta)
TAC<-rbind(TAVISS,te)
assign("TAVISS", TAC, envir=globalenv())
}


annee<-format(ddn_paris, format = "%Y")
ddn_paris_2013<-ddn_paris[format(ddn_paris, format = "%Y")=="2013"]




ddn<-as.list(ddn_paris_2013[1:1]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv1_2013<-TAVISS
write.table(sauv1_2013,file="E:/temoins_p2013v1_1406.xls")


ddn<-as.list(ddn_paris_2013[2:2]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv2_2013<-TAVISS
write.table(sauv2_2013,file="E:/temoins_p2013v2_1406.xls")

ddn<-as.list(ddn_paris_2013[3:3]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv2b_2013<-TAVISS
write.table(sauv2b_2013,file="E:/temoins_p2013v2b_1406.xls")



ddn<-as.list(ddn_paris_2013[4:4]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv3_2013<-TAVISS
write.table(sauv3_2013,file="E:/temoins_p2013v3_1406.xls")


ddn<-as.list(ddn_paris_2013[5:5]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv4_2013<-TAVISS
write.table(sauv4_2013,file="E:/temoins_p2013v4_1406.xls")



ddn<-as.list(ddn_paris_2013[6:6]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv5_2013<-TAVISS
write.table(sauv5_2013,file="E:/temoins_p2013v5_1406.xls")

ddn<-as.list(ddn_paris_2013[7:7]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv6_2013<-TAVISS
write.table(sauv6_2013,file="E:/temoins_p2013v6_1406.xls")

ddn<-as.list(ddn_paris_2013[8:8]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv7_2013<-TAVISS
write.table(sauv7_2013,file="E:/temoins_p2013v7_1406.xls")



ddn<-as.list(ddn_paris_2013[9:9]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv8_2013<-TAVISS
write.table(sauv8_2013,file="E:/temoins_p2013v8_1406.xls")


ddn<-as.list(ddn_paris_2013[10:10]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv9_2013<-TAVISS
write.table(sauv9_2013,file="E:/temoins_p2013v9_1506.xls")

ddn<-as.list(ddn_paris_2013[11:11]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv10_2013<-TAVISS
write.table(sauv10_2013,file="E:/temoins_p2013v10_1506.xls")

ddn<-as.list(ddn_paris_2013[12:12]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv11_2013<-TAVIS
write.table(sauv11_2013,file="E:/temoins_p2013v11_1506.xls")


ddn<-as.list(ddn_paris_2013[13:13]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv12_2013<-TAVIS
write.table(sauv12_2013,file="E:/temoins_p2013v12_1506.xls")

ddn<-as.list(ddn_paris_2013[14:14]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv13_2013<-TAVIS
write.table(sauv13_2013,file="E:/temoins_p2013v13_1506.xls")


ddn<-as.list(ddn_paris_2013[15:15]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv14_2013<-TAVIS
write.table(sauv14_2013,file="E:/temoins_p2013v14_1506.xls")


ddn<-as.list(ddn_paris_2013[16:16]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv15_2013<-TAVIS
write.table(sauv15_2013,file="E:/temoins_p2013v15_1506.xls")


ddn<-as.list(ddn_paris_2013[17:17]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv16_2013<-TAVIS
write.table(sauv16_2013,file="E:/temoins_p2013v16_1506.xls")

ddn<-as.list(ddn_paris_2013[18:18]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv17_2013<-TAVIS
write.table(sauv17_2013,file="E:/temoins_p2013v17_1506.xls")


ddn<-as.list(ddn_paris_2013[19:19]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv18_2013<-TAVIS
write.table(sauv18_2013,file="E:/temoins_p2013v18_1506.xls")

ddn<-as.list(ddn_paris_2013[20:20]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv19_2013<-TAVIS
write.table(sauv19_2013,file="E:/temoins_p2013v19_1506.xls")

ddn<-as.list(ddn_paris_2013[21:21]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv20_2013<-TAVIS
write.table(sauv20_2013,file="E:/temoins_p2013v20_1506.xls")


ddn<-as.list(ddn_paris_2013[22:22]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv21_2013<-TAVIS
write.table(sauv21_2013,file="E:/temoins_p2013v21_1506.xls")


ddn<-as.list(ddn_paris_2013[23:23]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv22_2013<-TAVIS
write.table(sauv22_2013,file="E:/temoins_p2013v22_1506.xls")





TAVIN<-data.frame(NULL)

requete<-function(x){

ta<-essai_2013[essai_2013$datenaissance==x,c("num","datenaissance","sexe","poids","datenaissance_mere","num_v","voirie","prevoie","nom_v","code_p")]
te<-fix(ta)
TAC<-rbind(TAVIN,te)
assign("TAVIN", TAC, envir=globalenv())
}





ddn<-as.list(ddn_paris_2013[12:12]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv8_2013<-TAVIN
write.table(sauv8_2013,file="E:/temoins_p2013v8_1506.xls")

ddn<-as.list(ddn_paris_2013[13:13]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv9_2013<-TAVIN
write.table(sauv9_2013,file="E:/temoins_p2013v9_1506.xls")

TAVIN<-TAVIN[,1:(ncol(TAVIN)-1)]

ddn<-as.list(ddn_paris_2013[14:14]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv10_2013<-TAVIN
write.table(sauv10_2013,file="E:/temoins_p2013v10_1506.xls")

ddn<-as.list(ddn_paris_2013[15:15]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv11_2013<-TAVIN
write.table(sauv11_2013,file="E:/temoins_p2013v11_1506.xls")

ddn<-as.list(ddn_paris_2013[16:16]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv12_2013<-TAVIN
write.table(sauv12_2013,file="E:/temoins_p2013v12_1506.xls")

ddn<-as.list(ddn_paris_2013[17:17]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv13_2013<-TAVIN
write.table(sauv13_2013,file="E:/temoins_p2013v13_1506.xls")

ddn<-as.list(ddn_paris_2013[18:18]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv14_2013<-TAVIN
write.table(sauv14_2013,file="E:/temoins_p2013v14_1506.xls")

ddn<-as.list(ddn_paris_2013[19:19]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv15_2013<-TAVIN
write.table(sauv15_2013,file="E:/temoins_p2013v15_1506.xls")


ddn<-as.list(ddn_paris_2013[20:20]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv16_2013<-TAVIN
write.table(sauv16_2013,file="E:/temoins_p2013v16_1506.xls")

ddn<-as.list(ddn_paris_2013[21:21]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv17_2013<-TAVIN
write.table(sauv17_2013,file="E:/temoins_p2013v17_1506.xls")

ddn<-as.list(ddn_paris_2013[22:22]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv18_2013<-TAVIN
write.table(sauv18_2013,file="E:/temoins_p2013v18_1506.xls")

ddn<-as.list(ddn_paris_2013[23:23]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv19_2013<-TAVIN
write.table(sauv19_2013,file="E:/temoins_p2013v19_1506.xls")


ddn<-as.list(ddn_paris_2013[24:24]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv20_2013<-TAVIN
write.table(sauv20_2013,file="E:/temoins_p2013v20_1506.xls")


ddn<-as.list(ddn_paris_2013[25:25]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv21_2013<-TAVIN
write.table(sauv21_2013,file="E:/temoins_p2013v21_1506.xls")


ddn<-as.list(ddn_paris_2013[26:26]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv22_2013<-TAVIN
write.table(sauv22_2013,file="E:/temoins_p2013v22_1506.xls")

ddn<-as.list(ddn_paris_2013[27:27]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv23_2013<-TAVIN
write.table(sauv23_2013,file="E:/temoins_p2013v23_1506.xls")



ddn<-as.list(ddn_paris_2013[28:28]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv24_2013<-TAVIN
write.table(sauv24_2013,file="E:/temoins_p2013v24_1506.xls")










TAVIN<-TAVIN[,1:(ncol(TAVIN)-1)]



ddn<-as.list(ddn_paris_2013[6:10]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv3_2013<-TAVI
write.table(sauv3,file="E:/temoins_p_1306.xls")


ddn<-as.list(ddn_paris_2010[11:12]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv4_2013<-TAVI
write.table(sauv4,file="E:/temoins_p_1306v2.xls")

ddn<-as.list(ddn_paris_2010[13:15]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv5_2013<-TAVI
write.table(sauv5,file="E:/temoins_p_1306suitev3.xls")


ddn<-as.list(ddn_paris_2010[16:18]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv6_2013<-TAVI
write.table(sauv6,file="E:/temoins_p_1306suitev4.xls")


ddn<-as.list(ddn_paris_2010[19:20]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv7_2013<-TAVI
write.table(sauv7,file="E:/temoins_p_1306suitev5.xls")

ddn<-as.list(ddn_paris_2010[21:22]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv8_2013<-TAVI
write.table(sauv8,file="E:/temoins_p_1306suitev6.xls")

ddn<-as.list(ddn_paris_2010[23:24]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv9_2013<-TAVI
write.table(sauv9,file="E:/temoins_p_1306suitev7.xls")

ddn<-as.list(ddn_paris_2010[25:26]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv10_2013<-TAVI
write.table(sauv10,file="E:/temoins_p_1306suitev8.xls")

ddn<-as.list(ddn_paris_2010[27:27]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv11_2013<-TAVI
write.table(sauv11,file="E:/temoins_p_1306suitev9.xls")

ddn<-as.list(ddn_paris_2010[28:28]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv12_2013<-TAVI
write.table(sauv12,file="E:/temoins_p_1306suitev10.xls")


ddn<-as.list(ddn_paris_2010[29:30]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv13_2013<-TAVI
write.table(sauv13,file="E:/temoins_p_1406suitev11.xls")

ddn<-as.list(ddn_paris_2010[31:32]) ### sauvegarder à chaque fois !!!!
lapply(ddn,requete)

sauv14_2013<-TAVI
write.table(sauv14,file="E:/temoins_p_1406suitev12 .xls")









###


essai_2013<-merge(essai_2013,TA,by="num",all.x=T,suffixes = c("",".def"))
essai_2013$adresse<-paste(essai_2013$num_v.def,essai_2013$voirie.def,essai_2013$prevoie.def,essai_2013$nom_v.def,sep="")

# controle avenue,rues###



####



esai<-essai_2013[!is.na(essai_2013$nom_v.def),]
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

#return(essai_2013t)
#assign("essai_2013_2013", essai_2013_2013t, envir=globalenv())





