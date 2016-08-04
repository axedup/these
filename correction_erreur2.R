### correction à cause de ma faute ###


table(test_95$maternite_code,exclude=NULL) # 3 dans le 60, 1 dans le 76, 2 NA a virer 
table(test_95$maternite_code,test_95$maternite_nom,exclude=NULL)
dim(test_95)

test_95<-test_95[is.na(test_95$age_deces_hh),]
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
set.seed(5555751)
ds<-ds[sample(nrow(ds)),]
set.seed(5555751)
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
# table(test_95suite$datenaissance)
# table(test_95suite$Référence.Enfant)
# length(test_95suite$Référence.Enfant)
# test_95suite$datenaissance<-as.character(test_95suite$datenaissance)
# test_95$datenaissance<-as.character(test_95$datenaissance)



# dim(test_95)
# table(test_95$datenaissance)
# test_95<-rbind(test_95[,names(test_95)%in% names(test_95suite)],test_95suite)

dim(test_95)
table(test_95suite$datenaissance) 
length(test_95$Référence.Enfant) # pas de doublonns 
length(unique(test_95$Référence.Enfant))# pas de doublonns 

test_95[duplicated(test_95$Référence.Enfant),] # pas de doublons

table(test_95suite$Commune.insee,exclude=NULL) # pas de parisien ni de provinciaux 
table(test_95suite$maternite_code,exclude=NULL) # que des naissances en idf 

table(test_95suite$numt %in% fait_95$numt)

erreur_moi<-test_95suite
erreur_moi$cas<-0
erreur_moi$tbis<-FALSE

erreur_moi<-erreur_moi[,order(colnames(erreur_moi),decreasing = TRUE)]
erreur_moi<-erreur_moi[,!names(erreur_moi) %in% c("X", "Taille.en.cm", "poids2", "Poids.en.gr", 
                                                                    "Mère.Date.de.naissance",  
                                                                    "Ligne2", "Extension.numéro", "Enfant.Date.de.naissance", 
                                                                    "ddn_mater2", "comp",  "Code.postal","Apgar.à.5.mn","Apgar.à.1.mn","Commune.insee"  )]
dim(erreur_moi)


names(erreur_moi)[!names(erreur_moi) %in% names(cas_temoins_paris)]
names(cas_temoins_paris)[!names(cas_temoins_paris) %in% names(erreur_moi)]

names(erreur_moi)<-c("urgence", "typetransfert", "trisomie", "transfert", "testauditif", 
                     "tbis", "taille", "tabac", "spinabifida", "Source", "sexe", "rupturemembrane", 
                           "retardcroissance", "resultaudit", "resultatrecherche", "numunique", 
                           "reductionmembre", "rechaghbs", "rangnaissance", "rachieanesthesie", 
                           "presentation", "prepaccouchement", "premconsult", "preeclampsie", 
                           "pprofession", "polymalformation", "poids", "perimetre", "pathologieg", 
                           "parite", "pactivite", "oxygenotherapie", "omphalocele", "num",
                           "num_rue", "niveauetudes", "neurologique", "nbnesmoins2500", 
                           "nbnesavant37", "nbmortne", "nbjourhopital", "nbfoetus", "nbenfant", 
                           "nbecho", "nbdecedeav28j", "naissancepar", "mprofession", "motifcesariennem", 
                           "motifcesariennef", "mois_naiss_m", "mois_acc", "menacepremature", 
                           "maternite_nom", "maternite_code", "malformrenale", "malformcard", 
                           "mactivite", "commune", "nom_v", "lieutransfert_nom", "lieutransfert_code", 
                           "inutero", "intubation", "infotransfert", "hypertension", "hydrocephalie", 
                           "hta", "hepatitebvacc", "hepatitebinj", "gestite", "gestes_techniques", 
                           "fente", "echomorpho", "diabete", "dep_nais", "dep", "debuttravail", 
                           "ddnMest", "ddnest", "datenaissance_mere", "datenaissance", "commune_nais_code", 
                           "commune_famille_code", "coeffapgar5mn", "coeffapgar1mn", 
                           "code_p", "clartenucale", "cesarienne","cas", "bcg", "autrez", "autre_patho", 
                           "aucune_analgesie", "atresie", "antibiotherapie", "anomalie", 
                           "analgesieperi", "analgesiegene", "analgesieautr", "an_naiss_m", 
                           "an_acc", "allaitement", "alcool", "agegestationnel", "age_deces_jj", 
                           "age_deces_hh" )



dim(erreur_moi)
names(cas_temoins_parish)[!names(cas_temoins_parish) %in% names(erreur_moi)]

erreur_moi$num_v<-NA
erreur_moi$prevoie<-NA
erreur_moi$voirie<-NA

sort(names(erreur_moi))



erreur_moi<-erreur_moi[,order(colnames(erreur_moi))]
erreur_moi[1,]


summary(cas_95_trier)
summary(erreur_moi)

CORR<-as.vector(NULL)
for (i in names(cas_95_trier)){
  corr<-!class(cas_95_trier[,i])==class(erreur_moi[,i])
  CORR<-c(CORR,corr)
}

lapply(cas_95_trier[,CORR],class)  
lapply(erreur_moi[,CORR],class)    


erreur_moi$age_deces_jj <-as.integer(as.character(erreur_moi$age_deces_jj))
erreur_moi$an_acc<-as.integer(as.character(erreur_moi$an_acc))
erreur_moi$an_naiss_m<-as.integer(as.character(erreur_moi$an_naiss_m))
erreur_moi$autre_patho<-as.character(erreur_moi$autre_patho)
erreur_moi$autrez<-as.character(erreur_moi$autrez)
erreur_moi$commune<-as.character(erreur_moi$commune)
erreur_moi$commune_nais_code<-as.character(erreur_moi$commune_nais_code)
erreur_moi$datenaissance<-as.character(erreur_moi$datenaissance)
erreur_moi$datenaissance<-as.Date(erreur_moi$datenaissance,format="%Y-%m-%d")

erreur_moi$datenaissance_mere<-as.character(erreur_moi$datenaissance_mere)
#cas_95_trier$datenaissance_mere<-as.Date(cas_95_trier$datenaissance_mere,format="%d/%m/%Y")
#cas_95_trier$datenaissance_mere<-as.character(cas_95_trier$datenaissance_mere)


erreur_moi$ddnest<-as.character(erreur_moi$ddnest)
#cas_95_trier$ddnest<-as.character(cas_95_trier$ddnest)
erreur_moi$ddnMest<-as.character(erreur_moi$ddnMest)
#cas_95_trier$ddnMest<-as.character(cas_95_trier$ddnMest)

erreur_moi$dep<-as.integer(as.character(erreur_moi$dep))
erreur_moi$dep_nais<-as.character(erreur_moi$dep_nais)
erreur_moi$hydrocephalie<-as.integer(as.character(erreur_moi$hydrocephalie))
erreur_moi$infotransfert<-as.character(erreur_moi$infotransfert)
erreur_moi$lieutransfert_code<-as.character(erreur_moi$lieutransfert_code)
erreur_moi$lieutransfert_nom<-as.character(erreur_moi$lieutransfert_nom)
erreur_moi$malformcard<-as.integer(as.character(erreur_moi$malformcard))
erreur_moi$maternite_code<-as.character(erreur_moi$maternite_code)
erreur_moi$maternite_nom<-as.character(erreur_moi$maternite_nom)
erreur_moi$mois_acc<-as.integer(as.character(erreur_moi$mois_acc))
erreur_moi$mois_naiss_m<-as.integer(as.character(erreur_moi$mois_naiss_m))
erreur_moi$nom_v<-as.character(erreur_moi$nom_v)
erreur_moi$num<-as.character(erreur_moi$num)
erreur_moi$num_rue<-as.character(erreur_moi$num_rue)
erreur_moi$num_v<-as.character(erreur_moi$num_v)

erreur_moi$numunique<-as.character(erreur_moi$numunique)
#cas_95_trier$numunique<-as.character(cas_95_trier$numunique)
erreur_moi$trisomie<-as.integer(as.character(erreur_moi$trisomie))



cas_temoins_95_help2<-rbind(cas_95_trier,temoins_95_trier,erreur_moi)
dim(cas_temoins_95_help2)
tail(cas_temoins_95_help2)  ### le final 
