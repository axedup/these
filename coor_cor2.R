#### choix des coordonnées et calcul des expo par la moyennes####


# aucun IRIS attribué et géoloc naze : on balance erreur_iris 
# le surplus 95 : on balance res_geo_95
# p1 et p2 on balance iris-p1_95/paris...


table(res_geo$AFF %in% c("0","1","10","11"),exclude=NULL) #&135 naze
table(res_geo$AFF %in% c("0","1","10","11"), !substr(res_geo$IRIS,6,9)=="XXXX",exclude=NULL)

geocod_faux<-res_geo$id[!res_geo$AFF %in% c("0","1","10","11")]
acher<-geocod_faux[!geocod_faux %in% erreur_geocodage$id]
table(acher %in% erreur_iris$id)

coordonees_cor<-merge(res_geo,erreur_geocodage[,c("id","latitude","longitude")],by="id",all.x=T)
head(coordonees_cor)
coordonees_cor<-merge(coordonees_cor,erreur_iris[,c("id","correction_x","correction_y")],by="id",all.x=T)
coordonees_cor$latitude<-as.numeric(as.character(coordonees_cor$latitude))
coordonees_cor$longitude<-as.numeric(as.character(coordonees_cor$longitude))



### maintenant 
aconvert<-coordonees_cor[,c("id","X","Y")]
coordinates(aconvert)=~ X + Y

proj4string(aconvert)<-CRS("+init=epsg:27572") # zone II étendu 
summary(aconvert)
aconvert <- spTransform(aconvert, CRS("+init=epsg:2154")) # Lambert 93
summary(aconvert)

navteq<-data.frame(cbind(aconvert@coords,aconvert$id))
names(navteq)<-c("lat","lon","id")
navteq<-navteq[!navteq$id %in% geocod_faux,] # sans pb de géocodage 
dim(navteq)
navteq$id<-as.character(navteq$id)


aconvertbis<-coordonees_cor[!is.na(coordonees_cor$latitude),c("id","latitude","longitude")]
coordinates(aconvertbis)=~ longitude + latitude

proj4string(aconvertbis)<-CRS("+init=epsg:4326") # Wgs
summary(aconvertbis)
aconvertbis <- spTransform(aconvertbis, CRS("+init=epsg:2154")) # Lambert 93
summary(aconvertbis)

navteq_s<-data.frame(cbind(aconvertbis@coords,aconvertbis$id))
names(navteq_s)<-c("lat","lon","id")
navteq_s<-navteq_s[navteq_s$id %in% geocod_faux,]
dim(navteq_s)
navteq_s$id<-as.character(navteq_s$id) # pb de géocodage 

navteq_t<-data.frame(cbind(erreur_iris@coords,erreur_iris$id))
names(navteq_t)<-c("lat","lon","id")
navteq_t<-navteq_t[navteq_t$id %in% geocod_faux,]
dim(navteq_t)
navteq_t$id<-as.character(navteq_t$id) # pb de géocoadage corrigé à travers iris 


navteq_q<-data.frame(cbind(res_geo_95_suite@coords,as.character(res_geo_95_suite$Référence.Enfant))) ### attention là c'est numunique 
names(navteq_q)<-c("lat","lon","id")
navteq_q$lat<-as.numeric(as.character(navteq_q$lat))
navteq_q$lon<-as.numeric(as.character(navteq_q$lon))
navteq_q$id<-as.character(navteq_q$id)


### fichier a envoyer a qgis (Lmabert 93 ) - a faire tourner même pour boucle 

 dist_qgis<-rbind(navteq,navteq_s,navteq_t,navteq_q)
dim(dist_qgis)
str(dist_qgis)
length(unique(dist_qgis$id))
write.csv(x = dist_qgis,file="C:/Users/Louise/Documents/Desespoir/Bases/donnes_geo_codees_cor.csv") 


### import de la matrice de distance linéaires euclidienne calculé par qgis : apperment c'est acceptable de prendre ça même si c'est euclidien
matriced3 <- read.csv("~/Desespoir/matriced3.csv")

matriced3b<-matriced3[!duplicated(matriced3),]
dim(matriced3)
head(matriced3)
head(matriced)
matriced3b<-merge(matriced3b,expo,by.x="TargetID",by.y="id_inserm")
dim(matriced3)
head(matriced3)
jux<-table(matriced3$InputID)
table(jux)


ju<-table(matriced3b$InputID)
table(ju)
table(table(matriced3$InputID)==8)

matriced3b<-merge(matriced3b,res_geo[,c("id","MA___NumMalade")],by.x="InputID",by.y="id",all.x=T) # j'ai zappé les 
dim(matriced3b)
head(matriced3b)

matriced3b$numunique<-ifelse(is.na(matriced3b$MA___NumMalade),as.character(matriced3b$InputID),as.character(matriced3b$MA___NumMalade))

expo_moy<-matriced3b %>%
  group_by(numunique) %>%
  summarize(n=n(),
            mopb=weighted.mean( benzene_airp.x,1/Distance),
            mopn=weighted.mean( no2_airp.x,1/Distance)) # c'est l'inverse

length(unique(expo_moy$numunique))

cas_temoins_parisexpop<-merge(cas_temoins_parishbis,expo_moy,by="numunique")
cas_temoins_95_expop<-merge(cas_temoins_95_help,expo_moy,by="numunique")
dim(cas_temoins_95_expop)
dim(cas_temoins_95_help)


### préparation autre méthodes

dist_qgis_spa<-dist_qgis
coordinates(dist_qgis_spa)=~lat+lon
proj4string(dist_qgis_spa)<-CRS("+init=epsg:2154") # lambert 93
dist_qgis_spa <- spTransform(dist_qgis_spa, CRS("+init=epsg:4326")) # conversion en wgs
dist_qgis$lonr<-pi*dist_qgis_spa@coords[,2]/180 
dist_qgis$latr<-pi*dist_qgis_spa@coords[,1]/180 


def_ra <- spTransform(def, CRS("+init=epsg:4326")) # conversion en wgs des coord des points d'expo
def_ra$lonrexpo<-pi*def_ra@coords[,2]/180 
def_ra$latrexpo<-pi*def_ra@coords[,1]/180 



### autre m?thode : formule IGN la boucle fonctionne
matrice<-NULL
for(j in 1:dim(dist_qgis)[1]){
  D<-NULL
 I<-NULL 
for (i in 1:dim(def)[1]){
  lambda<-def_ra$lonrexpo[i]-dist_qgis$lonr[j]
  d<-acos(sin(dist_qgis$latr[j])*sin(def_ra$latrexpo[i])+cos(dist_qgis$latr[j])*cos(def_ra$latrexpo[i])*cos(lambda))
  dm<-d*6378137
  I<-c(I,i)
  D<-c(D,dm)
  }

j<-cbind(I,D)  
  
j<-j[order(D),]
j<-j[1:4,]
matrice<-rbind(matrice,j)
}

points<-def[matrice[,"I"],]
id<-rep(dist_qgis$id,each=4)

matrice_dist<-cbind(id,matrice,points@data)
matrice_dist<-merge(matrice_dist,res_geo[,c("id","MA___NumMalade")],by.x="id",by.y="id",all.x=T)



matrice_dist$numunique<-ifelse(is.na(matrice_dist$MA___NumMalade),as.character(matrice_dist$id),as.character(matrice_dist$MA___NumMalade))



expo_moy_2<-matrice_dist %>%
  group_by(numunique) %>%
  summarize(n=n(),
            mopb=weighted.mean( benzene_airp,1/D),
            mopn=weighted.mean( no2_airp,1/D))

### 3 points sont au même endroit donc D =0 et la moyenne est NUll
dim(expo_moy_2)
summary(expo_moy_2)

length(unique(expo_moy_2$numunique))
distnulle<-expo_moy_2$numunique[is.na(expo_moy_2$mopb)]

matrice_dist[matrice_dist$numunique %in% distnulle & matrice_dist$D==0,]


#####CORRECTION#####
expo_moy_2$mopb[expo_moy_2$numunique=="130988"]<-6.20
expo_moy_2$mopn[expo_moy_2$numunique=="130988"]<-68.00

expo_moy_2$mopb[expo_moy_2$numunique=="H_ENFANT110000012371"]<-1.60
expo_moy_2$mopn[expo_moy_2$numunique=="H_ENFANT110000012371"]<-36.00

expo_moy_2$mopb[expo_moy_2$numunique=="2010-21495"]<-2.57
expo_moy_2$mopn[expo_moy_2$numunique=="2010-21495"]<-48.21

#######

cas_temoins_parisexpop<-merge(cas_temoins_parishbis,expo_moy_2,by="numunique")
cas_temoins_95_expop<-merge(cas_temoins_95_help2,expo_moy_2,by="numunique") # 2 pour le nouveau patients
dim(cas_temoins_95_expop)
dim(cas_temoins_parisexpop)
dim(cas_temoins_95_help2)





# la matrice de résultats surrestime les distances car c'est l'arc de cercle 

join_bn$numunique<-as.character(join_bn[,"res_geo_95_suite@data[, \"Référence.Enfant\"]"])
join2_bn$numunique<-as.character(join2_bn[,"erreur_iris@data[, \"MA___NumMalade\"]"])

c<-bind_rows(join_bn,join2_bn)
c<-bind_rows(c,iris_sp_paris_bn)
c<-bind_rows(c,iris_p1_paris_bn)
c<-bind_rows(c,iris_p2_paris_bn)
c<-bind_rows(c,iris_p3_paris_bn)
c<-bind_rows(c,iris_sp_95_bn)
c<-bind_rows(c,iris_p1_95_bn)
c<-bind_rows(c,iris_p2_95_bn)
c<-bind_rows(c,iris_p3_95_bn)


c<-c[,c("numunique","moyenne_benzene","moyenne_no2","n")]
names(c)<-c("numunique","moyenne_benzene","moyenne_no2","np")
 #data.frame(mapply(c, join_bn,  join2_bn, SIMPLIFY=FALSE))
dim(c)

cas_temoins_parisexpoi<-merge(cas_temoins_parisexpop,c,by="numunique",suffixes=c(".",".i"))
dim(cas_temoins_parisexpoi)
cas_temoins_95expoi<-merge(cas_temoins_95_expop,c,by="numunique",suffixes=c(".",".i"))
dim(cas_temoins_95expoi)
cas_temoins_95expoi<-cas_temoins_95expoi[!duplicated(cas_temoins_95expoi$numunique),]
dim(cas_temoins_95expoi)
#cas_temoins_parisexpoi<-merge(cas_temoins_parisexpoi,join2_bn,by.x="numunique",by.y="erreur_iris@data[, \"MA___NumMalade\"]",all.x=T)
#cas_temoins_parisexpoi$moyenne_benzenei<-ifelse(!is.na(cas_temoins_parisexpoi$moyenne_benzene.y),cas_temoins_parisexpoi$moyenne_benzene.y,cas_temoins_parisexpoi$moyenne_benzene.x) 
#cas_temoins_parisexpoi$moyenne_no2i<-ifelse(!is.na(cas_temoins_parisexpoi$moyenne_no2.y),cas_temoins_parisexpoi$moyenne_no2.y,cas_temoins_parisexpoi$moyenne_no2.x) 
#cas_temoins_parisexpoi<-merge(cas_temoins_parisexpoi,iris_sp_paris_bn,by="numunique",all.x=T,suffixes=c("",".3"))
#cas_temoins_parisexpoi$moyenne_benzenei<-ifelse(!is.na(cas_temoins_parisexpoi$moyenne_benzene.y),cas_temoins_parisexpoi$moyenne_benzene.y,cas_temoins_parisexpoi$moyenne_benzene.x) 
#cas_temoins_parisexpoi$moyenne_no2i<-ifelse(!is.na(cas_temoins_parisexpoi$moyenne_no2.y),cas_temoins_parisexpoi$moyenne_no2.y,cas_temoins_parisexpoi$moyenne_no2.x) 

cas_temoins_parisexpoi$imp<-ifelse(is.na(cas_temoins_parisexpoi$moyenne_benzene),1,0)

cas_temoins_parisexpoi$moyenne_benzene<-ifelse(is.na(cas_temoins_parisexpoi$moyenne_benzene),cas_temoins_parisexpoi$mopb,cas_temoins_95expoi$moyenne_benzene)
cas_temoins_parisexpoi$moyenne_no2<-ifelse(is.na(cas_temoins_parisexpoi$moyenne_no2),cas_temoins_parisexpoi$mopn,cas_temoins_95expoi$moyenne_no2)
dim(cas_temoins_parisexpoi)


cas_temoins_95expoi$imp<-ifelse(is.na(cas_temoins_95expoi$moyenne_benzene),1,0)
cas_temoins_95expoi$moyenne_benzene<-ifelse(is.na(cas_temoins_95expoi$moyenne_benzene),cas_temoins_95expoi$mopb,cas_temoins_95expoi$moyenne_benzene)
cas_temoins_95expoi$moyenne_no2<-ifelse(is.na(cas_temoins_95expoi$moyenne_no2),cas_temoins_95expoi$mopn,cas_temoins_95expoi$moyenne_no2)
dim(cas_temoins_95expoi)

# cas_temoins_parisexpoi<-merge(cas_temoins_parishbis,join_bn,by.x="numunique",by.y="res_geo_95_suite@data[, \"Référence.Enfant\"]",all.x=T)
# cas_temoins_parisexpoi<-merge(cas_temoins_parisexpoi,join2_bn,by.x="numunique",by.y="erreur_iris@data[, \"MA___NumMalade\"]",all.x=T)
# cas_temoins_parisexpoi$moyenne_benzenei<-ifelse(!is.na(cas_temoins_parisexpoi$moyenne_benzene.y),cas_temoins_parisexpoi$moyenne_benzene.y,cas_temoins_parisexpoi$moyenne_benzene.x) 
# cas_temoins_parisexpoi$moyenne_no2i<-ifelse(!is.na(cas_temoins_parisexpoi$moyenne_no2.y),cas_temoins_parisexpoi$moyenne_no2.y,cas_temoins_parisexpoi$moyenne_no2.x) 
# cas_temoins_parisexpoi<-merge(cas_temoins_parisexpoi,iris_sp_paris_bn,by="numunique",all.x=T,suffixes=c("",".3"))
# cas_temoins_parisexpoi$moyenne_benzenei<-ifelse(!is.na(cas_temoins_parisexpoi$moyenne_benzene.y),cas_temoins_parisexpoi$moyenne_benzene.y,cas_temoins_parisexpoi$moyenne_benzene.x) 
# cas_temoins_parisexpoi$moyenne_no2i<-ifelse(!is.na(cas_temoins_parisexpoi$moyenne_no2.y),cas_temoins_parisexpoi$moyenne_no2.y,cas_temoins_parisexpoi$moyenne_no2.x) 



CORR<-as.vector(NULL)
for (i in names(cas_temoins_95expoi)){
  corr<-!class(cas_temoins_95expoi[,i])==class(cas_temoins_parisexpoi[,i])
  CORR<-c(CORR,corr)
}
lapply(cas_temoins_95expoi[,CORR],class)  



CORR<-as.vector(NULL)
for (i in names(cas_temoins_parisexpoi)){
  corr<-!class(cas_temoins_95expoi[,i])==class(cas_temoins_parisexpoi[,i])
  CORR<-c(CORR,corr)
}


lapply(cas_temoins_parisexpoi[,CORR],class)  

cas_temoins_95expoi$code_p<-as.character(cas_temoins_95expoi$code_p)
table(cas_temoins_parisexpoi$datenaissance_mere,exclude=NULL)
table(cas_temoins_parisexpoi$datenaissance_mere,is.na(as.Date(as.character(cas_temoins_parisexpoi$datenaissance_mere),format="%d/%m/%Y")),exclude=NULL)

cas_temoins_parisexpoi$datenaissance_mere<-gsub(pattern="(^[0-9]{4})\\-([0-9]{2})\\-([0-9]{2})$",
                                            replacement="\\3\\/\\2\\/\\1",x=cas_temoins_parisexpoi$datenaissance_mere)

table(cas_temoins_parish$datenaissance_mere,exclude=NULL)


table(is.na(as.Date(as.character(cas_temoins_parish$datenaissance_meres),format="%d/%m/%Y")),exclude=NULL)

table(cas_temoins_parish$datenaissance_mere)
cas_temoins_parisexpoi$datenaissance_mere<-as.Date(as.character(cas_temoins_parisexpoi$datenaissance_mere),format="%d/%m/%Y")
#cas_temoins_parisexpoi$datenaissance_meres<-NULL

cas_temoins_95expoi$prevoie<-as.character(cas_temoins_95expoi$prevoie)
cas_temoins_95expoi$voirie<-as.character(cas_temoins_95expoi$voirie)
cas_temoins_parisexpoi$num<-as.character(cas_temoins_parisexpoi$num)
cas_temoins_95expoi$ddnest<-as.Date(cas_temoins_95expoi$ddnest)
cas_temoins_95expoi$ddnMest<-as.Date(cas_temoins_95expoi$ddnMest)


#### a faire tourner si raté avec le ifelse et la moyenne
cas_temoins_95expoi_trier<-cas_temoins_95expoi
cas_temoins_parisexpoi_trier<-cas_temoins_parisexpoi


cas_temoins_95expoi_trier<-cas_temoins_95expoi_trier[,order(colnames(cas_temoins_95expoi_trier))]
cas_temoins_parisexpoi_trier<-cas_temoins_parisexpoi_trier[,order(colnames(cas_temoins_parisexpoi_trier))]


cas_temoinsexpoi<-rbind(cas_temoins_parisexpoi_trier,cas_temoins_95expoi_trier)
dim(cas_temoinsexpoi)
 

### on vire le décès  

cas_temoinsexpoi<-cas_temoinsexpoi[is.na(cas_temoinsexpoi$age_deces_hh) ,]
dim(cas_temoinsexpoi)
