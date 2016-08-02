library(rgdal)
library(sp)
library(gstat)
library(raster)
library(maptools)

### Géolocalisation : stat, correction, attribution à l'IRIS####

res_geo<-read.csv2("C:/Users/Louise/Documents/Desespoir/Bases/geocoder/iris_1500.csv")
res_geo_95_suite<-read.csv2("C:/Users/Louise/Documents/Desespoir/Bases/geocoder/supplement_adresse_95_geocod2.csv",encoding = "UTF-8")
#☻ 1596 adresses

head(res_geo_95_suite)
legende<-read.csv2("C:/Users/Louise/Documents/Desespoir/Bases/geocoder/Copie de AFF_XY.csv")

#` import des corrections 
#erreur_iris<-read.csv2("C:/Users/Louise/Documents/Desespoir/Bases/geocoder/Export_erreur_ign_v2.csv",encoding = "UTF-8")
erreur_iris<-read.csv2("F:/Export_erreur_ign_v2.csv",encoding = "UTF-8")

erreur_iris<-merge(erreur_iris,res_geo[c("MA___NumMalade","id")],by="id")

#erreur_geocodage<-read.csv2("C:/Users/Louise/Documents/Desespoir/Bases/geocoder/correction_geocodage_mais_irisinsee_ok.geocoded_2.csv",encoding = "UTF-8")
erreur_geocodage<-read.csv2("F:/correction_geocodage_mais_irisinsee_ok.geocoded_2.csv",encoding = "UTF-8")


erreur_geocodage$latitude<-as.character(erreur_geocodage$latitude)
erreur_geocodage$longitude<-as.character(erreur_geocodage$longitude)
erreur_geocodage$correction_x<-as.character(erreur_geocodage$correction_x)
erreur_geocodage$correction_y<-as.character(erreur_geocodage$correction_y)

erreur_geocodage$latitude<-ifelse(erreur_geocodage$correction_x=="",erreur_geocodage$latitude,erreur_geocodage$correction_x)
erreur_geocodage$longitude<-ifelse(erreur_geocodage$correction_y=="",erreur_geocodage$longitude,erreur_geocodage$correction_y)





# attention aux IRIS discordant va falloir revérifier :
# géocodage ok et iris divergent : on a vérifier et les mieux c'est le système de la base
# géocodage bof : on prend l'iris de la base 
# pas d'iris avec la base : bah on prend celle obtenue avec la jointure. Les coordonnées seront fonction de la qualité du géocodgae
# iris correct mais non retrouvé car évol entre 2000 et 2012 :


evol<-res_geo[is.na(res_geo$IRIS_2012) & !substr(res_geo$IRIS,6,9)=="XXXX",]
table(evol$AFF)



iris_paris<-merge(cas_temoins_parishbis,res_geo[,!names(res_geo) %in% c("numunique")],by.x="numunique",by.y="MA___NumMalade")
  dim(iris_paris)

  
table(substr(iris_paris$IRIS,6,9)=="XXXX")
table(iris_paris$AFF)
  

iris_paris_erreur<-iris_paris[substr(iris_paris$IRIS,6,9)=="XXXX" & !iris_paris$AFF %in% c("0","1","10","11") , c("numunique","id","adr1","X","Y")]
dim(iris_paris_erreur)

table(iris_paris_erreur$id %in% erreur_iris$id)


### refaire retourner si changement tirage au sort témoins 95 
iris_95<-merge(cas_temoins_95_help2,res_geo[,!names(res_geo) %in% c("numunique")],by.x="numunique",by.y="MA___NumMalade",all.x=TRUE)
dim(iris_95)
iris_95<-merge(iris_95,res_geo_95_suite[,names(res_geo_95_suite) %in% c("latitude","longitude","result_score","result_type","result_house","result_name","Référence.Enfant")],by.x="numunique",by.y="Référence.Enfant",all.x=TRUE)
dim(iris_95)

table(substr(iris_95$IRIS,6,9)=="XXXX" & !is.na(iris_95$IRIS))


iris_95_erreur<-iris_95[((substr(iris_95$IRIS,6,9)=="XXXX" & !is.na(iris_95$IRIS))| is.na(iris_95$IRIS)) & !iris_95$AFF %in% c("0","1","10","11") , c("numunique","id","adr1","X","Y","result_name","latitude","longitude")]
dim(iris_95_erreur)

match(iris_95_erreur$id,erreur_iris$id)


######### Liste finale des IRIS : concerne les adresses géocodés par géocibles 
### sans probleme

iris_sp_paris<-iris_paris[!substr(iris_paris$IRIS,6,9)=="XXXX" & iris_paris$iris_diff==0 & !is.na(iris_paris$iris_diff),c("numunique","IRIS")]

iris_sp_955<-iris_95[!substr(iris_95$IRIS,6,9)=="XXXX" & !is.na(iris_95$IRIS) & iris_95$iris_diff==0 & !is.na(iris_95$iris_diff),c("numunique","IRIS")]


### iris 2000 trouvé mais pas de correspondance en 2012 et géoloc ok on prend l'iris_2012


iris_p1_paris<-iris_paris[!substr(iris_paris$IRIS,6,9)=="XXXX" & is.na(iris_paris$IRIS_2012) & iris_paris$AFF %in% c("0","1","10","11"),c("numunique","TOPO_IRIS")]

iris_p1_95<-iris_95[!substr(iris_95$IRIS,6,9)=="XXXX" & !is.na(iris_95$IRIS) & is.na(iris_95$IRIS_2012)& iris_95$AFF %in% c("0","1","10","11"),c("numunique","TOPO_IRIS")]


### pas d'iris base mais la géoloc correcte permet de prendre les ccordonnées Navteq

iris_p2_paris<-iris_paris[substr(iris_paris$IRIS,6,9)=="XXXX"  & iris_paris$AFF %in% c("0","1","10","11"),c("numunique","TOPO_IRIS")]

iris_p2_95<-iris_95[substr(iris_95$IRIS,6,9)=="XXXX" & !is.na(iris_95$IRIS) & iris_95$AFF %in% c("0","1","10","11") & !is.na(iris_95$AFF),c("numunique","TOPO_IRIS")]

### iris discordant (iris 2000 existe en 2012): c'est la faute a Navteq

iris_p3_paris<-iris_paris[!substr(iris_paris$IRIS,6,9)=="XXXX"  & iris_paris$iris_diff=="1" & !is.na(iris_paris$IRIS_2012),c("numunique","IRIS")]

iris_p3_95<-iris_95[!substr(iris_95$IRIS,6,9)=="XXXX" & !is.na(iris_95$IRIS) & iris_95$iris_diff==1 & !is.na(iris_95$iris_diff) &  !is.na(iris_95$IRIS_2012),c("numunique","IRIS")]


######### attribuer iris :  ça concerne les iris en erreur corrigés et les 95 en extra non géocodés par géocible 

erreur_iris$correction_xl93<-as.numeric(as.character(erreur_iris$correction_x))
erreur_iris$correction_yl93<-as.numeric(as.character(erreur_iris$correction_y))

coordinates(erreur_iris)=~correction_yl93 + correction_xl93

proj4string(erreur_iris)<-CRS("+init=epsg:4326")
erreur_iris <- spTransform(erreur_iris, CRS("+init=epsg:2154")) 

# a retourner si changment 95 
res_geo_95_suite$longitude<-as.numeric(as.character(res_geo_95_suite$longitude))
res_geo_95_suite$latitude<-as.numeric(as.character(res_geo_95_suite$latitude))
coordinates(res_geo_95_suite)=~longitude + latitude
proj4string(res_geo_95_suite)<-CRS("+init=epsg:4326")
res_geo_95_suite <- spTransform(res_geo_95_suite, CRS("+init=epsg:2154")) 


# fixe
c_iris_75 <- shapefile("~/Desespoir/Bases/contours_iris_2013/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2014-06-00379/CONTOURS-IRIS_1-0_SHP_LAMB93_R11-2013/CONTOURS-IRIS_1-0_SHP_LAMB93_D75-2013/CONTOURS-IRIS75.shp")
c_iris_77 <- shapefile("~/Desespoir/Bases/contours_iris_2013/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2014-06-00379/CONTOURS-IRIS_1-0_SHP_LAMB93_R11-2013/CONTOURS-IRIS_1-0_SHP_LAMB93_D77-2013/CONTOURS-IRIS77.shp")
c_iris_78 <- shapefile("~/Desespoir/Bases/contours_iris_2013/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2014-06-00379/CONTOURS-IRIS_1-0_SHP_LAMB93_R11-2013/CONTOURS-IRIS_1-0_SHP_LAMB93_D78-2013/CONTOURS-IRIS78.shp")
c_iris_91 <- shapefile("~/Desespoir/Bases/contours_iris_2013/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2014-06-00379/CONTOURS-IRIS_1-0_SHP_LAMB93_R11-2013/CONTOURS-IRIS_1-0_SHP_LAMB93_D91-2013/CONTOURS-IRIS91.shp")
c_iris_92 <- shapefile("~/Desespoir/Bases/contours_iris_2013/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2014-06-00379/CONTOURS-IRIS_1-0_SHP_LAMB93_R11-2013/CONTOURS-IRIS_1-0_SHP_LAMB93_D92-2013/CONTOURS-IRIS92.shp")
c_iris_93 <- shapefile("~/Desespoir/Bases/contours_iris_2013/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2014-06-00379/CONTOURS-IRIS_1-0_SHP_LAMB93_R11-2013/CONTOURS-IRIS_1-0_SHP_LAMB93_D93-2013/CONTOURS-IRIS93.shp")
c_iris_94 <- shapefile("~/Desespoir/Bases/contours_iris_2013/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2014-06-00379/CONTOURS-IRIS_1-0_SHP_LAMB93_R11-2013/CONTOURS-IRIS_1-0_SHP_LAMB93_D94-2013/CONTOURS-IRIS94.shp")
c_iris_95 <- shapefile("~/Desespoir/Bases/contours_iris_2013/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2014-06-00379/CONTOURS-IRIS_1-0_SHP_LAMB93_R11-2013/CONTOURS-IRIS_1-0_SHP_LAMB93_D95-2013/CONTOURS-IRIS95.shp")


sapply(c_iris_75@polygons , function(x) slot(x, "ID"))
id_95<-sapply(c_iris_95@polygons , function(x) slot(x, "ID"))
id_91<-sapply(c_iris_91@polygons , function(x) slot(x, "ID"))
id_92<-sapply(c_iris_92@polygons , function(x) slot(x, "ID"))
id_93<-sapply(c_iris_93@polygons , function(x) slot(x, "ID"))
id_94<-sapply(c_iris_94@polygons , function(x) slot(x, "ID"))
id_77<-sapply(c_iris_77@polygons , function(x) slot(x, "ID"))
id_78<-sapply(c_iris_78@polygons , function(x) slot(x, "ID"))

c_iris_95 <- spChFIDs(obj = c_iris_95, x = paste(95,id_95,sep="-"))
c_iris_91 <- spChFIDs(obj = c_iris_91, x = paste(91,id_91,sep="-"))
c_iris_92 <- spChFIDs(obj = c_iris_92, x = paste(92,id_92,sep="-"))
c_iris_93 <- spChFIDs(obj = c_iris_93, x = paste(93,id_93,sep="-"))
c_iris_94 <- spChFIDs(obj = c_iris_94, x = paste(94,id_94,sep="-"))
c_iris_77 <- spChFIDs(obj = c_iris_77, x = paste(77,id_77,sep="-"))
c_iris_78 <- spChFIDs(obj = c_iris_78, x = paste(78,id_78,sep="-"))

aus2 <- spRbind(c_iris_75, c_iris_95)
aus2 <- spRbind(aus2,c_iris_91)
aus2 <- spRbind(aus2,c_iris_92)
aus2 <- spRbind(aus2,c_iris_93)
aus2 <- spRbind(aus2,c_iris_94)
aus2 <- spRbind(aus2,c_iris_77)
aus2 <- spRbind(aus2,c_iris_78)
aus2 <- spTransform(aus2, CRS("+init=epsg:2154")) 
### aus2 c'est le fichier geo de touttes les iris d'ilde de france 


join<-over(res_geo_95_suite,aus2) # les 6 adresses à refaire retourner en cas de changement 
join<-cbind(join,res_geo_95_suite@data[,"Référence.Enfant"])

join2<-over(erreur_iris,aus2)
join2<-cbind(join2,erreur_iris@data[,"MA___NumMalade"])


####


write.csv(erreur_iris[,c("MA___NumMalade","correction_x","correction_y")],file="G:/a_mettre_ds_Qgis.csv")
