#library(sas7bdat)
#library(sp)


expo<-read.sas7bdat("C:\\Users\\Louise\\Documents\\Desespoir\\Bases\\donnes_airp_geocap_2002_2007.sas7bdat")
head(expo)
coord<-read.sas7bdat("C:\\Users\\Louise\\Documents\\Desespoir\\Bases\\geocap_geocodage.sas7bdat")
head(coord)

coord2<-read.csv2("C:\\Users\\Louise\\Documents\\Desespoir\\Bases\\coordonnees.csv",header=T)
dim(coord2)
head(coord2)



def<-merge(expo,coord2,by="id_inserm",suffix=c(".nc",".c"))
sec<-def$id_inserm[is.na(def$Geo_X) | is.na(def$Geo_Y)]
str(sec)
def<-def[!is.na(def$Geo_X) & !is.na(def$Geo_Y),]
dim(def)
dim(coord)
dim(expo)

def2<-merge(expo[expo$id_inserm %in% sec,],coord[,c("id_inserm","Geo_X","Geo_Y")],by="id_inserm",suffix=c(".nc",".c"))
dim(def2)### on peut pas retrouver les 61


#### nouvel import des données d'expo ###

def<-read.csv2("C:\\Users\\Louise\\Documents\\Desespoir\\Bases\\airparif.csv",header=T,stringsAsFactors = FALSE,dec=".")

coordinates(def)=~ Geo_X + Geo_Y

proj4string(def)<-CRS("+init=epsg:27572") # lambert II étendu 
summary(def)
wgs84PbData <- spTransform(def, CRS("+init=epsg:2154")) # lambert 93

summary(wgs84PbData)


export<-cbind(wgs84PbData$Geo_X,wgs84PbData$Geo_Y,wgs84PbData$benzene_airp,wgs84PbData$no2_airp,wgs84PbData$id_inserm)
write.csv(export,file="C:/Users/Louise/Documents/Desespoir/Bases/expo.csv")

### on fait la fusion expo/IRIS 
join_expo<-over(wgs84PbData,aus2)
join_expo<-cbind(join_expo,wgs84PbData@data[,c("id_inserm","benzene_airp","no2_airp")])

resume_expo<-join_expo %>%
  group_by(DCOMIRIS) %>%
  summarise(n=n(),
            moyenne_benzene=mean(benzene_airp,na.rm=T),
            moyenne_no2=mean(no2_airp,na.rm=T))

### faisons fusionner nos patients avec la base resume_expo : c'est fait base par base ....
join_bn<-merge(join,resume_expo,by= "DCOMIRIS",all.x=T)

averif1<-join_bn[is.na(join_bn$moyenne_no2),"res_geo_95_suite@data[, \"Référence.Enfant\"]"]

join2_bn<-merge(join2,resume_expo,by= "DCOMIRIS",all.x=T)
averif2<-join2_bn[is.na(join2_bn$moyenne_no2),"erreur_iris@data[, \"MA___NumMalade\"]"]


iris_sp_paris_bn<-merge(iris_sp_paris,resume_expo,by.x="IRIS",by.y= "DCOMIRIS",all.x=T)
averif3<-iris_sp_paris_bn[is.na(iris_sp_paris_bn$moyenne_no2),"numunique"]
iris_p2_paris_bn<-merge(iris_p2_paris,resume_expo,by.x="TOPO_IRIS",by.y= "DCOMIRIS",all.x=T)
averif4<-iris_p2_paris_bn[is.na(iris_p2_paris_bn$moyenne_no2),"numunique"]

iris_p3_paris_bn<-merge(iris_p3_paris,resume_expo,by.x="IRIS",by.y= "DCOMIRIS",all.x=T)
averif5<-iris_p3_paris_bn[is.na(iris_p3_paris_bn$moyenne_no2),"numunique"]



iris_sp_95_bn<-merge(iris_sp_955,resume_expo,by.x="IRIS",by.y= "DCOMIRIS",all.x=T)
averif6<-iris_sp_95_bn[is.na(iris_sp_95_bn$moyenne_no2),"numunique"]

iris_p1_95_bn<-merge(iris_p1_95,resume_expo,by.x="TOPO_IRIS",by.y= "DCOMIRIS",all.x=T)
averif7<-iris_p1_95_bn[is.na(iris_p1_95_bn$moyenne_no2),"numunique"]


iris_p2_95_bn<-merge(iris_p2_95,resume_expo,by.x="TOPO_IRIS",by.y= "DCOMIRIS",all.x=T)
averif8<-iris_p2_95_bn[is.na(iris_p2_95_bn$moyenne_no2),"numunique"]

iris_p3_95_bn<-merge(iris_p3_95,resume_expo,by.x="IRIS",by.y= "DCOMIRIS",all.x=T)
averif9<-iris_p3_95_bn[is.na(iris_p3_95_bn$moyenne_no2),"numunique"]






#####


