a<-matrix(c(22,-2,-2,-2,16,4,-2,4,16),3,3)
a<-(1/6)*a
x = eigen(a)
x$values # valeurs propres
x$vectors

library("rgdal")
library("dplyr")

install.packages("cartography")
library("cartography")
install.packages("ghit")
ghit::install_github(c("leeper/tabulizerjars", "leeper/tabulizer"), INSTALL_opts = "--no-multiarch", dependencies = c("Depends", "Imports"))
library(tabulizer)
tab<-extract_tables('dt51-sources_et_methodes.pdf',pages=c(11,12,13),guess=TRUE)
tabb<-do.call(rbind,tab)

tac<-as.data.frame(tabb[,c(3,6)])
tac$dep<-as.numeric(as.character(tac[,1]))

tac<-read.csv2("C:/Users/User/these/cs8.csv")
names(tac)<-c("RÉGION", "DÉPARTEMENT", "dep", "certif", 
  "enfants", "pour")
tac$dep<-gsub(pattern="(^[0-9]{1}$)",replacement="0\\1",tac$dep)
tac$dep<-as.numeric(tac$dep)

tac$pour<-as.character(tac$pour)
tac$pour<-gsub(pattern=",",replacement=".",tac$pour)
tac$pour<-as.numeric(tac$pour)

tac$enfants<-as.character(tac$enfants)
tac$enfants<-gsub(pattern=" ",replacement="",tac$enfants)
tac$enfants<-as.numeric(tac$enfants)



#ogrInfo(dsn = "G:/INCa/Inca",layer="departements-20140306-100m")
reg <- readOGR(dsn=path.expand("G:/INCa"),layer="departements-20140306-100m", stringsAsFactors=TRUE)
reg@data
plot(reg)
plot(reg[!reg$nom %in% c("Guyane","Martinique","Guadeloupe","La RÃƒÂ©union","Mayotte"),])

t<-merge(reg@data,tac,by.x="code_insee",by.y="dep",all.x=TRUE)

reg@data <- data.frame(reg@data,t)

cas<-read.csv2("cas.csv")
cas$DepNaiss<-gsub(pattern="(^[0-9]{1}$)",replacement="0\\1",cas$DepNaiss)


cas_2<-cas %>%
  group_by(DepNaiss) %>%
  summarise (n=n())


tu<-merge(reg@data,cas_2,by.x="code_insee",by.y="DepNaiss",all.x=TRUE)

reg@data <- data.frame(reg@data,tu)
classTemps <- classIntervals(reg@data$pour, 5, style = "quantile")
# Choix d'une palette de couleur pour les 5 catÃ©gories
palette <- brewer.pal(n = 6, name = "YlOrRd")


reg@data$pourt<-as.character(cut(reg@data$pour, breaks = c(0,20,50,80,90,95,101), labels = palette, include.lowest = TRUE))
legende <- as.character(c("moins de 20 %","entre 20-50 %", "50-80 %","80-90 %","90-95 %","95-100 %"))

paletten <- brewer.pal(n = 5, name = "OrRd")

reg@data$enfantss<-as.numeric(as.character(reg@data$enfants))
reg@data$enft<-as.character(cut(reg@data$enfantss, breaks = 5, labels = paletten, include.lowest = TRUE))

legended <- as.character(c("< 7800","7800-14900", "14900-22100","22100-29200","29200-36400"))


#iris
plot(reg[!reg$nom %in% c("Guyane","Martinique","Guadeloupe","La Réunion","Mayotte"),],col=reg@data$pourt)
legend("topleft",legend=legende,fill=palette,cex=0.8,pt.cex=20)
plot(reg[!reg$nom %in% c("Guyane","Martinique","Guadeloupe","La Réunion","Mayotte"),],col=reg@data$enft)
legend("left",legend=legended,fill=palette,cex=0.8,pt.cex=20)
!reg$nom %in% c("Guyane","Martinique","Guadeloupe","La Réunion","Mayotte"),]

propSymbolsLayer(spdf = reg, df = reg@data, var = "n", legend.pos = "y",inches=0.20)
labelLayer(spdf = reg, df= reg@data, spdfid = 'code_insee', txt="n", col = "black",
           cex = 0.8)
