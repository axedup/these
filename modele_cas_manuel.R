### Merging cas paris 2010-2013###

cas_<- read.csv("~/Desespoir/Bases/Liste_cas_IDF_compl4crypte.csv.gpg.out", sep=";", quote="", stringsAsFactors=FALSE,na.strings = "")
cas_<-cas_93[!is.na(cas_93$prevoie),]
cas_$MA__DateNaissance<-as.Date(as.character(cas_93$MA__DateNaissance), "%d/%m/%Y")
str(cas_93$MA__DateNaissance)
#cas_93$MA__DateNaissance<-as.Date(as.character(cas_93$MA__DateNaissance), "%d/%m/%Y")
dim(cas_93)# 94 cas au 21/06 
head(cas_93)

### base ors 2010-2013 
# les bases ors ont les mÃªmes nombre de variables seuls les noms (au tiret pres) différent : à adpater selon la région  
which(!names(data_2010_93) %in% names(data_2011_93))
which(!names(data_2011_93) %in% names(data_2012_93))
which(!names(data_2012_93) %in% names(data_2013_93))
which(!names(data_2010_93) %in% names(data_2012_93))

names(data_2010)<-names(data_2012) # euh un sort avant c'est mieux 
data_1013<-rbind(data_2010,data_2011,data_2012,data_2013)
names(data_2010_93) %in% names(data_2011_93)
names(data_2011_93)
names(data_2012_93)
names(data_2013_93)




### merge
# je vais virer les 3 premiers car pas bien rempli
# pas normal de galÃ©rer sur les ifelse....

cas_93$ddn_mater2<-ifelse(is.na(cas_93$ddn.mater),"13/13/1313",cas_93$ddn.mater)
cas_93$ddn_mater2<-ifelse(cas_93$ddn_mater2=="nc ","13/13/1313",cas_93$ddn_mater2)
cas_93$ddn_mater2<-ifelse(cas_93$ddn_mater2=="nc","13/13/1313",cas_93$ddn_mater2)
table(cas_93$ddn_mater2,exclude=NULL)

# je prÃ©fÃ¨re comme Ã§a 
cas_93$ddn_mater2<-ifelse(is.na(cas_93$ddn.mater) | cas_93$ddn.mater=="nc "|cas_93$ddn.mater=="nc" ,"13/12/1913",cas_93$ddn.mater)
cas_93$ddn_mater2<-as.Date(cas_93$ddn_mater2,format="%d/%m/%Y")
table(cas_93$ddn_mater2,exclude=NULL)

table(cas_93$poids,exclude=NULL)
cas_93$poids2<-ifelse(is.na(cas_93$poids),40000,cas_93$poids)
table(cas_93$poids2,exclude=NULL)
str(cas_93$poids2)
#cas_93$MA__DateNaissance<-as.Date(cas_93$MA__DateNaissance,format="%d/%m/%Y")
table(cas_93$MA__DateNaissance,exclude=NULL)

cas_93<-cas_93[order(cas_93$MA__DateNaissance,cas_93$ddn_mater2),]

data_1013$ddn_mater2<-as.character(data_1013$datenaissance_mere)
table(data_1013$ddn_mater2,exclude=NULL)
table(is.na(data_1013$ddn_mater2))
data_1013$ddn_mater2<-ifelse(is.na(data_1013$ddn_mater2),"1913-12-13",data_1013$ddn_mater2)
table(is.na(data_1013$ddn_mater2))
table(data_1013$ddn_mater2)
data_1013$ddn_mater2<-as.Date(data_1013$ddn_mater2,format="%Y-%m-%d")
table(is.na(data_1013$ddn_mater2)
      table(data_1013$ddn_mater2)
      
      table(is.na(data_1013$poids))
      data_1013$poids2<-as.character(data_1013$poids)
      data_1013$poids2<-ifelse(is.na(data_1013$poids2),"40000",data_1013$poids2)
      data_1013$poids2<-as.numeric(data_1013$poids2)
      table(is.na(data_1013$poids2))
      str(data_1013$poids2)
      
      data_1013<-data_1013[order(data_1013$datenaissance,data_1013$ddn_mater2),]
      data_1013$numt<-1:dim(data_1013)[1]
      
      
#mÃªme codage de sexe 
test<-merge(cas_93[-3,],data_1013,by.x=c("MA__DateNaissance","MA__Sexe","ddn_mater2","poids2"),by.y=c("datenaissance","sexe","ddn_mater2","poids2"),suffixes=c(".r",".c") )
test<-test[,order(colnames(test),decreasing=TRUE)]
      
      table(test$MA__LieuNaissCom %in% test$commune_famille_code)
      
      dim(test)
      length(test$MA___NumMalade)
      length(unique(test$MA___NumMalade)) ### pas de gag !!!!!!!
      
      
      
      
# étape des controles de non fusion       
      cas_93[which(!cas_93$MA___NumMalade %in% test$MA___NumMalade ),]
      
    
      controle_1<-data_1013[data_1013$datenaissance=="2010-02-09",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
      controle_1<-controle_1[controle_1$sexe==1,]
      controle_1<-controle_1[order(c(controle_1$ddn_mater2,controle_1$poids2)),] 
      controle_1 # 3148
      
      
      
      
      controle_1<-data_1013[data_1013$datenaissance=="2010-10-19",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
      controle_1<-controle_1[controle_1$sexe==2,]
      controle_1<-controle_1[order(c(controle_1$ddn_mater2,controle_1$poids2)),] 
      controle_1 # 23512
      
      
      controle_1<-data_1013[data_1013$datenaissance=="2011-10-30",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
      controle_1<-controle_1[controle_1$sexe==1,]
      #controle_1<-controle_1[order(c(controle_1$ddn_mater2,controle_1$poids2)),] 
      controle_1[controle_1$poids2==3620,]
      controle_1 # trouvÃ© 53014
      
      controle_1<-data_1013[data_1013$datenaissance=="2011-09-27",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
      controle_1<-controle_1[controle_1$sexe==1,]
      #controle_1<-controle_1[order(c(controle_1$ddn_mater2,controle_1$poids2)),] # manquante ++++
      controle_1
      controle_1[controle_1$poids2==3150,]
      
      
      
      v<-data_1013[data_1013$poids2==3150,c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
      v[v$taille==480 ,]
      v[v$ddn_mater2=="1913-12-13" ,]## erreur date de naissance il est nÃ© le 2011-08-27. #48008
      
      
      controle_1<-data_1013[data_1013$datenaissance=="2012-07-01",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
      controle_1<-controle_1[controle_1$sexe==2,]
      #controle_1<-controle_1[order(c(controle_1$ddn_mater2,controle_1$poids2)),] # trouve 71292 erreur ddn mater 
      controle_1
      
      controle_1<-data_1013[data_1013$datenaissance=="2012-02-07",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
      controle_1<-controle_1[controle_1$sexe==2,]
      #controle_1<-controle_1[order(c(controle_1$ddn_mater2,controle_1$poids2)),] # trouve 60478
      controle_1
      
      
      controle_1<-data_1013[data_1013$datenaissance=="2010-11-06",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
      controle_1<-controle_1[controle_1$sexe==2,]
      controle_1[controle_1$poids2==2435,]
      #controle_1<-controle_1[order(c(controle_1$ddn_mater2,controle_1$poids2)),] 
      controle_1 #25073
      
      controle_1<-data_1013[data_1013$datenaissance=="2012-11-01",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
      
      controle_1[controle_1$poids2==4200,]
      controle_1[controle_1$taille==52.5,]
      #controle_1<-controle_1[order(c(controle_1$ddn_mater2,controle_1$poids2)),] m nquante  #78315 pb date de naissance enfant
      controle_1 
      
      
      v<-data_1013[data_1013$poids2==4200,c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code")]
      v[v$taille==52.5 ,]
      
      
      controle_1<-data_1013[data_1013$datenaissance=="2012-06-25",c("numt","datenaissance","ddn_mater2","sexe","poids2","taille","commune_nais_code","commune_famille_code")]
      
      controle_1[controle_1$taille==46.0,]
      #controle_1<-controle_1[order(c(controle_1$ddn_mater2,controle_1$poids2)),]
      controle_1 #70799
      
      
      
      ############ Fusion des corrections 
      ### !!!! c'est pas un merge et r ordonne selon son propre ordre 
      
      ############################################################### Bien vérifier 
      g<-cbind(cas_93[which(!cas_93$MA___NumMalade %in% test$MA___NumMalade ),],data_1013[data_1013$numt %in% c(3148,25073,78315,70799,23512,53014,48008,71292,60478),!names(data_1013) %in% c("datenaissance","sexe","ddn_mater2","poids2")])
      
      g[,c("MA___NumMalade","numt")]
      
      g<-g[,order(colnames(g),decreasing=TRUE)]
      names(g)<-names(test)
      fait<-rbind(test,g)
      # éape à revoir 
      fait<-merge(fait,data_1013[,c("numt","datenaissance","poids")], by="numt",suffixes=c("",".c")) # pour corrige les gags d'eeureur de ddn dans le registe 
      names(fait)
      dim(fait)
      
      
      cas_93<-fait
      cas_93$cas<-1
      cas_93$tbis<-grepl(pattern="^b",cas_93$num)
#      cas_93$num_rue<-gsub(pattern="^b*([0-9].*)",replace="\\1",cas_93$num)
      
#      table(cas_93$num_rue,cas_93$num)
      
      
            ### cr?ation num?ro unique 
      
      
      set.seed(8521)# a faire tourner a chaque add
      add<-sample(seq(from =1, to =dim(cas_93)[1], by = 1), size = dim(cas_93)[1], replace = FALSE)
      
      cas_93$numunique<-cas_93$MA___NumMalade*add
      
      
      # ### choix de la date de naissance mater : ddn.mater car on a recup des date de naisance mater dans les dossier sauf pour les 3 premiers erreur de saisie - un doute 
      # # aptientes entre le 1 et le cas du m?me mois 
      # 
      # 
      # cas_93$ddn.mater[cas_93$datenaissance=="2010-11-06"]<-"1980-03-08"
      # cas_93$datenaissance_mere[cas_93$datenaissance=="2010-11-06"]
      # 
      # ## EUH LA C42TAIT PAS A FAIRE#
      # cas_93$ddn.mater[cas_93$datenaissance=="2012-06-25"]<-c(NA,"1970-01-06")
      # cas_93$datenaissance_mere[cas_93$datenaissance=="2012-06-25"]
      # cas_93$ddn.mater[cas_93$datenaissance=="2012-06-25"]
      # 
      # 
      # cas_93$ddn.mater[cas_93$datenaissance=="2012-10-01"]<-c("1975-10-26")
      # cas_93$ddn.mater[cas_93$datenaissance=="2012-10-01"]
      # cas_93$datenaissance_mere[cas_93$datenaissance=="2012-10-01"]
      
      
      write.csv(cas_93,"F:/cas_93.csv")
      write.csv(cbind(cas_93$MA___NumMalade,cas_93$numunique),"~/Desespoir/Bases/correspondance_cas_93.csv")
      write.csv(cas_93[,c("MA___NumMalade","datenaissance_mere","MA__Sexe","poids.c","taille.c","MA__DateNaissance","num_rue","prevoie","voie","nom.rue")],"F:/cas_93_geocoder.csv")
      
      