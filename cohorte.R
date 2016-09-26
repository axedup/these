data_paris$cas<-ifelse(data_paris$numt %in% cas_paris$numt,1,0)
table(data_paris$cas)


data_1013$cas<-ifelse(data_1013$numt %in% cas_95$numt,1,0)
table(data_1013$cas)


cohorte<-rbind(data_paris,data_1013[!data_1013$Source %in% c("75","91","92","93","94","77","78"),])
# on en perd car y a ceux qui ne résident pas a Pairs ou Val de marne
cohorte$cp_naissance<-substr(cohorte$commune_nais_code,1,2)
table(cohorte$cas)



date_diag<-merge(cohorte[cohorte$cas==1,],type_cancer,by=c("datenaissance","sexe","cp_naissance"))
