###

edi<-read.sas7bdat("C:/Users/Louise/Documents/EDI/edi07.sas7bdat")
coorection<-read.csv2("C:/Users/Louise/Documents/EDI/passage_merge_iris.csv")

edi<-merge(edi,coorection,by="IRIS",all.x=TRUE)
edi$IRIS_reel<-as.character(edi$IRIS_reel)
edi$IRIS<-as.character(edi$IRIS)
edi$DCOMIRIS<-ifelse(!is.na(edi$IRIS_reel),edi$IRIS_reel,edi$IRIS)

base_sauv<-cas_temoinsexpoi
cas_temoinsexpoi<-merge(cas_temoinsexpoi,edi,by="DCOMIRIS",all.x=T)
