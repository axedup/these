### import Stata ###

#library(foreign)

data_2010 <- read.dta("C:/Users/Louise/Documents/Desespoir/Bases/CS8/ORS/bases pour cancer enfant/base PCS 2010.dta") 
data_2011 <- read.dta("C:/Users/Louise/Documents/Desespoir/Bases/CS8/ORS/bases pour cancer enfant/base PCS 2011.dta") 
data_2012 <- read.dta("C:/Users/Louise/Documents/Desespoir/Bases/CS8/ORS/bases pour cancer enfant/base PCS 2012.dta") 
data_2013 <- read.dta("C:/Users/Louise/Documents/Desespoir/BasesCS8/ORS/bases pour cancer enfant/base PCS 2013.dta") 
### les dates sont importés directement en date. 


### paris ne prend que les enfants résidant à la naissance à Paris mais forcément nés à Paris 
data_2010_paris<-data_2010[data_2010$Source=="75" & !is.na(data_2010$Source),] 
data_2011_paris<-data_2011[data_2011$Source=="75" & !is.na(data_2011$Source),] 
data_2012_paris<-data_2012[data_2012$Source=="75" & !is.na(data_2012$Source),] 
data_2013_paris<-data_2013[data_2013$Source=="75" & !is.na(data_2013$Source),] 


## export témoins
write.table(data_2010_paris,"F:/base_a_comp_2010.xls")
write.table(data_2011_paris,"F:/base_a_comp_2011.xls")
write.table(data_2012_paris,"F:/base_a_comp_2012.xls")
write.table(data_2013_paris,"F:/base_a_comp_2013.xls")



######========La suite c'est des explorations des bases========###### 


## recherche Na variable d'intéret##

table(is.na(data_2010$datenaissance))


summary(data_2010_paris$datenaissance) 
table(is.na(data_2010_paris$datenaissance))
table(data_2010_paris$datenaissance) 


summary(data_2010_paris$datenaissance_mere) 
table(is.na(data_2010_paris$datenaissance_mere))
table(data_2010_paris$datenaissance_mere)

summary(data_2010_paris$sexe) 
table(is.na(data_2010_paris$sexe))


summary(data_2010_paris$commune_nais_code) 
table(is.na(data_2010_paris$commune_nais_code))
table(data_2010_paris$commune_nais_code)

summary(data_2010_paris$commune_famille_code) 
table(is.na(data_2010_paris$commune_famille_code))
table(data_2010_paris$commune_famille_code) 


summary(data_2010_paris$poids) 

table(data_2010_paris$poids) 
table(is.na(data_2010_paris$poids))
length(unique(data_2010_paris$poids))

summary(data_2010_paris$taille) 
table(is.na(data_2010_paris$taille))
 length(unique(data_2010_paris$taille))
 
 
 
 summary(data_2010_paris$perimetre) 
 table(is.na(data_2010_paris$perimetre))
      length(unique(data_2010_paris$perimetre)) 
 
      ## 2011
      
      table(is.na(data_2011$datenaissance))
      
      
      summary(data_2011_paris$datenaissance) 
      table(is.na(data_2011_paris$datenaissance))
      table(data_2011_paris$datenaissance) 
      
      
      summary(data_2011_paris$datenaissance_mere) 
      table(is.na(data_2011_paris$datenaissance_mere))
      table(data_2011_paris$datenaissance_mere)
      
      summary(data_2011_paris$sexe) 
      table(is.na(data_2011_paris$sexe))
      
      
      summary(data_2011_paris$commune_nais_code) 
      table(is.na(data_2011_paris$commune_nais_code))
      table(data_2011_paris$commune_nais_code)
      
      summary(data_2011_paris$commune_famille_code) 
      table(is.na(data_2011_paris$commune_famille_code))
      table(data_2011_paris$commune_famille_code) 
      
      
      summary(data_2011_paris$poids) 
      
      table(data_2011_paris$poids) 
      table(is.na(data_2011_paris$poids))
      length(unique(data_2011_paris$poids))
      
      summary(data_2011_paris$taille) 
      table(is.na(data_2011_paris$taille))
      length(unique(data_2011_paris$taille))
      
      
      
      summary(data_2011_paris$perimetre) 
      table(is.na(data_2011_paris$perimetre))
      length(unique(data_2011_paris$perimetre))       
      
      
      
### 2012      
      
      
      table(is.na(data_2012$datenaissance))
      
      
      summary(data_2012_paris$datenaissance) 
      table(is.na(data_2012_paris$datenaissance))
      table(data_2012_paris$datenaissance) 
      
      
      summary(data_2012_paris$datenaissance_mere) 
      table(is.na(data_2012_paris$datenaissance_mere))
      table(data_2012_paris$datenaissance_mere)
      
      summary(data_2012_paris$sexe) 
      table(is.na(data_2012_paris$sexe))
      
      
      summary(data_2012_paris$commune_nais_code) 
      table(is.na(data_2012_paris$commune_nais_code))
      table(data_2012_paris$commune_nais_code)
      
      summary(data_2012_paris$commune_famille_code) 
      table(is.na(data_2012_paris$commune_famille_code))
      table(data_2012_paris$commune_famille_code) 
      
      
      summary(data_2012_paris$poids) 
      
      table(data_2012_paris$poids) 
      table(is.na(data_2012_paris$poids))
      length(unique(data_2012_paris$poids))
      
      summary(data_2012_paris$taille) 
      table(is.na(data_2012_paris$taille))
      length(unique(data_2012_paris$taille))
      
      
      
      summary(data_2012_paris$perimetre) 
      table(is.na(data_2012_paris$perimetre))
      length(unique(data_2012_paris$perimetre))   
      
##2013
      table(is.na(data_2013$datenaissance))
      
      
      summary(data_2013_paris$datenaissance) 
      table(is.na(data_2013_paris$datenaissance))
      table(data_2013_paris$datenaissance) 
      
      
      summary(data_2013_paris$datenaissance_mere) 
      table(is.na(data_2013_paris$datenaissance_mere))
      table(data_2013_paris$datenaissance_mere)
      
      summary(data_2013_paris$sexe) 
      table(is.na(data_2013_paris$sexe))
      
      
      summary(data_2013_paris$commune_nais_code) 
      table(is.na(data_2013_paris$commune_nais_code))
      table(data_2013_paris$commune_nais_code)
      
      summary(data_2013_paris$commune_famille_code) 
      table(is.na(data_2013_paris$commune_famille_code))
      table(data_2013_paris$commune_famille_code) 
      
      
      summary(data_2013_paris$poids) 
      
      table(data_2013_paris$poids) 
      table(is.na(data_2013_paris$poids))
      length(unique(data_2013_paris$poids))
      
      summary(data_2013_paris$taille) 
      table(is.na(data_2013_paris$taille))
      length(unique(data_2013_paris$taille))
      
      
      
      summary(data_2013_paris$perimetre) 
      table(is.na(data_2013_paris$perimetre))
      length(unique(data_2013_paris$perimetre))         
      
      
      
      
      
      
      
      
### valeurs aberrantes ###
      
      
      
      
essai<-data_2010_paris[data_2010_paris$datenaissance=="2010-07-28",]      
dim(essai)

table(essai$commune_famille_code)

essai2<-data_2010_paris[data_2010_paris$datenaissance=="2010-07-28" & data_2010_paris$commune_famille_code=="75018",]  
dim(essai2)

length(unique(essai2$poids))

RE<-NULL
for ( i in unique(data_2010_paris$datenaissance)){
  for (j in unique(data_2010_paris$commune_famille_code)) {
    
    essai2<-data_2010_paris[data_2010_paris$datenaissance==i & data_2010_paris$commune_famille_code==j,]  
    vu<-dim(essai2)[1]
    va<-length(unique(essai2$poids))
    re<-(vu-va)
    RE<-c(RE,re)
    
    
  }
}

table(RE)




REA<-NULL
for ( i in unique(data_2010_paris$datenaissance)){
  for (j in unique(data_2010_paris$commune_famille_code)) {
    
    essai2<-data_2010_paris[data_2010_paris$datenaissance==i & data_2010_paris$commune_famille_code==j,]  
    vu<-dim(essai2)[1]
    va<-length(unique(essai2$datenaissance_mere))
    re<-(vu-va)
    REA<-c(REA,re)
    
    
  }
}

table(REA)

ddn<-unique(data_2010_paris$datenaissance)
ddn<-ddn[order(ddn)]

commf<-unique(data_2010_paris$commune_famille_code)
commf<-commf[order(commf)]


RES<-NULL
REZ<-NULL
ESSAI2<-NULL
for ( i in ddn){
  for (j in commf) {
    for(s in 1:2){
    
    essai2<-data_2010_paris[data_2010_paris$datenaissance==i & data_2010_paris$commune_famille_code==j & data_2010_paris$sexe==s & !is.na(data_2010_paris$sexe),]  
    vu<-dim(essai2)[1]
    va<-length(unique(essai2$poids))
    re<-(vu-va)
    rez<-rep(re,dim(essai2)[1])
    RES<-c(RES,re)
    REZ<-c(REZ,rez)
    ESSAI2<-rbind(ESSAI2,essai2)
    
  }}
}


table(RES)
table(REZ)


ba<-data_2010_paris[!is.na(data_2010_paris$sexe),]
baw<-ba[order(ba$datenaissance,ba$commune_famille_code,ba$sexe),]
suite<-cbind(baw,REZ)

controle<-suite[!REZ==0,]

table(is.na(controle$datenaissance_mere))

length(unique(controle$datenaissance_mere[!is.na(controle$datenaissance_mere)]))
length(unique(controle$commune_nais_code[!is.na(controle$commune_nais_code)]))



RES<-NULL
REZ<-NULL
ESSAI2<-NULL
for ( i in ddn){
  for (j in commf) {
    for(s in 1:2){
      
      essai2<-data_2010_paris[data_2010_paris$datenaissance==i & data_2010_paris$commune_famille_code==j & data_2010_paris$sexe==s & !is.na(data_2010_paris$sexe),]  
      vu<-dim(essai2)[1]
      va<-length(unique(essai2$poids))
      re<-(vu-va)
      #rez<-rep(re,dim(essai2)[1])
      RES<-c(RES,re)
      #REZ<-c(REZ,rez)
      #ESSAI2<-rbind(ESSAI2,essai2)
      
    }}
}









RESM<-NULL
for ( i in unique(data_2010_paris$datenaissance)){
  for (j in unique(data_2010_paris$commune_famille_code)) {
    for(p in unique(data_2010_paris$poids)){
    for(s in 1:2){
      
      essai2<-data_2010_paris[data_2010_paris$datenaissance==i & data_2010_paris$commune_famille_code==j & data_2010_paris$sexe==s & !is.na(data_2010_paris$sexe)
                              & data_2010_paris$poids==p ,]  
      vu<-dim(essai2)[1]
      va<-length(unique(essai2$datenaissance_mere[!is.na(essai2$datenaissance_mere)]))
      re<-(vu-va)
      RESM<-c(RESM,re)
      
      
    }}}
}

table(RESM)
