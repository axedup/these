#### imputation multiples####

 ini <- mice(nhanes2, maxit = 0, print = FALSE)
post <- ini$post
 k <- seq(1, 1.5, 0.1)
 est <- vector("list", length(k))
 for (i in 1:length(k)) {
  post["chl"] <- paste("imp[[j]][,i] <-", k[i], "* imp[[j]][,i]")
  imp <- mice(nhanes2, post = post, seed = 10, print = FALSE, maxit = 20)
 fit <- with(imp, lm(bmi ~ age + chl))
 
 
 
 # on va imputer en fonction des expos car c'est pas des données persos 
 # age mater # alcool et tabac 
 cas_temoinsexpoi$rupturemembrane<-as.factor( cas_temoinsexpoi$rupturemembrane)
 base_imput<-cas_temoinsexpoi[c( "age",  "agegestationnel", 
                                 "allaitement", "analgesieautr", 
                                "analgesiegene", "analgesieperi", "anomalie", "antibiotherapie", 
                                "atresie", "aucune_analgesie", "autre_patho", "bcg", 
                                "cas", "cesarienne", "clartenucale", "coeffapgar1mn", 
                                "coeffapgar5mn", "commune_famille_code", "commune_nais_code", 
                                
                                "debuttravail", "dep", "dep_nais",
                                 "echomorpho", "fente", "gestes_techniques", 
                                "gestite", "gestite.f", "gestite.f2","code_p", 
                             
                                "mactivite", 
                                 "menacepremature", "mois_acc", "mois_naiss_m", 
                                "mprofession",  "naissancepar", "naissancepar.f2", 
                                "nbdecedeav28j", "nbecho", "nbenfant", "nbfoetus", "nbjourhopital", 
                                "nbmortne", "nbnesavant37", "nbnesmoins2500",
                                "niveauetudes", 
                              "pactivite", "parite", "parite.f", "parite.f2", 
                                "pathologieg", "perimetre", "perimetre2", "poids", 
                                "pprofession", "preeclampsie", "premconsult", "prepaccouchement", 
                                "presentation","rachieanesthesie", "rangnaissance", 
                               
                                "retardcroissance", "rupturemembrane", "sexe", "Source", 
                                "taille", "tailles", "distance_iris","distance_moy_pts",
                                "urgence","moyenne_benzene","moyenne_no2","mopb","mopn",
                                "moyenne_benzene.f", "moyenne_benzene.f2", "moyenne_benzene.f3", 
                                "mopb.f", "mopb.f2", "mopb.f3", "moyenne_no2.f", "moyenne_no2.f2", 
                                "mopn.f", "mopn.f2", "vb", "age.f", "poids.f", "agegestationnel.f", 
                                "coeffapgar5mncor", "coeffapgar5mncor.f","polymalformation",
                             "spinabifida",
                              "fente",
                              "atresie",
                              "omphalocele",
                              "reductionmembre",
                              "malformrenale",
                              "hydrocephalie",
                              "malformcard",
                              "trisomie"
 )]
 
 base_imput$coeffapgar5mncor<-as.factor(base_imput$coeffapgar5mncor)
 base_imput$gestite<-as.factor(base_imput$gestite)
 base_imput$prepaccouchement<-as.factor(base_imput$prepaccouchement)
 base_imput$presentation<-as.factor(base_imput$presentation)
 
 
 
 ini <- mice(base_imput, maxit = 0, print = FALSE)
 imput_method<-c(age="pmm", agegestationnel ="pmm", allaitement="", analgesieautr="", analgesiegene="", analgesieperi="", anomalie="", antibiotherapie="", 
   atresie="", aucune_analgesie ="", autre_patho="", bcg="", cas="", cesarienne="logreg", clartenucale="logreg", coeffapgar1mn="polyreg", 
   coeffapgar5mn="", 
   commune_famille_code="", commune_nais_code="", debuttravail="pmm", dep="", dep_nais="",echomorpho="", fente="",gestes_techniques="", 
   gestite="polyreg", gestite.f="", gestite.f2="", code_p="", mactivite="", menacepremature ="", mois_acc="",  mois_naiss_m="", 
   mprofession="", naissancepar="polyreg", 
   naissancepar.f2="", nbdecedeav28j="", 
   nbecho="pmm", nbenfant="pmm", nbfoetus="pmm", nbjourhopital="pmm", nbmortne="pmm",  nbnesavant37="pmm", nbnesmoins2500="pmm", 
   niveauetudes="",  pactivite="", parite="pmm", parite.f="", parite.f2="", pathologieg="", perimetre="", 
   perimetre2="pmm", poids="pmm", pprofession="", preeclampsie="", premconsult="pmm", prepaccouchement="logreg", presentation="polyreg", 
   rachieanesthesie="",  
   rangnaissance="pmm", retardcroissance="pmm",rupturemembrane="logreg",  sexe="", Source="", taille="", tailles="pmm", distance_iris="", distance_moy_pts="", 
   urgence="logreg", moyenne_benzene="", moyenne_no2="", 
   mopb="", mopn="", moyenne_benzene.f="", moyenne_benzene.f2="", moyenne_benzene.f3="", mopb.f="", mopb.f2 ="", mopb.f3="",
   moyenne_no2.f="", moyenne_no2.f2="", 
   mopn.f="", mopn.f2="", vb="", age.f="", 
   poids.f="", agegestationnel.f ="", coeffapgar5mncor="polyreg", coeffapgar5mncor.f="", polymalformation ="", spinabifida="", fente.1="", atresie.1="", omphalocele="", 
   reductionmembre="", malformrenale="", hydrocephalie="", malformcard="", trisomie="")
 
 imput_method["parite.f2"]<-"~I(as.factor(ifelse(parite >= 4,4,parite)))"
 imput_method["gestite.f2"]<-"~I(ifelse(gestite >= 4,4,gestite))"
 imput_method["age.f"]<-"~I(cut(age,breaks=c(0,25,30,35,40,80),include.lowest = T,right = F))"
# imput_method["naissancepar.f2"]<-"~I(as.factor(ifelse(naissancepar %in% c(cesar prog ,cesar urg,cesar sp ) & !is.na(naissancepar),cesar,naissancepar)))"
 #imput_method["vb"]<-"~I(as.factor(ifelse(naissancepar %in% c(vbi,vbni) & !is.na(naissancepar.f2),vb,naissancepar.f2)))"
imput_method["poids.f"]<-"~I(cut(poids,breaks=c(0,2500,3000,3500,4000,8000),include.lowest = T,right = F))"

# imput_method["coeffapgar5mncor.f"]<-"~I(ifelse(coeffapgar5mncor %in% c(9,10),9-10,coeffapgar5mncor))"
 #imput_method["coeffapgar5mncor.f"]<-"~I(ifelse(!coeffapgar5mncor %in% c(9,10) & !is.na(coeffapgar5mncor),9,coeffapgar5mncor.f))"
 imput_method["agegestationnel.f"]<-"~I(cut(agegestationnel,breaks=c(22,35,38,40,45),include.lowest = T,right = F))"
 
 pred<-ini$pred
 pred[c("niveauetudes","mprofession","pactivite","pprofession","mactivite","vb","coeffapgar5mn","nbdecedeav28j","taille","fente.1" ,"atresie.1","omphalocele","reductionmembre","malformrenale","hydrocephalie", "malformcard","trisomie" ,"coeffapgar5mncor.f","agegestationnel.f","poids.f","age.f","distance_moy_pts","distance_iris","tailles","rachieanesthesie","preeclampsie","perimetre2","pathologieg","parite.f2","parite.f","naissancepar.f2","gestite.f2","gestite.f","gestes_techniques","fente","bcg","autre_patho","aucune_analgesie","atresie","antibiotherapie","analgesieperi","analgesiegene","analgesieautr","anomalie","moyenne_benzene","moyenne_no2" ,"mopb", "mopn", "moyenne_benzene.f" ,"moyenne_benzene.f2" ,"moyenne_benzene.f3", "mopb.f" ,"mopb.f2" ,"mopb.f3", "moyenne_no2.f"),]<-0
 
 
 ta<-mice(base_imput,maxit=1,method=imput_method,pred=pred)
 
 
imput_ta<-complete(ta)
imput_ta$naissancepar.f2<-as.factor(ifelse( imput_ta$naissancepar %in% c("cesar prog" ,"cesar urg","cesar sp" ) & !is.na( imput_ta$naissancepar),"cesar", imput_ta$naissancepar))
imput_ta$vb<-as.factor(ifelse(imput_ta$naissancepar %in% c("vbi","vbni") & !is.na(imput_ta$naissancepar.f2),"vb",imput_ta$naissancepar.f2))
imput_ta$coeffapgar5mncor.f<-ifelse(imput_ta$coeffapgar5mncor %in% c(9,10),"9-10",imput_ta$coeffapgar5mncor)
imput_ta$coeffapgar5mncor.f<-ifelse(!imput_ta$coeffapgar5mncor %in% c(9,10) & !is.na(imput_ta$coeffapgar5mncor),"<9",imput_ta$coeffapgar5mncor.f)
