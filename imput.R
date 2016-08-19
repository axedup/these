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
 
 ini <- mice(base_imput, maxit = 0, print = FALSE)
 imput_method<-c(age="pmm", agegestationnel ="", allaitement="", analgesieautr="", analgesiegene="", analgesieperi="", anomalie="", antibiotherapie="", 
   atresie="", aucune_analgesie ="", autre_patho="", bcg="", cas="", cesarienne="logreg", clartenucale="logreg", coeffapgar1mn="polyreg", coeffapgar5mn="", 
   commune_famille_code="", commune_nais_code="", debuttravail="polyreg", dep="", dep_nais="",echomorpho="", fente="",gestes_techniques="", 
   gestite="polyreg", gestite.f="", 
   gestite.f2="", code_p="", mactivite="", menacepremature ="", mois_acc="",  mois_naiss_m="", mprofession="pmm", naissancepar="polyreg", 
   naissancepar.f2="", nbdecedeav28j="pmm", 
   
   nbecho="pmm", nbenfant="pmm", nbfoetus="pmm", nbjourhopital="pmm", nbmortne="pmm",  nbnesavant37="pmm", nbnesmoins2500="pmm", 
   niveauetudes="polyreg",  pactivite="pmm", parite="pmm", parite.f="", parite.f2="", pathologieg="", perimetre="", 
   perimetre2="pmm", poids="pmm", pprofession="pmm", preeclampsie="", premconsult="pmm", prepaccouchement="logreg", presentation="polyreg", rachieanesthesie="",  
   rangnaissance="pmm", retardcroissance="pmm",rupturemembrane="logreg",  sexe="", Source="", taille="", tailles="pmm", distance_iris="", distance_moy_pts="", 
   urgence="logreg", moyenne_benzene="", moyenne_no2="", 
   mopb="", mopn="", moyenne_benzene.f="", moyenne_benzene.f2="", moyenne_benzene.f3="", mopb.f="", mopb.f2 ="", mopb.f3="", moyenne_no2.f="", moyenne_no2.f2="", 
   mopn.f="", mopn.f2="", vb="logreg", age.f="", 
   poids.f="", agegestationnel.f ="", coeffapgar5mncor="polyreg", coeffapgar5mncor.f="", polymalformation ="", spinabifida="", fente.1="", atresie.1="", omphalocele="", 
   reductionmembre="", malformrenale="", hydrocephalie="", malformcard="", trisomie="")
 
 pred<-ini$pred
 pred[c("taille","fente.1" ,"atresie.1","omphalocele","reductionmembre","malformrenale","hydrocephalie", "malformcard","trisomie" ,"coeffapgar5mncor.f","agegestationnel.f","poids.f","age.f","distance_moy_pts","distance_iris","tailles","rachieanesthesie","preeclampsie","perimetre2","pathologieg","parite.f2","parite.f","naissancepar.f2","gestite.f2","gestite.f","gestes_techniques","fente","bcg","autre_patho","aucune_analgesie","atresie","antibiotherapie","analgesieperi","analgesiegene","analgesieautr","anomalie","moyenne_benzene","moyenne_no2" ,"mopb", "mopn", "moyenne_benzene.f" ,"moyenne_benzene.f2" ,"moyenne_benzene.f3", "mopb.f" ,"mopb.f2" ,"mopb.f3", "moyenne_no2.f"),]<-0
 
 
 ta<-mice(base_imput,maxit=1,method=imput_method,pred=pred)
 