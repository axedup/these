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

 base_imput<-cas_temoinsexpoi[c( "age",  "agegestationnel", 
                                "alcool", "allaitement", "an_acc", "an_naiss_m", "analgesieautr", 
                                "analgesiegene", "analgesieperi", "anomalie", "antibiotherapie", 
                                "atresie", "aucune_analgesie", "autre_patho", "autrez", "bcg", 
                                "cas", "cesarienne", "clartenucale", "code_p", "coeffapgar1mn", 
                                "coeffapgar5mn", "commune", "commune_famille_code", "commune_nais_code", 
                                
                                "debuttravail", "dep", "dep_nais",
                                 "echomorpho", "fente", "gestes_techniques", 
                                "gestite", "gestite.f", "gestite.f2", "hepatitebinj", "hepatitebvacc", 
                                "hydrocephalie",
                                "intubation", "inutero", 
                                "mactivite", "malformcard", "malformrenale",  
                                 "menacepremature", "mois_acc", "mois_naiss_m", 
                                "mopb", "mopn", "moyenne_benzene", 
                                "moyenne_no2", "mprofession",  "naissancepar", "naissancepar.f2", 
                                "nbdecedeav28j", "nbecho", "nbenfant", "nbfoetus", "nbjourhopital", 
                                "nbmortne", "nbnesavant37", "nbnesmoins2500", "neurologique", 
                                "niveauetudes", "nom_v", "np", "num", "num_rue", "num_v", "omphalocele", 
                                "oxygenotherapie", "pactivite", "parite", "parite.f", "parite.f2", 
                                "pathologieg", "perimetre", "perimetre2", "poids", "polymalformation", 
                                "pprofession", "preeclampsie", "premconsult", "prepaccouchement", 
                                "presentation", "prevoie", "rachieanesthesie", "rangnaissance", 
                                "rechaghbs",
                                "retardcroissance", "rupturemembrane", "sexe", "Source", "spinabifida", 
                                "tabac", "taille", "tailles", "testauditif", "transfert", 
                                "trisomie", "typetransfert", "urgence","distance_iris", 
                                "moyenne_benzene.f", "moyenne_benzene.f2", "moyenne_benzene.f3", 
                                "mopb.f", "mopb.f2", "mopb.f3", "moyenne_no2.f", "moyenne_no2.f2", 
                                "mopn.f", "mopn.f2", "vb", "age.f", "poids.f", "agegestationnel.f", 
                                "coeffapgar5mncor", "coeffapgar5mncor.f")]