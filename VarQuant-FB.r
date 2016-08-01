# Frédérique BERGER
# 26/07/2013

# Fonction pour décrire plusieurs variables quantitatives 
# -----------------------------------------------------

#xc <- c("tt.bdca2.epi.moy", "tt.bdca2.epi.max", "tt.bdca2.chor.moy", "tt.bdca2.chor.max") # Vecteur de variables quantitatives
#nomx <- c("BDCA2 moy (Epith tum)", "BDCA2 max (Epith tum)", "BDCA2 moy (Chor tum)", "BDCA2 max (Chor tum)") # vecteur de noms de variables

# quantifb(xc, nomx, data,  RAPPORT=F,  SAVEFILE=F, chemin=NULL)
quantifb <- function(xc, nomx, data, RAPPORT=F,  SAVEFILE=F, chemin=NULL,fichier=NULL){
                                   # xc=vecteurs de variables quantitatives; 
                                   # nomx=vecteur de noms de variables quanti
                                   # SAVEFILE = si on veut sauvergarder le tableau dans excel
                                   # chemin=indique le dossier où sauvegarder le tableau (doit se terminer par un /)
t1 <- NULL
for(i in 1:length(xc)){
    l1 <- as.table(summary(data[, xc[i]]))
    et <- round(sd(data[, xc[i]], na.rm=T), 2)
        if( any(is.na(data[, xc[i]])) ) {  # A faire si certaines variables ont des NA et pas d'autres, sinon, le tableau n'a as le même nbre de col
                l1b <- c(l1)
        } else { 
                l1b <- c(l1, "")
                }
        l1c <- c(l1b, et)
        t1 <- rbind(t1, l1c)
    }
    rownames(t1) <- nomx  
    colnames(t1) <- c("Min","1st Quart","Median","Mean","3rd quart","Max","NA","SD")
    print(t1)
    # Rapport ASCII
    if (RAPPORT) r$add(ascii(t1, include.rownames = TRUE, include.colnames = TRUE, header=T))

    # sauvegarde de t1
    if (SAVEFILE) write.table(t1, sep="\t", file=paste(chemin, fichier,".xls") )
}




# Fonctions desc.quant et test.quant
# ----------------------------------

# Variables quantitatives en fonction de variables qualitatives:
# 2 Fonctions: une fonction descriptive seul et une fonction descriptive avec les tests.
# Dans les 2 cas, possibilité de créer le rapport ASCII en précisant RAPPORT=T/F


# "desc.quant" : Fonction qui décrit les variables quantitatives selon les classes de variables qualitatives

desc.quant <- function(varquant, varqual,  nomquant, nomqual, data, RAPPORT=F,  SAVEFILE=F, chemin=NULL){  
            # varquant = vecteur de variables quantitatives 
            # varqual = vecteur de variables qualitatives 
            # nomquant = vecteur de nom de variables quantitatives
            # nomqual = vecteur de nom de variables qualitatives
            # data=jeu de données
            # RAPPORT ASCII FALSE / TRUE
            # SAVEFILE=F si on ne veut pas sauvegarder le fichier dans un fichier excel, sinon SAVEFILE=T
            # si SAVEFILE=T, préciser l'emplacement de la sauvegarde
            
  for(i in 1:length(varquant)){
    x <- varquant[i]
    cat(nomquant[i],"\n")
    
        tabsu2 <- NULL
        tabres <- NULL
        for(j in 1:length(varqual)){
          y <- varqual[j] 

            # Création du tableau de résultats
            lev <-levels(data[,y])
            nlev <- nlevels(data[,y])
            lsu <- NULL
            tabsu <- NULL
            valid.ntot <- NULL
                for(i in 1:nlev){
                    d <- data[,x][which(data[,y]==lev[i])] # Vecteur des valeurs de x pour la classe i
                    n <- length(d)
                    valid.d <- d[!is.na(d)] #  Vecteur des valeurs de x pour la classe i sans les données manquantes
                    valid.n <- length(valid.d) # Nombre de patients sans NA pour la variable et la classe considérée
                    su <- summary(data[,x][which(data[,y]==lev[i])])
                    et <- round(sd(data[,x][which(data[,y]==lev[i])], na.rm=T),2)
                    valid.ntot <- c(valid.ntot, valid.n)
                    lsu <- c(valid.n, su[1], su[3], su[4], su[6], et, su[7])
                    tabsu <- rbind(tabsu, lsu) # tableau de description de la variable x selon les classes de y
                    }
            tabsu2 <- cbind(lev, tabsu)
            colnames(tabsu2) <- c("Categories", "N (sans NA)", "Min", "Median", "Mean", "Max", "sd", "NA") # Tableau d'une variable x 
                                                                                                                  #  en fc  des var y    
            tabres <- rbind(tabres, tabsu2)  # Tableau de la variable x en fonction de toutes les variables y
     
          }  # Fin de la boucle sur y
               
              nom2 <- NULL
              for (f in 1:length(varqual)) {
                    nom1 <- c(nomqual[f], rep("", nlevels(data[,varqual[f]])-1))
                    nom2 <- c(nom2 , nom1)
              } 
              rownames(tabres) <- nom2
              print(tabres)
              
              # Rapport ASCII
              if (RAPPORT) r$addParagraphs(sexpr(nomquant[i]))
              if (RAPPORT) r$add(ascii(tabres, include.rownames = TRUE, include.colnames = TRUE, header=T))
              
              # Sauvegarde au format excel
              if (SAVEFILE) write.table(tabres, sep="\t", file=paste(chemin, "tabres.xls", sep=""))
    } # Fin de la boucle sur x    
} # fin de la fonction


## -----------------------------------------------------------------------------


# "test.quant": Fonction qui compare les moyennes de variables quantitatives selon les classes d'une variable quali
#test.quant(il, varqual, "il", nomqual, data=, RAPPORT=F, SAVEFILE=T, chemin="sortie")
test.quant <- function(varquant, varqual,  nomquant, nomqual, data, RAPPORT=F,  SAVEFILE=F, chemin=NULL,fichier=NULL){  
        # varquant = vecteur de variables quantitatives 
        # varqual = vecteur de variables qualitatives 
        # nomquant = vecteur de nom de variables quantitatives
        # nomqual = vecteur de nom de variables qualitatives
        # data=jeu de données
        # PVAL: si on veut récupérer le vecteur de pvalue pour les corriger
        # RAPPORT ASCII FALSE / TRUE
        # SAVEFILE=F si on ne veut pas sauvegarder le fichier dans un fichier excel, sinon SAVEFILE=T
        # si SAVEFILE=T, préciser l'emplacement de la sauvegarde
      
  tabres3 <- NULL  
  pval3 <- NULL 
  for(i in 1:length(varquant)){
    x <- varquant[i]
    cat(nomquant[i],"\n")
      
        pval2 <- NULL     
        tabsu2 <- NULL
        tabres2 <- NULL
        
        for(j in 1:length(varqual)){
          y <- varqual[j] 
          
            # Création du tableau de résultats
            lev <-levels(data[,y])
            nlev <- nlevels(data[,y])
            lsu <- NULL
            tabsu <- NULL
            valid.ntot <- NULL
                for(k in 1:nlev){
                    d <- data[,x][which(data[,y]==lev[k])] # Vecteur des valeurs de x pour la classe i
                    n <- length(d)
                    valid.d <- d[!is.na(d)] #  Vecteur des valeurs de x pour la classe i sans les données manquantes
                    valid.n <- length(valid.d) # Nombre de patients sans NA pour la variable et la classe considérée
                    su <- summary(data[,x][which(data[,y]==lev[k])])
                    et <- round(sd(data[,x][which(data[,y]==lev[k])], na.rm=T),2)
                    valid.ntot <- c(valid.ntot, valid.n)
                    lsu <- c(valid.n, su[1], su[3], su[4], su[6], et, su[7])
                    tabsu <- rbind(tabsu, lsu) # tableau de description de la variable x selon les classes de y
                    }

            # Test de Bartlett pour vérifier l'homogénéité des variances
            if(any(valid.ntot<2)){
            } else {
                  bartlett <- bartlett.test(data[,x]~data[,y])
            }
    
            # choix de la méthode de test de comparaison de moyennes
            if(any(valid.ntot<2)){   
                  methode <- "Test impossible"
            } else {
                    if(nlev<=2 & all(valid.ntot)>30 & bartlett$p.value > 0.05) {
                       methode <- "Student"
                    } else { 
                            if(nlev<=2 & all(valid.ntot)>30 & bartlett$p.value < 0.05){
                                methode <- "Student Welch"
                            } else {
                                    if(nlev<=2 & any(valid.ntot)<30 & bartlett$p.value > 0.05){
                                        methode <- "Student Welch"
                                    } else { 
                                            if(nlev<=2 & any(valid.ntot)<30 & bartlett$p.value < 0.05){
                                                methode <- "Wilcoxon"
                                            } else { 
                                                    if(nlev>2 & all(valid.ntot)>30 & bartlett$p.value > 0.05) {
                                                        methode <- "ANOVA"
                                                    } else { 
                                                          methode <- "Kruskal-Wallis"
                                                            }
                                                    }
                                            }    
                                    }
                            }
                      }               
            # Réalisation du test choisi à l'étape précédente
            ind <- match(methode, c("Student", "Student Welch", "Wilcoxon", "ANOVA","Kruskal-Wallis", "Test impossible" ))
            test <- switch(ind, t.test(data[,x] ~ data[,y], var.equal=T), t.test(data[,x] ~ data[,y],var.equal=F), 
                            wilcox.test(data[,x] ~ data[,y]),  aov(data[,x] ~ data[,y]), kruskal.test(data[,x] ~ data[,y]), NA )
            pval <- test$p.value
            # Récupération de la pvalue pour la mise en forme dans un tableau
            if (methode=="Test impossible"){
                  p <- NA
            } else { 
                    if(test$p.value > 0.05 | (test$p.value < 0.05 & test$p.value > 0.001) ){
                        p <- round(test$p.value,3)
                    } else {
                            p <- "<0.001"
                            }
                    }
 
            # Tableau de résultat final
            if(nlev<=2){
                 restest <- c(methode, p) 
            } else {
                 restest <- c(methode, p, rep("", nlev-2) )
                    }             
                                
            tabres <- cbind(lev, tabsu,restest)
            colnames(tabres) <- c("Categories", "N (sans NA)", "Min", "Median", "Mean", "Max", "sd", "NA", "Test") # Tableau d'une variable x 
                                                                                                              #  en fc  des var y    
            tabres2 <- rbind(tabres2, tabres)  # Tableau de la variable x en fonction de toutes les variables y
            
            pval2 <- c(pval2, pval) # Pvalue des tests de la variable x en fc de toutes les variables y
          }  # Fin de la boucle sur y
              
              pval3 <- c(pval3, pval2) # Pvalue de tous les tests, pour le cas où on veut récupérer les pvalues pour les corriger      
                             
              nom2 <- NULL
              for (f in 1:length(varqual)) {
                    nom1 <- c(nomqual[f], rep("", nlevels(data[,varqual[f]])-1))
                    nom2 <- c(nom2 , nom1)
              } 
              rownames(tabres2) <- nom2
              print(tabres2)
              
              tabres3 <- rbind(tabres3, tabres2)
              #tabres3<-data.frame(tabres3)
              r<-dim(tabres3)[1]/length(varquant)
              variable<-rep(nomquant,each=r)
              tabres5<-cbind(variable,tabres3)
              
              # Rapport ASCII
              if (RAPPORT) r$addParagraphs(sexpr(nomquant[i]))
              if (RAPPORT) r$add(ascii(tabres2, include.rownames = TRUE, include.colnames = TRUE, header=T))
      
              # Sauvegarde au format excel
              if (SAVEFILE) write.table(tabres5, sep="\t",  file=paste(chemin,fichier,".xls"))
               
              
    } # Fin de la boucle sur x  
    return(pval3)
} # fin de la fonction

