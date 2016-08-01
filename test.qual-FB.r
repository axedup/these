# Frédérique BERGER
# 03/06/2013

# Croisement de 2 variables qualitatives à 2 ou plus modalités et test de comparaison choisi en fc des conditions d'application

# Utilise en partie le scripte issu du package biostat de la fonction Tableau, créé par Yann De Rycke, notamment pour le choix du test.

# La boucle fait un tableau par test (croisement de 2 variables) 
# Si on veut un tableau pour le croisement de x variables avec une variable y1 puis un nouveau tableau pour le croisement de x-1 variables 
# avec la variable y2, il faut insérer cette fonction dans une boucle: voir scripte à la fin de la fonction

# La fonction peut engendrer des "warnings" dans R dû au test du Chi2 :
                    # "In chisq.test(x, y, correct = F) : Chi-squared approximation may be incorrect"

# Utilisation de la fonction:
#toto <- test.qual(x,y,nomx, test=T)  x et y : variables au format facteur 

test.qual <- function(x,y,nomx, test=F, RAPPORT=F, SAVEFILE=F, chemin=NULL){ 
                               # x= variable en ligne et y= variable en colonne, nomx = nom de la variable x, test = T ou F
          t1na <- table(x,y, useNA="ifany")
          t1 <- table(x,y)
          csom <- apply(t1, 2, sum) # somme en colonne
          t1b <- rbind(t1, csom)
          lsom <- apply(t1b, 1,sum)# somme en ligne
          t1c <- cbind(t1b, lsom)
          colnames(t1c) <- c(levels(y), "total")
          rownames(t1c) <- c(levels(x), "total")
          t1p <-  round(prop.table(t1, 2)*100, 2)

          nx <- nlevels(x)
          ny <- nlevels(y)
          ddl <- prod(c(nx-1,ny-1))

          # calcul des attendus
          att <- NULL
          for( i in 1:nx){
                   att2 <- NULL
                          for ( j in 1:ny ){
                                att1 <-  prod(t1c[nx+1,j],t1c[i,ny+1])/t1c[nx+1, ny+1]
                                att2 <- c(att2, att1)
                          }
                  att <- c(att, att2)
          }

         # Choix du test
         if(any(lsom==0) | any(csom==0)) { # Quand une des classes d'une variable est nulle, pas de test possible
                methode <- "Aucune"
         } else {
                if (ddl == 1) {
                        if (all(att >= 5)) {
                                            CA <- T
                                            methode <- "Khi2"
                        } else {
                                if (all(att >= 3)) {
                                                    CA <- T
                                                    methode <- "Yates"
                                } else {
                                        if (any(att < 3)) {
                                                          CA <- T
                                                          methode <- "Fisher"
                                                          } else {
                                                                  CA <- F
                                                                  methode <- "Aucune"
                                                                  }
                                        }
                                }
                        } else {
                                methode <- "Khi2"
                                        if (any(att < 5)) {
                                                          CA <- F
                                                          } else {
                                                                 CA <- T
                                                                  }
                                }
                  }
            # Réalisation du test
            ind <- match(methode, c("Khi2", "Yates", "Fisher", "Aucune"))
            testq <- switch(ind, chisq.test(x,y, correct=F), chisq.test(x,y, correct=T), fisher.test(x,y), NA)
            p <- switch(ind, testq$p.value, testq$p.value, testq$p.value, NA)
                # récupération de la p-value 
                if (is.na(p)) {   # Cas où le test n'est pas possible
                        pval <- NA
                } else {          # Cas où le test est possible
                          if (p <0.001 & !is.na(p)){
                                                    pval <- "<0.001"
                                                    } else {
                                                            pval <- round(p, 3)
                                                            }
                        }

            # Mise en forme des résultats            
              # Pour ajouter les lignes de NA dans le tableau des proportions si les variables ont des NA
              if(any(is.na(x)) & !any(is.na(y))) { # cas où il n'y a des NA que dans la variable x
                        t1pc <- rbind(t1p, rep("",dim(t1)[2]))
              } else {
                      if(any(is.na(y)) & !any(is.na(x))) { # cas où il n'y a des NA que dans la variable y
                            t1pc <- cbind(t1p, rep("",dim(t1)[1]))
                      } else {
                               if( any(is.na(x)) & any(is.na(y)) ){ # cas où il y a des NA dans les variables x et y
                                  t1pb <- cbind(t1p, rep("", dim(t1)[1]))
                                  t1pc <- rbind(t1pb, rep("", dim(t1pb)[2]))
                                } else {
                                        t1pc <- t1p
                                        }
                                }
                      }

          # Tableau sans les résultats de test
          t2 <- NULL
          for (i in 1:dim(t1pc)[1]) {  # nbre de lignes
                lign <- NULL
                for (j in 1:dim(t1pc)[2]){ # nbre de colonne
                          if(t1pc[i,j]==""){
                                casei <- t1na[i,j]
                          } else {
                                casei <- paste(t1na[i,j],"(", t1pc[i,j], ")")
                                  }
                          lign <- c(lign, casei)
                }
                          lign2 <- c(levels(x)[i],lign)
                t2 <- rbind(t2, lign2)
          }
          # Nom des colonnes
          nomcol <- levels(y)

                  if (any(is.na(y))) {
                          colnames(t2) <- c("modalité", nomcol , "NA")
                  } else {
                          colnames(t2) <- c("modalité",  nomcol)
                          }
        # Nom des lignes
                  if (any(is.na(x))) {
                          rownames(t2) <- rep(nomx,nx+1)
                  } else {
                          rownames(t2) <- rep(nomx,nx)
                          }
        # Tableau avec les résultats de test
        if(test){
                if (any(is.na(x))) {
                        ptest <- c(paste(pval,"(",methode,")"), rep("", nx))
                } else {
                        ptest <- c(paste(pval,"(",methode,")"), rep("",nx-1))
                }
                t3 <- cbind(t2, ptest)
                colnames(t3) <- c(colnames(t2), "pvalue")
        } else {
                t3 <- t2
                }
    #print(t3)
    
        # Rapport ASCII
        if (RAPPORT) r$add(ascii(t3, include.rownames = TRUE, include.colnames = TRUE, header=T))
      
        # Sauvegarde au format excel
        if (SAVEFILE) write.table(t3,  sep="\t", file=paste(chemin, "t3.xls"))
          
          return(t3)
    
 }






 #toto <- test.qual(x,y,nomx, test=T)  x et y : variables au format facteur 

test.qualmoi <- function(x,y,nomx, test=F, RAPPORT=F, SAVEFILE=F, chemin=NULL){ 
                               # x= variable en ligne et y= variable en colonne, nomx = nom de la variable x, test = T ou F
          t1na <- table(x,y, useNA="ifany")
          t1 <- table(x,y)
          csom <- apply(t1, 2, sum) # somme en colonne
          t1b <- rbind(t1, csom)
          lsom <- apply(t1b, 1,sum)# somme en ligne
          t1c <- cbind(t1b, lsom)
          colnames(t1c) <- c(levels(y), "total")
          rownames(t1c) <- c(levels(x), "total")
          t1p <-  round(prop.table(t1, 2)*100, 2)

          nx <- nlevels(x)
          ny <- nlevels(y)
          ddl <- prod(c(nx-1,ny-1))

          # calcul des attendus
          att <- NULL
          for( i in 1:nx){
                   att2 <- NULL
                          for ( j in 1:ny ){
                                att1 <-  prod(t1c[nx+1,j],t1c[i,ny+1])/t1c[nx+1, ny+1]
                                att2 <- c(att2, att1)
                          }
                  att <- c(att, att2)
          }

         # Choix du test
         if(any(lsom==0) | any(csom==0)) { # Quand une des classes d'une variable est nulle, pas de test possible
                methode <- "Aucune"
         } else {
                if (ddl == 1) {
                        if (all(att >= 5)) {
                                            CA <- T
                                            methode <- "Khi2"
                        } else {
                                if (all(att >= 3)) {
                                                    CA <- T
                                                    methode <- "Yates"
                                } else {
                                        if (any(att < 3)) {
                                                          CA <- T
                                                          methode <- "Fisher"
                                                          } else {
                                                                  CA <- F
                                                                  methode <- "Aucune"
                                                                  }
                                        }
                                }
                        } else {
                                methode <- "Khi2"
                                        if (any(att < 5)) {
                                                          CA <- F
                                                          } else {
                                                                 CA <- T
                                                                  }
                                }
                  }
            # Réalisation du test
            ind <- match(methode, c("Khi2", "Yates", "Fisher", "Aucune"))
            testq <- switch(ind, chisq.test(x,y, correct=F), chisq.test(x,y, correct=T), fisher.test(x,y), NA)
            p <- switch(ind, testq$p.value, testq$p.value, testq$p.value, NA)
                # récupération de la p-value 
                if (is.na(p)) {   # Cas où le test n'est pas possible
                        pval <- NA
                } else {          # Cas où le test est possible
                          if (p <0.001 & !is.na(p)){
                                                    pval <- "<0.001"
                                                    } else {
                                                            pval <- round(p, 3)
                                                            }
                        }

            # Mise en forme des résultats            
              # Pour ajouter les lignes de NA dans le tableau des proportions si les variables ont des NA
              if(any(is.na(x)) & !any(is.na(y))) { # cas où il n'y a des NA que dans la variable x
                        t1pc <- rbind(t1p, rep("",dim(t1)[2]))
              } else {
                      if(any(is.na(y)) & !any(is.na(x))) { # cas où il n'y a des NA que dans la variable y
                            t1pc <- cbind(t1p, rep("",dim(t1)[1]))
                      } else {
                               if( any(is.na(x)) & any(is.na(y)) ){ # cas où il y a des NA dans les variables x et y
                                  t1pb <- cbind(t1p, rep("", dim(t1)[1]))
                                  t1pc <- rbind(t1pb, rep("", dim(t1pb)[2]))
                                } else {
                                        t1pc <- t1p
                                        }
                                }
                      }

          # Tableau sans les résultats de test
          t2 <- NULL
          for (i in 1:dim(t1pc)[1]) {  # nbre de lignes
                lign <- NULL
                for (j in 1:dim(t1pc)[2]){ # nbre de colonne
                          if(t1pc[i,j]==""){
                                casei <- t1na[i,j]
                          } else {
                                casei <- paste(t1na[i,j],"(", t1pc[i,j], ")")
                                  }
                          lign <- c(lign, casei)
                }
                          lign2 <- c(levels(x)[i],lign)
                t2 <- rbind(t2, lign2)
          }
          # Nom des colonnes
          nomcol <- levels(y)

                  if (any(is.na(y))) {
                          colnames(t2) <- c("modalité", nomcol , "NA")
                  } else {
                          colnames(t2) <- c("modalité",  nomcol)
                          }
        # Nom des lignes
                  if (any(is.na(x))) {
                          rownames(t2) <- rep(nomx,nx+1)
                  } else {
                          rownames(t2) <- rep(nomx,nx)
                          }
        # Tableau avec les résultats de test
        if(test){
                if (any(is.na(x))) {
                        ptest <- c(paste(pval,"(",methode,")"), rep("", nx))
                } else {
                        ptest <- c(paste(pval,"(",methode,")"), rep("",nx-1))
                }
                t3 <- cbind(t2, ptest)
                colnames(t3) <- c(colnames(t2), "pvalue")
        } else {
                t3 <- t2
                }
    print(t3)
    
      

    
 }
## -----------------------------------------------------------------------------

## TEST 1 variables quali versus une liste de variable quali:

#t5 <- NULL
#for (l in 1:length(vqual1)) {
#          x <- vqual1[l]
#          nomx <- nqual1[l]
#          t4 <- test.qual(data[,x], data$vqual2.f, nomx, test=T)
#          t5 <- rbind(t5, t4)
#}

# Il est possible de croiser une liste de variables quali avec  une autre liste de variables quali, à condition que les variables correspondant à 
# vqual2 aient toute le même nombre de catégories, sinon on ne peut pas faire un rbind des tableaux de résultats. 
# exemple:
#t6 <- NULL
#for(k in 1:length(vqual1)) { 
#    y <- vqual1[k]
#    nomy <- nomvqual1[k]
#    cat(nomy, "\n")
#    
#        t5 <-  NULL
#        for (l in 1:length(vqual2)) {
#            t4 <- NULL
#            x <- vqual2[l]
#            nomx <- nomvqual2[l]
#          
#            t4 <- test.qual(data[,x], data[,y], nomx, test=T)
#            t5 <- rbind(t5, t4)
#            } # fin de la boucle sur x
#      t6 <- rbind(t6, t5)
#              
#     # Sauvegarde au format excel
#    write.table(t6, file=paste("chemin", "t6.xls", sep="/"))      
#} # Fin de la boucle sur y  






# ------------------------------------------------------------------------------



# Scripte pour faire des tableaux de croisement de plusieurs variables à la fois dans ASCII

#v2 <- c("oms.f", "alb.f", "ca.15.3.f", "ace.f", "lympho.f", "ldh.f", "pini.f", "scbarb.f", "scbarb.f2", "rmh08.prono.f", "rmh10.prono.f",
#         "meta.foie.f", "meta.gg.f", "meta.poumon.f", "meta.os.f", "meta.autre.f")
#vnom <- c("OMS", "Albuminémie", "CA 15-3", "ACE", "Nbre de lymphocytes", "LDH", "PINI", "Score de Barbot (3 classes)", 
#            "Score de Barbot (2 classes)", "RMH 2008", "RMH 2010", "Méta au foie", "Méta ganglion", "Méta Poumon", "Méta OS", "Méta Autre")
#length(v2)
#length(vnom)
#
#     # Rapport
#    if (RAPPORT) r$addSection("Variables qualitatives",2)
#
#for (k in 1:(length(v2)-1)){
#      for (l in 2:length(v2)) {
#          x <- v2[l]
#          y <- v2[k]
#          nomx <- vnom[l]
#          cat(vnom[k])
#              if ( l <= k) {
#                     cat(k,l, x, y,"\n")
#              } else { 
#                      if (x==y) {   
#                        stop
#                      } else {   
#                          t4 <- NULL
#                          t4 <- test.qual(data[,x],data[,y],nomx, test=T)
#                      }
#              }
#          t5 <- rbind(t5, t4)
#      }
#      if (RAPPORT) r$addParagraphs(sexpr(vnom[k]))
#      if (RAPPORT) r$add(ascii(t5, include.rownames = TRUE, include.colnames = TRUE, format = "f", width=70, header=T)) 
#      t5 <- NULL
#      t4 <- NULL
#}
#    if (RAPPORT) r$create()
# 
