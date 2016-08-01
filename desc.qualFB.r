

reorder<-gdata::reorder.factor
# Description de chaque variable quali une  à  une et résultat dans un tableau global
      # Donne un tableau avec n et % en colonne et les noms des levels en ligne. 
quali <- function(x, nomx, data,fichier, RAPPORT=F, SAVEFILE=F, chemin=NULL,ordonner,seq=NULL,numerique){   # x = variable ou vecteur de variable en factor
                                               # nomx = nom de la variable ou vecteur de nom de variable 
                                               # data= jeu de données
                                               # RAPPORT = F si pas de rapport ascii ou T si il y a un rapport ascii
                                               # SAVEFILE=F si on ne veut pas sauvegarder le fichier dans un fichier excel, sinon SAVEFILE=T
                                               # si SAVEFILE=T, préciser l'emplacement de la sauvegarde
        t4 <- NULL
        for (i in 1:length(x)){
          t3 <- NULL
          v <- data[ ,x[i]]
          t1 <- table(v, useNA="always")
          t2 <- c(round(prop.table(t1[1:(nlevels(v))])*100, 1), "-")
          t3 <- cbind(t1, t2)
         
          colnames(t3) <- c("n","%")
          rownames(t3) <- c(paste(nomx[i], levels(v), sep=": "), paste(nomx[i], "NA", sep=": "))
          t3<-data.frame(t3)
        if(ordonner[i]) {
         
            t3$rn<- rownames(t3)
            t3$rn<- as.factor(t3$rn)
            vi<-reorder(t3$rn,new.order=seq[[i]])
        
            
             t3<-t3[match(unlist(dimnames(table(vi))),t3$rn),]
          }
          
        if(numerique[i]) {
    
          t3$num<- as.numeric(as.character(t3$n))
          vi<-sort(t3$num,decreasing = T)
          
          t3<-t3[match(vi,t3$num),]
          

         
          
        }
          
          t3<-t3[,1:2]
           t4 <- rbind(t4, t3)
          }      
      
        
      # Reorder (gdata)  
        
        print(t4)
      
      
      
      # Rapport ASCII
      if (RAPPORT) r$add(ascii(t4, include.rownames = TRUE, include.colnames = TRUE, width=60, header=T))
      
      # Sauvegarde au format excel
      if (SAVEFILE) write.table(t4,  sep="\t", file=paste(chemin, fichier,".xls",sep=""))
    

    } # fin de la fonction

#Exemple: 
#        x <- c("age.f", "histo2.f")
#        nomx <- c("Age", "Histologie")
#        quali(x, nomx, dtg)      
#



# Description qualitative 2*2 variables

#x <- barbot$rmh10.prono.f
#y <- barbot$scbarb.f
#nomx <- "RMH 2010"
#nomy <- "score de Barbot"
#
#tata <- desc.qual(barbot$rmh10.prono.f, barbot$scbarb.f,"RMH 2010")


desc.qual <- function(x,y,nomx, RAPPORT=F, SAVEFILE=F, chemin=NULL){  # x= variable en ligne et y= variable en colonne, nomx = nom de la variable x
          t1na <- table(x,y, useNA="ifany")   # MARG = 1 ou 2 selon le sens du calcul des %
          t1 <- table(x,y)
#          csom <- apply(t1, 2, sum) # somme en colonne
#          t1b <- rbind(t1, csom)
#          lsom <- apply(t1b, 1,sum)# somme en ligne
#          t1c <- cbind(t1b, lsom)
#          colnames(t1c) <- c(levels(y), "total")
#          rownames(t1c) <- c(levels(x), "total")
          
          t1p <-  round((prop.table(t1, 2)*100), 2)

              # Pour ajouter les lignes de NA dans le tableau des proportions si les variables ont des NA
              if(any(is.na(x)) & !any(is.na(y))) {
                        t1pc <- rbind(t1p, rep("",dim(t1)[2]))
              } else {
                      if(any(is.na(y)) & !any(is.na(y))) {
                            t1pc <- cbind(t1p, rep("",dim(t1)[1]))
                      } else {
                               if( any(is.na(x)) & any(is.na(y)) ){
                                  t1pb <- cbind(t1p, rep("", dim(t1)[1]))
                                  t1pc <- rbind(t1pb, rep("", dim(t1pb)[2]))
                                } else {
                                        t1pc <- t1p
                                        }
                                }
                      }

          # Tableau de résultats mis en forme sans les résultats de test
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
                      # }
                  if (any(is.na(y))) {
                          colnames(t2) <- c("modalité", nomcol, "NA")
                  } else {
                          colnames(t2) <- c("modalité", nomcol)
                          }
        # Nom des lignes
        rownames(t2) <- c(nomx,rep("", dim(t2)[1]-1))

        print(t2)
         
         # Rapport ASCII
        if (RAPPORT) r$add(ascii(t2, include.rownames = TRUE, include.colnames = TRUE, width=60, header=T))

         # Sauvegarde au format excel
         if (SAVEFILE) write.table(t2, sep="\t", file=paste(chemin_res, "t2.xls", sep=""))

 }


#### # Description qualitative 1 variables quali vs une liste d'autres variables quali

desc.qual2 <- function(x,y,nomx, data, RAPPORT=F, SAVEFILE=F, chemin=NULL){ 
                         # x= vesteur de variables en facteur (seront en ligne dans le tableau)
                         # y = variable en colonne de la forme data$trucmuche,
                         # vecteur de nom des variables x

        t3 <- NULL
        for(i in 1:length(x)){

          t1na <- table(data[,x[i]],y, useNA="ifany")   # MARG = 1 ou 2 selon le sens du calcul des %
          t1 <- table(data[,x[i]],y)
#          csom <- apply(t1, 2, sum) # somme en colonne
#          t1b <- rbind(t1, csom)
#          lsom <- apply(t1b, 1,sum)# somme en ligne
#          t1c <- cbind(t1b, lsom)
#          colnames(t1c) <- c(levels(y), "total")
#          rownames(t1c) <- c(levels(x), "total")

          t1p <-  round((prop.table(t1, 2)*100), 2)

              # Pour ajouter les lignes de NA dans le tableau des proportions si les variables ont des NA
              if( any(is.na(data[,x[i]])) & !any(is.na(y)) ) {
                        t1pc <- rbind(t1p, rep("",dim(t1)[2]))
              } else {
                      if( any(is.na(y)) & !any(is.na(data[,x[i]])) ) {
                            t1pc <- cbind(t1p, rep("",dim(t1)[1]))
                      } else {
                               if( any(is.na(data[,x[i]])) & any(is.na(y)) ){
                                  t1pb <- cbind(t1p, rep("", dim(t1)[1]))
                                  t1pc <- rbind(t1pb, rep("", dim(t1pb)[2]))
                                } else {
                                        t1pc <- t1p
                                        }
                                }
                      }

          # Tableau de résultats mis en forme sans les résultats de test
          t2 <- NULL
          for (k in 1:dim(t1pc)[1]) {  # nbre de lignes
                lign <- NULL
                for (j in 1:dim(t1pc)[2]){ # nbre de colonne
                          if(t1pc[k,j]==""){
                                casei <- t1na[k,j]
                          } else {
                                casei <- paste(t1na[k,j],"(", t1pc[k,j], ")")
                                  }
                          lign <- c(lign, casei)
                }
                          lign2 <- c(levels(data[,x[i]])[k],lign)
                t2 <- rbind(t2, lign2)
          }
          # Nom des colonnes
          nomcol <- levels(y)
                      # }
                  if (any(is.na(y))) {
                          colnames(t2) <- c("modalité", nomcol, "NA")
                  } else {
                          colnames(t2) <- c("modalité", nomcol)
                          }
        # Nom des lignes
        rownames(t2) <- c(nomx[i],rep("", dim(t2)[1]-1))
        t3<- rbind(t3, t2)
       
        }

        print(t3)
         # Rapport ASCII
        if (RAPPORT) r$add(ascii(t3, include.rownames = TRUE, include.colnames = TRUE, width=60, header=T))

         # Sauvegarde au format excel
         if (SAVEFILE) write.table(t3, sep="\t", file=paste(chemin_res, "t3.xls", sep=""))

 }
