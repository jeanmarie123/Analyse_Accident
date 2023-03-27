
# cgargement library ggplot2
library(ggplot2)

# Chargement de la library questionr contenant la fonction freq 
library(questionr)

# chargement package 'stargazer'
library(stargazer)

library(car)
library(lattice)

library(BioStatR)

library(Factoshiny)

library(FactoMineR)

library(tidyverse)

summary(Df_Acc_copie_F)

# Calulons l'effectif
frequence <- freq(Df_Acc_copie_F$catu)
frequence

frequence_2 <- freq(Df_Acc_copie_F$trajet)
frequence_2

frequence_3 <- freq(Df_Acc_copie_F$catr)
frequence_3

frequence_4 <- freq(Df_Acc_copie_F$circ)
frequence_4

frequence_5 <- freq(Df_Acc_copie_F$prof)
frequence_5

frequence_6 <- freq(Df_Acc_copie_F$situ)
frequence_6

frequence_7 <- freq(Df_Acc_copie_F$lum)
frequence_7

frequence_8 <- freq(Df_Acc_copie_F$agg)
frequence_8

frequence_9 <- freq(Df_Acc_copie_F$col)
frequence_9

frequence_10 <- freq(Df_Acc_copie_F$catv)
frequence_10


# traçons le diagrame en bar 

barplot(frequence$`val%`, ylab = "Valeur pourcentage",  
        names.arg = row.names(frequence))

barplot(frequence_2$`val%`, ylab = "Valeur pourcentage",  
        names.arg = row.names(frequence_2))


barplot(frequence_3$`val%`, ylab = "Valeur pourcentage",  
        names.arg = row.names(frequence_3))

barplot(frequence_4$`val%`, ylab = "Valeur pourcentage",  
        names.arg = row.names(frequence_4))

barplot(frequence_5$`val%`, ylab = "Valeur pourcentage",  
        names.arg = row.names(frequence_5))

barplot(frequence_7$`val%`, ylab = "Valeur pourcentage",  
        names.arg = row.names(frequence_7))

barplot(frequence_8$`val%`, ylab = "Valeur pourcentage",  
        names.arg = row.names(frequence_8))

barplot(frequence_9$`val%`, ylab = "Valeur pourcentage",  
        names.arg = row.names(frequence_9))

barplot(frequence_10$`val%`, ylab = "Valeur pourcentage",  
        names.arg = row.names(frequence_10))

#ggplot(data, aes(x = trajet)) + geom_bar()

#affichage des indicateurs statistique pour les variables quantitative 


stargazer(Df_Acc_copie_F[c("Age")], 
          type = "text", 
          summary.stat  = c("n", "min", "p25", "median", "mean", "p75", "max", 
                            "sd"))

Boxplot( ~ Age, data = Df_Acc_copie_F, main = "Boite à moustaches variable Age")


n <- nrow(Df_Acc_copie_F)#nombre d'individus
k <- ceiling(1 + log(n)/log(2))#nombre de classes
## construction de l'histogramme
histogram(Df_Acc_copie_F$Sexe_Féminin, nint = k, type = "density")



ggplot(Df_Acc_copie_F, aes (x =  dep)) + geom_bar()



# On va chercher à faire l'analyse bivariée
summary(Df_Acc_copie_F)

#grav <- Df_Acc_copie_F$grav
#col <- Df_Acc_copie_F$col
#Df_Acc_copie_F$grav <- as.character(Df_Acc_copie_F$grav)
#Df_Acc_copie_F$col <- as.character(Df_Acc_copie_F$col)
#ggplot(Df_Acc_copie_F, aes (x = grav, fill = col)) + geom_bar(position = "dodge")

ggplot(Df_Acc_copie_F) +
  aes (x = grav, fill = col) + 
  geom_bar(position = "dodge") +
  xlab("Gravité accident (Victime)") + 
  ylab("Effectif") +
  labs(fill = " Type de Collision")

# Calcul du rapport de correlation eta2
eta2(Df_Acc_copie_F$grav, Df_Acc_copie_F$col)


# Test de l'association
anova(lm(Df_Acc_copie_F$grav~Df_Acc_copie_F$col))


ggplot(Df_Acc_copie_F) +
  aes (x = agg, fill = lum) + 
  geom_bar(position = "dodge") +
  xlab("Localisation") + 
  ylab("Effectif") +
  labs(fill = "Condition d'éclairage")

chisq.test(Df_Acc_copie_F$agg, Df_Acc_copie_F$lum)

ggplot(Df_Acc_copie_F) +
  aes (x = grav, fill = circ) + 
  geom_bar(position = "dodge") +
  xlab("Gravité accident (Victime)") + 
  ylab("Effectif") +
  labs(fill = "Circulation")

chisq.test(Df_Acc_copie_F$grav, Df_Acc_copie_F$circ)

eta2(Df_Acc_copie_F$grav, Df_Acc_copie_F$circ)
anova(lm(Df_Acc_copie_F$grav~Df_Acc_copie_F$circ))

ggplot(Df_Acc_copie_F) +
  aes (x = grav, fill = trajet) + 
  geom_bar(position = "dodge") +
  xlab("Gravité accident (Victime)") + 
  ylab("Effectif") +
  labs(fill = "Tajet")


eta2(Df_Acc_copie_F$grav, Df_Acc_copie_F$trajet)
anova(lm(Df_Acc_copie_F$grav~Df_Acc_copie_F$trajet))


ggplot(Df_Acc_copie_F) +
  aes (x = grav, fill = catu) + 
  geom_bar(position = "dodge") +
  xlab("Gravité accident (Victime)") + 
  ylab("Effectif") +
  labs(fill = "Catégorie Usagers")



# liaison entre gravité et situation accident
anova(lm(Df_Acc_copie_F$grav~Df_Acc_copie_F$situ))


# liaison entre gravité et catégorie de route
anova(lm(Df_Acc_copie_F$grav~Df_Acc_copie_F$catr))

# liaison entre gravité et catégorie categorie usagers
anova(lm(Df_Acc_copie_F$grav~Df_Acc_copie_F$catu))

# liaison entre gravité et infrastructure
anova(lm(Df_Acc_copie_F$grav~Df_Acc_copie_F$infra))

# liaison entre gravité accident et sexe
anova(lm(Df_Acc_copie_F$grav~Df_Acc_copie_F$sexe))

# liaison entre gravité et Age
anova(lm(Df_Acc_copie_F$grav~Df_Acc_copie_F$Age))

# liaison entre categorie et vitesse
anova(lm(Df_Acc_copie_F$vma~Df_Acc_copie_F$catr))

# liaison entre gravité et vitesse
anova(lm(Df_Acc_copie_F$vma~Df_Acc_copie_F$grav))

# liaison entre type de collision et lumière
chisq.test(Df_Acc_copie_F$col, Df_Acc_copie_F$lum)

# Liason entre gravité accident et categorie usager
chisq.test(Df_Acc_copie_F$grav, Df_Acc_copie_F$catu)

#eta2(Accident_2021_IDF_F$grav, Accident_2021_IDF_F$catu)


# ACM 
#D <- Factoshiny::MCAshiny(Df_Acc_copie_F)

#Da <- FactoMineR::MCA(Df_Acc_copie_F)



chisq.test(Df_Acc_copie_F$col, Df_Acc_copie_F$lum)
summary(Df_Acc_copie_F) 

frequence_12 <- freq(Df_Acc_copie_F$catu)
frequence_12

frequence_13 <- freq(Df_Acc_copie_FF$trajet)
frequence_13

frequence_14 <- freq(Df_Acc_copie_F$catr)
frequence_14

frequence_15 <- freq(Df_Acc_copie_F$circ)
frequence_15

frequence_16 <- freq(Df_Acc_copie_F$vosp)
frequence_16

frequence_17 <- freq(Df_Acc_copie_F$prof)
frequence_17

frequence_18 <- freq(Df_Acc_copie_F$plan)
frequence_18

frequence_19 <- freq(Df_Acc_copie_F$surf)
frequence_19

frequence_20 <- freq(Df_Acc_copie_F$infra)
frequence_20

frequence_21 <- freq(Df_Acc_copie_F$situ)
frequence_21

frequence_22 <- freq(Df_Acc_copie_F$lum)
frequence_22

frequence_23 <- freq(Df_Acc_copie_F$agg)
frequence_23

frequence_24 <- freq(Df_Acc_copie_F$int)
frequence_24

frequence_25 <- freq(Df_Acc_copie_F$atm)
frequence_25

frequence_26 <- freq(Df_Acc_copie_F$col)
frequence_26

# Copie du dataframe
#Df_Acc <- Accident_2021_IDF_F
#Df_Acc

#summary(Df_Acc)

#df <- freq(Df_Acc$trajet)
#df

# REgroupons les modalités 
#Df_Acc$trajet <- as.character(Df_Acc$trajet)
#Df_Acc$trajet[Df_Acc$trajet %in% c("Courses – achats", 
#                                   "Domicile – ecole")] <- "Autre"


#Df_Acc$vosp <- as.character(Df_Acc$vosp)
#Df_Acc$vosp[Df_Acc$vosp %in% c("Voie reservée")] <- "Bande cyclable"


#Df_Acc$catr <- as.character(Df_Acc$catr)
#Df_Acc$catr[Df_Acc$catr %in% c("Parc de stationnement ouvert à la circulation publique",
                               "autre", "Hors réseau public")] <- "Route nationale"



#Df_Acc$prof <- as.character(Df_Acc$prof)
#Df_Acc$prof[Df_Acc$prof %in% c("Bas de cote")] <- "Sommet de cote"


#Df_Acc$plan <- as.character(Df_Acc$plan)
#Df_Acc$plan[Df_Acc$plan %in% c("En S")] <- "En courbe à droite"


#Df_Acc$surf <- as.character(Df_Acc$surf)
#Df_Acc$surf[Df_Acc$surf %in% c("Boue", "Corps gras huile", "Corps gras huile", 
#                               "Enneigée", "Flaques", "Autre",  
#                               "Inondée", "Verglacée")] <- "Mouillée"

#Df_Acc$situ <- as.character(Df_Acc$situ)
#Df_Acc$situ[Df_Acc$situ %in% c("Sur autre voie spéciale", 
#                               "Sur bande d’arrêt d’urgence", 
#                               "Autres")] <- "Sur accotement"


#Df_Acc$infra <- as.character(Df_Acc$infra)
#Df_Acc$infra[Df_Acc$infra %in% c("Bretelle d échangeur ou de raccordement", 
#                               "Chantier", "Pont autopont", "Voie ferrée", 
#                               "Zone de péage")] <- "zonne pietonne"


#Df_Acc$lum <- as.character(Df_Acc$lum)
#Df_Acc$lum[Df_Acc$lum %in% c("Nuit avec eclairage public eteint"
#                             )] <- "Nuit sans eclairage public"


#Df_Acc$int <- as.character(Df_Acc$int)
#Df_Acc$int[Df_Acc$int %in% c("Passage à niveau", "Giratoire","Place", 
#                             "Intersection à plus de 4 branches")] <- "Autre intersection"



#Df_Acc$atm <- as.character(Df_Acc$atm)
#Df_Acc$atm[Df_Acc$atm %in% c("Brouillard_fumée", "Neige_grêle", 
#                             "Temps éblouissan", "Pluie forte",
#                             "Vent fort_tempête")] <- "Temps éblouissant"

#freq(Df_Acc$atm)

#Df_Acc$trajet.reg <- NULL

#class(Df_Acc$grav)

#summary(Df_Acc)

summary(Df_Acc_copie_F)

frequence_31 <- freq(Df_Acc_copie_F$grav)
frequence_31


Df_acm <- Factoshiny(Df_Acc_copie_F)

#Df_123 <- as.factor(Df_Acc_copie_F)
#Df_123
#summary(Df_123)

#res.mca <- MCA(Df_123, ncp = 14, graph = FALSE)
#hc <- HCPC(res.mca, kk = 100, description = FALSE, graph = FALSE)
#plot(hc, choice = "tree")
#plot(hc, choice = "map", draw.tree = FALSE)
#plot(hc, choice = "3D.map")
#catdes(hc$Df_123.clust, ncol(hc$Df_123.clust))
