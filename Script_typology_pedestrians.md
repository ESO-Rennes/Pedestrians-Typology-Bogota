---
title: "Construction d'une typologie de piétons à partir d'une enquête ménages déplacements et représentation cartographique des profils : l'exemple de Bogotá"
author: "[Arthur Ducasse](https://perso.univ-rennes2.fr/arthur.ducasse), [Florent Demoraes](https://perso.univ-rennes2.fr/florent.demoraes)"
date: "2023-03-03"

---

```r setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = 'F:/Rennes/Recherche/Thèses_suivies/Arthur Ducasse/Article_Typo_pietons')
```

# Calcul des profils de piétons au moyen d'une AFDM et d'une CAH
## Chargement des packages nécessaires
```r loading packages, warning=FALSE, message=FALSE, error=FALSE
#Chargement des packages
library(FactoMineR) # Pour realiser une analyse multivariee
library(openxlsx) # Pour importer/exporter depuis/vers Excel
library(factoextra) # pour produire des graphiques esthetiques a partir des resultats de l'AFDM
library(ggplot2) # package necessaire au fonctionnement de factoextra
library(sf) # pour manipuler les objets ayant une composante spatiale
library(spatstat) # pour le lissage spatial
library(sp) # pour utiliser la fonction as.owin de spatstat
library(maptools) # pour utiliser la fonction as.owin de spatstat
library(cartography) # pour la representation cartographique et l'habillage des cartes
library(raster) # pour le traitement de donnees matricielles
library(tidyverse) # pour utiliser la fonction "separate"
```

## Import du jeu de données et résumé statistique
```r loading dataset, message=FALSE, error=FALSE
VariablesTypoUltraMarcheurs <- read.xlsx("02_analyse_ultra_marcheurs_v6.xlsx")
rownames(VariablesTypoUltraMarcheurs) <- VariablesTypoUltraMarcheurs$id_hog_pers # pour ajouter l'id des personnes comme entête de ligne
str(VariablesTypoUltraMarcheurs) # Pour connaître le type des variables et avoir un aperçu des valeurs prises pour chaque variable
head(VariablesTypoUltraMarcheurs)
summary(VariablesTypoUltraMarcheurs) # Résumé des valeurs contenues dans l'objet
```

## Apurement du jeu de données
```r dataset cleaning
VariablesTypoUltraMarcheurs <- na.omit(VariablesTypoUltraMarcheurs) # suppression des individus pour lesquels des valeurs ne sont pas renseignées

VariablesTypoUltraMarcheurs <- VariablesTypoUltraMarcheurs[which(VariablesTypoUltraMarcheurs$num_bici <= 10),] # suppression des individus résidant dans des ménages dans lesquels il y a plus de 10 vélos (situation peu probable, sans doute des erreurs de saisie)

VariablesTypoUltraMarcheurs <- VariablesTypoUltraMarcheurs[which(VariablesTypoUltraMarcheurs$calif_exp <= 5),] # suppression des individus ayant des qualifications d'experiences de trajet superieures à 5

VariablesTypoUltraMarcheurs <- VariablesTypoUltraMarcheurs[which(VariablesTypoUltraMarcheurs$duracion_prom <= 300),] # suppression des individus ayant une durée moyenne de déplacement à pied supérieure à 5 heures (300 minutes)

```

## Suppression des colonnes inutiles pour l'AFDM
```r variable suppression
# colonnes d'identification des individus
VariablesTypoUltraMarcheurs$id_hog <- NULL 
VariablesTypoUltraMarcheurs$id_hog_pers <- NULL

# colonnes contenant des indications de localisation spatiale
VariablesTypoUltraMarcheurs$ZAT_Dest_1er_viaje_desde_casa <- NULL 

# Autres colonnes 
VariablesTypoUltraMarcheurs$f_exp <- NULL
View(VariablesTypoUltraMarcheurs) # Resume variables
summary(VariablesTypoUltraMarcheurs)
str(VariablesTypoUltraMarcheurs)
```

## Calcul de la première AFDM (Analyse factorielle des Données Mixtes) 
```r first FAMD, message=FALSE, error=FALSE, warning=FALSE, fig.height=15, fig.width=15
res <- FAMD(VariablesTypoUltraMarcheurs, ncp = 10, graph = FALSE)

#Affichage des plans factoriels
plot(res, (choix = c("ind", "var", "quanti", "quali")), axes = c(1, 2), lab.var = TRUE, lab.ind = FALSE, autoLab = "yes")
plot(res, choix = c("var"), axes = c(1, 2), lab.var = TRUE, lab.ind = FALSE, autoLab = "yes") 
plot(res, choix = c("quanti"), axes = c(1, 2), lab.var = TRUE, lab.ind = FALSE, autoLab = "yes") 
plot(res, choix = c("quali"), axes = c(1, 2), lab.var = TRUE, lab.ind = FALSE, autoLab = "yes")

#Affichage de la fréquence cumulée des valeurs propres
get_eigenvalue(res)
fviz_screeplot(res)

```

## Affichage des variables pour en placer en supplémentaire
```r view variables
print(as.data.frame(colnames(VariablesTypoUltraMarcheurs)))
```
## Calcul d'une deuxième AFDM après passage des variables peu discriminantes en supplémentaire
```r second FAMD, message=FALSE, warning=FALSE, error=FALSE, fig.height=15, fig.width=15

res <- FAMD(VariablesTypoUltraMarcheurs, ncp = 6, sup.var = c(3, 4, 17), graph = FALSE) ##sup.var signifie qu'on passe les variables calif_exp (3), duracion_prom (4) et num_moto (17) en supplémentaires. ncp = 6 --> on ne garde que 6 axes, qui expliquent déjà 40% de l'inertie totale.

#Affichage des plans factoriels
plot(res, (choix = c("ind", "var", "quanti", "quali")), axes = c(1, 2), lab.var = TRUE, lab.ind = FALSE, autoLab = "yes", palette=palette(c("grey","red","blue")), title = "Factor map of individuals (pedestrians) and categories")
plot(res, choix = c("var"), axes = c(1, 2), lab.var = TRUE, lab.ind = FALSE, autoLab = "yes", title = "Variable's factor map", palette=palette(c("grey","red","blue"))) 
plot(res, choix = c("quanti"), axes = c(1, 2), lab.var = TRUE, lab.ind = FALSE, autoLab = "yes") 
plot(res, choix = c("quali"), axes = c(1, 2), lab.var = TRUE, lab.ind = FALSE, autoLab = "yes")

#Affichage de la fréquence cumulée des valeurs propres
get_eigenvalue(res)
fviz_screeplot(res)

```
## Calcul de la CAH (Classification Ascendante Hiérarchique) et graphiques
```r clustering calculation, message=FALSE, error=FALSE, warning=FALSE, fig.height=20, fig.width=20
nbclasses <- 6 # Definition du nombre de classes. 
res.hcpc <- HCPC(res, kk=Inf, nb.clust=nbclasses, consol=FALSE, graph = FALSE) 
#consol = FALSE signifie qu'on n'applique pas a l'issue de la CAH une consolidation par les k-means. graph = FALSE pour ne pas afficher les sorties graphiques. kk=Inf signifie qu'aucune partition prealable n'est réalisée.
#plot(res.hcpc, choice="tree", tree.barplot = FALSE, cex = 0.01)
#Affichage dans le plan factoriel des classes
fviz_cluster(res.hcpc,
             shape = 20,
             repel = FALSE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map",
)
```

## Création d'un tableau contenant les effectifs par classe et de l'histogramme correspondant
```r clusters size
res.hcpc.dataclust <- as.data.frame(res.hcpc[["data.clust"]]) # conversion du tableau obtenu avec la CAH en dataframe
TableauContingence <- data.frame(res.hcpc.dataclust[,"clust"]) # Préparation d'un tableau des effectifs de chaque classe
names(TableauContingence) <- c("clust")
TableauContingence$Count <- 1
TableauContingence <- aggregate(Count ~ clust, TableauContingence, sum)  
View(TableauContingence) # Affichage du tableau d'effectifs de classe
barplot(TableauContingence$Count, names = TableauContingence$clust, main = "Individuals' distribution by cluster") #Affichage des effectifs de chaque classe en histogramme.

```

## Identification des variables catégorielles ne prenant que deux modalités afin d'alléger la caractérisation des profils.
```r identification of binaries categorical variables
# Cette opération est nécessaire pour simplifier ensuite les graphiques
TableauVariablesCategorielles <- Filter(is.character, VariablesTypoUltraMarcheurs)

NbModalitesParVariable <- data.frame()

for (i in colnames(TableauVariablesCategorielles)){

  # Recuperation du nombre de modalités pour chaque variable catégorielle
  fichier<- length(unique(TableauVariablesCategorielles[[i]]))
  NbModalitesParVariable <- rbind(NbModalitesParVariable, fichier)
  
}

NbModalitesParVariable$Variable <- colnames(TableauVariablesCategorielles)

VariablesDeuxModalites <- NbModalitesParVariable[which(NbModalitesParVariable[,1] == 2),]
VariablesDeuxModalites <- as.character(VariablesDeuxModalites$Variable)
```

## Définition du seuil de la valeur V.test à retenir pour conserver les variables décrivant les différentes classes
```r setting a vtest value
# Par défaut v.test > |1.96| - Relever le seuil permet de filtrer le nombre de variables qui décriront les classes. On ne garde que celles qui sont les plus significatives
Seuil.v.test <- 3.29
# Avec ce seuil, on accepte un seuil d'erreur à 0,001.
# en savoir plus https://www.medcalc.org/manual/values-of-the-normal-distribution.php
```

## Création des tableaux décrivant les différentes classes par les variables quanti et quali et affichage des diagrammes
```r devising a dataframe for cluster description, fig.height=12, fig.width=12
# On ne garde qu'une modalité pour les variables n'ayant que deux modalités
for (i in 1:nbclasses){

  # Récuperation dans des tableaux de la description de chacune des n classes par les variables quantitatives
  b <- as.data.frame(res.hcpc$desc.var$quanti[[i]])
  b <- b[which(colnames(b) == 'v.test')]
  b <- signif(b,3) # pour arrondir les valeurs

  # Récuperation dans des tableaux de la description de chacune des n classes par les variables catégorielles
  c <- as.data.frame(res.hcpc$desc.var$category[[i]])
  d <- as.data.frame(c$label <- rownames(c))
  d <- as.data.frame(c$label <- sub("\\=.*", "", c$label))
  d <- as.data.frame(c[which(c$label %in% VariablesDeuxModalites), ])
  d <- as.data.frame(d[which(d$v.test > 0),])
  e <- as.data.frame(c[which(!c$label %in% VariablesDeuxModalites), ])
  f <- as.data.frame(rbind(d, e))
  g <- as.data.frame(f$label <- NULL)
  c <- signif(f,3) # pour arrondir les valeurs
  c <- c[which(colnames(c) == 'v.test')]

  # Assemblage des tableaux avec les variables quantitatives et qualitatives
  h <- as.data.frame(rbind(b,c))
  h <- as.data.frame(cbind(h, h$label <- row.names(h)))
  colnames(h) <- c("v.test", "label")
  h <- as.data.frame(h[order(h$v.test, decreasing = TRUE), ])
  h <- as.data.frame(h[which(!h$v.test == "Inf"),])
  h <- as.data.frame(h[which(abs(h$v.test) >= Seuil.v.test),])
  
   # Pour tracer l'ensemble des variables significatives sur les diagrammes en barres, il faut au préalable remplacer les V.test infinies (-Inf et Inf). On propose de les remplacer en reprenant les V.test min et max auxquelles on applique un coefficient
  j <- h[which(h$v.test != -Inf),] # création tableau sans les valeurs -Inf
  min.j <- min(j$v.test) # sélection valeur minimale
  h$v.test[which(h$v.test == -Inf)] <- min.j*1.2 # remplacement des -Inf par valeur minimale (multipliée par un coefficient 1.2)
  j <- h[which(h$v.test != Inf),] # création tableau sans les valeurs Inf
  max.j <- max(j$v.test) # sélection valeur maximale
  h$v.test[which(h$v.test == Inf)] <- max.j*1.2 # remplacement des Inf par valeur maximale (multipliée par un coefficient 1.2)
  
  # pour tracer les diagrammes en barres 
  op <- par(oma=c(0,7,0,0))
  barplot((h$v.test), names = row.names(h), border = "white", horiz = TRUE, las = 1, xlim = c(-5-max(abs(h$v.test)), 5 + max(abs(h$v.test))), cex.names=0.8, main = paste("profil",i, sep=""))
  
  
}

```

## Récupération de la variable clust, et réintégration de la variable id_hog_pers au jeu de données initial
```r merging cluster variable into original dataset
VariablesTypoUltraMarcheurs$profil <- res.hcpc.dataclust$clust
VariablesTypoUltraMarcheurs$id_hog_pers <- row.names(res.hcpc.dataclust)

# utile par la suite pour récupérer les coord. XY du lieu de résidence du ménage
df <- data.frame(x = VariablesTypoUltraMarcheurs$id_hog_pers)
df <- df %>% separate(x, c("id_hog", "id_pers"))
VariablesTypoUltraMarcheurs$id_hog <- df$id_hog
```

## Exporter le tableur au format .xlsx
```r saving final table to xlsx file
write.xlsx(VariablesTypoUltraMarcheurs, "VariablesTypoUltraMarcheursFINAL.xlsx")
#browseURL("VariablesTypoUltraMarcheursFINAL.xlsx")
```

# Cartographie des lieux de résidence des marcheurs suivant leur profil
## Cartes des lieux de résidence (représentation sous forme de surfaces lissées)
### Importation du tableau contenant les coordonnées des îlots de résidence des marcheurs
```r importing pedestrians home coordinates
CoordXY_Hogar <- read.xlsx("CoordXY_Hogar_Alea.xlsx")
```

### Jointure des coordonnées XY du lieu de résidence dans le tableau des marcheurs
```r merging tables
VariablesTypoUltraMarcheurs <- merge(VariablesTypoUltraMarcheurs, CoordXY_Hogar, by = "id_hog")
```

## Importation de la couche SIG des UTAM (Unités territoriales d'analyse de la mobilité)
```r importing UTAM, warning=FALSE, message=FALSE, error=FALSE
UTAM <- st_read("EMU2019_UTAM_Bgta_Carto_Variables_Nbre_Completo_v3.gpkg")
```

## Création d’un semis de points (objet sf) à partir des coordonnées XY des lieux de résidence des marcheurs
```r creating an sf object from places of residence XY coordinates, fig.height=15, fig.width=15
ResidMarcheurs<- st_as_sf(VariablesTypoUltraMarcheurs, coords=c("X_Hog_Alea","Y_Hog_Alea"), crs = st_crs(UTAM)$srid)
plot(st_geometry(ResidMarcheurs))
```

## Préparation du jeu de données pour l'élaboration des cartes
```r devising a dataframe for cluster mapping
# étape nécessaire en vue de créer des cartes lissées donnant à voir la concentration des lieux de résidence des marcheurs suivant leur profil
# pour créer une liste des profils de marcheurs, dans l'ordre croissant des classes
TypoProfilMarcheurs <- VariablesTypoUltraMarcheurs[order(VariablesTypoUltraMarcheurs$profil, decreasing = FALSE), ]
ListProfils <- unique(TypoProfilMarcheurs$profil)
str(TypoProfilMarcheurs)

##Création des objets sf dont seront extrait les toponymes qui seront affichés sur les cartes.
#Pour les lieux de résidence
topores1 <- UTAM[which(UTAM$UTAM_NOMBR == "CHAPINERO" | UTAM$UTAM_NOMBR == "USAQUEN"| UTAM$UTAM_NOMBR == "EL PRADO"),]
topores2 <- UTAM[which(UTAM$UTAM_NOMBR == "LA CANDELARIA"| UTAM$UTAM_NOMBR == "CARVAJAL" | UTAM$UTAM_NOMBR == "SAN CRISTOBAL NORTE"| UTAM$UTAM_NOMBR == "CHAPINERO"),]
topores3 <- UTAM[which(UTAM$UTAM_NOMBR == "LA CANDELARIA"| UTAM$UTAM_NOMBR == "BOSA OCCIDENTAL" | UTAM$UTAM_NOMBR == "SOACHA PUEBLO" | UTAM$UTAM_NOMBR == "SUBA" | UTAM$UTAM_NOMBR == "LA URIBE"| UTAM$UTAM_NOMBR == "GRAN YOMASA"| UTAM$UTAM_NOMBR == "TUNJUELITO"| UTAM$UTAM_NOMBR == "AMERICAS"| UTAM$UTAM_NOMBR == "ALAMOS"| UTAM$UTAM_NOMBR == "SOPO"| UTAM$UTAM_NOMBR == "TENJO"| UTAM$UTAM_NOMBR == "CHAPINERO"),]
topores4 <- UTAM[which(UTAM$UTAM_NOMBR == "EL LUCERO"| UTAM$UTAM_NOMBR == "BOSA OCCIDENTAL" | UTAM$UTAM_NOMBR == "SOACHA PUEBLO" | UTAM$UTAM_NOMBR == "20 DE JULIO" | UTAM$UTAM_NOMBR == "CIUDAD USME"| UTAM$UTAM_NOMBR == "SOPO"| UTAM$UTAM_NOMBR == "TENJO" | UTAM$UTAM_NOMBR == "CHIA" | UTAM$UTAM_NOMBR == "CAJICA" | UTAM$UTAM_NOMBR == "EL ROSAL" | UTAM$UTAM_NOMBR == "BOJACA" | UTAM$UTAM_NOMBR == "FACATATIVA"| UTAM$UTAM_NOMBR == "LA CALERA" | UTAM$UTAM_NOMBR == "SIBATE"| UTAM$UTAM_NOMBR == "TUNJUELITO"| UTAM$UTAM_NOMBR == "SAN CRISTOBAL NORTE"| UTAM$UTAM_NOMBR == "GRAN YOMASA"| UTAM$UTAM_NOMBR == "MINUTO DE DIOS"| UTAM$UTAM_NOMBR == "SUBA"| UTAM$UTAM_NOMBR == "FUNZA"| UTAM$UTAM_NOMBR == "FACATATIVA"| UTAM$UTAM_NOMBR == "MADRID"| UTAM$UTAM_NOMBR == "SAN ISIDRO - PATIOS"| UTAM$UTAM_NOMBR == "COTA"| UTAM$UTAM_NOMBR == "TABIO"| UTAM$UTAM_NOMBR == "ZIPAQUIRA"| UTAM$UTAM_NOMBR == "TOCANCIPA"| UTAM$UTAM_NOMBR == "GACHANCIPA"| UTAM$UTAM_NOMBR == "LA CANDELARIA"),]
topores5 <- UTAM[which(UTAM$UTAM_NOMBR == "MINUTO DE DIOS"| UTAM$UTAM_NOMBR == "BOSA OCCIDENTAL" | UTAM$UTAM_NOMBR == "SOACHA PUEBLO" | UTAM$UTAM_NOMBR == "SUBA" | UTAM$UTAM_NOMBR == "CIUDAD USME" | UTAM$UTAM_NOMBR == "20 DE JULIO"| UTAM$UTAM_NOMBR == "TOCANCIPA"| UTAM$UTAM_NOMBR == "GACHANCIPA"| UTAM$UTAM_NOMBR == "LA CALERA"| UTAM$UTAM_NOMBR == "SOPO"| UTAM$UTAM_NOMBR == "TENJO"| UTAM$UTAM_NOMBR == "FUNZA"| UTAM$UTAM_NOMBR == "MADRID"| UTAM$UTAM_NOMBR == "EL ROSAL"| UTAM$UTAM_NOMBR == "GRAN YOMASA"| UTAM$UTAM_NOMBR == "BOJACA"| UTAM$UTAM_NOMBR == "LA URIBE"| UTAM$UTAM_NOMBR == "TUNJUELITO"| UTAM$UTAM_NOMBR == "AMERICAS"| UTAM$UTAM_NOMBR == "LA CANDELARIA"),]
topores6 <- UTAM[which(UTAM$UTAM_NOMBR == "EL LUCERO"| UTAM$UTAM_NOMBR == "BOSA OCCIDENTAL" | UTAM$UTAM_NOMBR == "SUBA" | UTAM$UTAM_NOMBR == "VENECIA"| UTAM$UTAM_NOMBR == "ENGATIVA" | UTAM$UTAM_NOMBR == "LA URIBE"| UTAM$UTAM_NOMBR == "SOACHA PUEBLO" | UTAM$UTAM_NOMBR == "FUNZA"| UTAM$UTAM_NOMBR == "DIANA TURBAY" | UTAM$UTAM_NOMBR == "GRAN YOMASA"),]

#Sauvegarde des objets sf dans une liste
toporesid <- list(topores1, topores2, topores3, topores4, topores5, topores6)
```

## Production d'une série de cartes montrant la concentration des lieux de résidence des marcheurs selon leur profil
```r mapping the concentration of pedestrians place of residence, fig.width=15, fig.height=15, warning=FALSE
#Définition du nombre de classes
nclass <- 5

#création d'un ensemble de palettes de couleurs (on en crée ici plus qu'il n'en faut, pour avoir de la marge si l'on souhaite augmenter le nombre de classes)
pal1 <- hcl.colors(nclass, "YlOrRd", rev = TRUE)
pal2 <- hcl.colors(nclass, "Greens 3", rev = TRUE)
pal3 <- hcl.colors(nclass, "Reds 3", rev = TRUE)
pal4 <- hcl.colors(nclass, "Grays", rev = TRUE)
pal5 <- hcl.colors(nclass, "Oslo", rev = FALSE)
pal6 <- hcl.colors(nclass, "Red-Purple", rev = TRUE)
pal7 <- hcl.colors(nclass, "Inferno", rev = TRUE)
pal8 <- hcl.colors(nclass, "Reds", rev = TRUE)
pla9 <- hcl.colors(nclass, "Blues 3", rev = TRUE)
pal10 <- hcl.colors(nclass, "Purples 3", rev = TRUE)

#Sauvegarde des palettes dans une liste
pal <- list(pal1, pal2, pal3, pal4, pal5, pal6, pal7, pal8, pla9, pal10)

#Définition des limites des UTAM comme emprise pour le lissage
Emprise <- as.owin(as(st_union(st_buffer(UTAM, 1000)), "Spatial"))

#Paramétrage des marges pour insérer le titre général et les titres de chaque carte
par(oma=c(3.5,0,3,0)+0.1, mar = c(0, 0.5, 1.2, 0.5))

plot.new()

#Pour découper la fenêtre en 1 ligne et 6 colonnes (6 profils)
par(mfrow=c(3,2))

#Boucle pour produire et cartographier une surface de densité par profil
for (i in ListProfils){
  
  #Récuperation des jeux de données par profil
  fichier <- TypoProfilMarcheurs[which(TypoProfilMarcheurs$profil == i),]
  
  #Pour récupérer les coordonnées des lieux de résidence
  pts <- st_coordinates(st_geometry(st_as_sf(fichier, coords=c("X_Hog_Alea","Y_Hog_Alea"), crs = st_crs(UTAM)$srid)))
  
  #Pour créer un objet ppp (format spatstat) et y intégrer l'emprise
  fichier.ppp <- ppp(pts[,1], pts[,2], window = Emprise)
  
  # pour définir la taille du rayon (en m.)
  rayon <- 1000
  
  # pour définir la taille du pixel (en m.)
  resol <- 100 # ici 100m x 100m = 1 ha.
  
  # pour calculer la surface de densité (rayon lissage : 1000 m et résolution spatiale de l'image : 100m x 100m = 1 ha)
  cartelissee <- density(fichier.ppp, sigma = rayon, kernel = "gaussian", eps = resol)
  
  #Conversion de la surface lissée au format raster
  cartelissee.raster <- raster(cartelissee)
  crs(cartelissee.raster) <- st_crs(UTAM)$srid # pour spécifier un SCR à l'objet raster
  
  #Découpage du raster sur l'emprise des UTAM
  cartelissee.raster <- mask(cartelissee.raster, UTAM) 
  
  #Passage des densités à des effectifs (multiplication des densités par la surface du cercle)
  values(cartelissee.raster) <- values(cartelissee.raster) * pi*rayon**2

  #Définition des seuils de classes (discrétisation en intervalles égaux)
  bks <- seq(from = cellStats(cartelissee.raster, stat = min), 
            to = cellStats(cartelissee.raster, stat = max), 
            by = (cellStats(cartelissee.raster, stat = max) - cellStats(cartelissee.raster, stat = min)) / nclass)
  
  #Reclassification de la surface lissée
  cartelissee.reclass <- cut(cartelissee.raster, breaks = bks, include.lowest = FALSE, right = TRUE, dig.lab = 3, ordered_result = FALSE)
  
  #Vectorisation de la surface reclassée (calcul un peu long)
  cartelissee.vecteur <- as(rasterToPolygons(cartelissee.reclass, n=4, na.rm=TRUE, digits=12, dissolve=TRUE), "sf")
  
  #Tracer la carte
  plot(st_geometry(UTAM), border = "white", bg= "grey90")
  
  typoLayer(
    x = cartelissee.vecteur,
    var="layer",
    col = unlist(pal[as.numeric(i)]),
    lwd = 0.1,
    border = unlist(pal[as.numeric(i)]),
    legend.pos = "n",
    add = TRUE)
  
   labelLayer(
    x = toporesid[[as.numeric(i)]], 
    txt = "UTAM_NOMBR", 
    col= "black", 
    cex = 0.3, 
    font = 4, 
    halo = TRUE, 
    bg = "white", 
    r = 0.05, 
    overlap = FALSE, 
    show.lines = FALSE)
  
  legendChoro(
         pos = "bottomright",
         title.txt = "Number of walker residence\nplaces in a 1 km radius.",
         title.cex = 0.6,
         breaks = bks, 
         values.rnd = 2,
         nodata = FALSE,
         col = unlist(pal[as.numeric(i)]),
         border = "white",
         horiz = FALSE
       )

  title(main =paste("Cluster",i, sep="-"))
  
  mtext(text = paste0("n = ", nrow(fichier), " walkers"), 
      side = 3, line = -2, adj = 0.1, cex = 0.7)
  
}

barscale(
  lwd = 1.5,
  cex = 0.6,
  pos = "bottomleft",
  style = "pretty",
  unit = "m"
)

north(pos = "topright")

#Pour afficher le titre principal et la source
mtext("Concentration of walkers' home location by profile in the metropolitan area of Bogota", cex = 1.3, side=3,line=1,adj=0.5,outer=TRUE)
mtext("   Sources : Union Temporal STEER & CNC, EMD, 2019 - SDM - radius buffer : 1000 m, resolution : 1 ha", side=1, line=1, adj=0, cex=0.6, outer=TRUE)
```

# Cartographie des lieux de destination associés au premier déplacement des marcheurs depuis leur domicile 
## Importation de la couche SIG représentant les lieux de première destination (centroïdes des ZAT - Zones d'analyse du transport)
```r importing first trip destination GIS layer, warning=FALSE, message=FALSE, error=FALSE
#Le premier trajet à pied des marcheurs est répété entre 4 et 5 fois dans la semaine, ce qui le rend assez représentatif des déplacements à pied des marcheurs, puisque le deuxième trajet est un peu moins répété, alors que le 3e est bien plus occasionnel. 
destino <- st_read("ZAT_PrimerDestino.gpkg")
plot(st_geometry(destino), pch = "+", cex = 0.5)
```

## Création d'une liste de marcheurs dont la ZAT de destination du premier déplacement est renseignée
```r creating a list of pedestrians whose first destination is known
listdestino <- as.character(destino$id_concat)
```

## Sélection des marcheurs ayant une ZAT de destination connue pour leur premier déplacement
```r selecting pedestrians whose first destination is known
TypoProfilMarcheurs <- VariablesTypoUltraMarcheurs[VariablesTypoUltraMarcheurs$id_hog_pers %in% listdestino, ]
```

## Jointure pour récupérer le profil (clust) sur le lieu de destination
```r merging clusters with zat destination
destino <- merge(destino, TypoProfilMarcheurs[c(22:23)], by.x = "id_concat", by.y = "id_hog_pers")
```

## Construction d'une liste contenant des objets sf dont seront extrait les toponymes à figurer sur les cartes des lieux de destination
```r name of destination places

##Création des objets sf dont seront extrait les toponymes qui seront affichés sur les cartes

topodest1 <- UTAM[which(UTAM$UTAM_NOMBR == "USAQUEN" | UTAM$UTAM_NOMBR == "CHICO LAGO"),]
topodest2 <- UTAM[which(UTAM$UTAM_NOMBR == "LA CANDELARIA"| UTAM$UTAM_NOMBR == "TEUSAQUILLO" | UTAM$UTAM_NOMBR == "CHICO LAGO"| UTAM$UTAM_NOMBR == "SAN CRISTOBAL NORTE"| UTAM$UTAM_NOMBR == "BRITALIA"),]
topodest3 <- UTAM[which(UTAM$UTAM_NOMBR == "LA CANDELARIA"| UTAM$UTAM_NOMBR == "TEUSAQUILLO" | UTAM$UTAM_NOMBR == "SOACHA PUEBLO" | UTAM$UTAM_NOMBR == "20 DE JULIO" | UTAM$UTAM_NOMBR == "GRAN YOMASA" | UTAM$UTAM_NOMBR == "ENGATIVA" | UTAM$UTAM_NOMBR == "TENJO" | UTAM$UTAM_NOMBR == "VENECIA"| UTAM$UTAM_NOMBR == "SOPO"| UTAM$UTAM_NOMBR == "MINUTO DE DIOS"| UTAM$UTAM_NOMBR == "BOSA OCCIDENTAL"| UTAM$UTAM_NOMBR == "SUBA"| UTAM$UTAM_NOMBR == "LA URIBE"| UTAM$UTAM_NOMBR == "FONTIBON"| UTAM$UTAM_NOMBR == "CHICO LAGO"| UTAM$UTAM_NOMBR == "AMERICAS"| UTAM$UTAM_NOMBR == "TUNJUELITO"),]
topodest4 <- UTAM[which(UTAM$UTAM_NOMBR == "LA CANDELARIA"| UTAM$UTAM_NOMBR == "BOSA OCCIDENTAL" | UTAM$UTAM_NOMBR == "SOACHA PUEBLO" | UTAM$UTAM_NOMBR == "FUNZA" | UTAM$UTAM_NOMBR == "GRAN YOMASA"| UTAM$UTAM_NOMBR == "SOPO"| UTAM$UTAM_NOMBR == "TENJO" | UTAM$UTAM_NOMBR == "CHIA" | UTAM$UTAM_NOMBR == "CAJICA" | UTAM$UTAM_NOMBR == "EL ROSAL" | UTAM$UTAM_NOMBR == "BOJACA" | UTAM$UTAM_NOMBR == "FACATATIVA"| UTAM$UTAM_NOMBR == "LA CALERA" | UTAM$UTAM_NOMBR == "MADRID"| UTAM$UTAM_NOMBR == "MINUTO DE DIOS"| UTAM$UTAM_NOMBR == "ZIPAQUIRA"| UTAM$UTAM_NOMBR == "TOCANCIPA"| UTAM$UTAM_NOMBR == "GACHANCIPA"| UTAM$UTAM_NOMBR == "COTA"| UTAM$UTAM_NOMBR == "SIBATE"| UTAM$UTAM_NOMBR == "TABIO"| UTAM$UTAM_NOMBR == "SUBA"| UTAM$UTAM_NOMBR == "SAN CRISTOBAL NORTE"| UTAM$UTAM_NOMBR == "FONTIBON"| UTAM$UTAM_NOMBR == "VENECIA"| UTAM$UTAM_NOMBR == "20 DE JULIO"| UTAM$UTAM_NOMBR == "AMERICAS"| UTAM$UTAM_NOMBR == "CHAPINERO"),]
topodest5 <- UTAM[which(UTAM$UTAM_NOMBR == "TEUSAQUILLO"| UTAM$UTAM_NOMBR == "BOSA OCCIDENTAL" | UTAM$UTAM_NOMBR == "SOACHA PUEBLO" | UTAM$UTAM_NOMBR == "SUBA" | UTAM$UTAM_NOMBR == "CIUDAD USME" | UTAM$UTAM_NOMBR == "20 DE JULIO"| UTAM$UTAM_NOMBR == "TOCANCIPA"| UTAM$UTAM_NOMBR == "GACHANCIPA"| UTAM$UTAM_NOMBR == "LA CALERA"| UTAM$UTAM_NOMBR == "SOPO"| UTAM$UTAM_NOMBR == "GRAN YOMASA"| UTAM$UTAM_NOMBR == "FUNZA"| UTAM$UTAM_NOMBR == "MADRID"| UTAM$UTAM_NOMBR == "BOJACA"| UTAM$UTAM_NOMBR == "TUNJUELITO"| UTAM$UTAM_NOMBR == "LA URIBE"| UTAM$UTAM_NOMBR == "SIBATE"| UTAM$UTAM_NOMBR == "ISMAEL PERDOMO"| UTAM$UTAM_NOMBR == "EL ROSAL"| UTAM$UTAM_NOMBR == "MINUTO DE DIOS"| UTAM$UTAM_NOMBR == "SANTA CECILIA"| UTAM$UTAM_NOMBR == "FONTIBON"| UTAM$UTAM_NOMBR == "TENJO"| UTAM$UTAM_NOMBR == "LOS ALCAZARES"),]
topodest6 <- UTAM[which(UTAM$UTAM_NOMBR == "BOSA OCCIDENTAL" | UTAM$UTAM_NOMBR == "SUBA" | UTAM$UTAM_NOMBR == "GRAN YOMASA"| UTAM$UTAM_NOMBR == "LA URIBE"| UTAM$UTAM_NOMBR == "SOACHA PUEBLO"| UTAM$UTAM_NOMBR == "VENECIA"| UTAM$UTAM_NOMBR == "TEUSAQUILLO"| UTAM$UTAM_NOMBR == "ENGATIVA"| UTAM$UTAM_NOMBR == "FUNZA"| UTAM$UTAM_NOMBR == "KENNEDY CENTRAL"| UTAM$UTAM_NOMBR == "TUNJUELITO"| UTAM$UTAM_NOMBR == "RESTREPO"),]

#Sauvegarde des objets sf dans une liste
topodest <- list(topodest1, topodest2, topodest3, topodest4, topodest5, topodest6)

```


## Production d'une deuxième série de cartes représentant la concentration des lieux associés à la première destination des marcheurs
```r mapping the concentration of pedestrians first destination, warning=FALSE, fig.height=15, fig.width=15

#Paramétrage des marges pour insérer le titre général et les titres de chaque carte
par(oma=c(3.5,0,3,0)+0.1, mar = c(0, 0.5, 1.2, 0.5))

plot.new()

#Pour découper la fenêtre en 2 lignes et 2 colonnes (4 profils)
par(mfrow=c(3,2))

#Définition du nombre de classes
nclass <- 5

#Définition d'une boucle pour produire et cartographier une surface de densité par profil
for (i in ListProfils){
  
  #Récuperation des jeux de données par profil
  fichier <- destino[which(destino$profil == i),]
  
  #Récupération des coordonnées des lieux de résidence
  pts <- st_coordinates(st_geometry(fichier))
  
  #Création d'un objet ppp (format spatstat) et intégration de l'emprise
  fichier.ppp <- ppp(pts[,1], pts[,2], window = Emprise)
  
  #Définition de la taille du rayon (en m.)
  rayon <- 1000
  
  #Définition de la taille du pixel (en m.)
  resol <- 100 # ici 100m x 100m = 1 ha.
  
  #Calcul de la surface de densité (rayon lissage : 1000 m et résolution spatiale de l'image : 100m x 100m = 1 ha)
  cartelissee <- density(fichier.ppp, sigma = rayon, kernel = "gaussian", eps = resol)
  
  #Conversion de la surface lissée au format raster
  cartelissee.raster <- raster(cartelissee)
  crs(cartelissee.raster) <- st_crs(UTAM)$srid
  
  #Découpage du raster sur l'emprise des UTAM
  cartelissee.raster <- mask(cartelissee.raster, UTAM) 
  
  #Passage des densités à des effectifs (multiplication des densités par la surface du cercle)
  values(cartelissee.raster) <- values(cartelissee.raster) * pi*rayon**2

  #Définition des seuils de classes (discrétisation en intervalles égaux)
  bks <- seq(from = cellStats(cartelissee.raster, stat = min), 
            to = cellStats(cartelissee.raster, stat = max), 
            by = (cellStats(cartelissee.raster, stat = max) - cellStats(cartelissee.raster, stat = min)) / nclass)
  
  #Reclassification de la surface lissée
  cartelissee.reclass <- cut(cartelissee.raster, breaks = bks, include.lowest = FALSE, right = TRUE, dig.lab = 3, ordered_result = FALSE)
  
  #Vectorisation de la surface reclassée (calcul un peu long)
  cartelissee.vecteur <- as(rasterToPolygons(cartelissee.reclass, n=4, na.rm=TRUE, digits=12, dissolve=TRUE), "sf")
  
  #Tracé de la carte
  plot(st_geometry(UTAM), border = "white", bg= "grey90")
  
  typoLayer(
   x = cartelissee.vecteur,
   var="layer",
   col = unlist(pal[as.numeric(i)]),
   lwd = 0.1,
   border = unlist(pal[as.numeric(i)]),
   legend.pos = "n",
   add = TRUE)
  
   labelLayer(
    x = topodest[[as.numeric(i)]], 
    txt = "UTAM_NOMBR", 
    col= "black", 
    cex = 0.3, 
    font = 4, 
    halo = TRUE, 
    bg = "white", 
    r = 0.05, 
    overlap = FALSE, 
    show.lines = FALSE)
  
  legendChoro(
         pos = "bottomright",
         title.txt = "Number of destination places\nin a 1 km radius.",
         title.cex = 0.6,
         breaks = bks, 
         values.rnd = 2,
         nodata = FALSE,
         col = unlist(pal[as.numeric(i)]),
         border = "white",
         horiz = FALSE
       )

  title(main =paste("Cluster",i, sep="-"))
  
  mtext(text = paste0("n = ", nrow(fichier), " walkers"), 
      side = 3, line = -2, adj = 0.1, cex = 0.7)
  
}

barscale(
  lwd = 1.5,
  cex = 0.6,
  pos = "bottomleft",
  style = "pretty",
  unit = "m"
)

north(pos = "topright")

# Pour afficher le titre principal et la source
mtext("Concentration of walkers' first destination places in the metropolitan area of Bogota", cex = 1.3, side=3,line=1,adj=0.5,outer=TRUE)
mtext("   Sources : Union Temporal STEER & CNC, EMD, 2019 - SDM - radius buffer : 1000 m, resolution : 1 ha", side=1, line=1, adj=0, cex=0.6, outer=TRUE)
```





















