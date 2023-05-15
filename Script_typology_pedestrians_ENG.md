---
title: "Devising a typology of pedestrians from a household travel survey and mapping the resulting profiles : the example of Bogotá"
author: "Arthur Ducasse: https://perso.univ-rennes2.fr/arthur.ducasse, Florent Demoraes: https://perso.univ-rennes2.fr/florent.demoraes"
date: "2023-03-03"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = 'F:/Rennes/Recherche/Thèses_suivies/Arthur Ducasse/Article_Typo_pietons')
```

# Computing pedestrian profiles using FAMD and HAC
## Loading the required packages
```{r loading packages, warning=FALSE, message=FALSE, error=FALSE}
#Loading the packages
library(FactoMineR)
library(openxlsx)
library(factoextra)
library(ggplot2)
library(sf)
library(spatstat)
library(sp)
library(maptools)
library(cartography)
library(raster)
library(tidyverse)
```

## Importing the data set and statistical summary
```{r loading dataset, message=FALSE, error=FALSE}
VariablesTypoUltraMarcheurs <- read.xlsx("02_analyse_ultra_marcheurs_v6.xlsx")
rownames(VariablesTypoUltraMarcheurs) <- VariablesTypoUltraMarcheurs$id_hog_pers # to add person id as rownames
str(VariablesTypoUltraMarcheurs) # to get the type of the variables et a heading of the values
summary(VariablesTypoUltraMarcheurs)
```

## Cleansing of the dataset
```{r dataset cleansing}
VariablesTypoUltraMarcheurs <- na.omit(VariablesTypoUltraMarcheurs) # to remove rows with NA values

VariablesTypoUltraMarcheurs <- VariablesTypoUltraMarcheurs[which(VariablesTypoUltraMarcheurs$num_bici <= 10),] # to remove individuals living in households owning 10 bicycles or more (which is very unlikely and probably an error from the data capture)

VariablesTypoUltraMarcheurs <- VariablesTypoUltraMarcheurs[which(VariablesTypoUltraMarcheurs$calif_exp <= 5),] # to remove individuals with trips qualifications higher than 5 (which is impossible)

VariablesTypoUltraMarcheurs <- VariablesTypoUltraMarcheurs[which(VariablesTypoUltraMarcheurs$duracion_prom <= 300),] # to remove individuals with trips whose duration is higher than 5 hours (300 minutes)

```

## Removal of unnecessary columns for FAMD
```{r variable removal}
# individual identification columns
VariablesTypoUltraMarcheurs$id_hog <- NULL 
VariablesTypoUltraMarcheurs$id_hog_pers <- NULL

# columns containing spatial location information
VariablesTypoUltraMarcheurs$ZAT_Dest_1er_viaje_desde_casa <- NULL 

# Other columns 
VariablesTypoUltraMarcheurs$f_exp <- NULL
```

## Calculation of the first FAMD (Factor Analysis of Mixed Data) 
```{r first FAMD, message=FALSE, error=FALSE, warning=FALSE, fig.height=15, fig.width=15}
res <- FAMD(VariablesTypoUltraMarcheurs, ncp = 10, graph = FALSE)

#Plot of factorial maps
plot(res, (choix = c("ind", "var", "quanti", "quali")), axes = c(1, 2), lab.var = TRUE, lab.ind = FALSE, autoLab = "yes")
plot(res, choix = c("var"), axes = c(1, 2), lab.var = TRUE, lab.ind = FALSE, autoLab = "yes") 
plot(res, choix = c("quanti"), axes = c(1, 2), lab.var = TRUE, lab.ind = FALSE, autoLab = "yes", title = "Factor map of variables") 
#plot(res, choix = c("quali"), axes = c(1, 2), lab.var = TRUE, lab.ind = FALSE, autoLab = "yes")

#Plot of the cumulative frequency of eigenvalues
get_eigenvalue(res)
fviz_screeplot(res)

```

## Displaying the list of the variables
```{r view variables}
print(as.data.frame(colnames(VariablesTypoUltraMarcheurs)))
```
## Calculation of a second FAMD with the low discriminating variables set as supplementary variables
```{r second FAMD, message=FALSE, warning=FALSE, error=FALSE, fig.height=15, fig.width=15}

res <- FAMD(VariablesTypoUltraMarcheurs, ncp = 6, sup.var = c(3, 4, 17), graph = FALSE) ##sup.var means that the following variables: calif_exp (3), duracion_prom (4) et num_moto (17) are set as supplementary variables. ncp = 6 --> we only keep the first 6 dimensions that already explain 42% of the total inertia

#Plot of factorial maps
plot(res, (choix = c("ind", "var", "quanti", "quali")), axes = c(1, 2), lab.var = TRUE, lab.ind = FALSE, autoLab = "yes", palette=palette(c("grey","red","blue")), title = "Factor map of individuals (pedestrians) and categories")
plot(res, choix = c("var"), axes = c(1, 2), lab.var = TRUE, lab.ind = FALSE, autoLab = "yes", title = "Factor map of variables", palette=palette(c("grey","red","blue"))) 
plot(res, choix = c("quanti"), axes = c(1, 2), lab.var = TRUE, lab.ind = FALSE, autoLab = "yes", title = "Factor map of quantitative variables") 
plot(res, choix = c("quali"), axes = c(1, 2), lab.var = TRUE, lab.ind = FALSE, autoLab = "yes")

#Plot of the cumulative frequency of eigenvalues
get_eigenvalue(res)
fviz_screeplot(res)

```

## Computation of the HAC (Hierarchical Agglomerative Clustering) and plot of the resulting graphs
```{r clustering calculation, message=FALSE, error=FALSE, warning=FALSE, fig.height=20, fig.width=20}
nbclasses <- 6 # to set the number of clusters 
res.hcpc <- HCPC(res, kk=Inf, nb.clust=nbclasses, consol=FALSE, graph = FALSE) 
#consol = FALSE means that a k-means based consolidation is not applied at the end of the HAC process. graph = FALSE means that results are not plotted. kk=Inf means that no prior partition to the dataset is applied.
plot(res.hcpc, choice="tree", tree.barplot = FALSE, cex = 0.01, main = "Clustering dendrogram")
#Plot of the clusters convex polygons in the factor map of individuals
fviz_cluster(res.hcpc,
             shape = 20,
             repel = FALSE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map",
)
```

## Creation of a table containing clusters size and the corresponding histogram
```{r clusters size}
res.hcpc.dataclust <- as.data.frame(res.hcpc[["data.clust"]]) # coerce HAC slot into a dataframe
TableauContingence <- data.frame(res.hcpc.dataclust[,"clust"]) # Preparing a table to get clusters size
names(TableauContingence) <- c("clust")
TableauContingence$Count <- 1
TableauContingence <- aggregate(Count ~ clust, TableauContingence, sum)  
barplot(TableauContingence$Count, names = TableauContingence$clust, main = "Distribution of individuals by cluster")

```

## Identification of the variables containing only two categories so as to simplify the characterization of the profiles
```{r identification of binaries categorical variables}
# This step is required to simplify afterwards the graphs
TableauVariablesCategorielles <- Filter(is.character, VariablesTypoUltraMarcheurs)

NbModalitesParVariable <- data.frame()

for (i in colnames(TableauVariablesCategorielles)){

  # To get the number of categories in each categorical variable
  fichier<- length(unique(TableauVariablesCategorielles[[i]]))
  NbModalitesParVariable <- rbind(NbModalitesParVariable, fichier)
  
}

NbModalitesParVariable$Variable <- colnames(TableauVariablesCategorielles)

VariablesDeuxModalites <- NbModalitesParVariable[which(NbModalitesParVariable[,1] == 2),]
VariablesDeuxModalites <- as.character(VariablesDeuxModalites$Variable)
```

## Setting the threshold of the V.test value to keep only the significant variables for describing the clusters
```{r setting a vtest value}
# By default v.test > |1.96| - Setting a higher value allows to filter the number of variables for describing the clusters. We only keep the most significant ones.
Seuil.v.test <- 3.29
# Using this threshold, we assume a 0.001 risk of error.
# knowing more : https://www.medcalc.org/manual/values-of-the-normal-distribution.php
```

## Creation of the tables required for describing the clusters based on both quantitative and qualitative variables and plotting of the diagrams
```{r devising a dataframe for cluster description, fig.height=12, fig.width=12}
# We only keep one category for binary categorical variables
for (i in 1:nbclasses){

  # To populate a dataframe with the description of each one on the n clusters based on quantitative variables
  b <- as.data.frame(res.hcpc$desc.var$quanti[[i]])
  b <- b[which(colnames(b) == 'v.test')]
  b <- signif(b,3) # to round values

  # To populate a dataframe with the description of each one on the n clusters based on qualitative variables
  c <- as.data.frame(res.hcpc$desc.var$category[[i]])
  d <- as.data.frame(c$label <- rownames(c))
  d <- as.data.frame(c$label <- sub("\\=.*", "", c$label))
  d <- as.data.frame(c[which(c$label %in% VariablesDeuxModalites), ])
  d <- as.data.frame(d[which(d$v.test > 0),])
  e <- as.data.frame(c[which(!c$label %in% VariablesDeuxModalites), ])
  f <- as.data.frame(rbind(d, e))
  g <- as.data.frame(f$label <- NULL)
  c <- signif(f,3) # to round values
  c <- c[which(colnames(c) == 'v.test')]

  # Binding both dataframes
  h <- as.data.frame(rbind(b,c))
  h <- as.data.frame(cbind(h, h$label <- row.names(h)))
  colnames(h) <- c("v.test", "label")
  h <- as.data.frame(h[order(h$v.test, decreasing = TRUE), ])
  h <- as.data.frame(h[which(!h$v.test == "Inf"),])
  h <- as.data.frame(h[which(abs(h$v.test) >= Seuil.v.test),])
  
   # To plot all the significant variables on a diagram, we need to handle infinite V.test (-Inf and Inf). A solution is to replace them using the min and max V.test to which a coefficient is applied (1.2 in our example)
  j <- h[which(h$v.test != -Inf),] # to create a table without -Inf values
  min.j <- min(j$v.test) # to set minimum value
  h$v.test[which(h$v.test == -Inf)] <- min.j*1.2 # replacement of the -Inf values by the previous minimum value (multiplied by a 1.2 coefficient)
  j <- h[which(h$v.test != Inf),] # to create a table without Inf values
  max.j <- max(j$v.test) # to set maximum value
  h$v.test[which(h$v.test == Inf)] <- max.j*1.2 # replacement of the Inf values by the previous maximum value (multiplied by a 1.2 coefficient)
  
  # to create a bar plot 
  op <- par(oma=c(0,7,0,0))
  barplot((h$v.test), names = row.names(h), border = "white", horiz = TRUE, las = 1, xlim = c(-5-max(abs(h$v.test)), 5 + max(abs(h$v.test))), cex.names=0.8, main = paste("cluster-",i, sep=""))
  
  
}

```

## Extracting the "clust" variable and merging the id_hog_pers variable to the initial dataset
```{r merging cluster variable into original dataset}
VariablesTypoUltraMarcheurs$profil <- res.hcpc.dataclust$clust
VariablesTypoUltraMarcheurs$id_hog_pers <- row.names(res.hcpc.dataclust)

# useful for the following steps to fetch the XY coordinates of household's place of residence
df <- data.frame(x = VariablesTypoUltraMarcheurs$id_hog_pers)
df <- df %>% separate(x, c("id_hog", "id_pers"))
VariablesTypoUltraMarcheurs$id_hog <- df$id_hog
```

## Exporting the table to xlsx file
```{r saving final table to xlsx file}
write.xlsx(VariablesTypoUltraMarcheurs, "VariablesTypoUltraMarcheursFINAL.xlsx")
#browseURL("VariablesTypoUltraMarcheursFINAL.xlsx")
```

# Mapping the places of residence of the pedestrians based on their profile (smoothed surfaces)
## Importing the table containing the coordinates of the urban blocks where pedestrians reside
```{r importing pedestrians home coordinates}
CoordXY_Hogar <- read.xlsx("CoordXY_Hogar_Alea.xlsx")
```

## Merging XY coordinates of home to the pedestrian dataset
```{r merging tables}
VariablesTypoUltraMarcheurs <- merge(VariablesTypoUltraMarcheurs, CoordXY_Hogar, by = "id_hog")
```

## Importing UTAM (Territorial Unit for Mobility Analysis) GIS layer 
```{r importing UTAM, warning=FALSE, message=FALSE, error=FALSE}
UTAM <- st_read("EMU2019_UTAM_Bgta_Carto_Variables_Nbre_Completo_v3.gpkg")
```

## Creating a point pattern (sf object) based on XY coordinates of pedestrians' place of residence 
```{r creating an sf object from places of residence XY coordinates, fig.height=15, fig.width=15}
ResidMarcheurs<- st_as_sf(VariablesTypoUltraMarcheurs, coords=c("X_Hog_Alea","Y_Hog_Alea"), crs = st_crs(UTAM)$srid)
plot(st_geometry(ResidMarcheurs))
```

## Preparing the dataset for generating maps
```{r devising a dataframe for cluster mapping}
#required step for generating smoothed maps showing concentrations in pedestrians' place of residence based on their profile
#to create a list of pedestrian profiles, sorted by clusters order
TypoProfilMarcheurs <- VariablesTypoUltraMarcheurs[order(VariablesTypoUltraMarcheurs$profil, decreasing = FALSE), ]
ListProfils <- unique(TypoProfilMarcheurs$profil)

##to create sf objects from which place names will be extracted for labeling maps
#For the places of residence
topores1 <- UTAM[which(UTAM$UTAM_NOMBR == "CHAPINERO" | UTAM$UTAM_NOMBR == "USAQUEN"| UTAM$UTAM_NOMBR == "EL PRADO"),]
topores2 <- UTAM[which(UTAM$UTAM_NOMBR == "LA CANDELARIA"| UTAM$UTAM_NOMBR == "CARVAJAL" | UTAM$UTAM_NOMBR == "SAN CRISTOBAL NORTE"| UTAM$UTAM_NOMBR == "CHAPINERO"),]
topores3 <- UTAM[which(UTAM$UTAM_NOMBR == "LA CANDELARIA"| UTAM$UTAM_NOMBR == "BOSA OCCIDENTAL" | UTAM$UTAM_NOMBR == "SOACHA PUEBLO" | UTAM$UTAM_NOMBR == "SUBA" | UTAM$UTAM_NOMBR == "LA URIBE"| UTAM$UTAM_NOMBR == "GRAN YOMASA"| UTAM$UTAM_NOMBR == "TUNJUELITO"| UTAM$UTAM_NOMBR == "AMERICAS"| UTAM$UTAM_NOMBR == "ALAMOS"| UTAM$UTAM_NOMBR == "SOPO"| UTAM$UTAM_NOMBR == "TENJO"| UTAM$UTAM_NOMBR == "CHAPINERO"),]
topores4 <- UTAM[which(UTAM$UTAM_NOMBR == "EL LUCERO"| UTAM$UTAM_NOMBR == "BOSA OCCIDENTAL" | UTAM$UTAM_NOMBR == "SOACHA PUEBLO" | UTAM$UTAM_NOMBR == "20 DE JULIO" | UTAM$UTAM_NOMBR == "CIUDAD USME"| UTAM$UTAM_NOMBR == "SOPO"| UTAM$UTAM_NOMBR == "TENJO" | UTAM$UTAM_NOMBR == "CHIA" | UTAM$UTAM_NOMBR == "CAJICA" | UTAM$UTAM_NOMBR == "EL ROSAL" | UTAM$UTAM_NOMBR == "BOJACA" | UTAM$UTAM_NOMBR == "FACATATIVA"| UTAM$UTAM_NOMBR == "LA CALERA" | UTAM$UTAM_NOMBR == "SIBATE"| UTAM$UTAM_NOMBR == "TUNJUELITO"| UTAM$UTAM_NOMBR == "SAN CRISTOBAL NORTE"| UTAM$UTAM_NOMBR == "GRAN YOMASA"| UTAM$UTAM_NOMBR == "MINUTO DE DIOS"| UTAM$UTAM_NOMBR == "SUBA"| UTAM$UTAM_NOMBR == "FUNZA"| UTAM$UTAM_NOMBR == "FACATATIVA"| UTAM$UTAM_NOMBR == "MADRID"| UTAM$UTAM_NOMBR == "SAN ISIDRO - PATIOS"| UTAM$UTAM_NOMBR == "COTA"| UTAM$UTAM_NOMBR == "TABIO"| UTAM$UTAM_NOMBR == "ZIPAQUIRA"| UTAM$UTAM_NOMBR == "TOCANCIPA"| UTAM$UTAM_NOMBR == "GACHANCIPA"| UTAM$UTAM_NOMBR == "LA CANDELARIA"),]
topores5 <- UTAM[which(UTAM$UTAM_NOMBR == "MINUTO DE DIOS"| UTAM$UTAM_NOMBR == "BOSA OCCIDENTAL" | UTAM$UTAM_NOMBR == "SOACHA PUEBLO" | UTAM$UTAM_NOMBR == "SUBA" | UTAM$UTAM_NOMBR == "CIUDAD USME" | UTAM$UTAM_NOMBR == "20 DE JULIO"| UTAM$UTAM_NOMBR == "TOCANCIPA"| UTAM$UTAM_NOMBR == "GACHANCIPA"| UTAM$UTAM_NOMBR == "LA CALERA"| UTAM$UTAM_NOMBR == "SOPO"| UTAM$UTAM_NOMBR == "TENJO"| UTAM$UTAM_NOMBR == "FUNZA"| UTAM$UTAM_NOMBR == "MADRID"| UTAM$UTAM_NOMBR == "EL ROSAL"| UTAM$UTAM_NOMBR == "GRAN YOMASA"| UTAM$UTAM_NOMBR == "BOJACA"| UTAM$UTAM_NOMBR == "LA URIBE"| UTAM$UTAM_NOMBR == "TUNJUELITO"| UTAM$UTAM_NOMBR == "AMERICAS"| UTAM$UTAM_NOMBR == "LA CANDELARIA"),]
topores6 <- UTAM[which(UTAM$UTAM_NOMBR == "EL LUCERO"| UTAM$UTAM_NOMBR == "BOSA OCCIDENTAL" | UTAM$UTAM_NOMBR == "SUBA" | UTAM$UTAM_NOMBR == "VENECIA"| UTAM$UTAM_NOMBR == "ENGATIVA" | UTAM$UTAM_NOMBR == "LA URIBE"| UTAM$UTAM_NOMBR == "SOACHA PUEBLO" | UTAM$UTAM_NOMBR == "FUNZA"| UTAM$UTAM_NOMBR == "DIANA TURBAY" | UTAM$UTAM_NOMBR == "GRAN YOMASA"),]

#to save sf objects to a list
toporesid <- list(topores1, topores2, topores3, topores4, topores5, topores6)
```

## Batch process to generate a collection of maps showing concentrations in pedestrians' place of residence based on their profile
```{r mapping the concentration of pedestrians place of residence, fig.width=15, fig.height=15, warning=FALSE}
#Setting the number of clusters
nclass <- 5

#to create a set of color palettes (more palettes than needed are here created to have room to increase the number of classes)
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

#to save the palettes in a list
pal <- list(pal1, pal2, pal3, pal4, pal5, pal6, pal7, pal8, pla9, pal10)

#to set UTAM outer limits as smoothing mask
Emprise <- as.owin(as(st_union(st_buffer(UTAM, 1000)), "Spatial"))

#to set margins to insert a main title and each map title 
par(oma=c(3.5,0,3,0)+0.1, mar = c(0, 0.5, 1.2, 0.5))

plot.new()

#to split the window into 3 rows and 2 columns (6 profiles)
par(mfrow=c(3,2))

#Loop for computing and mapping a smoothed surface of density for each profile
for (i in ListProfils){
  
  #To get the dataset of each profile
  fichier <- TypoProfilMarcheurs[which(TypoProfilMarcheurs$profil == i),]
  
  #To get the place of residence coordinates
  pts <- st_coordinates(st_geometry(st_as_sf(fichier, coords=c("X_Hog_Alea","Y_Hog_Alea"), crs = st_crs(UTAM)$srid)))
  
  #To create a ppp object (spatstat format) including the mask for smoothing
  fichier.ppp <- ppp(pts[,1], pts[,2], window = Emprise)
  
  #To set smoothing bandwidth (in meter)
  rayon <- 1000
  
  #To set pixel size (in meter)
  resol <- 100 # here 100m x 100m = 1 ha.
  
  #To calculate the surface of density (smoothing bandwidth : 1000 m and image spatial resolution : 100m x 100m = 1 ha)
  cartelissee <- density(fichier.ppp, sigma = rayon, kernel = "gaussian", eps = resol)
  
  #To coerce the smoothed surface to raster format
  cartelissee.raster <- raster(cartelissee)
  crs(cartelissee.raster) <- st_crs(UTAM)$srid # to specify a SCR for the raster object
  
  #To cut the raster based on UTAM mask
  cartelissee.raster <- mask(cartelissee.raster, UTAM) 
  
  #To coerce densities to numbers of pedestrians (multiplication of densities by the circle surface)
  values(cartelissee.raster) <- values(cartelissee.raster) * pi*rayon**2

  #To set breaks (equal intervals binning)
  bks <- seq(from = cellStats(cartelissee.raster, stat = min), 
            to = cellStats(cartelissee.raster, stat = max), 
            by = (cellStats(cartelissee.raster, stat = max) - cellStats(cartelissee.raster, stat = min)) / nclass)
  
  #To reclassify the smoothed surface
  cartelissee.reclass <- cut(cartelissee.raster, breaks = bks, include.lowest = FALSE, right = TRUE, dig.lab = 3, ordered_result = FALSE)
  
  #To vectorize the reclassed surface (takes a while)
  cartelissee.vecteur <- as(rasterToPolygons(cartelissee.reclass, n=4, na.rm=TRUE, digits=12, dissolve=TRUE), "sf")
  
  #To display the map
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
         title.txt = "Number of walker residence\nplaces in a 1-km radius.",
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

#To display the main title and sources
mtext("Concentration of walkers' home location by profile in the metropolitan area of Bogota", cex = 1.3, side=3,line=1,adj=0.5,outer=TRUE)
mtext("   Sources : Union Temporal STEER & CNC, EMD, 2019 - SDM - smoothing bandwidth : 1000 m, resolution : 1 ha", side=1, line=1, adj=0, cex=0.6, outer=TRUE)
```

# Mapping the places of destination corresponding to the first trip made by walkers from home (smoothed surfaces)
## Importing GIS layer containing the destinations of the pedestrians' first trips (ZAT -Traffic Analysis Zone- centroids)
```{r importing first trip destination GIS layer, warning=FALSE, message=FALSE, error=FALSE}
#The first walker trip is repeated between 4 and 5 times in a week, making it fairly representative of walker trips, as the second trip is somewhat less repeated, while the 3rd is much more occasional. 
destino <- st_read("ZAT_PrimerDestino.gpkg")
plot(st_geometry(destino), pch = "+", cex = 0.5)
```

## Preparing the dataset for generating maps
```{r Preparing the dataset for generating maps}
#Creating a list of pedestrians whose first destination is known 
listdestino <- as.character(destino$id_concat)

#Selecting pedestrians whose first destination is known
TypoProfilMarcheurs <- VariablesTypoUltraMarcheurs[VariablesTypoUltraMarcheurs$id_hog_pers %in% listdestino, ]

#Merging the profile (clust) to the destination places dataset
destino <- merge(destino, TypoProfilMarcheurs[c(22:23)], by.x = "id_concat", by.y = "id_hog_pers")

#Creating sf objects from which place names will be extracted for labeling maps
#For the places of destination
topodest1 <- UTAM[which(UTAM$UTAM_NOMBR == "USAQUEN" | UTAM$UTAM_NOMBR == "CHICO LAGO"),]
topodest2 <- UTAM[which(UTAM$UTAM_NOMBR == "LA CANDELARIA"| UTAM$UTAM_NOMBR == "TEUSAQUILLO" | UTAM$UTAM_NOMBR == "CHICO LAGO"| UTAM$UTAM_NOMBR == "SAN CRISTOBAL NORTE"| UTAM$UTAM_NOMBR == "BRITALIA"),]
topodest3 <- UTAM[which(UTAM$UTAM_NOMBR == "LA CANDELARIA"| UTAM$UTAM_NOMBR == "TEUSAQUILLO" | UTAM$UTAM_NOMBR == "SOACHA PUEBLO" | UTAM$UTAM_NOMBR == "20 DE JULIO" | UTAM$UTAM_NOMBR == "GRAN YOMASA" | UTAM$UTAM_NOMBR == "ENGATIVA" | UTAM$UTAM_NOMBR == "TENJO" | UTAM$UTAM_NOMBR == "VENECIA"| UTAM$UTAM_NOMBR == "SOPO"| UTAM$UTAM_NOMBR == "MINUTO DE DIOS"| UTAM$UTAM_NOMBR == "BOSA OCCIDENTAL"| UTAM$UTAM_NOMBR == "SUBA"| UTAM$UTAM_NOMBR == "LA URIBE"| UTAM$UTAM_NOMBR == "FONTIBON"| UTAM$UTAM_NOMBR == "CHICO LAGO"| UTAM$UTAM_NOMBR == "AMERICAS"| UTAM$UTAM_NOMBR == "TUNJUELITO"),]
topodest4 <- UTAM[which(UTAM$UTAM_NOMBR == "LA CANDELARIA"| UTAM$UTAM_NOMBR == "BOSA OCCIDENTAL" | UTAM$UTAM_NOMBR == "SOACHA PUEBLO" | UTAM$UTAM_NOMBR == "FUNZA" | UTAM$UTAM_NOMBR == "GRAN YOMASA"| UTAM$UTAM_NOMBR == "SOPO"| UTAM$UTAM_NOMBR == "TENJO" | UTAM$UTAM_NOMBR == "CHIA" | UTAM$UTAM_NOMBR == "CAJICA" | UTAM$UTAM_NOMBR == "EL ROSAL" | UTAM$UTAM_NOMBR == "BOJACA" | UTAM$UTAM_NOMBR == "FACATATIVA"| UTAM$UTAM_NOMBR == "LA CALERA" | UTAM$UTAM_NOMBR == "MADRID"| UTAM$UTAM_NOMBR == "MINUTO DE DIOS"| UTAM$UTAM_NOMBR == "ZIPAQUIRA"| UTAM$UTAM_NOMBR == "TOCANCIPA"| UTAM$UTAM_NOMBR == "GACHANCIPA"| UTAM$UTAM_NOMBR == "COTA"| UTAM$UTAM_NOMBR == "SIBATE"| UTAM$UTAM_NOMBR == "TABIO"| UTAM$UTAM_NOMBR == "SUBA"| UTAM$UTAM_NOMBR == "SAN CRISTOBAL NORTE"| UTAM$UTAM_NOMBR == "FONTIBON"| UTAM$UTAM_NOMBR == "VENECIA"| UTAM$UTAM_NOMBR == "20 DE JULIO"| UTAM$UTAM_NOMBR == "AMERICAS"| UTAM$UTAM_NOMBR == "CHAPINERO"),]
topodest5 <- UTAM[which(UTAM$UTAM_NOMBR == "TEUSAQUILLO"| UTAM$UTAM_NOMBR == "BOSA OCCIDENTAL" | UTAM$UTAM_NOMBR == "SOACHA PUEBLO" | UTAM$UTAM_NOMBR == "SUBA" | UTAM$UTAM_NOMBR == "CIUDAD USME" | UTAM$UTAM_NOMBR == "20 DE JULIO"| UTAM$UTAM_NOMBR == "TOCANCIPA"| UTAM$UTAM_NOMBR == "GACHANCIPA"| UTAM$UTAM_NOMBR == "LA CALERA"| UTAM$UTAM_NOMBR == "SOPO"| UTAM$UTAM_NOMBR == "GRAN YOMASA"| UTAM$UTAM_NOMBR == "FUNZA"| UTAM$UTAM_NOMBR == "MADRID"| UTAM$UTAM_NOMBR == "BOJACA"| UTAM$UTAM_NOMBR == "TUNJUELITO"| UTAM$UTAM_NOMBR == "LA URIBE"| UTAM$UTAM_NOMBR == "SIBATE"| UTAM$UTAM_NOMBR == "ISMAEL PERDOMO"| UTAM$UTAM_NOMBR == "EL ROSAL"| UTAM$UTAM_NOMBR == "MINUTO DE DIOS"| UTAM$UTAM_NOMBR == "SANTA CECILIA"| UTAM$UTAM_NOMBR == "FONTIBON"| UTAM$UTAM_NOMBR == "TENJO"| UTAM$UTAM_NOMBR == "LOS ALCAZARES"),]
topodest6 <- UTAM[which(UTAM$UTAM_NOMBR == "BOSA OCCIDENTAL" | UTAM$UTAM_NOMBR == "SUBA" | UTAM$UTAM_NOMBR == "GRAN YOMASA"| UTAM$UTAM_NOMBR == "LA URIBE"| UTAM$UTAM_NOMBR == "SOACHA PUEBLO"| UTAM$UTAM_NOMBR == "VENECIA"| UTAM$UTAM_NOMBR == "TEUSAQUILLO"| UTAM$UTAM_NOMBR == "ENGATIVA"| UTAM$UTAM_NOMBR == "FUNZA"| UTAM$UTAM_NOMBR == "KENNEDY CENTRAL"| UTAM$UTAM_NOMBR == "TUNJUELITO"| UTAM$UTAM_NOMBR == "RESTREPO"),]

#to save sf objects to a list
topodest <- list(topodest1, topodest2, topodest3, topodest4, topodest5, topodest6)
```


## Batch process for generating a second collection of maps showing the concentration of places refering to the first destination of walkers
```{r mapping the concentration of pedestrians first destination, warning=FALSE, fig.height=15, fig.width=15}

#to set margins to insert a main title and each map titles 
par(oma=c(3.5,0,3,0)+0.1, mar = c(0, 0.5, 1.2, 0.5))

plot.new()

#to split the window into 3 rows and 2 columns (6 profiles)
par(mfrow=c(3,2))

#to set the number of classes
nclass <- 5

#Loop for computing and mapping a smoothed surface of density for each profile
for (i in ListProfils){
  
  #To get the dataset of each profile
  fichier <- destino[which(destino$profil == i),]
  
  #To get the place of residence coordinates
  pts <- st_coordinates(st_geometry(fichier))
  
  #To create a ppp object (spatstat format) including the mask for smoothing
  fichier.ppp <- ppp(pts[,1], pts[,2], window = Emprise)
  
  #To set smoothing bandwidth (in meter)
  rayon <- 1000
  
  #To set pixel size (in meter)
  resol <- 100 # here 100m x 100m = 1 ha.
  
  #To calculate the surface of density (smoothing bandwidth : 1000 m and image spatial resolution : 100m x 100m = 1 ha)
  cartelissee <- density(fichier.ppp, sigma = rayon, kernel = "gaussian", eps = resol)
  
  #To coerce the smoothed surface to raster format
  cartelissee.raster <- raster(cartelissee)
  crs(cartelissee.raster) <- st_crs(UTAM)$srid
  
  #To cut the raster based on UTAM mask
  cartelissee.raster <- mask(cartelissee.raster, UTAM) 
  
  #To coerce densities to numbers of pedestrians (multiplication of densities by the circle surface)
  values(cartelissee.raster) <- values(cartelissee.raster) * pi*rayon**2

  #To set breaks (equal intervals binning)
  bks <- seq(from = cellStats(cartelissee.raster, stat = min), 
            to = cellStats(cartelissee.raster, stat = max), 
            by = (cellStats(cartelissee.raster, stat = max) - cellStats(cartelissee.raster, stat = min)) / nclass)
  
  #To reclassify the smoothed surface
  cartelissee.reclass <- cut(cartelissee.raster, breaks = bks, include.lowest = FALSE, right = TRUE, dig.lab = 3, ordered_result = FALSE)
  
  #To vectorize the reclassed surface (takes a while)
  cartelissee.vecteur <- as(rasterToPolygons(cartelissee.reclass, n=4, na.rm=TRUE, digits=12, dissolve=TRUE), "sf")
  
  #To display the map
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
         title.txt = "Number of destination places\nin a 1-km radius.",
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

#To display the main title and sources
mtext("Concentration of walkers' first destination places in the metropolitan area of Bogota", cex = 1.3, side=3,line=1,adj=0.5,outer=TRUE)
mtext("   Sources : Union Temporal STEER & CNC, EMD, 2019 - SDM - smoothing bandwidth : 1000 m, resolution : 1 ha", side=1, line=1, adj=0, cex=0.6, outer=TRUE)
```


