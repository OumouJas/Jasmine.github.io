##### Projet d'économétrie
install.packages("maptools", repos = "https://packagemanager.posit.co/cran/2023-10-13")

### Projet d'Économétrie Spatiale - Boston ###
# Script complet pour RStudio

### 1. Lecture des données shapefile
library(sp)
library(maptools)
library(spdep)
library(spatialreg)

#chemin du fichier
setwd("C:/Users/houmo/Documents/universite/UPPA_2024-2025/econometrie_Spatiale/Projet-20250514")  

data.spdf <- readShapePoly("datapoly.shp")

### 2. Carte des aires de recensement
plot(data.spdf)

### 3. Calcul des centres des polygones
centre <- coordinates(data.spdf)

### 4. Ajout des numéros des aires
noms <- rownames(data.spdf@data)
text(centre[,1], centre[,2], noms, cex=0.6, pos=3)

### 5. Matrice de poids spatiaux - triangulation de Delaunay
wtri.nb <- tri2nb(centre)
plot(data.spdf, border='grey', axes=TRUE)
text(centre[,1], centre[,2], noms, cex=.55)
plot(wtri.nb, centre, add=TRUE, col="red")

### 6. Diagramme de Moran et test de Moran
W <- nb2listw(wtri.nb, style="W")
moran.plot(data.spdf@data$MEDV, listw=W)
moran.test(data.spdf@data$MEDV, listw=W)


### 7. Estimation par MCO
lm.ols <- lm(MEDV ~ CRIM + RM + DIS + LSTAT + ZN + CHAS, data=data.spdf@data)
summary(lm.ols)

### 9. Test de Moran sur les résidus
lm.morantest(lm.ols, listw=W)

### 10. Tests RSerr, RSlag, robustes, Burridge, Anselin & Florax
lm.RStests(lm.ols, W, test="all")

#Test de facteur commun
lm.sem <- errorsarlm(MEDV ~ CRIM + RM + DIS + LSTAT + ZN + CHAS, data=data.spdf, listw=W)
lm.sdm <- lagsarlm(MEDV ~ CRIM + RM + DIS + LSTAT + ZN + CHAS, data=data.spdf, listw=W, type="mixed")
FC <- LR.Sarlm(lm.sdm, lm.sem)
1 - pchisq(FC[[1]][1], 3)

### 11. Estimation du modèle sélectionné
# Exemple : Modèle SAR
lm.sar <- lagsarlm(MEDV ~ CRIM + RM + DIS + LSTAT + ZN + CHAS, data=data.spdf, listw=W)
summary(lm.sar)

# Calcul des impacts si SAR ou SDM
Wc <- as(as_dgRMatrix_listw(W), "CsparseMatrix")
trMat <- trW(Wc, type="mult")
sar_impacts <- impacts(lm.sar, tr=trMat, R=1000)
summary(sar_impacts, zstats=TRUE, short=TRUE)

# Si SDM sélectionné
sdm_impacts <- impacts(lm.sdm, tr=trMat, R=1000)
summary(sdm_impacts, zstats=TRUE, short=TRUE)
