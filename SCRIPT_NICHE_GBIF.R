library (dismo)
library ('rgbif')
library (rgdal)
library (raster)
library (gtools)
library (rgeos)
library (rjson)
library (sp)
library (GSIF)
library (plotrix)
library (ggmap)
library (spatstat)
library (ecospat)
library (ggbiplot)
library (spatialEco)
library (fmsb)
library (Hmisc)
library (devtools)
library (ENMTools)

#Datos de GBIF
Dbroterigbif <- occ_search (scientificName = "Dianthus broteri",hasGeospatialIssue =FALSE, limit=50000, hasCoordinate=TRUE, basisOfRecord = "PRESERVED_SPECIMEN", return = "data")
Dinoxgbif <- occ_search (scientificName = "Dianthus inoxianus",hasGeospatialIssue =FALSE, limit=50000, hasCoordinate=TRUE, basisOfRecord = "PRESERVED_SPECIMEN", return = "data")
Dhinoxgbif <- occ_search (scientificName = "Dianthus hinoxianus",hasGeospatialIssue =FALSE, limit=50000, hasCoordinate=TRUE, basisOfRecord = "PRESERVED_SPECIMEN", return = "data")
Dvalengbif <- occ_search (scientificName = "Dianthus valentinus",hasGeospatialIssue =FALSE, limit=50000, hasCoordinate=TRUE, basisOfRecord = "PRESERVED_SPECIMEN", return = "data")

Dbroterigbif <- Dbroterigbif [Dbroterigbif$coordinateUncertaintyInMeters!="NA",]
Dbroterigbif <- Dbroterigbif [Dbroterigbif$coordinateUncertaintyInMeters<1000,]
# Dinoxgbif <- Dinoxgbif [Dinoxgbif$coordinateUncertaintyInMeters<1000,]
# Dinoxgbif <- Dinoxgbif [na.omit(Dinoxgbif$coordinateUncertaintyInMeters),]
# Dhinoxgbif <- Dhinoxgbif [Dhinoxgbif$coordinateUncertaintyInMeters<1000,]
# Dhinoxgbif <- Dhinoxgbif [na.omit(Dhinoxgbif$coordinateUncertaintyInMeters),]
# Dvalengbif <- Dvalengbif [Dvalengbif$coordinateUncertaintyInMeters<1000,]
# Dvalengbif <- Dvalengbif [na.omit(Dvalengbif$coordinateUncertaintyInMeters),]



todo <- rbind (Dbroterigbif,Dinoxgbif,Dhinoxgbif,Dvalengbif)
todo$name <- "Dianthus broteri"



gbifmap (todo, mapdatabase = "world", region = "Spain")


#Limpiar el dataframe
todo <- todo[,-c(1,2,5)]
todo_cleaned <- unique(todo)


#Mapa
e <- extent (-10,3.5,35.5,44)
coordinates(todo_cleaned) <- ~decimalLongitude+ decimalLatitude
crs.geo <- CRS ("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string (todo_cleaned) <- crs.geo 
map <- plot (gmap (e, type = "satellite"))
points <- points (Mercator(todo_cleaned), col = "red", pch=20, cex = 1.5)


# Quitamos los puntos en el mismo km^2
r <- raster(todo_cleaned)
res(r) <- 0.008333333
r <- extend(r, extent(r)+1)
cooDbroterigbif <- as.data.frame(gridSample(todo_cleaned, r, n=1))
cooDbroterigbif <- cbind(cooDbroterigbif, 1)
colnames(cooDbroterigbif)[3]<-"ploidy"
cooDbroterigbif$ploidy <- factor (cooDbroterigbif$ploidy, levels = c("1", "2x", "4x", "6x", "12x"), ordered = TRUE)


#Estimacion del nivel de ploidia por la ubicacion geografica (coordenadas)
cooDbroterigbif$ploidy[cooDbroterigbif$decimalLatitude > 38 & cooDbroterigbif$decimalLongitude > -9.5 & cooDbroterigbif$decimalLatitude < 39 & cooDbroterigbif$decimalLongitude < -8] <- "4x"
cooDbroterigbif$ploidy[cooDbroterigbif$decimalLatitude > 37 & cooDbroterigbif$decimalLongitude > -9 & cooDbroterigbif$decimalLatitude < 37.5 & cooDbroterigbif$decimalLongitude < -7.5] <- "2x"
cooDbroterigbif$ploidy[cooDbroterigbif$decimalLatitude > 36.9 & cooDbroterigbif$decimalLongitude > -7.4 & cooDbroterigbif$decimalLatitude < 37.6 & cooDbroterigbif$decimalLongitude < -6] <- "12x"
cooDbroterigbif$ploidy[cooDbroterigbif$decimalLatitude > 36 & cooDbroterigbif$decimalLongitude > -6.3 & cooDbroterigbif$decimalLatitude < 37.14 & cooDbroterigbif$decimalLongitude < -4.75] <- "4x"
cooDbroterigbif$ploidy[cooDbroterigbif$decimalLatitude > 36.5 & cooDbroterigbif$decimalLongitude > -4.75 & cooDbroterigbif$decimalLatitude < 38.1 & cooDbroterigbif$decimalLongitude < -1.67] <- "2x"
cooDbroterigbif$ploidy[cooDbroterigbif$decimalLatitude > 37.3 & cooDbroterigbif$decimalLongitude > -1.7 & cooDbroterigbif$decimalLatitude < 38.3 & cooDbroterigbif$decimalLongitude < -0.5] <- "6x"
cooDbroterigbif$ploidy[cooDbroterigbif$decimalLatitude > 38.2 & cooDbroterigbif$decimalLongitude > -2.33 & cooDbroterigbif$decimalLatitude < 38.83 & cooDbroterigbif$decimalLongitude < 0.33] <- "6x"
cooDbroterigbif$ploidy[cooDbroterigbif$decimalLatitude > 38.83 & cooDbroterigbif$decimalLongitude > -1.7 & cooDbroterigbif$decimalLatitude < 40.9 & cooDbroterigbif$decimalLongitude < 0.38] <- "4x"
cooDbroterigbif<-cooDbroterigbif[cooDbroterigbif$ploidy!="1",]


#===============PUNTOS EN DOÑANA (4x y 12x) DE FRAN===========#

e1 <- extent (-7,-6.4,36.8,37.5)

dianthuspops <- read.table ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/DianthusPoints.txt", header = T, sep = ",")
dianthuspops <- dianthuspops [,-c(1,4)]
dianthuspops <- dianthuspops [,c(2,1)]
colnames (dianthuspops) <- c("long", "lat")

coordinates(dianthuspops) <- ~long+ lat
proj4string(dianthuspops) <- crs.geo
r1 <- raster(dianthuspops)
res(r1) <- 0.008333333
r1 <- extend(r1, extent(r1)+1)
dianthuspops_fran <- as.data.frame(gridSample(dianthuspops, r1, n=1))

dianthuspops_fran <- cbind (dianthuspops_fran, 1)
colnames (dianthuspops_fran) [3] <- "ploidy"
dianthuspops_fran$ploidy <- factor (dianthuspops_fran$ploidy, levels = c("1", "4x", "12x"), ordered = TRUE)

dianthuspops_fran$ploidy[dianthuspops_fran$long < -6.72] <- "12x"
dianthuspops_fran$ploidy[dianthuspops_fran$long > -6.55] <- "12x"
dianthuspops_fran$ploidy[dianthuspops_fran$lat < 37.07] <- "12x"
dianthuspops_fran$ploidy[dianthuspops_fran$ploidy == "1"] <- "4x"

coordinates(dianthuspops_fran) <- ~long+ lat
proj4string(dianthuspops_fran) <- crs.geo
plot(gmap(e1, type = "satellite"))
points(Mercator(dianthuspops_fran), col=dianthuspops_fran$ploidy, pch=20, cex=1)

#===============PUNTOS EN DOÑANA (4x y 12x) DE FRAN===========#


dianthuspops_fran <- as.data.frame (dianthuspops_fran)
colnames(cooDbroterigbif) <- colnames(dianthuspops_fran)
cooDbroterigbif <- rbind (cooDbroterigbif, dianthuspops_fran)

cooDbroterigbif$ploidy<-factor(cooDbroterigbif$ploidy,levels = c("2x","4x","6x","12x"),ordered = TRUE)
ploidy <- as.data.frame (cooDbroterigbif$ploidy)

coordinates(cooDbroterigbif) <- ~long+ lat
proj4string(cooDbroterigbif) <- crs.geo
plot(gmap(e, type = "satellite"))
points(Mercator(cooDbroterigbif), col=cooDbroterigbif$ploidy, pch=20, cex=1)

cooDbroterigbifdata <- as.data.frame (cooDbroterigbif)
cooDbroterigbifdata <- cooDbroterigbifdata [,-3]
coordinates(cooDbroterigbifdata) <- ~long+ lat
proj4string(cooDbroterigbifdata) <- crs.geo


#carga de variables predictoras y union con mismos limites (chelsa, envirem, altitud, SoilGrids)
#extraccion de datos de las variables predictoras en las poblaciones

e <- extent (-10,3,35,42)

chelsafiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/chelsa", pattern = ".tif", full.names = TRUE))
chelsa <- stack (chelsafiles)
che.c <- crop (chelsa,e)

enviremfiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/envirem", pattern = ".bil", full.names = TRUE))
envirem <- stack (enviremfiles)
env.c <- crop (envirem, e)

alt15files <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/altitud_15", pattern = ".bil", full.names = TRUE))
alt16files <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/altitud_16", pattern = ".bil", full.names = TRUE))
alt15 <- stack (alt15files)
alt16 <- stack (alt16files)
alt.m <- merge (alt15, alt16, ext=e)

variables <- stack (che.c, env.c, alt.m)
names (variables) <- c("bio1","bio2","bio3","bio4","bio5","bio6","bio7","bio8","bio9","bio10","bio11","bio12","bio13","bio14","bio15","bio16","bio17","bio18","bio19","annualPET","aridityIndexThornthwaite","climaticMoistureIndex","continentality","embergerQ","growingDegDays0","growingDegDays5","maxTempColdest","minTempWarmest","monthCountByTemp10","PETColdestQuarter","PETDriestQuarter","PETseasonality","PETWarmestQuarter","PETWettestQuarter","thermicityIndex","topoWet","tri","elevation")

soilgrids<-extract.list(cooDbroterigbifdata, list.files("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas"),path = "D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas", ID = "ploidy")
colnames (soilgrids) <- c("ploidy","AWCh1","AWCh2","AWCh3","BLDFIE","CECSOL","ORCDRC","PHIHOX","SNDPPT","TEXMHT")
soilgrids$ploidy<-factor(soilgrids$ploidy, levels = c("2x", "4x", "6x", "12x"), ordered = TRUE)
soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="1","clay")
soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="2","silty clay")
soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="3","sandy clay")
soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="4","clay loam")
soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="5","silty clay loam")
soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="6","sandy clay loam")
soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="7","loam")
soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="8","silty loam")
soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="9","sandy loam")
soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="10","silt")
soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="11","loamy sand")
soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="12","sand")
soilgrids$TEXMHT<-factor(soilgrids$TEXMHT,levels = c("clay", "silty clay", "sandy clay", "clay loam","silty clay loam","sandy clay loam","loam","silty loam","sandy loam","silt","loamy sand","sand"))
soilgrids<-cbind(soilgrids,apply(soilgrids[,c(2:4)], 1, mean))
soilgrids<-soilgrids[,-c(1:4)]
colnames(soilgrids)[7]<-"AWC"
soilgrids <- soilgrids[,c(7,1,2,3,4,5,6)]


presvals <- extract (variables, cooDbroterigbifdata)
presvals <- cbind (ploidy, presvals, soilgrids) 
presvals$PHIHOX <- presvals$PHIHOX/10

#calculo del tri a partir de altitud con libreria spatialEco

tri.ext <- tri(alt.m)
projection(tri.ext) <- crs.geo 
trivalues<-extract(tri.ext,cooDbroterigbifdata)


#sustitucion de los valores tri por los del dataframe con NAs
presvals <- presvals[,-38]
presvals <- cbind (presvals, trivalues)
presvals <- presvals[,c(1:37,46,38:45)]
colnames(presvals)[38] <- "tri"
presvals2 <- na.omit (presvals)
ploidy <- as.data.frame(presvals2[,1])
colnames (ploidy) <- "ploidy"


#analisis para descartar variables muy correlacionadas
#PCA de puntos de presencia con variables seleccionadas

presvals.pca <- presvals2[,-c(1,46)]
selected <- vif_func(presvals.pca) 
presvals.pca.2 <- presvals.pca[,c(selected)]

ploidy <- ploidy$ploidy
ploidy <- factor (ploidy, levels = c("2x", "4x", "6x", "12x"), ordered = TRUE)

pca <- prcomp(presvals.pca.2, scale. = TRUE, retx = T)
ggbiplot(pca, obs.scale = 1,var.scale = 1,
         groups = ploidy, ellipse = TRUE, circle = FALSE, alpha =  1) +
  scale_color_discrete(name = '') +
  geom_point(aes(colour=ploidy), size = 3) +
  theme(legend.direction = 'vertical', legend.position = 'right')


#PCA con todas las variables
pca <- prcomp(presvals.pca, scale. = TRUE, retx = T)
ggbiplot(pca, obs.scale = 1,var.scale = 1,
         groups = ploidy, ellipse = TRUE, circle = FALSE, alpha =  1) +
  scale_color_discrete(name = '') +
  geom_point(aes(colour=ploidy), size = 3) +
  theme(legend.direction = 'vertical', legend.position = 'right')


#===============GENERAL BACKGROUND=============#
dbroteridata <- as.data.frame(cooDbroterigbifdata)
dbroteridata <- dbroteridata [,-3]
colnames(dbroteridata) <- c("long", "lat")
presvalsdata <- cbind (dbroteridata, presvals)
presvalsdata <- presvalsdata [,-48]
colnames (presvalsdata)[3] <- "ploidy"
presvalsdata <- na.omit (presvalsdata)
presvals2 <- presvalsdata
coordinates(presvals2) <- ~long+ lat
proj4string(presvals2) <- crs.geo

backgroundcoord <- presvalsdata [,c(1,2)]
coordinates(backgroundcoord) <- ~long+ lat
proj4string(backgroundcoord) <- crs.geo


mask <- raster(backgroundcoord)
res(mask) <- 0.008333333
x <- circles(backgroundcoord, d=50000, lonlat=TRUE)
#Se podria hacer un clip de los poligonos y el continente para que no salgan puntos en el mar, solucion provisional aumentar el N
pol <- gUnaryUnion(x@polygons)
samp <- spsample(pol, 1000, type='random', iter=25)
extent(mask) <- extent(pol) # Sirve para que las submuestras de los poligonos salgan en el extent de la muestra
cells <- cellFromXY(mask, samp)
length(cells)
cells <- unique(cells)
length(cells)
xy <- xyFromCell(mask, cells)
# A los puntos generados para el background unimos los de presencia para este citotipo (util para que no de error la funcion posterior ecospat.grid.clim.dyn)
xy <- as.data.frame(xy)
colnames(xy) <- c("long", "lat")

# Quitamos los puntos en el mismo km^2
coordinates(xy) <- ~long+ lat
proj4string(xy) <- crs.geo
r <- raster(xy)
res(r) <- 0.008333333
r <- extend(r, extent(r)+1)
backcoord_sel <- as.data.frame(gridSample(xy, r, n=1))
coordinates(backcoord_sel) <- ~long+ lat
proj4string(backcoord_sel) <- crs.geo

# Extraccion de variables para el background y modificacion de la tabla
backgroundclim <- extract(variables,backcoord_sel)
backgroundsoil <- extract.list(backcoord_sel, list.files("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas"),path = "D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas", ID = "ploidy")
backgrounddat <- cbind("background",as.data.frame(backcoord_sel),backgroundclim, backgroundsoil)
backgrounddat <- backgrounddat[,-42]

coordinates(backgrounddat) <- ~long+ lat
proj4string(backgrounddat) <- crs.geo
trivalues1 <- extract(tri.ext,backgrounddat)
backgrounddat <- as.data.frame(backgrounddat)
backgrounddat <- backgrounddat[,-40]
backgrounddat <- cbind (backgrounddat, trivalues1)
backgrounddat <- backgrounddat[,c(1:39,50,40:49)]
colnames(backgrounddat)[40] <- "tri" # Sustitucion de los valores tri por los del dataframe con NAs

backgrounddat.c <- na.omit(backgrounddat)
backgrounddat.c <- cbind(backgrounddat.c,apply(backgrounddat.c[,c(42:44)], 1, mean))
backgrounddat.c <- backgrounddat.c[,-c(42:44)]
colnames(backgrounddat.c)[48] <- "AWC"
backgrounddat.c <- backgrounddat.c[,c(1:41,48,42:47)]
backgrounddat.c <- backgrounddat.c[,-48]
backgrounddat.c <- backgrounddat.c[,c(2,3,1,4:47)]
colnames(backgrounddat.c) <- colnames(presvalsdata)

# Representacion de los puntos en el mapa alrededor de los de presencia
coordinates(backgrounddat.c) <- ~long+ lat
proj4string(backgrounddat.c) <- crs.geo
plot(gmap(e, type = "satellite"))
points(Mercator(backgrounddat.c), col = 'orange', pch=20, cex=1)
points(Mercator(presvals2), col=presvals2$ploidy, pch=20, cex=1)

#===============GENERAL BACKGROUND=============#

#===============PCA BACKGROUND=============#

todo <- rbind (presvalsdata, as.data.frame(backgrounddat.c))

todo.pca <- todo[,-c(1:3)]
selected2<-vif_func(todo.pca)
todo.pca.2 <- todo.pca[,c(selected2)]

todoploidy <- factor (todo$ploidy, levels = c("2x", "4x", "6x", "12x", "background"), ordered = TRUE)
w<-c(rep(0,nrow(presvalsdata)),rep(1,nrow(as.data.frame(backgrounddat.c))))

pcaback <-dudi.pca(todo.pca.2, row.w = w, center = TRUE, scale = TRUE, scannf = FALSE, nf = 2)
gcol = c("blue", "red", "green", "purple", "black")
s.label(pcaback$li, clabel = 0.1)
scatter(pcaback, clab.row = 0, posieig = "none", cex=0.1, clab.col = 0.5)
s.class(pcaback$li, todoploidy, col = gcol, add.plot = TRUE, cstar = 0, clabel = 0, cellipse = 1.5, pch = 16)
legend (9,8,c("2x", "4x", "6x", "12x","Background"), col = gcol, pch =19, text.width = 1.8, y.intersp = 0.5, cex = 0.8)


#===============ECOSPAT=============#

row.di<-which(todo[,3] == "2x")
row.te<-which(todo[,3] == "4x")
row.he<-which(todo[,3] == "6x")
row.do<-which(todo[,3] == "12x")


scores.clim<- pcaback$li 
scores.di<- pcaback$li[row.di,]		
scores.te<- pcaback$li[row.te,]	
scores.he<- pcaback$li[row.he,]	
scores.do<- pcaback$li[row.do,]	


zdi<- ecospat.grid.clim.dyn (scores.clim, scores.clim, scores.di, R=500)
zte<- ecospat.grid.clim.dyn (scores.clim, scores.clim, scores.te, R=500)
zhe<- ecospat.grid.clim.dyn (scores.clim, scores.clim, scores.he, R=500)
zdo<- ecospat.grid.clim.dyn (scores.clim, scores.clim, scores.do, R=500)

ecospat.plot.niche (zdi)
ecospat.plot.niche (zte)
ecospat.plot.niche (zhe)
ecospat.plot.niche (zdo)
ecospat.plot.niche.dyn (zdi, zte, quant = 0.75)


#EQUIVALENCY TEST
equivalency.test.dite<-ecospat.niche.equivalency.test (zdi, zte, 100, alternative = "lower")
equivalency.test.dihe<-ecospat.niche.equivalency.test (zdi, zhe, 100, alternative = "lower")
equivalency.test.dido<-ecospat.niche.equivalency.test (zdi, zdo, 100, alternative = "lower")
equivalency.test.tehe<-ecospat.niche.equivalency.test (zte, zhe, 100, alternative = "lower")
equivalency.test.tedo<-ecospat.niche.equivalency.test (zte, zdo, 100, alternative = "lower")
equivalency.test.hedo<-ecospat.niche.equivalency.test (zhe, zdo, 100, alternative = "lower")


#OVERLAP TEST
overlap.test.dite<-ecospat.niche.overlap (zdi, zte, cor=FALSE)
overlap.test.dihe<-ecospat.niche.overlap (zdi, zhe, cor=FALSE)
overlap.test.dido<-ecospat.niche.overlap (zdi, zdo, cor=FALSE)
overlap.test.tehe<-ecospat.niche.overlap (zte, zhe, cor=FALSE)
overlap.test.tedo<-ecospat.niche.overlap (zte, zdo, cor=FALSE)
overlap.test.hedo<-ecospat.niche.overlap (zhe, zdo, cor=FALSE)


#SIMILARITY TEST
similarity.testdite<-ecospat.niche.similarity.test (zdi, zte, 100, alternative = "greater")
similarity.testtedi<-ecospat.niche.similarity.test (zte, zdi, 100, alternative = "greater")
similarity.testdihe<-ecospat.niche.similarity.test (zdi, zhe, 100, alternative = "greater")
similarity.testhedi<-ecospat.niche.similarity.test (zhe, zdi, 100, alternative = "greater")
similarity.testdido<-ecospat.niche.similarity.test (zdi, zdo, 100, alternative = "greater")
similarity.testdodi<-ecospat.niche.similarity.test (zdo, zdi, 100, alternative = "greater")
similarity.testtehe<-ecospat.niche.similarity.test (zte, zhe, 100, alternative = "greater")
similarity.testhete<-ecospat.niche.similarity.test (zhe, zte, 100, alternative = "greater")
similarity.testtedo<-ecospat.niche.similarity.test (zte, zdo, 100, alternative = "greater")
similarity.testdote<-ecospat.niche.similarity.test (zdo, zte, 100, alternative = "greater")
similarity.testhedo<-ecospat.niche.similarity.test (zhe, zdo, 100, alternative = "greater")
similarity.testdohe<-ecospat.niche.similarity.test (zdo, zhe, 100, alternative = "greater")


# TABLA

overlap<-rbind(overlap.test.dite$D,overlap.test.dihe$D, overlap.test.dido$D,overlap.test.tehe$D, overlap.test.tedo$D, overlap.test.hedo$D)

similarityab<-rbind(similarity.testdite$p.D, similarity.testdihe$p.D, similarity.testdido$p.D, similarity.testtehe$p.D, similarity.testtedo$p.D, similarity.testhedo$p.D)
similarityba<-rbind(similarity.testtedi$p.D, similarity.testhedi$p.D, similarity.testdodi$p.D, similarity.testhete$p.D, similarity.testdote$p.D, similarity.testdohe$p.D)

equivalency<-rbind(equivalency.test.dite$p.D, equivalency.test.dihe$p.D, equivalency.test.dido$p.D, equivalency.test.tehe$p.D, equivalency.test.tedo$p.D, equivalency.test.hedo$p.D)

tablaresul<-data.frame(overlap,similarityab,similarityba,equivalency)

write.table (tablaresul, "results_gbif_vif.txt", sep = "\t")


#NICHE BREADTH
raster.breadth (zdi$w)
raster.breadth (zte$w)
raster.breadth (zhe$w)
raster.breadth (zdo$w)

boxplot (scores.di$Axis1, scores.te$Axis1, scores.he$Axis1, scores.do$Axis1, col = rainbow(4))
boxplot (scores.di$Axis2, scores.te$Axis2, scores.he$Axis2, scores.do$Axis2, col = rainbow(4))
