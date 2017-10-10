library(dismo)
library('rgbif')
library(rgdal)
library(raster)
library(gtools)
library(rgeos)
library(rjson)
library(sp)
library(GSIF)
library(plotrix)
library(ggmap)
library(spatstat)
library(ecospat)
library(Hmisc)

library(devtools)
install_github("danlwarren/ENMTools")
library(ENMTools)


#Datos de GBIF
Dbroterigbif<-occ_search(scientificName = "Dianthus broteri",hasGeospatialIssue =FALSE, limit=50000, fields='minimal', hasCoordinate=TRUE, basisOfRecord = "PRESERVED_SPECIMEN", return = "data")
Dinoxgbif<-occ_search(scientificName = "Dianthus inoxianus",hasGeospatialIssue =FALSE, limit=50000, fields='minimal', hasCoordinate=TRUE, basisOfRecord = "PRESERVED_SPECIMEN", return = "data")
Dhinoxgbif<-occ_search(scientificName = "Dianthus hinoxianus",hasGeospatialIssue =FALSE, limit=50000, fields='minimal', hasCoordinate=TRUE, basisOfRecord = "PRESERVED_SPECIMEN", return = "data")
Dvalengbif<-occ_search(scientificName = "Dianthus valentinus",hasGeospatialIssue =FALSE, limit=50000, fields='minimal', hasCoordinate=TRUE, basisOfRecord = "PRESERVED_SPECIMEN", return = "data")
todo<-rbind(Dbroterigbif,Dinoxgbif,Dhinoxgbif,Dvalengbif)
todo$name<-"Dianthus broteri"
gbifmap(todo, mapdatabase = "world", region = "Spain")


#Limpiar el dataframe
todo <- todo[,-c(1,2,5)]
todo_cleaned<- unique(todo)
cooDbroterigbif<-cbind(todo_cleaned,1)


#Mapa
e <- extent (-10,3.5,35.5,44)
coordinates(cooDbroterigbif)<- ~decimalLongitude+ decimalLatitude
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(cooDbroterigbif) <- crs.geo 
map<-plot(gmap(e, type = "satellite"))
points<-points(Mercator(cooDbroterigbif), col = "red", pch=20, cex = 1.5)

# Quitamos los puntos en el mismo km^2
coordinates(cooDbroterigbif) <- ~decimalLongitude+ decimalLatitude
proj4string(cooDbroterigbif) <- crs.geo
r <- raster(cooDbroterigbif)
res(r) <- 0.008333333
r <- extend(r, extent(r)+1)
coord_sel <- as.data.frame(gridSample(cooDbroterigbif, r, n=1))
coord_sel <- cbind (coord_sel, 1)
colnames(coord_sel)[3]<-"ploidy"



#Estimacion del nivel de ploidia por la ubicacion geografica (coordenadas)
coord_sel$ploidy[coord_sel$decimalLatitude > 38 & coord_sel$decimalLongitude > -9.5 & coord_sel$decimalLatitude < 39 & coord_sel$decimalLongitude < -8] <- "4x"
coord_sel$ploidy[coord_sel$decimalLatitude > 37 & coord_sel$decimalLongitude > -9 & coord_sel$decimalLatitude < 37.5 & coord_sel$decimalLongitude < -7.5] <- "2x"
coord_sel$ploidy[coord_sel$decimalLatitude > 36.9 & coord_sel$decimalLongitude > -7.4 & coord_sel$decimalLatitude < 37.6 & coord_sel$decimalLongitude < -6] <- "12x"
coord_sel$ploidy[coord_sel$decimalLatitude > 36 & coord_sel$decimalLongitude > -6.3 & coord_sel$decimalLatitude < 37.14 & coord_sel$decimalLongitude < -4.75] <- "4x"
coord_sel$ploidy[coord_sel$decimalLatitude > 36.5 & coord_sel$decimalLongitude > -4.75 & coord_sel$decimalLatitude < 38.1 & coord_sel$decimalLongitude < -1.67] <- "2x"
coord_sel$ploidy[coord_sel$decimalLatitude > 37.3 & coord_sel$decimalLongitude > -1.7 & coord_sel$decimalLatitude < 38.3 & coord_sel$decimalLongitude < -0.5] <- "6x"
coord_sel$ploidy[coord_sel$decimalLatitude > 38.2 & coord_sel$decimalLongitude > -2.33 & coord_sel$decimalLatitude < 38.83 & coord_sel$decimalLongitude < 0.33] <- "6x"
coord_sel$ploidy[coord_sel$decimalLatitude > 38.83 & coord_sel$decimalLongitude > -1.7 & coord_sel$decimalLatitude < 40.9 & coord_sel$decimalLongitude < 0.38] <- "4x"
#coord_sel$ploidy[c(16,311,324,405)] <- "1"
#coord_sel$ploidy[369] <- "6x"
#coord_sel$ploidy[c(16,285)] <- "4x"
cooDbroterigbifdata<-coord_sel[coord_sel$ploidy!="1",]

cooDbroterigbifdata$ploidy<-factor(cooDbroterigbifdata$ploidy,levels = c("2x","4x","6x","12x","1"),ordered = TRUE)
ploidy <- as.data.frame (cooDbroterigbifdata$ploidy)

coordinates(cooDbroterigbifdata) <- ~decimalLongitude+ decimalLatitude
proj4string(cooDbroterigbifdata) <- crs.geo
plot(gmap(e, type = "satellite"))
points(Mercator(cooDbroterigbifdata), col=cooDbroterigbifdata$ploidy, pch=20, cex=1)

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

cooDbroterigbifdata <- as.data.frame(cooDbroterigbifdata)
presvals <- cbind (presvals, cooDbroterigbifdata)
presvals <- presvals[,-1]
presvals <- presvals[,c(46:48, 1:45)]
presvals <- na.omit(presvals)
presvals2 <- presvals[,-48]

#analisis para descartar variables muy correlacionadas
#PCA de puntos de presencia con variables seleccionadas

# colvar <- presvals[c(2:45)]
# x <- cor(colvar, method="pearson")
# ecospat.npred (x, th=0.70)

presvals.pca <- presvals[,-c(1:3,48)]
presvals.pca <- cbind (presvals.pca,1)
correlations <- corSelect (presvals.pca, var.cols = 1:44, sp.cols = 45, cor.thresh = 0.70)
selected <- correlations$selected.var.cols 
presvals.pca.2 <- presvals.pca[,c(selected)]

ploidy <- presvals$ploidy
ploidy <- factor (ploidy, levels = c("2x", "4x", "6x", "12x"), ordered = TRUE)

pca <- prcomp(presvals.pca.2, scale. = TRUE, retx = T)
ggbiplot(pca, obs.scale = 1,var.scale = 1,
         groups = ploidy, ellipse = TRUE, circle = FALSE, alpha =  1) +
  scale_color_discrete(name = '') +
  geom_point(aes(colour=ploidy), size = 3) +
  theme(legend.direction = 'vertical', legend.position = 'right')


#PCA con todas las variables
pca <- prcomp(presvals[,-c(1,46)], scale. = TRUE, retx = T)
ggbiplot(pca, obs.scale = 1,var.scale = 1,
         groups = ploidy, ellipse = TRUE, circle = FALSE, alpha =  1) +
  scale_color_discrete(name = '') +
  geom_point(aes(colour=ploidy), size = 3) +
  theme(legend.direction = 'vertical', legend.position = 'right')



#===============GENERAL BACKGROUND=============#
backgroundcoord <- presvals [,c(1,2)]
coordinates(backgroundcoord) <- ~decimalLongitude+ decimalLatitude
proj4string(backgroundcoord) <- crs.geo


mask <- raster(backgroundcoord)
res(mask) <- 0.008333333
x <- circles(backgroundcoord, d=50000, lonlat=TRUE)
#Se podria hacer un clip de los poligonos y el continente para que no salgan puntos en el mar, solucion provisional aumentar el N
pol <- gUnaryUnion(x@polygons)
samp <- spsample(pol, 5000, type='random', iter=25)
extent(mask) <- extent(pol) # Sirve para que las submuestras de los poligonos salgan en el extent de la muestra
cells <- cellFromXY(mask, samp)
length(cells)
cells <- unique(cells)
length(cells)
xy <- xyFromCell(mask, cells)
# A los puntos generados para el background unimos los de presencia para este citotipo (util para que no de error la funcion posterior ecospat.grid.clim.dyn)
xy <- as.data.frame(xy)
colnames(xy) <- c("decimalLongitude", "decimalLatitude")

# Quitamos los puntos en el mismo km^2
coordinates(xy) <- ~decimalLongitude+ decimalLatitude
proj4string(xy) <- crs.geo
r <- raster(xy)
res(r) <- 0.008333333
r <- extend(r, extent(r)+1)
backcoord_sel <- as.data.frame(gridSample(xy, r, n=1))
coordinates(backcoord_sel) <- ~decimalLongitude+ decimalLatitude
proj4string(backcoord_sel) <- crs.geo

# Extraccion de variables para el background y modificacion de la tabla
backgroundclim <- extract(variables,backcoord_sel)
backgroundsoil <- extract.list(backcoord_sel, list.files("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas"),path = "D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas", ID = "ploidy")
backgrounddat <- cbind("background",as.data.frame(backcoord_sel),backgroundclim, backgroundsoil)
backgrounddat <- backgrounddat[,-42]

coordinates(backgrounddat) <- ~decimalLongitude+ decimalLatitude
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
colnames(backgrounddat.c) <- colnames(presvals2)

# Representacion de los puntos en el mapa alrededor de los de presencia
coordinates(backgrounddat.c) <- ~decimalLongitude+ decimalLatitude
proj4string(backgrounddat.c) <- crs.geo
coordinates(presvals) <- ~decimalLongitude+ decimalLatitude
proj4string(presvals) <- crs.geo

plot(gmap(e, type = "satellite"))
points(Mercator(backgrounddat.c), col = 'orange', pch=20, cex=1)
points(Mercator(presvals), col=presvals$ploidy, pch=20, cex=1)

#===============GENERAL BACKGROUND=============#

#===============PCA BACKGROUND=============#

todo <- rbind (presvals2, as.data.frame(backgrounddat.c))

todo.pca <- todo[,-c(1:3)]
todo.pca <- cbind (todo.pca,1)
correlations2 <- corSelect (todo.pca, var.cols = 1:44, sp.cols = 45, cor.thresh = 0.75)
selected2 <- correlations2$selected.var.cols 
todo.pca.2 <- todo.pca[,c(selected2)]

todoploidy <- todo$ploidy
w<-c(rep(0,nrow(presvals2)),rep(1,nrow(as.data.frame(backgrounddat.c))))

pcaback <-dudi.pca(todo.pca.2, row.w = w, center = TRUE, scale = TRUE, scannf = FALSE, nf = 2)
gcol = c("blue", "red", "green", "yellow", "orange")
s.label(pcaback$li, clabel = 0.1)
scatter(pcaback, clab.row = 0, posieig = "none", cex=0.1)
s.class(pcaback$li, todo[,3], col = gcol, add.plot = TRUE, cstar = 0, clabel = 0, cellipse = 1.5, pch = 16)


#===============ECOSPAT=============#

row.di<-which(todo[,3] == "2x")
row.te<-which(todo[,3] == "4x")
row.he<-which(todo[,3] == "6x")
row.do<-which(todo[,3] == "12x")
row.back<-which(todo[,3] == "background") 


scores.clim<- pcaback$li 
scores.di<- pcaback$li[row.di,]		
scores.te<- pcaback$li[row.te,]	
scores.he<- pcaback$li[row.he,]	
scores.do<- pcaback$li[row.do,]	


zdi<- ecospat.grid.clim.dyn (scores.clim, scores.clim, scores.di, R=100)
zte<- ecospat.grid.clim.dyn (scores.clim, scores.clim, scores.te, R=100)
zhe<- ecospat.grid.clim.dyn (scores.clim, scores.clim, scores.he, R=100)
zdo<- ecospat.grid.clim.dyn (scores.clim, scores.clim, scores.do, R=100)


#ecospat (tests)
equivalency.test.dite<-ecospat.niche.equivalency.test (zdi, zte, 100, alternative = "lower")
equivalency.test.dihe<-ecospat.niche.equivalency.test (zdi, zhe, 100, alternative = "lower")
equivalency.test.dido<-ecospat.niche.equivalency.test (zdi, zdo, 100, alternative = "lower")
equivalency.test.tehe<-ecospat.niche.equivalency.test (zte, zhe, 100, alternative = "lower")
equivalency.test.tedo<-ecospat.niche.equivalency.test (zte, zdo, 100, alternative = "lower")
equivalency.test.hedo<-ecospat.niche.equivalency.test (zhe, zdo, 100, alternative = "lower")

overlap.test.dite<-ecospat.niche.overlap (zdi, zte, cor=FALSE)
overlap.test.dihe<-ecospat.niche.overlap (zdi, zhe, cor=FALSE)
overlap.test.dido<-ecospat.niche.overlap (zdi, zdo, cor=FALSE)
overlap.test.tehe<-ecospat.niche.overlap (zte, zhe, cor=FALSE)
overlap.test.tedo<-ecospat.niche.overlap (zte, zdo, cor=FALSE)
overlap.test.hedo<-ecospat.niche.overlap (zhe, zdo, cor=FALSE)

ecospat.plot.niche.dyn (zhe, zdo, quant = 0.75)
ecospat.plot.niche (zdi)
ecospat.plot.niche (zte)
ecospat.plot.niche (zhe)
ecospat.plot.niche (zdo)


similarity.testdite<-ecospat.niche.similarity.test (zdi, zte, 100, alternative = "greater")
similarity.testtedi<-ecospat.niche.similarity.test (zte, zdi, 100, alternative = "greater")
similarity.testdihe<-ecospat.niche.similarity.test (zdi, zhe, 100, alternative = "greater")
similarity.testhedi<-ecospat.niche.similarity.test (zhe, zdi, 100, alternative = "greater")
similarity.testdido<-ecospat.niche.similarity.test (zdi, zdo, 100, alternative = "lower")
similarity.testdodi<-ecospat.niche.similarity.test (zdo, zdi, 100, alternative = "lower")
similarity.testtehe<-ecospat.niche.similarity.test (zte, zhe, 100, alternative = "lower")
similarity.testhete<-ecospat.niche.similarity.test (zhe, zte, 100, alternative = "lower")
similarity.testtedo<-ecospat.niche.similarity.test (zte, zdo, 100, alternative = "lower")
similarity.testdote<-ecospat.niche.similarity.test (zdo, zte, 100, alternative = "lower")
similarity.testhedo<-ecospat.niche.similarity.test (zhe, zdo, 100, alternative = "lower")
similarity.testdohe<-ecospat.niche.similarity.test (zdo, zhe, 100, alternative = "lower")


raster.breadth (zdi)
