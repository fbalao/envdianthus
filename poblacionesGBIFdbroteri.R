library(dismo)
library('rgbif')
library(rgdal)
library(raster)
library(gtools)
library(rjson)
library(sp)
library(GSIF)
library(plotrix)
library(ggmap)
library(spatstat)
library(ecospat)


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
colnames(cooDbroterigbif)[3]<-"ploidy"


#Mapa
e <- extent (-10,3.5,35.5,44)
coordinates(cooDbroterigbif)<- ~decimalLongitude+ decimalLatitude
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(cooDbroterigbif) <- crs.geo 
map<-plot(gmap(e, type = "satellite"))
points<-points(Mercator(cooDbroterigbif), col = "red", pch=20, cex = 1.5)


#Estimacion del nivel de ploidia por la ubicacion geografica (coordenadas)
cooDbroterigbifdata<-as.data.frame(cooDbroterigbif)
cooDbroterigbifdata$ploidy[cooDbroterigbifdata$decimalLatitude > 38 & cooDbroterigbifdata$decimalLongitude > -9.5 & cooDbroterigbifdata$decimalLatitude < 39 & cooDbroterigbifdata$decimalLongitude < -8] <- "4x"
cooDbroterigbifdata$ploidy[cooDbroterigbifdata$decimalLatitude > 37 & cooDbroterigbifdata$decimalLongitude > -9 & cooDbroterigbifdata$decimalLatitude < 37.5 & cooDbroterigbifdata$decimalLongitude < -7.5] <- "2x"
cooDbroterigbifdata$ploidy[cooDbroterigbifdata$decimalLatitude > 36.9 & cooDbroterigbifdata$decimalLongitude > -7.4 & cooDbroterigbifdata$decimalLatitude < 37.6 & cooDbroterigbifdata$decimalLongitude < -6] <- "12x"
cooDbroterigbifdata$ploidy[cooDbroterigbifdata$decimalLatitude > 36 & cooDbroterigbifdata$decimalLongitude > -6.3 & cooDbroterigbifdata$decimalLatitude < 37.14 & cooDbroterigbifdata$decimalLongitude < -4.75] <- "4x"
cooDbroterigbifdata$ploidy[cooDbroterigbifdata$decimalLatitude > 36.5 & cooDbroterigbifdata$decimalLongitude > -4.75 & cooDbroterigbifdata$decimalLatitude < 38.1 & cooDbroterigbifdata$decimalLongitude < -1.67] <- "2x"
cooDbroterigbifdata$ploidy[cooDbroterigbifdata$decimalLatitude > 37.3 & cooDbroterigbifdata$decimalLongitude > -1.7 & cooDbroterigbifdata$decimalLatitude < 38.3 & cooDbroterigbifdata$decimalLongitude < -0.5] <- "6x"
cooDbroterigbifdata$ploidy[cooDbroterigbifdata$decimalLatitude > 38.2 & cooDbroterigbifdata$decimalLongitude > -2.33 & cooDbroterigbifdata$decimalLatitude < 38.83 & cooDbroterigbifdata$decimalLongitude < 0.33] <- "6x"
cooDbroterigbifdata$ploidy[cooDbroterigbifdata$decimalLatitude > 38.83 & cooDbroterigbifdata$decimalLongitude > -1.7 & cooDbroterigbifdata$decimalLatitude < 40.9 & cooDbroterigbifdata$decimalLongitude < 0.38] <- "4x"
#cooDbroterigbifdata$ploidy[c(16,311,324,405)] <- "1"
#cooDbroterigbifdata$ploidy[369] <- "6x"
#cooDbroterigbifdata$ploidy[c(16,285)] <- "4x"
cooDbroterigbifdata<-cooDbroterigbifdata[cooDbroterigbifdata$ploidy!="1",]

cooDbroterigbifdata$ploidy<-factor(cooDbroterigbifdata$ploidy,levels = c("2x","4x","6x","12x","1"),ordered = TRUE)

e <- extent (-10,3.5,35.5,44)
coordinates(cooDbroterigbifdata)<- ~decimalLongitude+ decimalLatitude
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(cooDbroterigbif) <- crs.geo
map<-plot(gmap(e, type = "satellite"))
points<-points(Mercator(cooDbroterigbifdata), col = cooDbroterigbifdata$ploidy, pch=20, cex = 1.5)

cooDbroterigbifdata<-readRDS(file = "cooDbroterigbifdata.RDS")

#Extraccion de valores de variables predictoras
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

ploidy <- as.data.frame(cooDbroterigbifdata)[,3]
ploidy <- as.data.frame (ploidy)
coord <- as.data.frame(cooDbroterigbifdata)[,c(1,2)]
coord <- as.data.frame (coord)

presvals <- extract (variables, cooDbroterigbifdata)
presvals <- cbind (coord, ploidy, presvals, soilgrids) 
presvals$PHIHOX <- presvals$PHIHOX/10

tri.ext <- tri(alt.m)
projection(tri.ext) <- crs.geo 
trivalues<-extract(tri.ext,cooDbroterigbifdata)

presvals <- presvals[,-40]
presvals <- cbind (presvals, trivalues)
presvals <- presvals[,c(1:39,48,40:47)]
colnames(presvals)[40] <- "tri"

#pca
presvals<-na.omit(presvals)

presvals.pca <- presvals[,-c(1,2,3,48)]
presvals.pca <- cbind (presvals.pca,1)
correlations <- corSelect (presvals.pca, var.cols = 1:44, sp.cols = 45, cor.thresh = 0.75)
selected <- correlations$selected.var.cols 
presvals.pca.2 <- presvals.pca[,c(selected)]

ploidy2 <- presvals[,3]
ploidy2 <- factor (ploidy2, levels = c("2x", "4x", "6x", "12x"), ordered = TRUE)

pca <- prcomp(presvals.pca.2, scale. = TRUE, retx = T)
ggbiplot(pca, obs.scale = 1,var.scale = 1,
         groups = ploidy2, ellipse = TRUE, circle = FALSE, alpha =  1) +
  scale_color_discrete(name = '') +
  geom_point(aes(colour=ploidy2), size = 2) +
  theme(legend.direction = 'vertical', legend.position = 'right')


#autocorrelacion espacial
# presvals.mantel <- presvals[,-48]
# presvals.mantel <- cbind (presvals.mantel,0,0,0,0)
# colnames(presvals.mantel)[48:51]<-c("2x","4x","6x","12x")
# presvals.mantel$`2x`[presvals.mantel$ploidy=="2x"]<-1
# presvals.mantel$`4x`[presvals.mantel$ploidy=="4x"]<-1
# presvals.mantel$`6x`[presvals.mantel$ploidy=="6x"]<-1
# presvals.mantel$`12x`[presvals.mantel$ploidy=="12x"]<-1
# presvals.mantel <- presvals.mantel[,-3]
# presvals.mantel <- presvals.mantel[,c(2,1,3:50)]
# ecospat.mantel.correlogram(dfvar=presvals.mantel,colxy=1:2, n=500, colvar=3:46, 
#                            max=1000, nclass=10, nperm=100)

presvals2 <- presvals[,-48]
coordinates(presvals2)<- ~decimalLongitude+ decimalLatitude
proj4string(presvals2) <- crs.geo 

set.seed(110)
mask <- raster(presvals2)
res(mask) <- 0.008333333
x <- circles(presvals2, d=1000, lonlat=TRUE)
#Se podría hacer un clip de los poligonos y el continente para que no salgan puntos en el mar , solucion provisional aumentar el N
pol <- gUnaryUnion(x@polygons)
samp <- spsample(pol, 500, type='random', iter=25)
extent(mask)<-extent(pol) # Sirve para que las submuestras de los poligonos salgan en el extent de la muestra
cells <- cellFromXY(mask, samp)
length(cells)
cells <- unique(cells)
length(cells)
xy <- xyFromCell(mask, cells)
xy <- as.data.frame(xy)
coordinates(xy)<- ~x+ y
proj4string(xy) <- crs.geo

plot(pol)
points(xy)

ploidy <- as.data.frame (ploidy)
plot(gmap(e, type = "satellite"))
points(Mercator(xy), col = 'blue', pch=20)
mycols <- c("black", "green", "red", "white")
palette(mycols)
points(Mercator(presvals2), col=presvals2$ploidy, pch=20, cex=1)

presvalsdata <- as.data.frame(presvals2)
#presvalsdata <- presvalsdata[,c(46,47,1:45)]

backgroundclim<-extract(variables,xy)
backgroundsoil<-extract.list(xy, list.files("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas"),path = "D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas", ID = "ploidy")
backgrounddat<-cbind("background",as.data.frame(xy),backgroundclim, backgroundsoil)
backgrounddat<-backgrounddat[,-42]
backgrounddat.c<-na.omit(backgrounddat)
backgrounddat.c<-cbind(backgrounddat.c,apply(backgrounddat.c[,c(42:44)], 1, mean))
backgrounddat.c<-backgrounddat.c[,-c(42:44)]
colnames(backgrounddat.c)[48]<-"AWC"
backgrounddat.c <- backgrounddat.c[,c(1:41,48,42:47)]
backgrounddat.c <- backgrounddat.c[,-48]
backgrounddat.c <- backgrounddat.c[,c(2,3,1,4:47)]
colnames(backgrounddat.c)<-colnames(presvalsdata)

#puntos background corregidos (quitando los NA de coordenadas en el mar)
coordinates(backgrounddat.c)<- ~long+ lat
proj4string(backgrounddat.c) <- crs.geo
plot(gmap(e, type = "satellite"))
points(Mercator(backgrounddat.c), col = 'blue', pch=20)
mycols <- c("black", "green", "red", "white")
palette(mycols)
points(Mercator(presvals2), col=presvals2$ploidy, pch=20, cex=1)



#background data (x ploidy)
diploid <- as.data.frame(cooDbroterigbifdata)
diploid$ploidy <- factor (diploid$ploidy, levels = c("2x", "4x", "6x", "12x"), ordered = TRUE)
diploid <- diploid[diploid$ploidy=="2x",]
tetraploid <- as.data.frame(cooDbroterigbifdata)
tetraploid$ploidy <- factor (tetraploid$ploidy, levels = c("2x", "4x", "6x", "12x"), ordered = TRUE)
tetraploid <- tetraploid[tetraploid$ploidy=="4x",]
hexaploid <- as.data.frame(cooDbroterigbifdata)
hexaploid$ploidy <- factor (hexaploid$ploidy, levels = c("2x", "4x", "6x", "12x"), ordered = TRUE)
hexaploid <- hexaploid[hexaploid$ploidy=="6x",]
dodecaploid <- as.data.frame(cooDbroterigbifdata)
dodecaploid$ploidy <- factor (dodecaploid$ploidy, levels = c("2x", "4x", "6x", "12x"), ordered = TRUE)
dodecaploid <- dodecaploid[dodecaploid$ploidy=="12x",] 

coordinates(diploid)<- ~decimalLongitude+ decimalLatitude
proj4string(diploid) <- crs.geo 
coordinates(tetraploid)<- ~decimalLongitude+ decimalLatitude
proj4string(tetraploid) <- crs.geo 
coordinates(hexaploid)<- ~decimalLongitude+ decimalLatitude
proj4string(hexaploid) <- crs.geo 
coordinates(dodecaploid)<- ~decimalLongitude+ decimalLatitude
proj4string(dodecaploid) <- crs.geo 


#===============DIPLOID=============#
maskdi <- raster(diploid)
res(maskdi) <- 0.008333333
xdi <- circles(diploid, d=1000, lonlat=TRUE)
poldi <- gUnaryUnion(xdi@polygons)
sampdi <- spsample(poldi, 100, type='random', iter=25)
extent(maskdi)<-extent(poldi)
cellsdi <- cellFromXY(maskdi, sampdi)
length(cellsdi)
cellsdi <- unique(cellsdi)
length(cellsdi)
xydi <- xyFromCell(maskdi, cellsdi)
xydi <- as.data.frame(xydi)
coordinates(xydi)<- ~x+ y
proj4string(xydi) <- crs.geo

dibackgroundclim<-extract(variables,xydi)
dibackgroundsoil<-extract.list(xydi, list.files("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas"),path = "D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas", ID = "ploidy")
dibackgrounddat<-cbind("dibackground",as.data.frame(xydi),dibackgroundclim, dibackgroundsoil)
dibackgrounddat<-dibackgrounddat[,-42]
dibackgrounddat.c<-na.omit(dibackgrounddat)
dibackgrounddat.c<-cbind(dibackgrounddat.c,apply(dibackgrounddat.c[,c(42:44)], 1, mean))
dibackgrounddat.c<-dibackgrounddat.c[,-c(42:44)]
colnames(dibackgrounddat.c)[48]<-"AWC"
dibackgrounddat.c <- dibackgrounddat.c[,c(1:41,48,42:47)]
dibackgrounddat.c <- dibackgrounddat.c[,-48]
dibackgrounddat.c <- dibackgrounddat.c[,c(2,3,1,4:47)]
colnames(dibackgrounddat.c)<-colnames(presvalsdata)

coordinates(dibackgrounddat.c)<- ~decimalLongitude+ decimalLatitude
proj4string(dibackgrounddat.c) <- crs.geo
plot(gmap(e, type = "satellite"))
points(Mercator(dibackgrounddat.c), col = 'blue', pch=20)
points(Mercator(presvals2), col=presvals2$ploidy, pch=20, cex=1)

#===============DIPLOID=============#


#===============TETRAPLOID=============#

maskte <- raster(tetraploid)
res(maskte) <- 0.008333333
xte <- circles(tetraploid, d=1000, lonlat=TRUE)
polte <- gUnaryUnion(xte@polygons)
sampte <- spsample(polte, 100, type='random', iter=25)
extent(maskte)<-extent(polte)
cellste <- cellFromXY(maskte, sampte)
length(cellste)
cellste <- unique(cellste)
length(cellste)
xyte <- xyFromCell(maskte, cellste)
xyte <- as.data.frame(xyte)
coordinates(xyte)<- ~x+ y
proj4string(xyte) <- crs.geo

tebackgroundclim<-extract(variables,xyte)
tebackgroundsoil<-extract.list(xyte, list.files("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas"),path = "D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas", ID = "ploidy")
tebackgrounddat<-cbind("tebackground",as.data.frame(xyte),tebackgroundclim, tebackgroundsoil)
tebackgrounddat<-tebackgrounddat[,-42]
tebackgrounddat.c<-na.omit(tebackgrounddat)
tebackgrounddat.c<-cbind(tebackgrounddat.c,apply(tebackgrounddat.c[,c(42:44)], 1, mean))
tebackgrounddat.c<-tebackgrounddat.c[,-c(42:44)]
colnames(tebackgrounddat.c)[48]<-"AWC"
tebackgrounddat.c <- tebackgrounddat.c[,c(1:41,48,42:47)]
tebackgrounddat.c <- tebackgrounddat.c[,-48]
tebackgrounddat.c <- tebackgrounddat.c[,c(2,3,1,4:47)]
colnames(tebackgrounddat.c)<-colnames(presvalsdata)

coordinates(tebackgrounddat.c)<- ~decimalLongitude+ decimalLatitude
proj4string(tebackgrounddat.c) <- crs.geo
plot(gmap(e, type = "satellite"))
points(Mercator(tebackgrounddat.c), col = 'blue', pch=20)

#===============TETRAPLOID=============#

#===============HEXAPLOID=============#

maskhe <- raster(hexaploid)
res(maskhe) <- 0.008333333
xhe <- circles(hexaploid, d=1000, lonlat=TRUE)
polhe <- gUnaryUnion(xhe@polygons)
samphe <- spsample(polhe, 100, type='random', iher=25)
extent(maskhe)<-extent(polhe)
cellshe <- cellFromXY(maskhe, samphe)
length(cellshe)
cellshe <- unique(cellshe)
length(cellshe)
xyhe <- xyFromCell(maskhe, cellshe)
xyhe <- as.data.frame(xyhe)
coordinates(xyhe)<- ~x+ y
proj4string(xyhe) <- crs.geo

hebackgroundclim<-extract(variables,xyhe)
hebackgroundsoil<-extract.list(xyhe, list.files("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas"),path = "D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas", ID = "ploidy")
hebackgrounddat<-cbind("hebackground",as.data.frame(xyhe),hebackgroundclim, hebackgroundsoil)
hebackgrounddat<-hebackgrounddat[,-42]
hebackgrounddat.c<-na.omit(hebackgrounddat)
hebackgrounddat.c<-cbind(hebackgrounddat.c,apply(hebackgrounddat.c[,c(42:44)], 1, mean))
hebackgrounddat.c<-hebackgrounddat.c[,-c(42:44)]
colnames(hebackgrounddat.c)[48]<-"AWC"
hebackgrounddat.c <- hebackgrounddat.c[,c(1:41,48,42:47)]
hebackgrounddat.c <- hebackgrounddat.c[,-48]
hebackgrounddat.c <- hebackgrounddat.c[,c(2,3,1,4:47)]
colnames(hebackgrounddat.c)<-colnames(presvalsdata)

coordinates(hebackgrounddat.c)<- ~decimalLongitude+ decimalLatitude
proj4string(hebackgrounddat.c) <- crs.geo
plot(gmap(e, type = "satellite"))
points(Mercator(hebackgrounddat.c), col = 'blue', pch=20)

#===============HEXAPLOID=============#


#===============DODECAPLOID=============#

maskdo <- raster(dodecaploid)
res(maskdo) <- 0.008333333
xdo <- circles(dodecaploid, d=1000, lonlat=TRUE)
poldo <- gUnaryUnion(xdo@polygons)
sampdo <- spsample(poldo, 100, type='random', idor=25)
extent(maskdo)<-extent(poldo)
cellsdo <- cellFromXY(maskdo, sampdo)
length(cellsdo)
cellsdo <- unique(cellsdo)
length(cellsdo)
xydo <- xyFromCell(maskdo, cellsdo)
xydo <- as.data.frame(xydo)
coordinates(xydo)<- ~x+ y
proj4string(xydo) <- crs.geo

dobackgroundclim<-extract(variables,xydo)
dobackgroundsoil<-extract.list(xydo, list.files("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas"),path = "D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas", ID = "ploidy")
dobackgrounddat<-cbind("dobackground",as.data.frame(xydo),dobackgroundclim, dobackgroundsoil)
dobackgrounddat<-dobackgrounddat[,-42]
dobackgrounddat.c<-na.omit(dobackgrounddat)
dobackgrounddat.c<-cbind(dobackgrounddat.c,apply(dobackgrounddat.c[,c(42:44)], 1, mean))
dobackgrounddat.c<-dobackgrounddat.c[,-c(42:44)]
colnames(dobackgrounddat.c)[48]<-"AWC"
dobackgrounddat.c <- dobackgrounddat.c[,c(1:41,48,42:47)]
dobackgrounddat.c <- dobackgrounddat.c[,-48]
dobackgrounddat.c <- dobackgrounddat.c[,c(2,3,1,4:47)]
colnames(dobackgrounddat.c)<-colnames(presvalsdata)

coordinates(dobackgrounddat.c)<- ~decimalLongitude+ decimalLatitude
proj4string(dobackgrounddat.c) <- crs.geo
plot(gmap(e, type = "satellite"))
points(Mercator(dobackgrounddat.c), col = 'blue', pch=20)

#===============DODECAPLOID=============#

todo <- rbind (presvalsdata, as.data.frame(backgrounddat.c), as.data.frame(dibackgrounddat.c), as.data.frame(tebackgrounddat.c), as.data.frame(hebackgrounddat.c), as.data.frame(dobackgrounddat.c))

todo.pca <- todo[,-c(1:3)]
todo.pca <- cbind (todo.pca,1)
correlations2 <- corSelect (todo.pca, var.cols = 1:44, sp.cols = 45, cor.thresh = 0.75)
selected2 <- correlations2$selected.var.cols 
todo.pca.2 <- todo.pca[,c(selected2)]

todoploidy <- todo$ploidy

pcaback <- prcomp(todo.pca.2, scale. = TRUE, retx = T)
ggbiplot(pcaback, obs.scale = 1,var.scale = 1,
         groups = todoploidy, ellipse = TRUE, circle = FALSE, alpha =  1) +
  scale_color_discrete(name = '') +
  geom_point(aes(colour=todoploidy), size = 2) +
  theme(legend.direction = 'vertical', legend.position = 'right')

#===============ECOSPAT=============#

row.di<-which(todo[,3] == "2x")
row.te<-which(todo[,3] == "4x")
row.he<-which(todo[,3] == "6x")
row.do<-which(todo[,3] == "12x")
row.back<-which(todo[,3] == "background") 
row.bacdi<-which(todo[,3] == "dibackground") 
row.bacte<-which(todo[,3] == "tebackground")
row.bache<-which(todo[,3] == "hebackground")
row.bacdo<-which(todo[,3] == "dobackground")

scores.clim<- pcaback$x[row.back,1:2] 
scores.di<- pcaback$x[row.di,1:2]		
scores.te<- pcaback$x[row.te,1:2]	
scores.he<- pcaback$x[row.he,1:2]	
scores.do<- pcaback$x[row.do,1:2]	
scores.bacdi<- pcaback$x[row.bacdi,1:2]					
scores.bacte<- pcaback$x[row.bacte,1:2]	
scores.bache<- pcaback$x[row.bache,1:2]	
scores.bacdo<- pcaback$x[row.bacdo,1:2]	

zdi<- ecospat.grid.clim.dyn2(scores.clim, scores.bacdi, scores.di, R=100, th.sp = 0.05, th.env = 0.05)
zte<- ecospat.grid.clim.dyn2(scores.clim, scores.bacte, scores.te, R=100, th.sp = 0.05, th.env = 0.05)
zhe<- ecospat.grid.clim.dyn2(scores.clim, scores.bache, scores.he, R=100, th.sp = 0.05, th.env = 0.05)
zdo<- ecospat.grid.clim.dyn2(scores.clim, scores.bacdo, scores.do, R=100, th.sp = 0.05, th.env = 0.05)

equivalency.test.dite<-ecospat.niche.equivalency.test (zdi, zte, 1000, alternative = "lower",ncores=1)
equivalency.test.dihe<-ecospat.niche.equivalency.test (zdi, zhe, 1000, alternative = "lower",ncores=1)
equivalency.test.dido<-ecospat.niche.equivalency.test (zdi, zdo, 1000, alternative = "lower",ncores=1)
equivalency.test.tehe<-ecospat.niche.equivalency.test (zte, zhe, 1000, alternative = "lower",ncores=1)
equivalency.test.tedo<-ecospat.niche.equivalency.test (zte, zdo, 1000, alternative = "lower",ncores=1)
equivalency.test.hedo<-ecospat.niche.equivalency.test (zhe, zdo, 1000, alternative = "lower",ncores=1)

overlap.test.dite<-ecospat.niche.overlap (zdi, zte, cor=FALSE)
overlap.test.dihe<-ecospat.niche.overlap (zdi, zhe, cor=FALSE)
overlap.test.dido<-ecospat.niche.overlap (zdi, zdo, cor=FALSE)
overlap.test.tehe<-ecospat.niche.overlap (zte, zhe, cor=FALSE)
overlap.test.tedo<-ecospat.niche.overlap (zte, zdo, cor=FALSE)
overlap.test.hedo<-ecospat.niche.overlap (zhe, zdo, cor=FALSE)

ecospat.plot.niche.dyn (zhe, zdo, quant = 0.75)

niche.dyn <- ecospat.niche.dyn.index(zdi, zte, intersection=0.05)
ecospat.plot.niche.dyn (zdi, zte, quant = 0.75)
ecospat.shift.centroids(scores.di, scores.te, scores.bacdi,scores.bacte)
