library (dismo)
library (raster)
# library (rJava)
# library (rgdal)
library (rgeos)
# library (rgbif)
# library (rjson)
library (gtools)
# library (maps)
# library (ggmap)
library (fuzzySim)
# library (ade4)
# library (pcaMethods)
library (ecospat)
library (sp)
library (GSIF)
# library (caret)
# library (RCurl)
# library (gdalUtils)
# library (plotKML)
# library (XML)
# library (lattice)
# library (aqp)
# library (soiltexture)
library (ggbiplot)
# library (psych)
library (spatialEco)
library (spThin)


#dataset de poblaciones con coordenadas

dbroteri <- read.delim2 (file="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/Poblaciones_Nicho.csv", sep = ";", fileEncoding = "latin1", colClasses = c("factor", "factor", "numeric", "numeric"))
ploidy <- dbroteri[,2]
ploidy <- as.data.frame (ploidy)
rownames (ploidy) <- c("Monte Clerigo","Sao Bras de Alportel","Zafarraya 1","Zafarraya 2","Orgiva","Laroles","Lliria","Troia","Comporta","Donana (Peladillo)","Albufera de Valencia","Alcublas","Azuebar","Sierra de Espadan","Chiclana","Ronda","Calblanque (Cabezo de la Fuente)","Socovos","Cartagena","San Miguel de Salinas","Penon de Ifach","Valverde del Camino","Moguer","Hinojos","Donana (Acebron)","Donana (Puntal)","Huertos del Batan","Isla Cristina (Las Palmeritas)")
coordinates (dbroteri) <- ~long + lat
crs.geo <- CRS ("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string (dbroteri) <- crs.geo


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

soilgrids<-extract.list(dbroteri, list.files("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas"),path = "D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas", ID = "ploidy")
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


presvals <- extract (variables, dbroteri)
presvals <- cbind (ploidy, presvals, soilgrids) 
presvals$PHIHOX <- presvals$PHIHOX/10

#calculo del tri a partir de altitud con libreria spatialEco

tri.ext <- tri(alt.m)
projection(tri.ext) <- crs.geo 
trivalues<-extract(tri.ext,dbroteri)


#sustitucion de los valores tri por los del dataframe con NAs
presvals <- presvals[,-38]
presvals <- cbind (presvals, trivalues)
presvals <- presvals[,c(1:37,46,38:45)]
colnames(presvals)[38] <- "tri"


#analisis para descartar variables muy correlacionadas
#PCA de puntos de presencia con variables seleccionadas

colvar <- presvals[c(2:45)]
x <- cor(colvar, method="pearson")
ecospat.npred (x, th=0.70)

presvals.pca <- presvals[,-c(1,46)]
presvals.pca <- cbind (presvals.pca,1)
correlations <- corSelect (presvals.pca, var.cols = 1:44, sp.cols = 45, cor.thresh = 0.70)
selected <- correlations$selected.var.cols 
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
pca <- prcomp(presvals[,-c(1,46)], scale. = TRUE, retx = T)
ggbiplot(pca, obs.scale = 1,var.scale = 1,
         groups = ploidy, ellipse = TRUE, circle = FALSE, alpha =  1) +
  scale_color_discrete(name = '') +
  geom_point(aes(colour=ploidy), size = 3) +
  theme(legend.direction = 'vertical', legend.position = 'right')


#===============PREPARATION OF DATAFRAMES=============#

dbroteridata <- as.data.frame(dbroteri)
presvalsdata <- cbind (dbroteridata[,c(3,4)], presvals)
presvalsdata <- presvalsdata [,-48]
presvals2 <- presvalsdata
presvals2 <- presvals2 [,c(2,1,3:47)]
coordinates(presvalsdata)<- ~long+ lat
proj4string(presvalsdata) <- crs.geo 

diploid <- as.data.frame(dbroteri[-c(7:28),])
diploid <- diploid [,-c(1,2)]
tetraploid <- as.data.frame(dbroteri[c(7:16),])
tetraploid <- tetraploid [,-c(1,2)]
hexaploid <- as.data.frame(dbroteri[c(17:21),])
hexaploid <- hexaploid [,-c(1,2)]
dodecaploid <- as.data.frame(dbroteri[c(22:28),])
dodecaploid <- dodecaploid [,-c(1,2)]  

coordinates(diploid)<- ~long+ lat
proj4string(diploid) <- crs.geo 
coordinates(tetraploid)<- ~long+ lat
proj4string(tetraploid) <- crs.geo 
coordinates(hexaploid)<- ~long+ lat
proj4string(hexaploid) <- crs.geo 
coordinates(dodecaploid)<- ~long+ lat
proj4string(dodecaploid) <- crs.geo 

#===============PREPARATION OF DATAFRAMES=============#


#===============DIPLOID=============#

maskdi <- raster(diploid)
res(maskdi) <- 0.008333333
xdi <- circles(diploid, d=50000, lonlat=TRUE)
#Se podria hacer un clip de los poligonos y el continente para que no salgan puntos en el mar, solucion provisional aumentar el N
poldi <- gUnaryUnion(xdi@polygons)
sampdi <- spsample(poldi, 1000, type='random', iter=25)
extent(maskdi) <- extent(poldi) # Sirve para que las submuestras de los poligonos salgan en el extent de la muestra
cellsdi <- cellFromXY(maskdi, sampdi)
length(cellsdi)
cellsdi <- unique(cellsdi)
length(cellsdi)
xydi <- xyFromCell(maskdi, cellsdi)
# A los puntos generados para el background unimos los de presencia para este citotipo (util para que no de error la funcion posterior ecospat.grid.clim.dyn)
xydi <- as.data.frame(xydi)
diploid <- as.data.frame(diploid)
colnames(xydi) <- colnames(diploid)
dibackcoord <- rbind(xydi, diploid)

# Quitamos los puntos en el mismo km^2
coordinates(dibackcoord) <- ~long+ lat
proj4string(dibackcoord) <- crs.geo
r <- raster(dibackcoord)
res(r) <- 0.008333333
r <- extend(r, extent(r)+1)
dibackcoord_sel <- as.data.frame(gridSample(dibackcoord, r, n=1))
coordinates(dibackcoord_sel) <- ~long+ lat
proj4string(dibackcoord_sel) <- crs.geo

# Extraccion de variables para el background y modificacion de la tabla
dibackgroundclim <- extract(variables,dibackcoord_sel)
dibackgroundsoil <- extract.list(dibackcoord_sel, list.files("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas"),path = "D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas", ID = "ploidy")
dibackgrounddat <- cbind("dibackground",as.data.frame(dibackcoord_sel),dibackgroundclim, dibackgroundsoil)
dibackgrounddat <- dibackgrounddat[,-42]

coordinates(dibackgrounddat) <- ~long+ lat
proj4string(dibackgrounddat) <- crs.geo
trivalues1 <- extract(tri.ext,dibackgrounddat)
dibackgrounddat <- as.data.frame(dibackgrounddat)
dibackgrounddat <- dibackgrounddat[,-40]
dibackgrounddat <- cbind (dibackgrounddat, trivalues1)
dibackgrounddat <- dibackgrounddat[,c(1:39,50,40:49)]
colnames(dibackgrounddat)[40] <- "tri" # Sustitucion de los valores tri por los del dataframe con NAs

dibackgrounddat.c <- na.omit(dibackgrounddat)
dibackgrounddat.c <- cbind(dibackgrounddat.c,apply(dibackgrounddat.c[,c(42:44)], 1, mean))
dibackgrounddat.c <- dibackgrounddat.c[,-c(42:44)]
colnames(dibackgrounddat.c)[48] <- "AWC"
dibackgrounddat.c <- dibackgrounddat.c[,c(1:41,48,42:47)]
dibackgrounddat.c <- dibackgrounddat.c[,-48]
dibackgrounddat.c <- dibackgrounddat.c[,c(2,3,1,4:47)]
colnames(dibackgrounddat.c) <- colnames(presvals2)

# Representacion de los puntos en el mapa alrededor de los de presencia
coordinates(dibackgrounddat.c) <- ~long+ lat
proj4string(dibackgrounddat.c) <- crs.geo
plot(gmap(e, type = "satellite"))
points(Mercator(dibackgrounddat.c), col = 'orange', pch=20, cex=1)
points(Mercator(presvalsdata), col=presvalsdata$ploidy, pch=20, cex=1)

#===============DIPLOID=============#


#===============TETRAPLOID=============#

maskte <- raster(tetraploid)
res(maskte) <- 0.008333333
xte <- circles(tetraploid, d=50000, lonlat=TRUE)
#Se podria hacer un clip de los poligonos y el continente para que no salgan puntos en el mar, solucion provisional aumentar el N
polte <- gUnaryUnion(xte@polygons)
sampte <- spsample(polte, 1000, type='random', iter=25)
extent(maskte) <- extent(polte) # Sirve para que las submuestras de los poligonos salgan en el extent de la muestra
cellste <- cellFromXY(maskte, sampte)
length(cellste)
cellste <- unique(cellste)
length(cellste)
xyte <- xyFromCell(maskte, cellste)
# A los puntos generados para el background unimos los de presencia para este citotipo (util para que no de error la funcion posterior ecospat.grid.clim.dyn)
xyte <- as.data.frame(xyte)
tetraploid <- as.data.frame(tetraploid)
colnames(xyte) <- colnames(tetraploid)
tebackcoord <- rbind(xyte, tetraploid)

# Quitamos los puntos en el mismo km^2
coordinates(tebackcoord) <- ~long+ lat
proj4string(tebackcoord) <- crs.geo
r <- raster(tebackcoord)
res(r) <- 0.008333333
r <- extend(r, extent(r)+1)
tebackcoord_sel <- as.data.frame(gridSample(tebackcoord, r, n=1))
coordinates(tebackcoord_sel) <- ~long+ lat
proj4string(tebackcoord_sel) <- crs.geo

# Extraccion de variables para el background y modificacion de la tabla
tebackgroundclim <- extract(variables,tebackcoord_sel)
tebackgroundsoil <- extract.list(tebackcoord_sel, list.files("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas"),path = "D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas", ID = "ploidy")
tebackgrounddat <- cbind("tebackground",as.data.frame(tebackcoord_sel),tebackgroundclim, tebackgroundsoil)
tebackgrounddat <- tebackgrounddat[,-42]

coordinates(tebackgrounddat) <- ~long+ lat
proj4string(tebackgrounddat) <- crs.geo
trivalues2 <- extract(tri.ext,tebackgrounddat)
tebackgrounddat <- as.data.frame(tebackgrounddat)
tebackgrounddat <- tebackgrounddat[,-40]
tebackgrounddat <- cbind (tebackgrounddat, trivalues2)
tebackgrounddat <- tebackgrounddat[,c(1:39,50,40:49)]
colnames(tebackgrounddat)[40] <- "tri" # Sustitucion de los valores tri por los del dataframe con NAs

tebackgrounddat.c <- na.omit(tebackgrounddat)
tebackgrounddat.c <- cbind(tebackgrounddat.c,apply(tebackgrounddat.c[,c(42:44)], 1, mean))
tebackgrounddat.c <- tebackgrounddat.c[,-c(42:44)]
colnames(tebackgrounddat.c)[48] <- "AWC"
tebackgrounddat.c <- tebackgrounddat.c[,c(1:41,48,42:47)]
tebackgrounddat.c <- tebackgrounddat.c[,-48]
tebackgrounddat.c <- tebackgrounddat.c[,c(2,3,1,4:47)]
colnames(tebackgrounddat.c) <- colnames(presvals2)

# Representacion de los puntos en el mapa alrededor de los de presencia
coordinates(tebackgrounddat.c) <- ~long+ lat
proj4string(tebackgrounddat.c) <- crs.geo
plot(gmap(e, type = "satellite"))
points(Mercator(tebackgrounddat.c), col = 'orange', pch=20, cex=1)
points(Mercator(presvalsdata), col=presvalsdata$ploidy, pch=20, cex=1)

#===============TETRAPLOID=============#

#===============HEXAPLOID=============#

maskhe <- raster(hexaploid)
res(maskhe) <- 0.008333333
xhe <- circles(hexaploid, d=30000, lonlat=TRUE)
#Se podria hacer un clip de los poligonos y el continente para que no salgan puntos en el mar, solucion provisional aumentar el N
polhe <- gUnaryUnion(xhe@polygons)
samphe <- spsample(polhe, 500, type='random', iter=25)
extent(maskhe) <- extent(polhe) # Sirve para que las submuestras de los poligonos salgan en el extent de la muestra
cellshe <- cellFromXY(maskhe, samphe)
length(cellshe)
cellshe <- unique(cellshe)
length(cellshe)
xyhe <- xyFromCell(maskhe, cellshe)
# A los puntos generados para el background unimos los de presencia para este citotipo (util para que no de error la funcion posterior ecospat.grid.clim.dyn)
xyhe <- as.data.frame(xyhe)
hexaploid <- as.data.frame(hexaploid)
colnames(xyhe) <- colnames(hexaploid)
hebackcoord <- rbind(xyhe, hexaploid)

# Quitamos los puntos en el mismo km^2
coordinates(hebackcoord) <- ~long+ lat
proj4string(hebackcoord) <- crs.geo
r <- raster(hebackcoord)
res(r) <- 0.008333333
r <- extend(r, extent(r)+1)
hebackcoord_sel <- as.data.frame(gridSample(hebackcoord, r, n=1))
coordinates(hebackcoord_sel) <- ~long+ lat
proj4string(hebackcoord_sel) <- crs.geo

# Extraccion de variables para el background y modificacion de la tabla
hebackgroundclim <- extract(variables,hebackcoord_sel)
hebackgroundsoil <- extract.list(hebackcoord_sel, list.files("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas"),path = "D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas", ID = "ploidy")
hebackgrounddat <- cbind("hebackground",as.data.frame(hebackcoord_sel),hebackgroundclim, hebackgroundsoil)
hebackgrounddat <- hebackgrounddat[,-42]

coordinates(hebackgrounddat) <- ~long+ lat
proj4string(hebackgrounddat) <- crs.geo
trivalues3 <- extract(tri.ext,hebackgrounddat)
hebackgrounddat <- as.data.frame(hebackgrounddat)
hebackgrounddat <- hebackgrounddat[,-40]
hebackgrounddat <- cbind (hebackgrounddat, trivalues3)
hebackgrounddat <- hebackgrounddat[,c(1:39,50,40:49)]
colnames(hebackgrounddat)[40] <- "tri" # Sustitucion de los valores tri por los del dataframe con NAs

hebackgrounddat.c <- na.omit(hebackgrounddat)
hebackgrounddat.c <- cbind(hebackgrounddat.c,apply(hebackgrounddat.c[,c(42:44)], 1, mean))
hebackgrounddat.c <- hebackgrounddat.c[,-c(42:44)]
colnames(hebackgrounddat.c)[48] <- "AWC"
hebackgrounddat.c <- hebackgrounddat.c[,c(1:41,48,42:47)]
hebackgrounddat.c <- hebackgrounddat.c[,-48]
hebackgrounddat.c <- hebackgrounddat.c[,c(2,3,1,4:47)]
colnames(hebackgrounddat.c) <- colnames(presvals2)

# Representacion de los puntos en el mapa alrededor de los de presencia
coordinates(hebackgrounddat.c) <- ~long+ lat
proj4string(hebackgrounddat.c) <- crs.geo
plot(gmap(e, type = "satellite"))
points(Mercator(hebackgrounddat.c), col = 'orange', pch=20, cex=1)
points(Mercator(presvalsdata), col=presvalsdata$ploidy, pch=20, cex=1)

#===============HEXAPLOID=============#


#===============DODECAPLOID=============#

maskdo <- raster(dodecaploid)
res(maskdo) <- 0.008333333
xdo <- circles(dodecaploid, d=25000, lonlat=TRUE)
#Se podria hacer un clip de los poligonos y el continente para que no salgan puntos en el mar, solucion provisional aumentar el N
poldo <- gUnaryUnion(xdo@polygons)
sampdo <- spsample(poldo, 250, type='random', iter=25)
extent(maskdo) <- extent(poldo) # Sirve para que las submuestras de los poligonos salgan en el extent de la muestra
cellsdo <- cellFromXY(maskdo, sampdo)
length(cellsdo)
cellsdo <- unique(cellsdo)
length(cellsdo)
xydo <- xyFromCell(maskdo, cellsdo)
# A los puntos generados para el background unimos los de presencia para este citotipo (util para que no de error la funcion posterior ecospat.grid.clim.dyn)
xydo <- as.data.frame(xydo)
dodecaploid <- as.data.frame(dodecaploid)
colnames(xydo) <- colnames(dodecaploid)
dobackcoord <- rbind(xydo, dodecaploid)

# Quitamos los puntos en el mismo km^2
coordinates(dobackcoord) <- ~long+ lat
proj4string(dobackcoord) <- crs.geo
r <- raster(dobackcoord)
res(r) <- 0.008333333
r <- extend(r, extent(r)+1)
dobackcoord_sel <- as.data.frame(gridSample(dobackcoord, r, n=1))
coordinates(dobackcoord_sel) <- ~long+ lat
proj4string(dobackcoord_sel) <- crs.geo

# Extraccion de variables para el background y modificacion de la tabla
dobackgroundclim <- extract(variables,dobackcoord_sel)
dobackgroundsoil <- extract.list(dobackcoord_sel, list.files("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas"),path = "D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas", ID = "ploidy")
dobackgrounddat <- cbind("dobackground",as.data.frame(dobackcoord_sel),dobackgroundclim, dobackgroundsoil)
dobackgrounddat <- dobackgrounddat[,-42]

coordinates(dobackgrounddat) <- ~long+ lat
proj4string(dobackgrounddat) <- crs.geo
trivalues4 <- extract(tri.ext,dobackgrounddat)
dobackgrounddat <- as.data.frame(dobackgrounddat)
dobackgrounddat <- dobackgrounddat[,-40]
dobackgrounddat <- cbind (dobackgrounddat, trivalues4)
dobackgrounddat <- dobackgrounddat[,c(1:39,50,40:49)]
colnames(dobackgrounddat)[40] <- "tri" # Sustitucion de los valores tri por los del dataframe con NAs

dobackgrounddat.c <- na.omit(dobackgrounddat)
dobackgrounddat.c <- cbind(dobackgrounddat.c,apply(dobackgrounddat.c[,c(42:44)], 1, mean))
dobackgrounddat.c <- dobackgrounddat.c[,-c(42:44)]
colnames(dobackgrounddat.c)[48] <- "AWC"
dobackgrounddat.c <- dobackgrounddat.c[,c(1:41,48,42:47)]
dobackgrounddat.c <- dobackgrounddat.c[,-48]
dobackgrounddat.c <- dobackgrounddat.c[,c(2,3,1,4:47)]
colnames(dobackgrounddat.c) <- colnames(presvals2)

# Representacion de los puntos en el mapa alrededor de los de presencia
coordinates(dobackgrounddat.c) <- ~long+ lat
proj4string(dobackgrounddat.c) <- crs.geo
plot(gmap(e, type = "satellite"))
points(Mercator(dobackgrounddat.c), col = 'orange', pch=20, cex=1)
points(Mercator(presvalsdata), col=presvalsdata$ploidy, pch=20, cex=1)

#===============DODECAPLOID=============#

#===============GENERAL BACKGROUND=============#

backgrounddat.c <- rbind (presvals2, as.data.frame(dibackgrounddat.c), as.data.frame(tebackgrounddat.c), as.data.frame(hebackgrounddat.c), as.data.frame(dobackgrounddat.c))
backgrounddat.c$ploidy <- "background"
coordinates(backgrounddat.c) <- ~long+ lat
proj4string(backgrounddat.c) <- crs.geo

plot(gmap(e, type = "satellite"))
points(Mercator(backgrounddat.c), col = 'orange', pch=20, cex=1)
points(Mercator(presvalsdata), col=presvalsdata$ploidy, pch=20, cex=1)

#===============GENERAL BACKGROUND=============#

#===============PCA BACKGROUND=============#

todo <- rbind (presvals2, as.data.frame(backgrounddat.c), as.data.frame(dibackgrounddat.c), as.data.frame(tebackgrounddat.c), as.data.frame(hebackgrounddat.c), as.data.frame(dobackgrounddat.c))

todo.pca <- todo[,-c(1:3)]
todo.pca <- cbind (todo.pca,1)
correlations2 <- corSelect (todo.pca, var.cols = 1:44, sp.cols = 45, cor.thresh = 0.75)
selected2 <- correlations2$selected.var.cols 
todo.pca.2 <- todo.pca[,c(selected2)]

todoploidy <- todo$ploidy
w<-c(rep(0,nrow(presvals2)),rep(1,nrow(as.data.frame(backgrounddat.c))),rep(0,nrow(as.data.frame(dibackgrounddat.c))), rep(0,nrow(as.data.frame(tebackgrounddat.c))), rep(0,nrow(as.data.frame(hebackgrounddat.c))), rep(0,nrow(as.data.frame(dobackgrounddat.c))))

pcaback <-dudi.pca(todo.pca.2, row.w = w, center = TRUE, scale = TRUE, scannf = FALSE, nf = 2)
gcol = c("blue", "red", "green", "yellow", "orange", "purple", "cyan", "black", "grey")
s.label(pcaback$li, clabel = 0.1)
scatter(pcaback, clab.row = 0, posieig = "none", cex=0.1)
s.class(pcaback$li, todo[,3], col = gcol, add.plot = TRUE, cstar = 0, clabel = 0, cellipse = 1.5, pch = 16)


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

scores.clim<- pcaback$li[row.back,] 
scores.di<- pcaback$li[row.di,]		
scores.te<- pcaback$li[row.te,]	
scores.he<- pcaback$li[row.he,]	
scores.do<- pcaback$li[row.do,]	
scores.bacdi<- pcaback$li[row.bacdi,]					
scores.bacte<- pcaback$li[row.bacte,]	
scores.bache<- pcaback$li[row.bache,]	
scores.bacdo<- pcaback$li[row.bacdo,]	

zdi<- ecospat.grid.clim.dyn (scores.clim, scores.bacdi, scores.di, R=10)
zte<- ecospat.grid.clim.dyn (scores.clim, scores.bacte, scores.te, R=10)
zhe<- ecospat.grid.clim.dyn (scores.clim, scores.bache, scores.he, R=10)
zdo<- ecospat.grid.clim.dyn (scores.clim, scores.bacdo, scores.do, R=10)


#ecospat (tests)
equivalency.test.dite<-ecospat.niche.equivalency.test (zdi, zte, 1000, alternative = "lower")
equivalency.test.dihe<-ecospat.niche.equivalency.test (zdi, zhe, 1000, alternative = "lower")
equivalency.test.dido<-ecospat.niche.equivalency.test (zdi, zdo, 1000, alternative = "lower")
equivalency.test.tehe<-ecospat.niche.equivalency.test (zte, zhe, 1000, alternative = "lower")
equivalency.test.tedo<-ecospat.niche.equivalency.test (zte, zdo, 1000, alternative = "lower")
equivalency.test.hedo<-ecospat.niche.equivalency.test (zhe, zdo, 1000, alternative = "lower")

overlap.test.dite<-ecospat.niche.overlap (zdi, zte, cor=FALSE)
overlap.test.dihe<-ecospat.niche.overlap (zdi, zhe, cor=FALSE)
overlap.test.dido<-ecospat.niche.overlap (zdi, zdo, cor=FALSE)
overlap.test.tehe<-ecospat.niche.overlap (zte, zhe, cor=FALSE)
overlap.test.tedo<-ecospat.niche.overlap (zte, zdo, cor=FALSE)
overlap.test.hedo<-ecospat.niche.overlap (zhe, zdo, cor=FALSE)

ecospat.plot.niche.dyn (zdi, zdo, quant = 0.75)


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