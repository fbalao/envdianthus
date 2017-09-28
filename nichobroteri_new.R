  library (dismo)
 library (raster)
  library (rJava)
  library (rgdal)
library (rgeos)
library (rgbif)
library (rjson)
library (gtools)
  library (maps)
  library (ggmap)
  library (rgeos)
 library (fuzzySim)
  library (ade4)
 library (pcaMethods)
 library (ecospat)
 library (sp)
 library (GSIF)
 library (caret)
  library (RCurl)
  library (gdalUtils)
  library (plotKML)
  library (XML)
  library (lattice)
  library (aqp)
  library (soiltexture)
 library (ggbiplot)


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
#install.packages("spatialEco")
library(spatialEco)
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

presvals.pca <- presvals[,-c(1,46)]
presvals.pca <- cbind (presvals.pca,1)
correlations <- corSelect (presvals.pca, var.cols = 1:44, sp.cols = 45, cor.thresh = 0.75)
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


#background data (merged)
presvals2 <- presvals[,-46]
coordinates(presvals2)<-coordinates(dbroteri)
proj4string(presvals2) <- crs.geo 

set.seed(110)
mask <- raster(presvals2)
res(mask) <- 0.008333333
x <- circles(presvals2, d=75000, lonlat=TRUE)
#Se podría hacer un clip de los poligonos y el continente para que no salgan puntos en el mar , solucion provisional aumentar el N
pol <- gUnaryUnion(x@polygons)
samp <- spsample(pol, 100, type='random', iter=2500)
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
presvalsdata <- presvalsdata[,c(46,47,1:45)]

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


#===============DIPLOID=============#
maskdi <- raster(diploid)
res(maskdi) <- 0.008333333
xdi <- circles(diploid, d=75000, lonlat=TRUE)
poldi <- gUnaryUnion(xdi@polygons)
sampdi <- spsample(poldi, 30, type='random', iter=2500)
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

coordinates(dibackgrounddat.c)<- ~long+ lat
proj4string(dibackgrounddat.c) <- crs.geo
plot(gmap(e, type = "satellite"))
points(Mercator(dibackgrounddat.c), col = 'blue', pch=20)
points(Mercator(presvals2), col=presvals2$ploidy, pch=20, cex=1)

#===============DIPLOID=============#


#===============TETRAPLOID=============#

maskte <- raster(tetraploid)
res(maskte) <- 0.008333333
xte <- circles(tetraploid, d=75000, lonlat=TRUE)
polte <- gUnaryUnion(xte@polygons)
sampte <- spsample(polte, 30, type='random', iter=2500)
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

coordinates(tebackgrounddat.c)<- ~long+ lat
proj4string(tebackgrounddat.c) <- crs.geo
plot(gmap(e, type = "satellite"))
points(Mercator(tebackgrounddat.c), col = 'blue', pch=20)

#===============TETRAPLOID=============#

#===============HEXAPLOID=============#

maskhe <- raster(hexaploid)
res(maskhe) <- 0.008333333
xhe <- circles(hexaploid, d=75000, lonlat=TRUE)
polhe <- gUnaryUnion(xhe@polygons)
samphe <- spsample(polhe, 30, type='random', iher=2500)
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

coordinates(hebackgrounddat.c)<- ~long+ lat
proj4string(hebackgrounddat.c) <- crs.geo
plot(gmap(e, type = "satellite"))
points(Mercator(hebackgrounddat.c), col = 'blue', pch=20)

#===============HEXAPLOID=============#


#===============DODECAPLOID=============#

maskdo <- raster(dodecaploid)
res(maskdo) <- 0.008333333
xdo <- circles(dodecaploid, d=75000, lonlat=TRUE)
poldo <- gUnaryUnion(xdo@polygons)
sampdo <- spsample(poldo, 30, type='random', idor=2500)
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

coordinates(dobackgrounddat.c)<- ~long+ lat
proj4string(dobackgrounddat.c) <- crs.geo
plot(gmap(e, type = "satellite"))
points(Mercator(dobackgrounddat.c), col = 'blue', pch=20)

#===============DODECAPLOID=============#

todo <- rbind (presvalsdata, as.data.frame(backgrounddat.c), as.data.frame(dibackgrounddat.c), as.data.frame(tebackgrounddat.c), as.data.frame(hebackgrounddat.c), as.data.frame(dobackgrounddat.c))

todo.pca <- todo[,-c(1:3)]
todo.pca <- cbind (todo.pca,1)
correlations2 <- corSelect (todo.pca, var.cols = 1:44, sp.cols = 45, cor.thresh = 0.75)
selected2 <- correlations$selected.var.cols 
presvals.pca.2 <- presvals.pca[,c(selected)]



column <- c(c(rep(1,28),rep(0,649)))
todo1 <- cbind (todo, column)
correlationsbackground <- corSelect (todo1, var.cols = 4:47, sp.cols = 48)
selectedbackground <- correlationsbackground$selected.var.cols
todo.c <- todo[,c(selectedbackground)]
rem<-findCorrelation(cor(todo[,-c(1,2,3)]), cutoff = .80, verbose = F, names=T)

m <- as.data.frame (rep("background",235))
n <- as.data.frame (rep("2xbackground",99))
p <- as.data.frame (rep("4xbackground",187))
q <- as.data.frame (rep("6xbackground",95))
r <- as.data.frame (rep("12xbackground",33))
names(m)<-"ploidy"
names(n)<-"ploidy"
names(p)<-"ploidy"
names(q)<-"ploidy"
names(r)<-"ploidy"
x <- rbind(m,n,p,q,r)
column2 <- rbind(ploidy,x)
column2 <- column2$ploidy
colum2 <- factor (column2, levels = c("2x", "4x", "6x", "12x", "background", "2xbackground", "4xbackground", "6xbackground", "12xbackground"), ordered = TRUE)


pcaback <- prcomp(todo.c, scale. = TRUE, retx = T)
ggbiplot(pcaback, obs.scale = 1,var.scale = 1,
         groups = column2, ellipse = TRUE, circle = TRUE, alpha =  1) +
  scale_color_discrete(name = '') +
  geom_point(aes(colour=colum2), size = 2) +
  theme(legend.direction = 'vertical', legend.position = 'right')


#ECOSPAT
row.di<-which(todo[,1] == "2x")
row.te<-which(todo[,1] == "4x")
row.he<-which(todo[,1] == "6x")
row.do<-which(todo[,1] == "12x")
row.bacdi<-which(todo[,1] == "dibackground") 
row.bacte<-which(todo[,1] == "tebackground")
row.bache<-which(todo[,1] == "hebackground")
row.bacdo<-which(todo[,1] == "dobackground")

scores.clim<- pca.cal$li[(nrow(as.data.frame(presvals2))+1):nrow(todo.c),] 
scores.di<- pca.cal$li[row.di,]		
scores.te<- pca.cal$li[row.te,]	
scores.he<- pca.cal$li[row.he,]	
scores.do<- pca.cal$li[row.do,]	
scores.bacdi<- pca.cal$li[row.bacdi,]					
scores.bacte<- pca.cal$li[row.bacte,]	
scores.bache<- pca.cal$li[row.bache,]	
scores.bacdo<- pca.cal$li[row.bacdo,]	

zdi<- ecospat.grid.clim.dyn(scores.clim, scores.bacdi, scores.di,R=100)
zte<- ecospat.grid.clim.dyn(scores.clim, scores.bacte, scores.te,R=100)
zhe<- ecospat.grid.clim.dyn(scores.clim, scores.bache, scores.he,R=100)
zdo<- ecospat.grid.clim.dyn(scores.clim, scores.bacdo, scores.do,R=100)


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

ecospat.plot.niche.dyn (zdo, zte, quant = 0.75)


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