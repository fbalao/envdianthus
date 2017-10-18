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
library (fmsb)
library (Hmisc)
library (devtools)
library (ENMTools)
library (animation)
library (phytools)


#dataset de poblaciones con coordenadas

dbroteri <- read.delim2 (file="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/Poblaciones_Nicho.csv", sep = ";", fileEncoding = "latin1", colClasses = c("factor", "factor", "numeric", "numeric"))
ploidy <- dbroteri[,2]
ploidy <- as.data.frame (ploidy)
rownames (ploidy) <- c("Monte Clerigo","Sao Bras de Alportel","Zafarraya 1","Zafarraya 2","Orgiva","Laroles","Lliria","Troia","Comporta","Donana (Peladillo)","Albufera de Valencia","Albufera de Valencia 2","Alcublas","Azuebar","Sierra de Espadan","Chiclana","Ronda","Calblanque (Cabezo de la Fuente)","Socovos","Cartagena","San Miguel de Salinas","Penon de Ifach","Valverde del Camino","Moguer","Hinojos","Donana (Acebron)","Donana (Puntal)","Huertos del Batan","Isla Cristina (Las Palmeritas)")
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

presvals.pca <- presvals[,-c(1,46)]
selected <- vif_func(presvals.pca) 
presvals.pca.2 <- presvals.pca[,c(selected)]

# presvals.pca.corselect <- cbind (presvals.pca,1)
# correlations <- corSelect (presvals.pca.corselect, var.cols = 1:44, sp.cols = 45, cor.thresh = 0.75)
# presvals.pca.2.corselect <- presvals.pca.corselect[,correlations$selected.var.cols]


ploidy <- ploidy$ploidy
ploidy <- factor (ploidy, levels = c("2x", "4x", "6x", "12x"), ordered = TRUE)

pca <- prcomp(presvals.pca.2, scale. = TRUE, retx = T)
ggbiplot(pca, obs.scale = 1,var.scale = 1,
         groups = ploidy, ellipse = TRUE, circle = FALSE, alpha =  1) +
  scale_color_discrete(name = '') +
  geom_point(aes(colour=ploidy), size = 3) +
  theme(legend.direction = 'vertical', legend.position = 'right')


#PCA con todas las variables
pca_all <- prcomp(presvals.pca, scale. = TRUE, retx = T)
ggbiplot(pca, obs.scale = 1,var.scale = 1,
         groups = ploidy, ellipse = TRUE, circle = FALSE, alpha =  1) +
  scale_color_discrete(name = '') +
  geom_point(aes(colour=ploidy), size = 3) +
  theme(legend.direction = 'vertical', legend.position = 'right')


#===============GENERAL BACKGROUND=============#
dbroteridata <- as.data.frame(dbroteri)
presvalsdata <- cbind (dbroteridata[,c(3,4)], presvals)
presvalsdata <- presvalsdata [,-48]
presvalsdata <- presvalsdata [,c(2,1,3:47)]
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

# presvals.pca.corselect2 <- cbind (todo.pca,1)
# correlations2 <- corSelect (presvals.pca.corselect2, var.cols = 1:44, sp.cols = 45, cor.thresh = 0.75)
# presvals.pca.2.corselect2 <- presvals.pca.corselect2[,correlations2$selected.var.cols]

todoploidy <- todo$ploidy
w<-c(rep(0,nrow(presvalsdata)),rep(1,nrow(as.data.frame(backgrounddat.c))))

pcaback <-dudi.pca(todo.pca.2, row.w = w, center = TRUE, scale = TRUE, scannf = FALSE, nf = 2)
gcol = c("blue", "red", "green", "yellow", "orange")
s.label(pcaback$li, clabel = 0.1)
scatter(pcaback, clab.row = 0, posieig = "none", cex=0.1)
s.class(pcaback$li, todo[,3], col = gcol, add.plot = TRUE, cstar = 0, clabel = 0, cellipse = 1.5, pch = 16)


#===============PHYTOOLS=============#

dbroteri_arbol <- read.delim2 (file="D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/Poblaciones_Nicho_arbol.csv", sep = ";", fileEncoding = "latin1", colClasses = c("factor", "factor", "numeric", "numeric"))
ploidy_arbol <- dbroteri_arbol[,2]
ploidy_arbol <- as.data.frame (ploidy_arbol)
coordinates(dbroteri_arbol) <- ~long+ lat
proj4string(dbroteri_arbol) <- crs.geo

soilgrids_arbol<-extract.list(dbroteri_arbol, list.files("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas"),path = "D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2017_nicho/soilgrids/capas", ID = "ploidy")
colnames (soilgrids_arbol) <- c("ploidy","AWCh1","AWCh2","AWCh3","BLDFIE","CECSOL","ORCDRC","PHIHOX","SNDPPT","TEXMHT")
soilgrids_arbol$ploidy<-factor(soilgrids_arbol$ploidy, levels = c("2x", "4x", "6x", "12x"), ordered = TRUE)
soilgrids_arbol$TEXMHT<-replace(soilgrids_arbol$TEXMHT,soilgrids_arbol$TEXMHT=="1","clay")
soilgrids_arbol$TEXMHT<-replace(soilgrids_arbol$TEXMHT,soilgrids_arbol$TEXMHT=="2","silty clay")
soilgrids_arbol$TEXMHT<-replace(soilgrids_arbol$TEXMHT,soilgrids_arbol$TEXMHT=="3","sandy clay")
soilgrids_arbol$TEXMHT<-replace(soilgrids_arbol$TEXMHT,soilgrids_arbol$TEXMHT=="4","clay loam")
soilgrids_arbol$TEXMHT<-replace(soilgrids_arbol$TEXMHT,soilgrids_arbol$TEXMHT=="5","silty clay loam")
soilgrids_arbol$TEXMHT<-replace(soilgrids_arbol$TEXMHT,soilgrids_arbol$TEXMHT=="6","sandy clay loam")
soilgrids_arbol$TEXMHT<-replace(soilgrids_arbol$TEXMHT,soilgrids_arbol$TEXMHT=="7","loam")
soilgrids_arbol$TEXMHT<-replace(soilgrids_arbol$TEXMHT,soilgrids_arbol$TEXMHT=="8","silty loam")
soilgrids_arbol$TEXMHT<-replace(soilgrids_arbol$TEXMHT,soilgrids_arbol$TEXMHT=="9","sandy loam")
soilgrids_arbol$TEXMHT<-replace(soilgrids_arbol$TEXMHT,soilgrids_arbol$TEXMHT=="10","silt")
soilgrids_arbol$TEXMHT<-replace(soilgrids_arbol$TEXMHT,soilgrids_arbol$TEXMHT=="11","loamy sand")
soilgrids_arbol$TEXMHT<-replace(soilgrids_arbol$TEXMHT,soilgrids_arbol$TEXMHT=="12","sand")
soilgrids_arbol$TEXMHT<-factor(soilgrids_arbol$TEXMHT,levels = c("clay", "silty clay", "sandy clay", "clay loam","silty clay loam","sandy clay loam","loam","silty loam","sandy loam","silt","loamy sand","sand"))
soilgrids_arbol<-cbind(soilgrids_arbol,apply(soilgrids_arbol[,c(2:4)], 1, mean))
soilgrids_arbol<-soilgrids_arbol[,-c(1:4)]
colnames(soilgrids_arbol)[7]<-"AWC"
soilgrids_arbol <- soilgrids_arbol[,c(7,1,2,3,4,5,6)]

presvals_arbol <- extract (variables, dbroteri_arbol)
presvals_arbol <- cbind (ploidy_arbol, presvals_arbol, soilgrids_arbol) 
presvals_arbol$PHIHOX <- presvals_arbol$PHIHOX/10

#calculo del tri a partir de altitud con libreria spatialEco

tri.ext <- tri(alt.m)
projection(tri.ext) <- crs.geo 
trivalues_arbol<-extract(tri.ext,dbroteri_arbol)

presvals_arbol <- presvals_arbol[,-38]
presvals_arbol <- cbind (presvals_arbol, trivalues_arbol)
presvals_arbol <- presvals_arbol[,c(1:37,46,38:45)]
colnames(presvals_arbol)[38] <- "tri"

#BACKGROUND
dbroteridata_arbol <- as.data.frame(dbroteri_arbol)
presvalsdata_arbol <- cbind (dbroteridata_arbol[,c(3,4)], presvals_arbol)
presvalsdata_arbol <- presvalsdata_arbol [,-48]
presvalsdata_arbol <- presvalsdata_arbol [,c(2,1,3:47)]
presvals2_arbol <- presvalsdata_arbol
coordinates(presvals2_arbol) <- ~long+ lat
proj4string(presvals2_arbol) <- crs.geo

backgroundcoord_arbol <- presvalsdata_arbol [,c(1,2)]
coordinates(backgroundcoord_arbol) <- ~long+ lat
proj4string(backgroundcoord_arbol) <- crs.geo


mask <- raster(backgroundcoord_arbol)
res(mask) <- 0.008333333
x <- circles(backgroundcoord_arbol, d=50000, lonlat=TRUE)
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
colnames(backgrounddat.c) <- colnames(presvalsdata_arbol)

todo <- rbind (presvalsdata_arbol, as.data.frame(backgrounddat.c))

selected2<-vif_func(todo[,-c(1:3)])
todo.pca.2 <- todo[,-c(1:3)][,c(selected2)]

# presvals.pca.corselect2 <- cbind (todo.pca,1)
# correlations2 <- corSelect (presvals.pca.corselect2, var.cols = 1:44, sp.cols = 45, cor.thresh = 0.75)
# presvals.pca.2.corselect2 <- presvals.pca.corselect2[,correlations2$selected.var.cols]

todoploidy <- todo$ploidy
w<-c(rep(0,nrow(presvalsdata_arbol)),rep(1,nrow(as.data.frame(backgrounddat.c))))

pcaback <-dudi.pca(todo.pca.2, row.w = w, center = TRUE, scale = TRUE, scannf = FALSE, nf = 2)
gcol = c("blue", "red", "green", "yellow", "orange")
s.label(pcaback$li, clabel = 0.1)
scatter(pcaback, clab.row = 0, posieig = "none", cex=0.1)
s.class(pcaback$li, todo[,3], col = gcol, add.plot = TRUE, cstar = 0, clabel = 0, cellipse = 1.5, pch = 16)

muestras<-todo$ploidy_arbol!="background"
PCAphylo<-pcaback$li[muestras,]
saveRDS(PCAphylo,"PCAphylo.RDS")


#### Phylogenetic PCA (e.g., Revell 2009; Evolution)

phyl.pca(tree, Y, method="BM", mode="cov")

### Phylogenetical signal Enviromental variables Abouheif’s C statistic tested the null hypothesis
# that traits did not experience phylogenetic autocorrelation (based on the topology)

tree<-read.nexus("tree.nex")
plot(tree)
orden<-tree$tip.label # este es el orden de las poblaciones para los valores del PCA
#Extraer los valores del PCA -> PCA1values, PCA2values, PCA3values?


phylosig(tree,PCA1values,method="lambda",test=TRUE, nsim=100000)
phylosig(tree,PCA1values,method="K",test=TRUE, nsim=100000)
phylosig(tree,PCA2values,method="lambda",test=TRUE, nsim=100000)
phylosig(tree,PCA2values,method="K",test=TRUE, nsim=100000)

#===============PHYTOOLS=============#


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
equivalency.test.dite<-ecospat.niche.equivalency.test (zdi, zte, 100, alternative = "lower", ncores = 23 )
equivalency.test.dihe<-ecospat.niche.equivalency.test (zdi, zhe, 100, alternative = "lower", ncores = 23 )
equivalency.test.dido<-ecospat.niche.equivalency.test (zdi, zdo, 100, alternative = "lower", ncores = 23 )
equivalency.test.tehe<-ecospat.niche.equivalency.test (zte, zhe, 100, alternative = "lower", ncores = 23 )
equivalency.test.tedo<-ecospat.niche.equivalency.test (zte, zdo, 100, alternative = "lower", ncores = 23 )
equivalency.test.hedo<-ecospat.niche.equivalency.test (zhe, zdo, 100, alternative = "lower", ncores = 23 )


#OVERLAP TEST
overlap.test.dite<-ecospat.niche.overlap (zdi, zte, cor=FALSE)
overlap.test.dihe<-ecospat.niche.overlap (zdi, zhe, cor=FALSE)
overlap.test.dido<-ecospat.niche.overlap (zdi, zdo, cor=FALSE)
overlap.test.tehe<-ecospat.niche.overlap (zte, zhe, cor=FALSE)
overlap.test.tedo<-ecospat.niche.overlap (zte, zdo, cor=FALSE)
overlap.test.hedo<-ecospat.niche.overlap (zhe, zdo, cor=FALSE)


<<<<<<< HEAD
#SIMILARITY TEST greater alternative hypothesis more similar than random
similarity.testdite<-ecospat.niche.similarity.test (zdi, zte, 100, alternative = "greater", ncores = 23 )
similarity.testtedi<-ecospat.niche.similarity.test (zte, zdi, 100, alternative = "greater", ncores = 23 )
similarity.testdihe<-ecospat.niche.similarity.test (zdi, zhe, 100, alternative = "greater", ncores = 23 )
similarity.testhedi<-ecospat.niche.similarity.test (zhe, zdi, 100, alternative = "greater", ncores = 23 )
similarity.testdido<-ecospat.niche.similarity.test (zdi, zdo, 100, alternative = "greater", ncores = 23 )
similarity.testdodi<-ecospat.niche.similarity.test (zdo, zdi, 100, alternative = "greater", ncores = 23 )
similarity.testtehe<-ecospat.niche.similarity.test (zte, zhe, 100, alternative = "greater", ncores = 23 )
similarity.testhete<-ecospat.niche.similarity.test (zhe, zte, 100, alternative = "greater", ncores = 23 )
similarity.testtedo<-ecospat.niche.similarity.test (zte, zdo, 100, alternative = "greater", ncores = 23 )
similarity.testdote<-ecospat.niche.similarity.test (zdo, zte, 100, alternative = "greater", ncores = 23 )
similarity.testhedo<-ecospat.niche.similarity.test (zhe, zdo, 100, alternative = "greater", ncores = 23 )
similarity.testdohe<-ecospat.niche.similarity.test (zdo, zhe, 100, alternative = "greater", ncores = 23 )
=======
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

write.table (tablaresul, "results_pops_vif.txt", sep = "\t")
<<<<<<< HEAD
>>>>>>> 9eefbdc042c141b4f5ebb38d056b77026d2efcb0


# Tabla

overlap<-rbind(overlap.test.dite$D,overlap.test.dihe$D, overlap.test.dido$D,overlap.test.tehe$D, overlap.test.tedo$D, overlap.test.hedo$D)

similarityab<-rbind(similarity.testdite$p.D, similarity.testdihe$p.D, similarity.testdido$p.D, similarity.testtehe$p.D, similarity.testtedo$p.D, similarity.testhedo$p.D)
similarityba<-rbind(similarity.testtedi$p.D, similarity.testhedi$p.D, similarity.testdodi$p.D, similarity.testhete$p.D, similarity.testdote$p.D, similarity.testdohe$p.D)

equivalency<-rbind(equivalency.test.dite$p.D, equivalency.test.dihe$p.D, equivalency.test.dido$p.D, equivalency.test.tehe$p.D, equivalency.test.tedo$p.D, equivalency.test.hedo$p.D)
=======
>>>>>>> 9eefbdc042c141b4f5ebb38d056b77026d2efcb0

tablaresul<-data.frame(overlap,similarityab, similarityba,equivalency)
write.table(tablaresul, file="resultadosoverlapingcormethod.txt", sep="\t",  row.names = F)

#NICHE BREADTH
raster.breadth (zdi$w)
raster.breadth (zte$w)
raster.breadth (zhe$w)
raster.breadth (zdo$w)

