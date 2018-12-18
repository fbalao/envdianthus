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
library (vioplot)
library (adegraphics)
library (lattice)


###Downloading GBIF data
Dbroterigbif <- occ_search (scientificName = "Dianthus broteri",hasGeospatialIssue =FALSE, limit=50000, fields = c("name", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "year", "eventDate", "locality", "occurrenceRemarks", "recordedBy"), hasCoordinate=TRUE, basisOfRecord = "PRESERVED_SPECIMEN", return = "data")
Dinoxgbif <- occ_search (scientificName = "Dianthus inoxianus",hasGeospatialIssue =FALSE, limit=50000, fields = c("name", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "year", "eventDate", "locality", "occurrenceRemarks", "recordedBy"), hasCoordinate=TRUE, basisOfRecord = "PRESERVED_SPECIMEN", return = "data")
Dhinoxgbif <- occ_search (scientificName = "Dianthus hinoxianus",hasGeospatialIssue =FALSE, limit=50000, fields = c("name", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "year", "eventDate", "locality", "occurrenceRemarks", "recordedBy"), hasCoordinate=TRUE, basisOfRecord = "PRESERVED_SPECIMEN", return = "data")
Dvalengbif <- occ_search (scientificName = "Dianthus valentinus",hasGeospatialIssue =FALSE, limit=50000, fields = c("name", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "year", "eventDate", "locality", "occurrenceRemarks", "recordedBy"), hasCoordinate=TRUE, basisOfRecord = "PRESERVED_SPECIMEN", return = "data")


###Cleaning of location data
Dbroterigbif <- Dbroterigbif [Dbroterigbif$year>2004,]
Dbroterigbif <- Dbroterigbif [-c(129:189),]
Dhinoxgbif <- Dhinoxgbif [Dhinoxgbif$year>2004,]
Dhinoxgbif <- Dhinoxgbif [1,]
Dvalengbif <- Dvalengbif [1,]

Dbroterigbif <- Dbroterigbif [, c(1:3)]
Dinoxgbif <- Dinoxgbif [, c(1:3)]
Dhinoxgbif <- Dhinoxgbif [, c(1:3)]
Dvalengbif <- Dvalengbif [, c(1:3)]

todo <- rbind (Dbroterigbif,Dinoxgbif,Dhinoxgbif,Dvalengbif)
todo$name <- "Dianthus broteri"
gbifmap (todo, mapdatabase = "world", region = "Spain")


###Cleaning of duplicate records
todo_cleaned <- unique(todo)
todo_cleaned <- todo_cleaned [,-1]


###Map
e <- extent (-10,3.5,35.5,44)
coordinates(todo_cleaned) <- ~decimalLongitude+ decimalLatitude
crs.geo <- CRS ("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string (todo_cleaned) <- crs.geo
map <- plot (gmap (e, type = "satellite"))
points <- points (Mercator(todo_cleaned), col = "red", pch=20, cex = 1.5)


###Thinning the records to one per 1 km2 
r <- raster(todo_cleaned)
res(r) <- 0.008333333
r <- extend(r, extent(r)+1)
cooDbroterigbif <- as.data.frame(gridSample(todo_cleaned, r, n=1))


###Additional populations from our records
e1 <- extent (-7,-6.4,36.8,37.5)

dianthuspops <- read.table ("FILE.txt", header = T, sep = ",")
dianthuspops <- dianthuspops [,-c(1,4)]
dianthuspops <- dianthuspops [,c(2,1)]
colnames (dianthuspops) <- c("long", "lat")

coordinates(dianthuspops) <- ~long+ lat
proj4string(dianthuspops) <- crs.geo
r1 <- raster(dianthuspops)
res(r1) <- 0.008333333
r1 <- extend(r1, extent(r1)+1)
dianthuspops_fran <- as.data.frame(gridSample(dianthuspops, r1, n=1))

coordinates(dianthuspops_fran) <- ~long+ lat
proj4string(dianthuspops_fran) <- crs.geo
plot(gmap(e1, type = "satellite"))
points(Mercator(dianthuspops_fran), col="red" , pch=20, cex=1)

dianthuspops_fran <- as.data.frame (dianthuspops_fran)
colnames(cooDbroterigbif) <- colnames(dianthuspops_fran)
cooDbroterigbif.def <- rbind (cooDbroterigbif, dianthuspops_fran)
cooDbroterigbif.def2 <- cooDbroterigbif.def

coordinates(cooDbroterigbif.def2) <- ~long+ lat
proj4string (cooDbroterigbif.def2) <- crs.geo

r <- raster(cooDbroterigbif.def2)
res(r) <- 0.008333333
r <- extend(r, extent(r)+1)
cooDbroterigbif.def.data <- as.data.frame(gridSample(cooDbroterigbif.def2, r, n=1))

cooDbroterigbif.def.data <- cbind (cooDbroterigbif.def.data, 1)
colnames(cooDbroterigbif.def.data)[3]<-"ploidy"

###Ploidy estimation based on geographic coordinates
cooDbroterigbif.def.data$ploidy[cooDbroterigbif.def.data$lat > 38 & cooDbroterigbif.def.data$long > -9.5 & cooDbroterigbif.def.data$lat < 39 & cooDbroterigbif.def.data$long < -8] <- "4x"
cooDbroterigbif.def.data$ploidy[cooDbroterigbif.def.data$lat > 37 & cooDbroterigbif.def.data$long > -9 & cooDbroterigbif.def.data$lat < 37.5 & cooDbroterigbif.def.data$long < -7.5] <- "2x"
cooDbroterigbif.def.data$ploidy[cooDbroterigbif.def.data$lat > 36.9 & cooDbroterigbif.def.data$long > -7.4 & cooDbroterigbif.def.data$lat < 37.6 & cooDbroterigbif.def.data$long < -6] <- "12x"
cooDbroterigbif.def.data$ploidy[cooDbroterigbif.def.data$lat > 36 & cooDbroterigbif.def.data$long > -6.3 & cooDbroterigbif.def.data$lat < 37.14 & cooDbroterigbif.def.data$long < -4.72] <- "4x"
cooDbroterigbif.def.data$ploidy[cooDbroterigbif.def.data$lat > 36.5 & cooDbroterigbif.def.data$long > -4.7 & cooDbroterigbif.def.data$lat < 38.1 & cooDbroterigbif.def.data$long < -1.67] <- "2x"
cooDbroterigbif.def.data$ploidy[cooDbroterigbif.def.data$lat > 37.3 & cooDbroterigbif.def.data$long > -1.7 & cooDbroterigbif.def.data$lat < 38.3 & cooDbroterigbif.def.data$long < -0.5] <- "6x"
cooDbroterigbif.def.data$ploidy[cooDbroterigbif.def.data$lat > 38.2 & cooDbroterigbif.def.data$long > -2.33 & cooDbroterigbif.def.data$lat < 38.83 & cooDbroterigbif.def.data$long < 0.33] <- "6x"
cooDbroterigbif.def.data$ploidy[cooDbroterigbif.def.data$lat > 38.83 & cooDbroterigbif.def.data$long > -1.7 & cooDbroterigbif.def.data$lat < 40.9 & cooDbroterigbif.def.data$long < 0.38] <- "4x"
cooDbroterigbif.def.data$ploidy[c(58,62:63,120:123,125:130)]<- "4x"
cooDbroterigbif.def.data <- cooDbroterigbif.def.data[cooDbroterigbif.def.data$ploidy!="1",]


ploidy<-factor(cooDbroterigbif.def.data$ploidy,levels = c("2x","4x","6x","12x"),ordered = TRUE)

coordinates(cooDbroterigbif.def.data) <- ~long+ lat
proj4string(cooDbroterigbif.def.data) <- crs.geo
plot(gmap(e, type = "satellite"))
points(Mercator(cooDbroterigbif.def.data), col=ploidy, pch=20, cex=1)

plot(gmap(e1, type = "satellite"))
points(Mercator(cooDbroterigbif.def.data), col=ploidy, pch=20, cex=1)


cooDbroterigbif.def.vars <- as.data.frame (cooDbroterigbif.def.data)
cooDbroterigbif.def.vars <- cooDbroterigbif.def.vars [,c(3,1,2)]
coordinates(cooDbroterigbif.def.vars) <- ~long+ lat
proj4string(cooDbroterigbif.def.vars) <- crs.geo


###Downloading of predictor variables (chelsa, envirem, elevation, SoilGrids)
###Data extraction and merging into the same extent

#cooDbroterigbif.def.vars <- read.csv2 ("FILE.csv")
ploidy<-factor(cooDbroterigbif.def.vars$ploidy,levels = c("2x","4x","6x","12x"),ordered = TRUE)

crs.geo <- CRS ("+proj=longlat +ellps=WGS84 +datum=WGS84")
coordinates(cooDbroterigbif.def.vars) <- ~long+ lat
proj4string(cooDbroterigbif.def.vars) <- crs.geo

e <- extent (-10,4.5,35.5,44)

chelsafiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/chelsa", pattern = ".tif", full.names = TRUE))
chelsa <- stack (chelsafiles)
che.c <- crop (chelsa,e)

enviremfiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/envirem", pattern = ".bil", full.names = TRUE))
envirem <- stack (enviremfiles)
env.c <- crop (envirem, e)

alt15files <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/altitud_15", pattern = ".bil", full.names = TRUE))
alt16files <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/altitud_16", pattern = ".bil", full.names = TRUE))
alt15 <- stack (alt15files)
alt16 <- stack (alt16files)
alt.m <- merge (alt15, alt16, ext=e)

variables <- stack (che.c, env.c, alt.m)
names (variables) <- c("bio1","bio2","bio3","bio4","bio5","bio6","bio7","bio8","bio9","bio10","bio11","bio12","bio13","bio14","bio15","bio16","bio17","bio18","bio19","annualPET","climaticMoistureIndex","continentality","growingDegDays5","maxTempColdest","minTempWarmest","PETColdestQuarter","PETDriestQuarter","PETseasonality","PETWarmestQuarter","PETWettestQuarter","topoWet", "elevation")

soilgrids<-extract.list(cooDbroterigbif.def.vars, list.files("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/soilgrids/capas"),path = "D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/soilgrids/capas", ID = "ploidy")
colnames (soilgrids) <- c("ploidy","AWCh2","BLDFIE","CECSOL","ORCDRC","PHIHOX","SNDPPT")
soilgrids$ploidy<-factor(soilgrids$ploidy, levels = c("2x", "4x", "6x", "12x"), ordered = TRUE)
# soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="1","clay")
# soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="2","silty clay")
# soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="3","sandy clay")
# soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="4","clay loam")
# soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="5","silty clay loam")
# soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="6","sandy clay loam")
# soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="7","loam")
# soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="8","silty loam")
# soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="9","sandy loam")
# soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="10","silt")
# soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="11","loamy sand")
# soilgrids$TEXMHT<-replace(soilgrids$TEXMHT,soilgrids$TEXMHT=="12","sand")
# soilgrids$TEXMHT<-factor(soilgrids$TEXMHT,levels = c("clay", "silty clay", "sandy clay", "clay loam","silty clay loam","sandy clay loam","loam","silty loam","sandy loam","silt","loamy sand","sand"))
# soilgrids<-cbind(soilgrids,apply(soilgrids[,c(2:4)], 1, mean))
# soilgrids<-soilgrids[,-c(1:4)]
colnames(soilgrids)[2]<-"AWC"


presvals <- extract (variables, cooDbroterigbif.def.vars)
presvals <- cbind (ploidy, presvals, soilgrids) 
presvals$PHIHOX <- presvals$PHIHOX/10
presvals <- presvals [,-34]

###tri calculation from SpatilEco package
tri.ext <- tri(alt.m)
projection(tri.ext) <- crs.geo 
trivalues<-extract(tri.ext,cooDbroterigbif.def.vars)
presvals <- cbind (presvals, trivalues)
presvals <- presvals[,c(1:31,40,32:39)]
colnames(presvals)[32] <- "tri"
presvals2 <- na.omit (presvals)
ploidy <- as.data.frame(presvals2[,1])
colnames (ploidy) <- "ploidy"


###Removing correlated variables
###PCA only with presence points using non-correlated variables
presvals.pca <- presvals2[,-1]
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


###Creating the general background
dbroteridata <- as.data.frame(cooDbroterigbif.def.vars)
dbroteridata <- dbroteridata [,-1]
presvalsdata <- cbind (dbroteridata, presvals)
presvalsdata <- na.omit (presvalsdata)
presvals2 <- presvalsdata
coordinates(presvals2) <- ~long+ lat
proj4string(presvals2) <- crs.geo

backgroundcoord <- presvalsdata [,c(1,2)]
coordinates(backgroundcoord) <- ~long+ lat
proj4string(backgroundcoord) <- crs.geo

mask <- raster(backgroundcoord)
res(mask) <- 0.008333333
x <- circles(backgroundcoord, d=150000, lonlat=TRUE)
pol <- gUnaryUnion(x@polygons)
samp <- spsample(pol, 10000, type='random', iter=25)
extent(mask) <- extent(pol)
cells <- cellFromXY(mask, samp)
length(cells)
cells <- unique(cells)
length(cells)
xy <- xyFromCell(mask, cells)
xy <- as.data.frame(xy)
colnames(xy) <- c("long", "lat")

###Thinning the records to one per 1 km2 
coordinates(xy) <- ~long+ lat
proj4string(xy) <- crs.geo
r <- raster(xy)
res(r) <- 0.008333333
r <- extend(r, extent(r)+1)
backcoord_sel <- as.data.frame(gridSample(xy, r, n=1))
coordinates(backcoord_sel) <- ~long+ lat
proj4string(backcoord_sel) <- crs.geo

###Data extraction of background points and modification of the dataset
backgroundclim <- extract(variables,backcoord_sel)
backgroundsoil <- extract.list(backcoord_sel, list.files("FILE", ID = "ploidy"))
backgrounddat <- cbind("background",as.data.frame(backcoord_sel),backgroundclim, backgroundsoil)
backgrounddat <- backgrounddat[,-36]

coordinates(backgrounddat) <- ~long+ lat
proj4string(backgrounddat) <- crs.geo
trivalues1 <- extract(tri.ext,backgrounddat)
backgrounddat <- as.data.frame(backgrounddat)
backgrounddat <- cbind (backgrounddat, trivalues1)
backgrounddat <- backgrounddat[,c(1:33,42,34:41)]
colnames(backgrounddat)[34] <- "tri"

backgrounddat.c <- na.omit(backgrounddat)
backgrounddat.c <- backgrounddat.c[,c(2,3,1,4:42)]
colnames(backgrounddat.c) <- colnames(presvalsdata)

#Plotting a map with presence and background points
coordinates(backgrounddat.c) <- ~long+ lat
proj4string(backgrounddat.c) <- crs.geo
plot(gmap(e, type = "satellite"))
points(Mercator(backgrounddat.c), col = 'orange', pch=20, cex=1)
points(Mercator(presvals2), col=presvals2$ploidy, pch=20, cex=1)


###PCA with presence and background points
tododef <- rbind (presvalsdata, as.data.frame(backgrounddat.c))

#tododef <- read.csv2 ("tododef.csv")

todo.pca <- tododef[,-c(1:4)]
selected2<-vif_func(todo.pca)
todo.pca.2 <- todo.pca[,c(selected2)]

todoploidy <- factor (tododef$ploidy, levels = c("2x", "4xs", "4xe", "6x", "12x", "background"), ordered = TRUE)
w<-c(rep(0,nrow(presvalsdata)),rep(1,5155))
todoploidy2<-todoploidy[todoploidy!="background"]
todoploidy2 <- factor (todoploidy2, levels = c("2x", "4xs", "4xe", "6x", "12x"), ordered = T)

library (adegraphics)
frame.plot=FALSE

pcaback <-dudi.pca(todo.pca.2, row.w = w, center = TRUE, scale = TRUE, scannf = FALSE, nf = 2)
gcol = c("green", "darkred", "red", "blue", "purple")
adegpar(pellipses.alpha=0.3,ppoints.cex=1.5,plabels =list(cex=1.5, optim=T, alpha=0.8,boxes=list(draw=T, lwd=1.5, alpha=0.8)), plines.lwd = 2)
g1<-s.corcircle(pcaback$co/80, label = row.names(pcaback$co), fullcircle = TRUE)
g1_1<-update (g1, xlim = c(-2.4,2.2), ylim = c(-1,1.01))
adegpar(pellipses.alpha=0.3,ppoints.cex=1.5,plabels =list(cex=1.4, optim=T, alpha=1,boxes=list(draw=T, lwd=2, alpha=1)), plines.lwd = 2)
g2<-s.class(pcaback$li[1:143,], todoploidy2, col = gcol, porigin.origin=c(-0.5,-5), porigin.draw=T)
g2_2<-update (g2, xlim = c(-4.2,4.4), ylim = c(-9.2,-0.8))


###Niche comparison tests: cytotypes and 4x lineages (package ECOSPAT)
row.di<-which(tododef[,3] == "2x")
row.tes<-which(tododef[,3] == "4xs")
row.tee<-which(tododef[,3] == "4xe")
row.te<-which(tododef[,4] == "4x")
row.he<-which(tododef[,3] == "6x")
row.do<-which(tododef[,3] == "12x")


scores.clim<- pcaback$li 
scores.di<- pcaback$li[row.di,]
scores.te<- pcaback$li[row.te,]
scores.tes<- pcaback$li[row.tes,]	
scores.tee<- pcaback$li[row.tee,]	
scores.he<- pcaback$li[row.he,]	
scores.do<- pcaback$li[row.do,]	

zdi<- ecospat.grid.clim.dyn (scores.clim, scores.clim, scores.di, R=500)
zte<- ecospat.grid.clim.dyn (scores.clim, scores.clim, scores.te, R=500)
ztes<- ecospat.grid.clim.dyn (scores.clim, scores.clim, scores.tes, R=500)
ztee<- ecospat.grid.clim.dyn (scores.clim, scores.clim, scores.tee, R=500)
zhe<- ecospat.grid.clim.dyn (scores.clim, scores.clim, scores.he, R=500)
zdo<- ecospat.grid.clim.dyn (scores.clim, scores.clim, scores.do, R=500)

ecospat.plot.niche (zdi)
ecospat.plot.niche (zte)
ecospat.plot.niche (ztes)
ecospat.plot.niche (ztee)
ecospat.plot.niche (zhe)
ecospat.plot.niche (zdo)

###Equivalency tests
equivalency.test.dite<-ecospat.niche.equivalency.test (zdi, zte, 100, alternative = "lower")
equivalency.test.dihe<-ecospat.niche.equivalency.test (zdi, zhe, 100, alternative = "lower")
equivalency.test.dido<-ecospat.niche.equivalency.test (zdi, zdo, 100, alternative = "lower")
equivalency.test.tehe<-ecospat.niche.equivalency.test (zte, zhe, 100, alternative = "lower")
equivalency.test.tedo<-ecospat.niche.equivalency.test (zte, zdo, 100, alternative = "lower")
equivalency.test.hedo<-ecospat.niche.equivalency.test (zhe, zdo, 100, alternative = "lower")

equivalency.test.dites<-ecospat.niche.equivalency.test (zdi, ztes, 100, alternative = "lower")
equivalency.test.ditee<-ecospat.niche.equivalency.test (zdi, ztee, 100, alternative = "lower")
equivalency.test.hetes<-ecospat.niche.equivalency.test (zhe, ztes, 100, alternative = "lower")
equivalency.test.hetee<-ecospat.niche.equivalency.test (zhe, ztee, 100, alternative = "lower")
equivalency.test.dotes<-ecospat.niche.equivalency.test (zdo, ztes, 100, alternative = "lower")
equivalency.test.dotee<-ecospat.niche.equivalency.test (zdo, ztee, 100, alternative = "lower")

###Overlap tests
overlap.test.dite<-ecospat.niche.overlap (zdi, zte, cor=FALSE)
overlap.test.dihe<-ecospat.niche.overlap (zdi, zhe, cor=FALSE)
overlap.test.dido<-ecospat.niche.overlap (zdi, zdo, cor=FALSE)
overlap.test.tehe<-ecospat.niche.overlap (zte, zhe, cor=FALSE)
overlap.test.tedo<-ecospat.niche.overlap (zte, zdo, cor=FALSE)
overlap.test.hedo<-ecospat.niche.overlap (zhe, zdo, cor=FALSE)

###Similarity tests
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


###Vioplots
par(cex.lab=1.5, cex.axis=1.2, cex.main = 2)
vio1<-vioplot_fun (scores.di$Axis1, scores.te$Axis1, scores.tes$Axis1, scores.tee$Axis1, scores.he$Axis1, scores.do$Axis1, names = c("2x","4x","4xs","4xe","6x","12x"), col = c("green", "orange", "darkred", "red", "blue", "purple"))
title(xlab="Ploidy level", main = "Axis 1")
vioplot_fun (scores.di$Axis2, scores.te$Axis2, scores.tes$Axis2, scores.tee$Axis2, scores.he$Axis2, scores.do$Axis2, names = c("2x","4x","4xs","4xe","6x","12x"), col = c("green", "orange", "darkred", "red", "blue", "purple"))
title(xlab="Ploidy level", main = "Axis 2")


###Niche breadth
bredi<-raster.breadth (zdi$w)
brete<-raster.breadth (zte$w)
brehe<-raster.breadth (zhe$w)
bredo<-raster.breadth (zdo$w)
bretes<-raster.breadth (ztes$w)
bretee<-raster.breadth (ztee$w)


###Niche divergence test
#PC1
PC1dn<-t.test(scores.di[,1], scores.tes[,1]) 
PC1dn1<-t.test(scores.di[,1], scores.tee[,1]) 
PC1dn2<-t.test(scores.tes[,1], scores.tee[,1]) 
PC1dn3<-t.test(scores.he[,1], scores.tes[,1]) 
PC1dn4<-t.test(scores.he[,1], scores.tee[,1]) 
PC1dn5<-t.test(scores.do[,1], scores.tes[,1]) 
PC1dn6<-t.test(scores.do[,1], scores.tee[,1])
PC1dn7<-t.test(scores.di[,1], scores.te[,1]) 
PC1dn8<-t.test(scores.di[,1], scores.he[,1]) 
PC1dn9<-t.test(scores.di[,1], scores.do[,1]) 
PC1dn10<-t.test(scores.te[,1], scores.he[,1]) 
PC1dn11<-t.test(scores.te[,1], scores.do[,1]) 
PC1dn12<-t.test(scores.he[,1], scores.do[,1]) 

#PC2
PC2dn<-t.test(scores.di[,2], scores.tes[,2]) 
PC2dn1<-t.test(scores.di[,2], scores.tee[,2]) 
PC2dn2<-t.test(scores.tes[,2], scores.tee[,2]) 
PC2dn3<-t.test(scores.he[,2], scores.tes[,2]) 
PC2dn4<-t.test(scores.he[,2], scores.tee[,2]) 
PC2dn5<-t.test(scores.do[,2], scores.tes[,2]) 
PC2dn6<-t.test(scores.do[,2], scores.tee[,2])
PC2dn7<-t.test(scores.di[,2], scores.te[,2]) 
PC2dn8<-t.test(scores.di[,2], scores.he[,2]) 
PC2dn9<-t.test(scores.di[,2], scores.do[,2]) 
PC2dn10<-t.test(scores.te[,2], scores.he[,2]) 
PC2dn11<-t.test(scores.te[,2], scores.do[,2]) 
PC2dn12<-t.test(scores.he[,2], scores.do[,2])
