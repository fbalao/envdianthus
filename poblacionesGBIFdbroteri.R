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


#pca
presvals<-presvals[complete.cases(presvals[ ,4]),]

presvals.pca <- presvals[,-c(1,2,3,48)]
presvals.pca <- cbind (presvals.pca,1)
correlations <- corSelect (presvals.pca, var.cols = 1:44, sp.cols = 45, cor.thresh = 0.75)
selected <- correlations$selected.var.cols 
presvals.pca.2 <- presvals.pca[,c(selected)]

pc <- pca (presvals.pca.2, nPcs=3, method="nipals")
presvals.pca.def <- completeObs (pc)
presvals.pca.def <- as.data.frame (presvals.pca.def)

ploidy2 <- presvals[,3]
ploidy2 <- factor (ploidy2, levels = c("2x", "4x", "6x", "12x"), ordered = TRUE)

pca <- prcomp(presvals.pca.def, scale. = TRUE, retx = T)
ggbiplot(pca, obs.scale = 1,var.scale = 1,
         groups = ploidy2, ellipse = TRUE, circle = TRUE, alpha =  1) +
  scale_color_discrete(name = '') +
  geom_point(aes(colour=ploidy2), size = 2) +
  theme(legend.direction = 'vertical', legend.position = 'right')


#autocorrelacion espacial
presvals.mantel <- presvals[,-48]
presvals.mantel <- cbind (presvals.mantel,0,0,0,0)
colnames(presvals.mantel)[48:51]<-c("2x","4x","6x","12x")
presvals.mantel$`2x`[presvals.mantel$ploidy=="2x"]<-1
presvals.mantel$`4x`[presvals.mantel$ploidy=="4x"]<-1
presvals.mantel$`6x`[presvals.mantel$ploidy=="6x"]<-1
presvals.mantel$`12x`[presvals.mantel$ploidy=="12x"]<-1
presvals.mantel <- presvals.mantel[,-3]
presvals.mantel <- presvals.mantel[,c(2,1,3:50)]
ecospat.mantel.correlogram(dfvar=presvals.mantel,colxy=1:2, n=500, colvar=3:46, 
                           max=1000, nclass=10, nperm=100)
