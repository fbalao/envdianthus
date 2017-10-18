library (sp)


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

todoploidy <- factor (todo$ploidy, levels = c("2x", "4x", "6x", "12x", "background"), ordered = TRUE)
w<-c(rep(0,nrow(presvalsdata)),rep(1,nrow(as.data.frame(backgrounddat.c))))

pcaback <-dudi.pca(todo.pca.2, row.w = w, center = TRUE, scale = TRUE, scannf = FALSE, nf = 2)
gcol = c("blue", "red", "green", "purple", "black")
s.label(pcaback$li, clabel = 0.1)
scatter(pcaback, clab.row = 0, posieig = "none", cex=0.1, clab.col = 0.5)
s.class(pcaback$li, todoploidy, col = gcol, add.plot = TRUE, cstar = 0, clabel = 0, cellipse = 1.5, pch = 16)
legend (7.5,-1.8,c("2x", "4x", "6x", "12x","Background"), col = gcol, pch =19, text.width = 1.8, y.intersp = 0.5, cex = 0.8)

muestras<-todo$ploidy_arbol!="background"
PCAphylo<-pcaback$li[muestras,]
saveRDS(PCAphylo,"PCAphylo.RDS")