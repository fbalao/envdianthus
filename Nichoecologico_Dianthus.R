#R script Dactylorhiza Niche model divergence
# This script downloads species coordinates from gbif. 
# it clips the pH information from SoilGrids1km
# it clip the wordclim data

library(dismo)
library('rgbif')
library(rgdal)
library(raster)
library(gtools)
library(rjson)
library(sp)
library(GSIF)
library(plotrix)


#GBIF data
Dfucloc<-occ_search(scientificName = "Dianthus broteri",hasGeospatialIssue=FALSE,fields='minimal', limit=50000 ,hasCoordinate=TRUE)
gbifmap(Dfucloc[[3]])
dups <- duplicated(Dfucloc[[3]][,c(3,4)])
fuc_cleaned<- Dfucloc[[3]][dups, ]
fuc_cleaned
coofuc<-fuc_cleaned[,c(4,3)]
coordinates(coofuc)<- ~decimalLongitude+ decimalLatitude
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(coofuc) <- crs.geo 

data.shape<-readOGR(dsn="/home/fbalao/poligonodianthus.shp", layer="poligonodianthus")
#Own data
dianthus<-read.table("/home/fbalao/Datos/Proyectos/ProyectoExcelencia2016/Figuras/PCA_NichoEcologico_Dianthusbroteri/coordinatespopulationDianthus.csv",header=T)
coordinates(dianthus)<- ~Longitude+ Latitude
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(dianthus) <- crs.geo 


#PCA
data<-read.table("/home/fbalao/Datos/R/Analisis/Ecologicalniche/Datosbioclim.txt",header=T)
attach(data)
clima<-cbind(bio1,bio2,bio3,bio4,bio5,bio6,bio7,bio8,bio9,bio10,bio11,bio12,bio13,bio14,bio15,bio16,bio17,bio18,bio19)
library(ade4)
pca1 <- dudi.pca(clima, scann = FALSE, nf = 3)
scatter(pca1, clab.row = 0, posieig = "none", add.plot=T, clab.col = 0.8, col=2)
gcol<-c("blue", "red", "green", "yellow")
s.class(pca1$li, as.factor(data[,2]), col = gcol, add.plot = TRUE, cstar = 0, clabel = 1, cellipse = 1.5, cpoint = 2)
legend(-7,-1,legend = c("2x","4x","6x", "12x"),  pch=21, pt.bg = gcol, pt.cex=1.8)

# MANOVA on PC Axis y plidy levels

summary(manova(as.matrix(pca1$li)~as.factor(ploidy)))

boxplot(pca1$li$Axis1~as.factor(ploidy), col=gcol, ylab="Componente 1",xlab="Nivel de ploidía")
boxplot(pca1$li$Axis2~as.factor(ploidy), col=gcol, ylab="Componente 2",xlab="Nivel de ploidía")
boxplot(pca1$li$Axis3~as.factor(ploidy), col=gcol,  ylab="Componente 3",xlab="Nivel de ploidía")

#Figura completa

biolegend<-read.table("/home/fbalao/Datos/Proyectos/ProyectoExcelencia2016/Figuras/PCA_NichoEcologico_Dianthusbroteri/biolab.txt", sep="@")
layout(matrix(c(1,2,2,3,4,5),nrow=2, byrow =T ))
plot.new()
addtable2plot(-0.7,0,biolegend,display.rownames=F,hlines=F, cex=0.5, display.colnames = F, bty="n")
scatter(pca1, clab.row = 0, posieig = "none",clab.col = 1, col=2, xlim=c(10,10))
s.class(pca1$li, as.factor(data[,2]), col = gcol, add.plot = TRUE, cstar = 0, clabel = 1, cellipse = 1.5, cpoint = 2)
legend(-6.8,-0.1,legend = c("2x","4x","6x", "12x"),  pch=21, pt.bg = gcol, pt.cex=1, cex=0.7)
boxplot(pca1$li$Axis1~as.factor(ploidy), col=gcol, ylab="Componente 1",xlab="Nivel de ploidía")
boxplot(pca1$li$Axis2~as.factor(ploidy), col=gcol, ylab="Componente 2",xlab="Nivel de ploidía")
boxplot(pca1$li$Axis3~as.factor(ploidy), col=gcol,  ylab="Componente 3",xlab="Nivel de ploidía")

# Kernel plots
# plot(density(data$bio5[data$ploidy==2]), col="blue", xlim=c(200,500), ylim=c(0,0.25))
# lines(density(data$bio5[data$ploidy==4]), col="red")
# lines(density(data$bio5[data$ploidy==6]), col="green")
# lines(density(data$bio5[data$ploidy==12]), col="yellow")

#Soil
soilgrids.r <- REST.SoilGrids(c("ORCDRC","PHIHOX"))
ovfuc <- over(soilgrids.r, coofuc)
ovinc <- over(soilgrids.r, cooinc)
#pH
library(lattice)
library(aqp)
data(soil.legends)
plot(density(na.omit(ovfuc$PHIHOX.M.sd1)/10), col=3)
lines(density(na.omit(ovinc$PHIHOX.M.sd1)/10), col=2)

PHIHOX.range = range(soil.legends[["PHIHOX"]]$MIN, soil.legends[["PHIHOX"]]$MAX)



#BIOCLIM DATA
list.ras <- mixedsort(list.files("/home/fbalao/Datos/Dactylorhiza/BDGeoDact/wc2-5/", full.names = T, pattern = ".bil"))
bio <- stack(list.ras)
bio.brick <- brick(bio)
newext <- drawExtent()
bio.c<-crop(bio, newext)
projection(bio.c)<-crs.geo

prevfuc<-extract(bio.brick,coofuc)
previnc<-extract(bio.brick,cooinc)

biofuc<-cbind(as.data.frame(coofuc),prevfuc)
bioinc<-cbind(as.data.frame(cooinc),previnc)
bioinc<-na.omit(bioinc)
library('caret')
rem<-findCorrelation(cor(biofuc[,-c(1,2)]), cutoff = .90, verbose = F, names=F)
rem<-rem+2
biofuc.c<-biofuc[, -c(1,2,rem)]

pca <- prcomp(biofuc.c)
plot(pca$x[,1:2], pch=16, main=biofuc.c[1,"Species"]) # 2 first principal components
z

merged<-rbind(biofuc,bioinc)
sp<-c(rep('fuc',348), rep('inc',924))
merged<-cbind(sp,merged)
mergedtable<-merged[,-c(2,3)]
mergedtable

#PCA on the climatic variables
library(ade4)
pca1 <- dudi.pca(mergedtable[,-c(1,rem+1)], scann = FALSE, nf = 3)
barplot(pca1$eig)
gcol = c("blue", "red", "green", "red")

# s.class(dfxy = pca1$li, fac = as.factor(biofuc[,2]), col = gcol, xax = 1, yax = 2)
# s.corcircle(pca1$co, xax = 1, yax = 2)
# scatter(pca1, clab.row = 0, posieig = "none")
# s.class(pca1$li, mergedtable[,1], col = gcol, add.plot = TRUE, cstar = 1.5, clabel = 0, cellipse = 1.5)


biolegend<-read.table("/home/fbalao/Datos/Proyectos/ProyectoExcelencia2016/Figuras/PCA_NichoEcologico_Dianthusbroteri/biolab.txt", sep="@")
layout(matrix(c(1,2,2,3,4,5),nrow=2, byrow =T ))
plot.new()
addtable2plot(-0,0,biolegend,display.rownames=F,hlines=F, cex=0.5, display.colnames = F, bty="n")
scatter(pca1, clab.row = 0, posieig = "none",clab.col = 1, col=2, xlim=c(10,10))
s.class(pca1$li, as.factor(biofuc[,2]), col = gcol, add.plot = TRUE, cstar = 0, clabel = 1, cellipse = 1.5, cpoint = 2)
legend(-8,2,legend = c("2x","4x","6x", "12x"),  pch=21, pt.bg = gcol, pt.cex=1, cex=1)
boxplot(pca1$li$Axis1~as.factor(biofuc[,2]), col=gcol, ylab="Componente 1",xlab="Nivel de ploidía")
boxplot(pca1$li$Axis2~as.factor(biofuc[,2]), col=gcol, ylab="Componente 2",xlab="Nivel de ploidía")
boxplot(pca1$li$Axis3~as.factor(biofuc[,2]), col=gcol,  ylab="Componente 3",xlab="Nivel de ploidía")


#MANOVA
rem<-findCorrelation(cor(mergedtable[,-1]), cutoff = .70, verbose = F, names=F)
manovabio<-manova(as.matrix(mergedtable[,-c(1,rem+1)]) ~ as.factor(mergedtable[,1]) )
summary(manovabio)
