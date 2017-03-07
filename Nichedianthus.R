#R script dianthus polyploids model divergence
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
library(maps)
library(rgeos)
library(ade4)
library(caret)

#Dbroteri data gbif
Dbroteriloc<-occ_search(scientificName = "Dianthus broteri",hasGeospatialIssue=FALSE,fields=c('name','decimalLatitude','decimalLongitude', 'country'), limit=10000 ,hasCoordinate=TRUE, basisOfRecord="PRESERVED_SPECIMEN")
dups <- duplicated(Dbroteriloc[[3]][,c(2,3)]) #Look for duplicates
brot_cleaned<- Dbroteriloc[[3]][!dups, ] #Remove duplicates

summary(as.factor(fuc_cleaned$country)) #Number ocurrences by country



# #Dbroteri owndata
# owndata<-read.table("datos.txt", header=F)
# colnames(owndata)<- colnames(brot_cleaned)




coordinates(brot_cleaned)<- ~decimalLongitude+ decimalLatitude
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(brot_cleaned) <- crs.geo 

# #Remove ocurrences within 10km2
# r <- raster(coofuc_final)
# res(r) <- 0.08333333
# r <- extend(r, extent(r)+1)
# coofuc_final_sel <- as.data.frame(gridSample(coofuc_final, r, n=1))
# coordinates(coofuc_final_sel)<- ~decimalLongitude+ decimalLatitude
# proj4string(coofuc_final_sel) <- crs.geo 
# # p <- rasterToPolygons(r)
# # plot(p, border='gray')
# # points(coofuc_final)
# # points(acsel, cex=1, col='red', pch='x')



#MAP
map('world', xlim=c(-11,6), ylim=c(35,45))
box()
title(main="D. broteri")
points(brot_cleaned, pch=16, cex=0.4, col="orange")


#BIOCLIM DATA
#Read the raster, stack the raster, crop and merge
list.ras <- mixedsort(list.files("/home/fbalao/Datos/Dactylorhiza/BDGeoDact/wc0.5/", full.names = T, pattern = ".bil"))
bio <- stack(list.ras)
projection(bio)<-crs.geo

#Extract the bioclim variables
envbrot<-extract(bio,brot_cleaned)

  
#Tree coverage
vcf<-raster("/home/fbalao/MODIS/out.tif")
vcfbrot<-extract(vcf,brot_cleaned)
vcfbrot[vcfbrot==200]<-0

envbrot<-cbind(envbrot,vcfbrot)
envbrot<-na.omit(envbrot)

#PCA
library(ade4)
pca1 <- dudi.pca(envbrot, scann = FALSE, nf = 3)
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




#Soil
soilgrids.r <- REST.SoilGrids(c("ORCDRC","PHIHOX"))
pHbrot <- over(soilgrids.r, brot_cleaned)

#pH
library(lattice)
library(aqp)
data(soil.legends)
plot(density(na.omit(ovfuc$PHIHOX.M.sd1)/10), col=3)
lines(density(na.omit(ovinc$PHIHOX.M.sd1)/10), col=2)

PHIHOX.range = range(soil.legends[["PHIHOX"]]$MIN, soil.legends[["PHIHOX"]]$MAX)

