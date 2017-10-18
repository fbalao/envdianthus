#Continuous species distribution
cooDbroterigbifdata<-readRDS("cooDbroterigbifdata.RDS")

rast <- raster(ncol = 100, nrow = 100)
cooDbroterigbifdata$ploidy
extent(rast) <- extent(cooDbroterigbifdata)
x<-rasterize(cooDbroterigbifdata, rast, as.numeric(cooDbroterigbifdata$ploidy), fun = mean)
plot(x)


library(spatstat)
crn.pp = as(cooDbroterigbifdata, 'ppp')

q = quadratcount(crn.pp, 10, 10)
q2 = quadratcount(crn.pp[crn.pp$marks=="2x"], 10, 10)
q4 = quadratcount(crn.pp[crn.pp$marks=="4x"], 10, 10)
q6 = quadratcount(crn.pp[crn.pp$marks=="6x"], 10, 10)
q12 = quadratcount(crn.pp[crn.pp$marks=="12x"], 10, 10)
plot(q2)
plot(crn.pp, add=T)
coa<-dudi.coa(as.data.frame(matrix(q, ncol=10)))
