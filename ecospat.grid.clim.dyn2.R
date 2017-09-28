ecospat.grid.clim.dyn2<-function (glob, glob1, sp, R, th.sp = 0, th.env = 0, geomask = NULL) 
{
library(adehabitatMA)
library("adehabitatHR")
library(raster) 

  glob <- as.matrix(glob)
  glob1 <- as.matrix(glob1)
  sp <- as.matrix(sp)
  l <- list()
  if (ncol(glob) > 2) 
    stop("cannot calculate overlap with more than two axes")
  if (ncol(glob) == 1) {
    xmax <- max(glob[, 1])
    xmin <- min(glob[, 1])
    x <- seq(from = min(glob[, 1]), to = max(glob[, 1]), 
             length.out = R)
    sp.dens <- density(sp[, 1], kernel = "gaussian", from = xmin, 
                       to = xmax, n = R, cut = 0)
    glob1.dens <- density(glob1[, 1], kernel = "gaussian", 
                          from = xmin, to = xmax, n = R, cut = 0)
    z <- sp.dens$y * nrow(sp)/sum(sp.dens$y)
    Z <- glob1.dens$y * nrow(glob)/sum(glob1.dens$y)
    glob1r <- sapply(glob1, findInterval, glob1.dens$x)
    th.env <- quantile(glob1.dens$y[glob1r], th.env, na.rm=T)
    glob1rm <- which(Z < th.env)
    spr <- sapply(sp, findInterval, sp.dens$x)
    th.sp <- quantile(sp.dens$y[spr], th.sp,na.rm=T)
    sprm <- which(z < th.sp)
    z[sprm] <- 0
    Z[glob1rm] <- 0
    z.uncor <- z/max(z)
    z.cor <- z/Z
    z.cor[is.na(z.cor)] <- 0
    z.cor[z.cor == "Inf"] <- 0
    z.cor <- z.cor/max(z.cor)
    w <- z.uncor
    w[w > 0] <- 1
    l$x <- x
    l$z <- z
    l$z.uncor <- z.uncor
    l$z.cor <- z.cor
    l$Z <- Z
    l$glob <- glob
    l$glob1 <- glob1
    l$sp <- sp
    l$w <- w
  }
  if (ncol(glob) == 2) {
    xmin <- min(glob[, 1])
    xmax <- max(glob[, 1])
    ymin <- min(glob[, 2])
    ymax <- max(glob[, 2])
    glob1r <- data.frame(cbind((glob1[, 1] - xmin)/abs(xmax - 
                                                         xmin), (glob1[, 2] - ymin)/abs(ymax - ymin)))
    spr <- data.frame(cbind((sp[, 1] - xmin)/abs(xmax - xmin), 
                            (sp[, 2] - ymin)/abs(ymax - ymin)))
    mask <- ascgen(SpatialPoints(cbind((1:R)/R, (1:R)/R)), 
                   nrcol = R - 2, count = FALSE)
    sp.dens <- kernelUD(SpatialPoints(spr[, 1:2]), h = "href", 
                        grid = mask, kern = "bivnorm")
    sp.dens <- raster(xmn = xmin, xmx = xmax, ymn = ymin, 
                      ymx = ymax, matrix(sp.dens$ud, nrow = R))
    glob1.dens <- kernelUD(SpatialPoints(glob1r[, 1:2]), 
                           grid = mask, kern = "bivnorm")
    glob1.dens <- raster(xmn = xmin, xmx = xmax, ymn = ymin, 
                         ymx = ymax, matrix(glob1.dens$ud, nrow = R))
    x <- seq(from = min(glob[, 1]), to = max(glob[, 1]), 
             length.out = R)
    y <- seq(from = min(glob[, 2]), to = max(glob[, 2]), 
             length.out = R)
    glob1r <- extract(glob1.dens, glob1)
    Z.th <- quantile(glob1r, th.env, na.rm=T)
    glob1.dens[glob1.dens < Z.th] <- 0
    if (!is.null(geomask)) {
      proj4string(geomask) <- NA
      glob1.dens <- mask(glob1.dens, geomask, updatevalue = 0)
    }
    Z <- glob1.dens * nrow(glob1)/cellStats(glob1.dens, "sum")
    spr <- extract(sp.dens, sp)
    z.th <- quantile(spr, th.sp, na.rm=T)
    sp.dens[Z == 0] <- 0
    sp.dens[sp.dens < z.th] <- 0
    if (!is.null(geomask)) {
      sp.dens <- mask(sp.dens, geomask, updatevalue = 0)
    }
    z <- sp.dens * nrow(sp)/cellStats(sp.dens, "sum")
    z.uncor <- z/cellStats(z, "max")
    w <- z.uncor
    w[w > 0] <- 1
    z.cor <- z/Z
    z.cor[is.na(z.cor)] <- 0
    z.cor <- z.cor/cellStats(z.cor, "max")
    l$x <- x
    l$y <- y
    l$z <- z
    l$z.uncor <- z.uncor
    l$z.cor <- z.cor
    l$Z <- Z
    l$glob <- glob
    l$glob1 <- glob1
    l$sp <- sp
    l$w <- w
  }
  return(l)
}
