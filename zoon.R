library (zoon)
library (future.batchtools)
library (maxnet)
library (glmnet)
# library (spatialEco)

#==================POPULATIONS=====================#

# dbrot <- LocalOccurrenceData (filename="dbroteri.csv")
# 
# e <- extent (-10,3,35,44)
# chefiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/vars_selected_pops/chelsa", pattern = ".tif", full.names = TRUE))
# chelsa <- stack (chefiles)
# che.c <- crop (chelsa,e)
# 
# envfiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/vars_selected_pops/envirem", pattern = ".bil", full.names = TRUE))
# envirem <- stack (envfiles)
# env.c <- crop (envirem,e)
# 
# soilfiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/vars_selected_pops/soil", pattern = ".tif", full.names = TRUE))
# soilgrids <- stack (soilfiles)
# soil.c <- crop (soilgrids,e)
# 
# alt15files <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/altitud_15", pattern = ".bil", full.names = TRUE))
# alt16files <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/altitud_16", pattern = ".bil", full.names = TRUE))
# alt15 <- stack (alt15files)
# alt16 <- stack (alt16files)
# alt.m <- merge (alt15, alt16, ext=e)
# tri.ext <- tri(alt.m)
# 
# projection (che.c) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
# projection (env.c) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
# projection (soil.c) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
# projection (tri.ext) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
# combras <- CombineRasters(c(che.c, env.c, soil.c, tri.ext))
# vars.stack <- stack (combras [[1]], combras [[2]], combras [[3]], combras[[4]])
# writeRaster(vars.stack,"stack_zoon.grd", format="raster")

LoadModule('PrintMap')
LoadModule('PredictNewRasterMap')
LoadModule('PerformanceMeasures')
LoadModule('LocalOccurrenceData')


vars.stack <- stack("stack_zoon.grd")


work <- function () {
  workflow (occurrence = LocalOccurrenceData (filename="dbroteri.csv"),
            covariate  = LocalRaster (vars.stack),
            process    = Chain (Background (n=1000, bias=50), Crossvalidate),
            model      = MaxNet,
            output     = Chain (PrintMap, PredictNewRasterMap, PerformanceMeasures))
}

plan(batchtools_multicore)
workres <- work()

#==================POPULATIONS=====================#

maxnet <- function (p, data, f = maxnet.formula(p, data), regmult = 1, 
          regfun = maxnet.default.regularization, ...) 
{
  mm <- model.matrix(f, data)
  reg <- regfun(p, mm) * regmult
  weights <- p + (1 - p) * 100
  glmnet::glmnet.control(pmin = 1e-08, fdev = 0)
  model <- glmnet::glmnet(x = mm, y = as.factor(p), family = "binomial", 
                          standardize = F, penalty.factor = reg, lambda = 10^(seq(4, 
                                                                                  0, length.out = 92)) * sum(reg)/length(reg) * sum(p)/sum(weights), 
                          weights = weights, ...)
  class(model) <- c("maxnet", class(model))
  if (length(model$beta) < 200) 
    stop("Error: glmnet failed to complete regularization path")
  bb <- model$beta[, 200]
  model$betas <- bb[bb != 0]
  model$alpha <- 0
  rr <- predict.maxnet(model, data[p == 0, , drop = FALSE], 
                       type = "exponent", clamp = F)
  raw <- rr/sum(rr)
  model$entropy <- -sum(raw * log(raw))
  model$alpha <- -log(sum(rr))
  model$penalty.factor <- reg
  model$featuremins <- apply(mm, 2, min)
  model$featuremaxs <- apply(mm, 2, max)
  vv <- (sapply(data, class) != "factor")
  model$varmin <- apply(data[, vv, drop = FALSE], 2, min)
  model$varmax <- apply(data[, vv, drop = FALSE], 2, max)
  means <- apply(data[p == 1, vv, drop = FALSE], 2, mean)
  majorities <- sapply(names(data)[!vv], function(n) which.max(table(data[p == 
                                                                            1, n, drop = FALSE])))
  names(majorities) <- names(data)[!vv]
  model$samplemeans <- c(means, majorities)
  model$levels <- lapply(data, levels)
  model
}