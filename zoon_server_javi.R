library (zoon)
library (future)
library (spatialEco)

#==================POPULATIONS=====================#

# dbrot <- LocalOccurrenceData (filename="dbroteri.csv")
# 
# e <- extent (-10,4.5,35.5,44)
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
# writeRaster(vars.stack,"stack_zoon_pops.grd", format="raster")

# Chain(PrintMap, PerformanceMeasures),

LoadModule('PerformanceMeasures')

vars.stack <- stack("stack_zoon_pops.grd")
plan (multicore)

work <- function(){ workflow (occurrence = LocalOccurrenceData (filename="dbroteri.csv",
                                                                occurrenceType='presence/absence',
                                                                columns=c(long = 'longitude', lat = 'latitude', value = 'value', type = 'type')),
                              covariate  = LocalRaster(vars.stack),
                              process = Chain(StandardiseCov,
                                              Crossvalidate),
                              model = MaxNet,
                              output = PrintMap 
                              (dir="/home/javlopez"),
                              forceReproducible = TRUE)}

workres<-work()
save(workres, file = 'workflow_javi.RData')

#==================POPULATIONS=====================#


#==================GBIF=====================#

LoadModule('PerformanceMeasures')

vars.stack.gbif <- stack("stack_zoon_gbif.grd")
plan (multicore)

work1 <- function(){ workflow (occurrence = LocalOccurrenceData (filename = "dbroterigbif2.csv",
                                                                 occurrenceType='presence/absence',
                                                                 columns=c(long = 'longitude', lat = 'latitude', value = 'value', type = 'type')),
                              covariate  = LocalRaster(vars.stack.gbif),
                              process = Chain(StandardiseCov,
                                              Crossvalidate),
                              model = MaxNet,
                              output = PrintMap 
                              (dir = "/home/javlopez"),
                              forceReproducible = TRUE)}

workres1 <- work1()
save (workres1, file = 'workflow_javi_gbif.RData')

#==================GBIF=====================#