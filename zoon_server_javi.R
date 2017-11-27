library (zoon)
library (future)
library (ulimit)
library (gtools)
library (spatialEco)

#==================POPULATIONS=====================#

# dbrot <- LocalOccurrenceData (filename="dbroteri.csv")
# 
e <- extent (-10,4.5,35.5,44)
chefiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/vars_selected_gbif/chelsa", pattern = ".tif", full.names = TRUE))
chelsa <- stack (chefiles)
che.c <- crop (chelsa,e)

alt15files <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/altitud_15", pattern = ".bil", full.names = TRUE))
alt16files <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/altitud_16", pattern = ".bil", full.names = TRUE))
alt15 <- stack (alt15files)
alt16 <- stack (alt16files)
alt.m <- merge (alt15, alt16, ext=e)

envfiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/vars_selected_gbif/envirem", pattern = ".bil", full.names = TRUE))
envirem <- stack (envfiles)
env.c <- crop (envirem,e)

soilfiles <- mixedsort (list.files ("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/vars_selected_gbif/soil", pattern = ".tif", full.names = TRUE))
soilgrids <- stack (soilfiles)
soil.c <- crop (soilgrids,e)

tri.ext <- tri(alt.m)

projection (che.c) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
projection (alt.m) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
projection (env.c) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
projection (soil.c) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
projection (tri.ext) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

combras <- CombineRasters(c(che.c, env.c, soil.c, tri.ext))
vars.stack <- stack (combras [[1]], combras [[2]], combras [[3]], combras[[4]])
writeRaster(vars.stack,"stack_zoon_alllayers.grd", format="raster")


# Chain(PrintMap, PerformanceMeasures),

LoadModule('PerformanceMeasures')

vars.stack <- stack("D:/Copia de seguridad JAVI/UNIVERSIDAD DE SEVILLA/Experimentos Dianthus/Lopez_Juradoetal2018_nicho/stack_zoon_alllayers.grd")
plan (multicore)

work <- function(){ workflow (occurrence = LocalOccurrenceData (filename="dbroteri.csv",
                                                                occurrenceType='presence/absence',
                                                                columns=c(long = 'longitude', lat = 'latitude', value = 'value', type = 'type')),
                              covariate  = LocalRaster(vars.stack),
                              process = Chain(StandardiseCov,
                                              Crossvalidate),
                              model = MaxNet,
                              output = PrintMap (dir = "/home/javlopez", size = c (600,600)),
                              forceReproducible = TRUE)}

workres<-work()
save(workres, file = 'workflow_javi.RData')

#==================POPULATIONS=====================#


#==================GBIF=====================#

LoadModule('PerformanceMeasures')
LoadModule('ROCcurve')

vars.stack.gbif <- stack("stack_zoon_alllayers.grd")
plan (multicore)
ulimit::memory_limit(100000)

workgbif <- function(){ workflow (occurrence = LocalOccurrenceData (filename = "dbroterigbif.csv"),
                              covariate  = LocalRaster(vars.stack.gbif),
                              process = Chain (Background (n = 10000, bias = 100, seed = 999999), 
                                               StandardiseCov),
                              model = MaxNet,
                              output = Chain (PrintMap (dir = "/home/javlopez", 
                                                        size = c (600,600)),
                                              PerformanceMeasures, ROCcurve (newwin = FALSE)),
                              forceReproducible = TRUE)}

workresgbif <- workgbif()
save (output, file = 'workflow_gbif_nooutput.RData')

#==================GBIF=====================#

LoadModule ('ChangeWorkflow')

ChangeWorkflow (workresgbif, occurrence = NULL,
                covariate = NULL,
                process = NULL,
                model = NULL,
                output = DataSummary)

LocalOccurrenceData (filename = "dbroterigbif.csv", occurrenceType='presence', columns=c(long = 'longitude', lat = 'latitude', 
            value = 'value', type = 'type', fold = 'fold'), externalValidation = TRUE)