library (zoon)
library (future)
Sys.setenv(NOAWT=TRUE)
options(java.parameters="-Xmx120g")
plan(multicore)
LoadModule('PrintMap1.R')
vars.stack.gbif <- stack("stack_zoon_gbif.grd")
workgbif1 <- function(){ workflow (occurrence = LocalOccurrenceData (filename = "OCURRENCEFILE.csv", occurrenceType = 'presence/absence'),
                                   covariate  = LocalRaster(vars.stack.gbif),
                                   process = Chain (StandardiseCov, Crossvalidate (k = 10)),
                                   model = MaxEnt,
                                   output = Chain (PrintMap1 (dir = "DIR"), PerformanceMeasures))}

workresgbif1 <- workgbif1()
save (workresgbif1, file = 'workflow_FILE.RData')


###Testing significant deviation from random expectation
load ("workflow_FILE.RData")
library (dismo)

back<-workresgbif1[["process.output"]][[1]][["df"]]
back<-back[,-c(1:5)]
back<-back[,c(1:12,19,13:18)]

nullModel <- function (x, n = 10, rep = 99)
{
  e <- list()
  #stopifnot(n < nrow(x))
  for (r in 1:rep) {
    z <- sample(nrow(x), n)
    pres <- x[z, ] # randomly drawn presences
    absc <- x[-z, ] # remaining points set as absences
    rs10k <- sample(1:nrow(absc), 9500) # random select 10k rows
    absc <- absc[rs10k,]
    d <- rbind(pres, absc)
    v <- c(rep(1, nrow(pres)), rep(0, nrow(absc)))
    m.null <- maxent(d,v, args = c("noproduct", "nothreshold", "nohinge"))
    
    e[[r]] <- evaluate(pres, absc, m.null)
    cat("-")
    if (r%%50 == 0) 
      cat(" ", r, "\n")
    flush.console()
  }
  if (r%%50 != 0) {
    cat(" ", r, "\n")
  }
  else {
    cat("\n")
  }
  e
}

nulltest <- nullModel (back, n = X, rep = 99)
nulltest

mean(sapply(nulltest, function(x)x@auc))
max(sapply(nulltest, function(x)x@auc))