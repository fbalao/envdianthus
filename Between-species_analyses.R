### Testing for climatic niche conservatism ###

library(ade4)

# pca1 <-dudi.pca(X)
# Between-Class Analysis# En nuestro caso corresponde al BETWEEN-occ de Broennimann 2012
# Nos da el % de varianza (inertia) explicado por los grupos
Bocc <- bca(pca1, ploidyocc, scannf = FALSE)

#Monte-Carlo Test on the between-groups inertia percentage
rand1 <- rtest(bca, 99)
rand1
plot(rand1, main = "Monte-Carlo test")

###  Within-Class Analysis# En nuestro caso corresponde al WITHIN-occ de Broennimann 2012
witocc <- wca(pca1, ploidyocc, scan = FALSE, nf = 2)


### WITHIN-env. En este caso como grupo habría que poner los backgrounds de las distintas especies

witenc <- wca(pca1, backgroundploidy, scan = FALSE, nf = 2)



#### Phylogenetic PCA (e.g., Revell 2009; Evolution)

library(phytools)
phyl.pca(tree, Y, method="BM", mode="cov")

### Phylogenetical signal Enviromental variables Abouheif’s C statistic tested the null hypothesis
# that traits did not experience phylogenetic autocorrelation (based on the topology)

library(phytools)
tree<-read.nexus("tree.nex")
plot(tree)
orden<-tree$tip.label # este es el orden de las poblaciones para los valores del PCA
#Extraer los valorehansen(data = datos["PC2"], tree = ouchtree, regimes = datos["OU.4"],     sqrt.alpha = 1, sigma = 1)s del PCA -> PCA1values, PCA2values, PCA3values?
PCAphylo<-readRDS("PCAphylo.RDS")
rownames(PCAphylo)<-orden


phylosig(tree,PCAphylo[,1],method="lambda",test=TRUE, nsim=100000)
phylosig(tree,PCAphylo[,1],method="K",test=TRUE, nsim=100000)
phylosig(tree,PCAphylo[,2],method="lambda",test=TRUE, nsim=100000)
phylosig(tree,PCAphylo[,2],method="K",test=TRUE, nsim=100000)


#
library(adephylo)
library(phylobase)
PCA<-phylo4d(tree,PCAphylo)


table.phylo4d(PCA, ratio.tree = 1/3, cex.label=0.6, legend = F, cex.symbol=0.7,
              treetype="cladogram", box =F, scale=T, show.var.label=T, symbol="circles", grid=F)
x <- tdata(PCA, type="tip")

W <- proxTips(tree, met="Abouheif")
abouTests <- abouheif.moran(PCA)
plot(abouTests)
myProx <- vcv.phylo(tree)
abouTestsBrown <- abouheif.moran(PCA, W=myProx)

a1.ortgTest <- orthogram(x$Axis1, tree) # No señal en eje 1
a2.ortgTest <- orthogram(x$Axis2, tree) # Sí en eje 2

# OUwie
library("OUwie")

# OUCH
ouchtree<-readRDS("ouchtree.RDS")
datosouch<-readRDS("ouchtable.RDS")
PCAphylo2 <- PCAphylo[as.character(datosouch$labels[-(1:16)]) , ] # Reorder to fix ouchtree
rownames(PCAphylo2)==datosouch$labels[-(1:16)] # Check order

datosouch$PC1[-(1:16)]<-PCAphylo2$Axis1
datosouch$PC2[-(1:16)]<-PCAphylo2$Axis2

library(ouch)
#Brownian model
brownPC1 <- brown(datosouch['PC1'],ouchtree)
bootbrwonpc1<-bootstrap(brownPC1)

brownPC2 <- brown(datosouch['PC2'],ouchtree)
bootbrwonpc2<-bootstrap(brownPC2)

#OU model 1 optima
OU1PC1<-hansen(data = datosouch["PC1"], tree = ouchtree, regimes = datosouch["OU.1"],     sqrt.alpha = 1, sigma = 1)
OU1PC1boot<-bootstrap(OU1PC1)

OU1PC2<-hansen(data = datosouch["PC2"], tree = ouchtree, regimes = datosouch["OU.1"],     sqrt.alpha = 1, sigma = 1)
OU1PC2boot<-bootstrap(OU1PC2)

#OU model 4 optima == ploidy levels
OU4PC1<-hansen(data = datosouch["PC1"], tree = ouchtree, regimes = datosouch["OU.4"],     sqrt.alpha = 1, sigma = 1)
OU4PC1boot<-bootstrap(OU4PC1)

OU4PC2<-hansen(data = datosouch["PC2"], tree = ouchtree, regimes = datosouch["OU.4"],     sqrt.alpha = 1, sigma = 1)
OU4PC2boot<-bootstrap(OU4PC2)


t.test(OU1PC1boot$aic.c,bootbrwonpc1$aic.c)
t.test(OU4PC1boot$aic.c,bootbrwonpc1$aic.c)
t.test(OU1PC1boot$aic.c,OU4PC1boot$aic.c)
# los test muestrasn que PC1 sigue model browniano

t.test(OU1PC2boot$aic.c,bootbrwonpc2$aic.c)
t.test(OU4PC2boot$aic.c,bootbrwonpc2$aic.c)
t.test(OU1PC2boot$aic.c,OU4PC2boot$aic.c)
# los test muestran que PC2 sigue OU con 4 optima

# PC1
quantile(bootbrwonpc1$aic.c,c(0.05,0.5,0.95))
quantile(OU1PC1boot$aic.c,c(0.05,0.5,0.95))
quantile(OU4PC1boot$aic.c,c(0.05,0.5,0.95))

#PC2
quantile(bootbrwonpc2$aic.c,c(0.05,0.5,0.95))
quantile(OU1PC2boot$aic.c,c(0.05,0.5,0.95))
quantile(OU4PC2boot$aic.c,c(0.05,0.5,0.95))


boxplot(datosouch$PC1~factor(datosouch$OU.4,levels=c("2x","4x","6x","12x")), col=rainbow(4))
boxplot(datosouch$PC2~factor(datosouch$OU.4,levels=c("2x","4x","6x","12x")),col=rainbow(4))
