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

library(phangorn)
tree2<-chronopl(tree, lambda = 1)
phenogram(tree2,PCAphylo$Axis1)
xa<-c(x,fastAnc(tree,PCAphylo$Axis1))
H<-nodeHeights(tree)
Y<-matrix(xa[tree$edge],nrow(tree$edge),2)
plot.new()
plot.window(xlim=range(H),ylim=range(Y))
axis(1)
axis(2)
for(i in 1:nrow(tree$edge)) lines(H[i,],Y[i,],lwd=2)
title(xlab="time",ylab="phenotype")



#
library(adephylo)
library(phylobase)
library(phylosignal)
PCA<-phylo4d(tree,PCAphylo)

correlo1<-phyloCorrelogram(PCA, trait = "Axis1")

correlo2<-phyloCorrelogram(PCA, trait = "Axis2")
par(mfrow=c(1,2))
plot(correlo1, main= "PCA-env Axis 1", ylab="Moran's I")
plot(correlo2, main= "PCA-env Axis 2", ylab="Moran's I")
opographic wetness index
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

#Correlogram dist env dist phylo


# OUCH
ouchtree<-readRDS("ouchtree.RDS")
datosouch<-readRDS("ouchtable.RDS")
PCAphylo2 <- PCAphylo[as.character(datosouch$labels[-(1:16)]) , ] # Reorder to fix ouchtree
rownames(PCAphylo2)==datosouch$labels[-(1:16)] # Check git reset --mergeorder

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

#OU model 5 optima == ploidy levels + northern 4x
OU5PC1<-hansen(data = datosouch["PC1"], tree = ouchtree, regimes = datosouch["OU.5"],     sqrt.alpha = 1, sigma = 1)
OU5PC1boot<-bootstrap(OU4PC1)

OU5PC2<-hansen(data = datosouch["PC2"], tree = ouchtree, regimes = datosouch["OU.5"],     sqrt.alpha = 1, sigma = 1)
OU5PC2boot<-bootstrap(OU4PC2)

t.test(OU1PC1boot$aic.c,bootbrwonpc1$aic.c)
t.test(OU4PC1boot$aic.c,bootbrwonpc1$aic.c)
t.test(OU1PC1boot$aic.c,OU4PC1boot$aic.c)
t.test(OU1PC1boot$aic.c,OU5PC1boot$aic.c)
t.test(OU4PC1boot$aic.c,OU5PC1boot$aic.c)
t.test(bootbrwonpc1$aic.c,OU5PC1boot$aic.c)

# los test muestrasn que PC1 sigue model OU con 5 optimos

t.test(OU1PC2boot$aic.c,bootbrwonpc2$aic.c)
t.test(OU4PC2boot$aic.c,bootbrwonpc2$aic.c)
t.test(OU1PC2boot$aic.c,OU4PC2boot$aic.c)
t.test(OU1PC2boot$aic.c,OU5PC2boot$aic.c)
t.test(OU4PC2boot$aic.c,OU5PC2boot$aic.c)
t.test(bootbrwonpc2$aic.c,OU5PC2boot$aic.c)
# los test muestran que PC2 sigue OU con 4 optima

# PC1
quantile(bootbrwonpc1$aic.c,c(0.05,0.5,0.95))
quantile(OU1PC1boot$aic.c,c(0.05,0.5,0.95))
quantile(OU4PC1boot$aic.c,c(0.05,0.5,0.95))
quantile(OU5PC1boot$aic.c,c(0.05,0.5,0.95))

#PC2
quantile(bootbrwonpc2$aic.c,c(0.05,0.5,0.95))
quantile(OU1PC2boot$aic.c,c(0.05,0.5,0.95))
quantile(OU4PC2boot$aic.c,c(0.05,0.5,0.95))
quantile(OU5PC2boot$aic.c,c(0.05,0.5,0.95))

boxplot(datosouch$PC1~factor(datosouch$OU.4,levels=c("2x","4x","6x","12x")), col=rainbow(4))
boxplot(datosouch$PC2~factor(datosouch$OU.4,levels=c("2x","4x","6x","12x")),col=rainbow(4))


# El mismo analysis BM, OU1, OUM
library(mvMORPH)
state<-datosouch$OU.4
names(state)<-datosouch$labels
# Make the tree with mapped states using SIMMAP
tree<-  make.simmap(tree, state, model="ER", nsim=1)
plot(tree)

trait1_BM<-  mvBM(tree, PCAphylo[,1], model="BMM")
trait2_BM<-  mvBM(tree, PCAphylo[,2], model="BMM")

trait1_OU1<-  mvOU(tree, PCAphylo[,1], model="OU1")
trait2_OU1<-  mvOU(tree, PCAphylo[,2], model="OU1")

trait1_OUM<-  mvOU(tree, PCAphylo[,1], model="OUM")
trait2_OUM<-  mvOU(tree, PCAphylo[,2], model="OUM")

AIC(trait1_BM);AIC(trait1_OUM);AIC(trait1_OU1)
AIC(trait1_BM);AIC(trait2_OUM);AIC(trait2_OU1)

OUM<-  mvOU(tree, PCAphylo, model="OUM")

# Ajusta modelo BM con un único régimen o varias tasas de evolución
library(phytools)
fitBM_PCA1<-brownie.lite(tree,PCAphylo[,1], test="simulation")
fitBM_PCA2<-brownie.lite(tree,PCAphylo[,2], test="simulation")

# Ajusta OU
library(OUwie)
plotSimmap(tree,type="fan",fsize=0.8,ftype="i")
tree$node.label<-getStates(tree,"nodes")
dataPC1<-data.frame(Genus_species=PCA@label,Reg=factor(c(2,2,2,2,6,6,6,4,4,4,4,4,4,12,12,12,12,4,4,4,2,2)),X=PCA@data$Axis1)

fitBMPC1<-OUwie(tree,dataPC1,model="BM1",simmap.tree=TRUE)
fitOUMPC1<-OUwie(tree,dataPC1,model="OUM",simmap.tree=TRUE)
fitOUMAPC1<-OUwie(tree,dataPC1,model="OUMA",simmap.tree=TRUE)
fitOUMVAPC1<-OUwie(tree,dataPC1,model="OUMVA",simmap.tree=TRUE)

fitBMPC1$AICc
fitOUMPC1$AICc
fitOUMAPC1$AICc
fitOUMVAPC1$AICc


dataPC2<-data.frame(Genus_species=PCA@label,Reg=factor(c(2,2,2,2,6,6,6,4,4,4,4,4,4,12,12,12,12,4,4,4,2,2)),X=PCA@data$Axis2)

fitBMPC2<-OUwie(tree,dataPC2,model="BM1",simmap.tree=TRUE)
fitOUMPC2<-OUwie(tree,dataPC2,model="OUM",simmap.tree=TRUE)
fitOUMAPC2<-OUwie(tree,dataPC2,model="OUMA",simmap.tree=TRUE)
fitOUMVAPC2<-OUwie(tree,dataPC2,model="OUMVA",simmap.tree=TRUE)

fitBMPC2$AICc
fitOUMPC2$AICc
fitOUMAPC2$AICc
fitOUMVAPC2$AICc
