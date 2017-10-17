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
#Extraer los valores del PCA -> PCA1values, PCA2values, PCA3values?


phylosig(tree,PCA1values,method="lambda",test=TRUE, nsim=100000)
phylosig(tree,PCA1values,method="K",test=TRUE, nsim=100000)
phylosig(tree,PCA2values,method="labda",test=TRUE, nsim=100000)
phylosig(tree,PCA2values,method="K",test=TRUE, nsim=100000)
