###Phylogenetic signal
library(phytools)
library(phangorn)
library(ape)

###Loading tree and PCA values
tree<-read.tree("dianthustree25.nw")
tree2<-root(tree,outgroup = "BArportel")
tree2<-chronos(tree2)
tree2<-multi2di(tree2)
plot(tree2)

###PCA order
orden<-tree2$tip.label
PCAvalues<-read.csv2("scoresdata_gbif.csv", row.names = 4)
PCAphylo<-PCAvalues[orden,]

PCA1<-PCAphylo$Axis1
names(PCA1)<-tree2$tip.label
PCA2<-PCAphylo$Axis2
names(PCA2)<-tree2$tip.label

###Blomberg's K
phylosig(tree2,PCAphylo[,1],method="K",test=TRUE, nsim=100000)
phylosig(tree2,PCAphylo[,2],method="K",test=TRUE, nsim=100000)

########################################################################

###Phenograms
PCA1<-PCAphylo$Axis1
names(PCA1)<-tree2$tip.label
PCA2<-PCAphylo$Axis2
names(PCA2)<-tree2$tip.label

###Phylogeny with ploidy levels mapped
Ploidy<-PCAphylo$Ploidy #ploidy level assign
names(Ploidy)<-tree2$tip.label
trees <- make.simmap(tree2, Ploidy, model = "SYM", nsim=1)
plot(trees)

###Phylogeny with ploidy levels and lineages mapped
Ploidy2<-factor(Ploidy, levels=c("2x","4x","4xn","6x","12x"))
Ploidy2[20:25]<-"4xn"
trees2 <- make.simmap(tree2, Ploidy2, model = "ER", nsim=1)
plot(trees2)

###Figure
pdf(file="Phenograms.pdf", height = 10, width = 6, pagecentre=T)
par(mfcol=c(2,1), mai = c(0.8, 0.8, 0.1, 0.1))
phenogram(trees,PCA1, xlab="Relative time", ylab="Enviromental Axis 1", fsize=0.8,
          lwd=3, ylim=c(-3.3,2.8),cex.lab=2)
phenogram(trees,PCA2, xlab="Relative time", ylab="Enviromental Axis 1", fsize=0.8,
          lwd=3, ylim=c(-7,-0.7))

dev.off()


#################################################################################

###Niche evolution models

library(mvMORPH)

lambdaTree<-function(tree,lambda){
  ii<-which(tree$edge[,2]>length(tree$tip.label))
  H1<-nodeHeights(tree)
  tree$edge.length[ii]<-lambda*tree$edge.length[ii]
  H2<-nodeHeights(tree)
  tree$edge.length[-ii]<-tree$edge.length[-ii]+     H1[-ii,2]-H2[-ii,2]
  tree
}

treestar<-lambdaTree(tree, 0)


###Null Model
trait1_white<-mvBM(treestar, PCAphylo[,1], model="BM1")
trait2_white<-mvBM(treestar, PCAphylo[,2], model="BM1")

###Brownian model
trait1_BM<-mvBM(trees, PCAphylo[,1], model="BM1")
trait2_BM<-mvBM(trees, PCAphylo[,2], model="BM1")

###OU model 1 optimum
trait1_OU1<-mvOU(trees, PCAphylo[,1], model="OU1")
trait2_OU1<-mvOU(trees, PCAphylo[,2], model="OU1")

###OU model 4 optima
trait1_OUM4<-mvOU(trees, PCAphylo[,1], model="OUM")
trait2_OUM4<-mvOU(trees, PCAphylo[,2], model="OUM")

###OU model 5 optima
trait1_OUM5<-mvOU(trees2, PCAphylo[,1], model="OUM")
trait2_OUM5<-mvOU(trees2, PCAphylo[,2], model="OUM")


###Models comparison for PCA1
resultsPC1<-list(trait1_white,trait1_BM,trait1_OU1,trait1_OUM4,trait1_OUM5)

mvMORPH::aicw(resultsPC1, aicc=F)

###Models comparison for PCA2
resultsPC2<-list(trait2_white,trait2_BM,trait2_OU1,trait2_OUM4,trait2_OUM5)

mvMORPH::aicw(resultsPC2, aicc=F)


###Multivariate enviromental evolution models (both PCA axes)
Mwhite<-mvBM(treestar, PCAphylo[,1:2], model="BM1")
MBM1<-mvBM(trees, PCAphylo[,1:2], model="BM1")
MOU1<-mvOU(trees, PCAphylo[,1:2], model="OU1")
MOU4<-mvOU(trees, PCAphylo[,1:2], model="OUM")
MOU5<-mvOU(trees2, PCAphylo[,1:2], model="OUM")

results<-list(Mwhite,MBM1,MOU1,MOU4 ,MOU5)
aicw(results)
