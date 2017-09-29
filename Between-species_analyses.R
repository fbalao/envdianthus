### Testing for climatic niche conservatism ###

library(ade4)

# pca1 <-dudi.pca(X)
# Between-Class Analysis# En nuestro caso corresponde al BETWEEN-occ de Broennimann 2012
# Nos da el % de varianza (inertia) explicado por los grupos
Bocc <- bca(pca1, group, scannf = FALSE)

#Monte-Carlo Test on the between-groups inertia percentage
rand1 <- rtest(bca, 99)
rand1
plot(rand1, main = "Monte-Carlo test")




#### Phylogenetic PCA (e.g., Revell 2009; Evolution)

library(phytools)
phyl.pca(tree, Y, method="BM", mode="cov")

