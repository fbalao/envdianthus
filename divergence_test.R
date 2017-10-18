#Test divergence
PC1dn<-t.test(scores.fuc[,1],scores.inc[,1])
PC2dn<-t.test(scores.fuc[,2], scores.inc[,2])

dbPC1<-vector()
for (i in 1:9999){
  dbiPC1<-mean(sample(scores.bacinc[,1],500, replace=T))
  dbfPC1<-mean(sample(scores.bacfuc[,1],500, replace=T))
  dbPC1[i]<-dbfPC1-dbiPC1
}

dbPC2<-vector()
for (i in 1:9999){
  dbiPC2<-mean(sample(scores.bacinc[,2],500, replace=T))
  dbfPC2<-mean(sample(scores.bacfuc[,2],500, replace=T))
  dbPC2[i]<-dbfPC2-dbiPC2
}

dn1<-mean(scores.fuc[,1])- mean(scores.inc[,1])
db1<- quantile(dbPC1,c(0.025, .975))

dn2<-mean(scores.fuc[,2])- mean(scores.inc[,2])
db2<-quantile(dbPC2,c(0.025, .975))

#dbIC= (-0.011362342  0.002173957)
#daPC1=0.256439