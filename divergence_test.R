
PC1dn<-t.test(scores.di[,1], scores.te[,1]) # diferencias
PC1dn1<-t.test(scores.di[,1], scores.he[,1]) # no diferencias
PC1dn2<-t.test(scores.di[,1], scores.do[,1]) # diferencias
PC1dn3<-t.test(scores.te[,1], scores.he[,1]) # diferencias
PC1dn4<-t.test(scores.te[,1], scores.do[,1]) # diferencias
PC1dn5<-t.test(scores.he[,1], scores.do[,1]) # diferencias

PC2dn<-t.test(scores.di[,2], scores.te[,2]) # no diferencias
PC2dn1<-t.test(scores.di[,2], scores.he[,2]) # diferencias
PC2dn2<-t.test(scores.di[,2], scores.do[,2]) # diferencias
PC2dn3<-t.test(scores.te[,2], scores.he[,2]) # diferencias
PC2dn4<-t.test(scores.te[,2], scores.do[,2]) # diferencias
PC2dn5<-t.test(scores.he[,2], scores.do[,2]) # diferencias

dbPC1<-vector()
for (i in 1:9999){
  dbiPC1<-mean(sample(scores.clim[,1],500, replace=T))
  dbfPC1<-mean(sample(scores.clim[,1],500, replace=T))
  dbPC1[i]<-dbfPC1-dbiPC1
}

dbPC2<-vector()
for (i in 1:9999){
  dbiPC2<-mean(sample(scores.clim[,2],500, replace=T))
  dbfPC2<-mean(sample(scores.clim[,2],500, replace=T))
  dbPC2[i]<-dbfPC2-dbiPC2
}

dn1<-mean(scores.di[,1])- mean(scores.te[,1])
dn1_1<-mean(scores.di[,1])- mean(scores.he[,1])
dn1_2<-mean(scores.di[,1])- mean(scores.do[,1])
dn1_3<-mean(scores.te[,1])- mean(scores.he[,1])
dn1_4<-mean(scores.te[,1])- mean(scores.do[,1])
dn1_5<-mean(scores.he[,1])- mean(scores.do[,1])
db1<- quantile(dbPC1,c(0.025, .975))

dn2<-mean(scores.di[,2])- mean(scores.te[,2])
dn2_1<-mean(scores.di[,2])- mean(scores.he[,2])
dn2_2<-mean(scores.di[,2])- mean(scores.do[,2])
dn2_3<-mean(scores.te[,2])- mean(scores.he[,2])
dn2_4<-mean(scores.te[,2])- mean(scores.do[,2])
dn2_5<-mean(scores.he[,2])- mean(scores.do[,2])
db2<-quantile(dbPC2,c(0.025, .975))

#dbIC= (-0.011362342  0.002173957)
#daPC1=0.256439
