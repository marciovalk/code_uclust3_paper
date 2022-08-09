library(uclust)
library(sigclust2)
library(drc)
library(pdfCluster)

Rep<-100
n<-20
L<-1000
dif<-0.1
mu<-c(dif,2*dif)
n1<-1
n2<-n%/%3
n3<-n-(n1+n2)


truevector<-c(rep(1,n1),rep(2,n2),rep(3,n3))

ari.uclust3<-vector()
ari.sigclust<-vector()
ari.uhclust<-vector()

for(j in 1:Rep){
  
  
  data<-matrix(0,ncol=L,nrow=n) 
  for(k in 1:n1){
    data[k,]<-acf(arima.sim(n = L, list(ar = mu[1])),lag.max = LG.max,plot = FALSE)$acf[-1]
  }
  for(k in (n1+1):(n1+n2)){
    data[k,]<-acf(arima.sim(n = L, list(ar = mu[2])),lag.max = LG.max,plot = FALSE)$acf[-1]
  }
  for(k in (n-n3+1):n){
    data[k,]<-acf(rnorm(L,0,1),lag.max = LG.max,plot=FALSE)$acf[-1]
  }
  
  uclust3.ans<-uclust3(data=data)
  sigclust<-shc(data,metric = "euclidean", linkage = "average", alpha=0.05)
  uhclust<-uhclust(data=data,plot=FALSE)
  
  uclust3.groups<-vector()
  uclust3.groups[uclust3.ans$groups$cluster1]<-1;uclust3.groups[uclust3.ans$groups$cluster2]<-2;uclust3.groups[uclust3.ans$groups$cluster3]<-3
  
  ari.uclust3[j]<-adj.rand.index(uclust3.groups,truevector)
  ari.sigclust[j]<-adj.rand.index(shcutree(sigclust, alpha = 0.05, ci_idx = 1, ci_emp = FALSE),truevector)
  ari.uhclust[j]<-adj.rand.index(as.vector(uhclust$groups),truevector)
  
}

ptc.uclust3<-sum(ari.uclust3==1)/Rep
ptc.sigclust<-sum(ari.sigclust==1)/Rep
ptc.uhclust<-sum(ari.uhclust==1)/Rep
