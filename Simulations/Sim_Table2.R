require("pdfCluster")

Rep<-100

n<-20
L<-1000
n1<-n2<-n%/%3
n3<-n-(n1+n2)
mu<-c(0.5,1)

truevector<-c(rep(1,n1),rep(2,n2),rep(3,n3))

ARI.uclust3<-NULL 
ARI.kmeans<-NULL

for(j in 1:Rep){
  
  data<-matrix(0,ncol=L,nrow=n) 
  for(k in 1:n1){
    data[k,]<-rnorm(L,mu[1],1)
  }
  for(k in (n1+1):(n1+n2)){
    data[k,]<-rnorm(L,mu[2],1) 
  }
  for(k in (n-n3+1):n){
    data[k,]<-rnorm(L,0,1)
  }
  
  ans.uclust3<-uclust3(data=data)
  ans.kmeans<-kmeans(data,3)
  
  groups.uclust3<-vector()
  groups.uclust3[ans.uclust3$groups$cluster1]<-1;groups.uclust3[ans.uclust3$groups$cluster2]<-2;groups.uclust3[ans.uclust3$groups$cluster3]<-3
  ARI.uclust3[j]<-adj.rand.index(groups.uclust3,truevector)
  
  groups.kmeans<-ans.kmeans$cluster
  ARI.kmeans[j]<-adj.rand.index(groups.kmeans,truevector)
  
}

mean(ARI.uclust3);sd(ARI.uclust3)
mean(ARI.kmeans);sd(ARI.kmeans)
