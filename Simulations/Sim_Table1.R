Rep<-100

n<-10
L<-1000
n1<-1
n2<-5
n3<-n-(n1+n2)
mu<-c(0.5,1)

rejection<-vector()

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
  ans.test<-uclust3(data=data)
  
  rejection[j]<-(ans.test$ishomo==FALSE) 
  
}

sum(rejection)/Rep