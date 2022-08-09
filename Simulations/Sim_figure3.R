library(drc)


Rep<-100
n<-20
L<-1000
dif<-0.15
mu<-c(dif,2*dif)
n1<-1
n2<-n%/%3
n3<-n-(n1+n2)

rejection<-vector()
truevector<-c(rep(1,n1),rep(2,n2),rep(3,n3))

for(j in 1:Rep){
  
  
  data<-matrix(0,ncol=L,nrow=n) 
  
  for(k in 1:n1){
    data[k,]<-rnorm(L,mu[1],1)
  }
  for(k in (n1+1):(n1+n2)){
    data[k,]<-rnorm(L,mu[2],1) 
  }
  
  ans.test<-utest3(data=data,truevector)
  
  rejection[j]<-(ans.test$is.homo==FALSE) 
 
}

sum(rejection)/Rep