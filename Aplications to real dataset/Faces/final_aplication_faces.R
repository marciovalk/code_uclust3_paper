#library(uclust)
#library("mclust")
#library(sigclust2)

load(file="Data_3")
v=uhclust(data=Data,alpha=0.05) 


# ARI
true.v=c(rep(1,10),rep(2,10),rep(3,10))
ARI_uhc_pics=adjustedRandIndex(v$groups,true.v) #

##############
# Sigclust method 


sig=shc(as.matrix(Data),metric = "euclidean", linkage = "average", alpha=0.05)
plot(sig)

Data_extra=Data
rownames(Data_extra)=as.character(c(1:30))

sig1=shc(as.matrix(Data_extra),metric = "euclidean", linkage = "average", alpha=0.05)
plot(sig1)


# ARI
hc=hclust(dist(as.matrix(Data_extra)), method="average")
#plot(hc)
hch=(sig1$nd_type=="sig")*1
k.cut=sum(hch)+1
as.d=as.dendrogram(hc)
ct=cutree(as.d,k=k.cut)      
ARI_sig_pics=adjustedRandIndex(ct,true.v) 


###############
# uclust3 method 


md=as.matrix(dist(as.matrix(Data))^2)

uc3=uclust3(md=md)
groups.uclust3<-vector()
groups.uclust3[uc3$groups$cluster1]<-1
groups.uclust3[uc3$groups$cluster2]<-2
groups.uclust3[uc3$groups$cluster3]<-3
ARI_u3_pics=adjustedRandIndex(groups.uclust3,true.v)


res=c(ARI_uhc_pics,ARI_sig_pics,ARI_u3_pics)
names(res)=c("uhclust","sigclust","uclust3")
res
#uhclust  sigclust   uclust3 
#0.8585366 0.7788037 1.0000000 


