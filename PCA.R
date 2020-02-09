## Principal Components Analysis

#explore the dataset
str(USArrests)
USArrests #data must be numerical
summary(USArrests)

states=row.names(USArrests)
states
names(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
apply(USArrests, 2, sd)

#perform Principal Components Analysis using prcomp function
pr.out=prcomp(USArrests, scale=TRUE)

names(pr.out) #outputs from prcomp
pr.out$center #means used for scaling before implementiong PCA (same as mean above)
pr.out$scale  #sdev used for scaling before implementiong PCA (same as sdev above)
pr.out$rotation #pc loading vector. multiply by the data to get the pc score vector
pr.out$x #pc has already done the above multiplication. this gives the pc score vector
biplot(pr.out, scale=0) #plots the first 2 pcs from the data.scale 0 ensures that arrows are scaled to represent the loadings

#principal components are only unique up to a sign change. do the following to recreate diagram in book 10.1
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)

pr.out$sdev #sdev of the principal components
pr.var=pr.out$sdev^2
pr.var #variance of the principal components

pve=pr.var/sum(pr.var)
pve #proportion of variance explained by each principal component

#plotting pve of each component
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')

#plotting cumulative pve of each component
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')

### NCI60 Data
install.packages("ISLR")
library(ISLR)
nci.labs<-NCI60$labs
nci.data<-NCI60$data

dim(nci.data)

nci.labs[1:4]
table(nci.labs)

#we can perform pca and clustering without making use of the cancer types
#and then compare the pca and clustering results to the cancer types to see if they agree

### PCA on the NCI60 Data
pr.out=prcomp(nci.data, scale=TRUE) #there will be 64 pcs

#function which assigns a distinct color to each element of a numeric vector
#function will be used to assign a color to each of the 64 cell lines based on the cancer type to which it corresponds
#rainbow function takes a positive integer and returns a vector containing that number of distinct colors
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

#plotting the pc score vectors
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19,xlab="Z1",ylab="Z2") #pc 1 and 2
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19,xlab="Z1",ylab="Z3") #pc 1 nd 3

summary(pr.out)
plot(pr.out)  #plotting variance explained by each pc

#but it is more informative to plot the proportion of variance explained (pve)
pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve,  type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component", col="brown3")
