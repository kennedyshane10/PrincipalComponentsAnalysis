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