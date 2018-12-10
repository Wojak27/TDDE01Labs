data = read.csv(file="/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab\ 2/NIRSpectra.csv",sep=";", dec=",")
set.seed(12345)

data1 = data
#creating matrix
data1$Viscosity = c()
res=prcomp(data1)
lambda=res$sdev^2
#eigenvalues
lambda
#proportion of variation sprintf("%2.3f",lambda/sum(lambda)*100) screeplot(res)
sprintf("%2.3f",lambda/sum(lambda)*100) 
#components 1 and 2 capture over 99% of the total variance
screeplot(res)

plot(res$x[,1], res$x[,2], ylab = "PC2", xlab = "PC1")
#res$x[,1][res$x[,1]>1]

#assignment 4.2
U=res$rotation

plot(U[ , 1 ], main="Traceplot, PC1", ylim=range(-0.5,0.5))
plot(U[ , 2 ],main="Traceplot, PC2", ylim = range(-0.5,0.5))

#Assignment 4.3

#setting data
S<-U
set.seed(12345)
#mixing matrix
A <-matrix(c(0.291, 0.6557, -0.5439, 0.5572), 2, 2)

plot(1:length(S[,1]), S[,1],xlab = "PC1", ylab = "", main="Traceplot, PC1", ylim = range(-0.5,0.5))
#par(new = TRUE)
plot(1:length(S[,2]), S[,2], xlab = "PC2", ylab = "", main="Traceplot, PC2", ylim = range(-0.5,0.5))

par(mfcol = c(1, 2))
plot(1:length(X[,1]), X[,1],xlab = "W'1", ylab = "", main="Traceplot, W'", ylim = range(-0.5,0.5))
plot(1:length(X[,2]), X[,2], xlab = "W'2", ylab = "", main="Traceplot, W'", ylim = range(-0.5,0.5))
library(fastICA)
#using ICA to estimate signals
a <-fastICA(data1, 2) #ICA

W.prime = a$K%*%a$W
plot(1:length(W.prime[,1]), W.prime[,1],xlab = "W'1", ylab = "", main="Traceplot, W'1", ylim = range(-13,3))
plot(1:length(W.prime[,2]), W.prime[,2],xlab = "W'2", ylab = "", main="Traceplot, W'2", ylim = range(-13,3))

plot(a$S[,1], a$S[,2])
