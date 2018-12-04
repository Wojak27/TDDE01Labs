data = read.csv2(file="/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab\ 2/NIRSpectra.csv",head=TRUE,sep=";")
set.seed(12345)

data1 = data
#creating matrix
output <- matrix(unlist(data1), ncol = 127, byrow = FALSE)
data1$Viscosity = c()
res=prcomp(data1)
lambda=res$sdev^2
#eigenvalues
lambda
#proportion of variation sprintf("%2.3f",lambda/sum(lambda)*100) screeplot(res)
sprintf("%2.3f",lambda/sum(lambda)*100) 
#components 1 and 2 capture over 99% of the total variance
screeplot(res)

plot(res$x[,1], res$x[,2], ylim=c(-5,15))

#assignment 4.2
U=res$rotation
head(U)

Loadings <- as.data.frame(res$rotation[,1:2])
plot(Loadings[,1], main="Traceplot, PC1")
plot(Loadings[,2],main="Traceplot, PC2")

#Assignment 4.3

#setting data
S<-Loadings
set.seed(12345)
#mixing matrix
A <-matrix(c(0.291, 0.6557, -0.5439, 0.5572), 2, 2)
par(mfcol = c(1, 2))
plot(1:length(S[,1]), S[,1],xlab = "PC1", ylab = "", main="Traceplot, PC1")
plot(1:length(S[,2]), S[,2], xlab = "PC2", ylab = "", main="Traceplot, PC2")

#mixing components
X <-as.matrix(S) %*% A
par(mfcol = c(1, 2))
plot(1:length(X[,1]), X[,1],xlab = "W'1", ylab = "", main="Traceplot, W'")
plot(1:length(X[,2]), X[,2], xlab = "W'2", ylab = "", main="Traceplot, W'")

#using ICA to estimate signals
a <-fastICA(X, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1, method = "R", row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE) #ICA
par(mfcol = c(1, 2))
plot(1:length(a$S[,1]), a$S[,1], xlab = "S'1", ylab = "")
plot(1:length(a$S[,2]), a$S[,2], xlab = "S'2", ylab = "")

plot(a$S[,1], a$S[,2], ylim=c(-5,15))
