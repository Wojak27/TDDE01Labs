my_data = readxl::read_excel(path = "/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab1/Assignment2/machines.xlsx", 1)
hist(my_data$Length)

logLikelihood = function(x, theta){

  theta.return = 1:length(theta)
  
  for(a in 1 : length(theta)){
    probVector = theta[a]*exp(-theta[a]*x)
    theta.return[a] = log(prod(probVector))
  }
  
  return(theta.return)
}

library(ggplot2)
theta = seq(0, 4, by=0.001)
result1.vector = logLikelihood(my_data$Length, theta)
result1 = data.frame(theta = theta, maxlik= result1.vector)
result1$cat = "Entire dataset"
ggplot(result1, aes(theta, maxlik, colour = cat))+
  geom_point()
#print(max(result1))

result2.vector = logLikelihood(head(my_data,6), theta)

result2 = data.frame(theta= theta, maxlik= result2.vector)

result2$cat = "6 first points"

df = rbind(result2, result1 )

ggplot(df, aes(theta, maxlik, colour = cat))+
  geom_point()

  p.prior= function(lambda, theta){
  returnProb = lambda*exp(-theta*lambda)
  return(returnProb)
}

logLikelihoodlabda = function(theta,x, lambda){

  theta.return = 1:length(theta)
  
  for(a in 1 : length(theta)){
    probVector = theta[a]*exp(-theta[a]*x)
    theta.return[a] = log(prod(probVector)*p.prior(lambda, theta[a]))
  }
  
  return(theta.return)
}
result.bayes.vector = logLikelihoodlabda(theta,my_data$Length, 10)
result.bayes = data.frame(theta = theta, maxlik= result.bayes.vector)
result.bayes$cat = "Bayes model"

df = rbind(result2, result1, result.bayes )

cat("Max likelihood for all datapoints: ", as.numeric(max(result1.vector)), " with theta=", theta[as.numeric(which.max(result1.vector))])
cat("Max likelihood for first 6 datapoints: ", as.numeric(max(result2.vector)), " with theta=", theta[as.numeric(which.max(result2.vector))])
cat("Max likelihood for bayesian model: ", as.numeric(max(result.bayes.vector)), " with theta=", theta[as.numeric(which.max(result.bayes.vector))])
#print(paste("First 6 datapoints: "max(as.vector(result2)))
#print(paste("Bayesian model: "max(as.vector(result.bayes)))

ggplot(df, aes(theta, maxlik, colour = cat))+
  geom_point()

set.seed(12345)
randomPoints = rexp(50, rate=1.126)

hist(randomPoints)
#x <- seq(0, 3, length.out=1000)
#dat <- data.frame(x=x, px=dexp(x, rate=1.126))
#lines(x,dat$px , col="blue", lwd=2)