library(Amelia)
library(kknn)
library(fitdistrplus)
library(logspline)

#data for the machine lifetime. Length: shows lifetime of a machine
my_data = readxl::read_excel(path = "/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab1/Assignment2/machines.xlsx", 1)

n=dim(my_data)[1]
id=sample(1:n, n)

#splitting data into training data and test data
train = my_data[id,]
sort(train$Length)
descdist(train$Length, discrete = FALSE)

#the exponential distribution fits perfectly with all of the graphs
fit.norm = fitdist(train$Length, "exp")
plot(fit.norm)


pfunction = function(theta, x){
  returnVector = 1:x
  for(a in 1 : x){
    
    returnVector[a] = theta*exp(-theta*a)
    
  }
  return(returnVector)
}

#this is maximum log likelyhood funtion
logLikelyhood = function(x, theta){
  
  x = sort(x, decreasing = TRUE)
  
  theta.return = 1:length(theta)
  
  for(a in 1 : length(theta)){
    
    probVector = theta[a]*exp(-theta[a]*x)
    theta.return[a] = log(prod(probVector))
  }
  
  return(theta.return)
}

logLikelyhoodBayes = function(x, theta, landa){
  
  x = sort(x, decreasing = TRUE)
  
  theta.return = 1:length(theta)
  
  for(a in 1 : length(theta)){
    
    probVector = theta[a]*exp(-theta[a]*x)
    probLanda = landa*exp(-landa*x)
    probTot = prod(probVector)*prod(probLanda)
    theta.return[a] = log(probTot)/length(x)
  }
  print(length(theta.return))
  return(theta.return)
}

#plot data
#z = 1:length(data$Length)
#plot(z, sort(data$Length, decreasing = TRUE), col = "red")

#plot how theta varies, question 2
#with the higher amount of samples the model is more certain and can estimate results better.
theta = seq(0, 6, by=0.1)
result1 = logLikelyhood(my_data$Length, theta)
result2 = logLikelyhood(my_data$Length[1:6], theta)
resultBayes = logLikelyhoodBayes(my_data$Length, theta, 10)

print(max(result1))
print(max(result2))
print(max(resultBayes))
#plot how theta varies question 3
#plot(theta, resultBayes)
plot(theta, result2, col="red", lwd=2)
#plot(theta, result1, col="blue")

samples = sample(pfunction(max(result1),100), 50)


#the theta parameter decays faster with the entire set. this could mean that the first 6 values we take out from the 
#array have higher average than the entire array of data based on the peak of the curve.

train.sorted = sort(train$Length, decreasing = FALSE)
#hist(train.sorted, freq=FALSE, breaks=50, col="red")
#hist(samples, freq=FALSE, breaks=50, col="red")
#print(train)
#barplot(train$Length)
summary(train)

