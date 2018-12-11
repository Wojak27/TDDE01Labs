library(neuralnet)
set.seed(1234567890)
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
plot(trva$Var, trva$Sin)
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation
# Random initialization of the weights in the interval [-1, 1]
#winit <- # Your code here
round(runif(10,-1,1), 2)

threshold.vector = seq(0,10,by=1)
  
for(i in 1:10) {
  thr = i/1000
  threshold.vector[i] = thr
  nn <- neuralnet(tr$Sin~ tr$Var, threshold = thr, data = tr , hidden = 10, startweights = round(runif(31,-1,1), 2) )
    # Your code here
  prediction = compute(nn, va$Var)
  MSE.nn <- sum((va$Sin - prediction$net.result)^2)/length(va$Var)
  print(paste("Threshold: ",thr, "MSE: ", MSE.nn))
}
best.threshold = min(threshold.vector)

nn.best <- neuralnet(tr$Sin~ tr$Var, threshold = best.threshold, data = tr , hidden = 10, startweights = round(runif(31,-1,1), 2) )

predict.nn.best = compute(nn.best, va$Var)

print(paste("Selected threshold: ", best.threshold, " with MSE: ", sum((va$Sin - predict.nn.best$net.result)^2)/length(va$Var)))


plot(va$Var, predict.nn.best$net.result)
# Plot of the predictions (black dots) and the data (red dots)
plot(prediction(nn.best)$rep1)
points(trva, col = "red")

plot(nn.best)