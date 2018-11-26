library(ggplot2)
my_data4 = readxl::read_excel(path = "/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab1/tecator.xlsx", 1)
plot(my_data4$Protein, my_data4$Moisture, col = "blue")

linProtMois <- lm(my_data4$Moisture ~ my_data4$Protein)
#poly.model <- lm(my_data4$Moisture ~ poly(my_data4$Protein,5))

#summary(poly.model)
#summary(linProtMois)
#plot(fitted(poly.model),residuals(poly.model))
#new <- data.frame(Protein = my_data4$Protein)
#poly.predict <- predict(poly.model, newdata = new, interval="confidence")
#print(length(fitted(pred)))
abline(linProtMois)

#poly.predict.over = data.frame(protein= my_data4$Protein, pred = poly.predict[,1])
#poly.predict.fit = data.frame(protein= my_data4$Protein, pred = poly.predict[,2])
#poly.predict.under = data.frame(protein= my_data4$Protein, pred = poly.predict[,3])
#poly.moisture = data.frame(protein= my_data4$Protein, pred = my_data4$Moisture)

#poly.predict.over$cat = "over"
#poly.predict.fit$cat = "fit"
#poly.predict.under$cat = "under"
#poly.moisture$cat = "data points"

#df = rbind(poly.predict.over, poly.predict.fit, poly.predict.under,poly.moisture )

#ggplot(df, aes(protein, pred, colour = cat))+ geom_point()

#plot(my_data4$Protein, poly.predict[,1] , col='red',lwd=1)
#lines(my_data4$Protein,poly.predict[,2],col='blue',lwd=1)
#lines(my_data4$Protein,poly.predict[,3],col='black',lwd=1)


#linProtMois = summary(linProtMois)
#print(linProtMois)
#print(linProtMois$residuals)


#assignment 4.3

my_data4 = readxl::read_excel(path = "/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab1/tecator.xlsx", 1)

set.seed(12345)

dimension = dim(my_data4)[1]
set.seed(12345)
id = sample(1:dimension, floor(dimension*0.5))
train = my_data4[id,]
test = my_data4[-id,]

#models:
M1 = lm(Moisture ~ poly(Protein,1), data = train)
M2 = lm(Moisture ~ poly(Protein,2), data = train)
M3 = lm(Moisture ~ poly(Protein,3), data = train)
M4 = lm(Moisture ~ poly(Protein,4), data = train)
M5 = lm(Moisture ~ poly(Protein,5), data = train)
M6 = lm(Moisture ~ poly(Protein,6), data = train)

new <- data.frame(Protein = train$Protein)
M1.predicted.train = predict(M1, newdata = new)
M2.predicted.train = predict(M2, newdata = new) #interval = "confidence" ?
M3.predicted.train = predict(M3, newdata = new)
M4.predicted.train = predict(M4, newdata = new)
M5.predicted.train = predict(M5, newdata = new)
M6.predicted.train = predict(M6, newdata = new)

new.test <- data.frame(Protein = test$Protein)
M1.predicted.test = predict(M1, newdata = new.test)
M2.predicted.test = predict(M2, newdata = new.test) #interval = "confidence" ?
M3.predicted.test = predict(M3, newdata = new.test)
M4.predicted.test = predict(M4, newdata = new.test)
M5.predicted.test = predict(M5, newdata = new.test)
M6.predicted.test = predict(M6, newdata = new.test)

mse <- function(M, data){
  return(mean((data - M)^2))
}

print(length(test$Moisture))
print(paste("MSE 1 train",mse(M1.predicted.train, train$Moisture)))
print(paste("MSE 2 train",mse(M2.predicted.train, train$Moisture)))
print(paste("MSE 3 train",mse(M3.predicted.train, train$Moisture)))
print(paste("MSE 4 train",mse(M4.predicted.train, train$Moisture)))
print(paste("MSE 5 train",mse(M5.predicted.train, train$Moisture)))
print(paste("MSE 6 train",mse(M6.predicted.train, train$Moisture)))
print(paste("MSE 1 test",mse(M1.predicted.test, test$Moisture)))
print(paste("MSE 2 test",mse(M2.predicted.test, test$Moisture)))
print(paste("MSE 3 test",mse(M3.predicted.test, test$Moisture)))
print(paste("MSE 4 test",mse(M4.predicted.test, test$Moisture)))
print(paste("MSE 5 test",mse(M5.predicted.test, test$Moisture)))
print(paste("MSE 6 test",mse(M6.predicted.test, test$Moisture)))

MSE.1 = mse(M1.predicted.train, train$Moisture)
MSE.2 = mse(M2.predicted.train, train$Moisture)
MSE.3 = mse(M3.predicted.train, train$Moisture)
MSE.4 = mse(M4.predicted.train, train$Moisture)
MSE.5 = mse(M5.predicted.train, train$Moisture)
MSE.6 = mse(M6.predicted.train, train$Moisture)
MSE.1.v = mse(M1.predicted.test, test$Moisture)
MSE.2.v = mse(M2.predicted.test, test$Moisture)
MSE.3.v = mse(M3.predicted.test, test$Moisture)
MSE.4.v = mse(M4.predicted.test, test$Moisture)
MSE.5.v = mse(M5.predicted.test, test$Moisture)
MSE.6.v = mse(M6.predicted.test, test$Moisture)


iterations <- c(1,2,3,4,5,6)
results.training <- c(MSE.1,MSE.2,MSE.3,MSE.4,MSE.5,MSE.6)
results.validation <- c(MSE.1.v,MSE.2.v,MSE.3.v,MSE.4.v,MSE.5.v,MSE.6.v)

results.training.dataframe = data.frame(Iteration= iterations, MSE1 = results.training)
results.validation.dataframe = data.frame(Iteration= iterations, MSE2 = results.validation)

results.binded = cbind(results.training.dataframe, results.validation.dataframe$MSE2 )
colnames(results.binded) = c("Iteration", "MSE1", "MSE2")

ggplot(results.binded) +
  geom_line(aes(x = Iteration, y = MSE1, colour = "Training")) +
  geom_line(aes(x = Iteration, y = MSE2, colour = "Validation")) +
  labs(title="MSE vs Iteration", y="MSE", x="Iteration", color = "Legend") +
  scale_color_manual(values = c("blue", "orange"))

#Assignment 4.4

my_data4 = readxl::read_excel(path = "/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab1/tecator.xlsx", 1)

library(MASS)

fat.vector = my_data4[,102]
sliced.data = my_data4[,2:101]

model.sliced = lm(fat.vector$Fat~. , data = sliced.data)

reduced = stepAIC(model.sliced, direction = c("both"), trace = FALSE)

print(paste("reduced: ", length(reduced$coefficients-1))) #-1 for the coefficient B_0


#Assignment 4.5,4.6
library(glmnet)

my_data4 = readxl::read_excel(path = "/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab1/tecator.xlsx", 1)

set.seed(12345)
sliced.data = my_data4[,2:101]

ridge =glmnet(as.matrix(sliced.data),my_data4$Fat, alpha=0, family = "gaussian")
lasso=glmnet(as.matrix(sliced.data),my_data4$Fat, alpha=1, family = "gaussian")

plot(ridge, xvar="lambda", label=T)
plot(lasso, xvar="lambda", label=T)

#Assignment 4.7

library(glmnet)

lasso=glmnet(as.matrix(sliced.data),my_data4$Fat, alpha=1, family = "gaussian")
my_data4 = readxl::read_excel(path = "/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab1/tecator.xlsx", 1)
sliced.data = my_data4[,2:101]

model=cv.glmnet(as.matrix(sliced.data),my_data4$Fat, alpha=1,family="gaussian", lambda = c(lasso$lambda,0))
print(model$lambda.min) #lambda that minimizes the error 
plot(model)
#coef(model, s="lambda.min")

