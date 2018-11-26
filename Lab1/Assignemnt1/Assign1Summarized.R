library(kknn)
my_data = readxl::read_excel(path = "/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab1/Assignemnt1/spambase.xlsx", 1)
n=dim(my_data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train = my_data[id,]
test = my_data[-id,]

fit = glm(Spam~.,family=binomial(link='logit'),data=train)

results.train = predict(fit,train, type = "response")
results = predict(fit,test, type = "response")
results.train05 = ifelse(results.train > 0.5,1,0) 
#Assignment 1.2a
results05 = ifelse(results > 0.5,1,0) #Assignment 1.2b

#Assignment 1.2a
table(factor(train$Spam, labels=c("Actual not spam", "Actual spam")),
      factor(results.train05, labels=c("Pred not spam", "Pred spam")));

missclassification05.train = mean(results.train05 != train$Spam)
print(paste('Missclassification error train',missclassification05.train))
print(paste('Accuracy train ',1-missclassification05.train))

#Assignment 1.2b
table(factor(test$Spam, labels=c("Actual not spam", "Actual spam")),
      factor(results05, labels=c("Pred not spam", "Pred spam")));

missclassification05 = mean(results05 != test$Spam)
print(paste('Missclassification error test',missclassification05))
print(paste('Accuracy test ',1-missclassification05))

print(paste('Precission train: ',344/(142+344)))
print(paste('Precission test: ',336/(146+336)))

results.train09 = ifelse(results.train > 0.9,1,0) #Assignment 1.2a
results09 = ifelse(results > 0.9,1,0) #Assignment 1.2b

#Assignment 1.3a
table(factor(train$Spam, labels=c("Actual not spam", "Actual spam")),
      factor(results.train09, labels=c("Pred not spam", "Pred spam")));
missclassification09.train = mean(results.train09 != train$Spam)
print(paste('Missclassification error train',missclassification09.train))
print(paste('Accuracy train ',1-missclassification09.train))

#Assignment 1.3b
table(factor(test$Spam, labels=c("Actual not spam", "Actual spam")),
      factor(results09, labels=c("Pred not spam", "Pred spam")));
missclassification09 = mean(results09 != test$Spam)
print(paste('Missclassification error test',missclassification09))
print(paste('Accuracy test ',1-missclassification09))

print(paste('Recall train: ',6/(6+419)))
print(paste('Recall test: ',6/(6+427)))

modelknn.train30 <- kknn(Spam~., train, train, k=30)
modelknn.test30 <- kknn(Spam~., train, test, k=30)
result.train30 <- floor(fitted(modelknn.train30) + 0.5)
result.test30 <- floor(fitted(modelknn.test30) + 0.5)
misClasificError.kknn30.train = mean(result.train30 != train$Spam)
misClasificError.kknn30.test = mean(result.test30 != test$Spam)


print(paste("Misclassification K = 30, train, train", misClasificError.kknn30.train))
print(paste("Misclassification K = 30, train, test", misClasificError.kknn30.test))


modelknn.train1 <- kknn(Spam~., train, train, k=1)
modelknn.test1 <- kknn(Spam~., train, test, k=1)
result.train1 <- floor(fitted(modelknn.train1) + 0.5)
result.test1 <- floor(fitted(modelknn.test1) + 0.5)
misClasificError.kknn1.train = mean(result.train1 != train$Spam)
misClasificError.kknn1.test = mean(result.test1 != test$Spam)

print(paste("Misclassification K = 1, train, train",misClasificError.kknn1.train))
print(paste("Misclassification K = 1, train, train",misClasificError.kknn1.test))