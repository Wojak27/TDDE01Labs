#datavariable=read.table(file= file.choose(), header = T, sep=" ", na.strings=" ", stringsAsFactors=F)
library(xlsx)
library(Amelia)
library(kknn)

my_data = readxl::read_excel(path = "/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab1/Assignemnt1/spambase.xlsx", 1)
#missmap(data, main = "Missing values vs observed") #for plotting missing data
n=dim(my_data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))

#splitting data into training data and test data
train = my_data[id,]
test = my_data[-id,]
fit = glm(Spam~.,family=binomial(link='logit'),data=train)
summary(fit)

results = predict(fit,test)
results = ifelse(results > 0.5,1,0) #Assignment 1.2
#results = ifelse(results > 0.9,1,0) #Assignment 1.3
misClasificError = mean(results != test$Spam)

Tlogreg = table(factor(test$Spam, labels=c("Actual not spam", "Actual spam")),
      factor(results, labels=c("Pred not spam", "Pred spam")));
print(Tlogreg)
heatmap(Tlogreg)

#print(paste('Accuracy',1-misClasificError))
print(paste('Missclassification',misClasificError))

#kknn
#https://www.r-bloggers.com/k-nearest-neighbor-step-by-step-tutorial/
modelknn <- kknn(Spam~., train, train, k=30)
modelknn1 <- kknn(Spam~., train, test, k=1)

fitkknn <- fitted(modelknn)

result1 <- floor(fitkknn + 0.5)


misClasificError1 = mean(result1 != test$Spam)

#print(paste('Accuracy kknn',1-misClasificError1))
print(paste('Missclassification kknn',misClasificError1))

#kknn table
Tknn =table(factor(test$Spam, labels=c("Actual not spam", "Actual spam")),
            factor(result1, labels=c("Pred not spam", "Pred spam")));
print(Tknn)
#heatmap(Tknn)

#plotting logistic regression
#plot(Spam~., data=my_data, col="red4")  
#lines(Spam~., test, col="green4", lwd=2)

#logistic regression with glm() and predict()
#logistic regression vs linear regression
#https://stackoverflow.com/questions/12146914/what-is-the-difference-between-linear-regression-and-logistic-regression
#logistic regression tutorial in r
#https://datascienceplus.com/perform-logistic-regression-in-r/
#for plotting missing values in the dataset
#library(Amelia)
#missmap(training.data.raw, main = "Missing values vs observed")
