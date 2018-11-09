#datavariable=read.table(file= file.choose(), header = T, sep=" ", na.strings=" ", stringsAsFactors=F)
library(xlsx)
library(Amelia)
data = readxl::read_excel(path = "spambase.xlsx", 1)
#missmap(data, main = "Missing values vs observed") #for plotting missing data
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))

#splitting data into training data and test data
train=data[id,]
test=data[-id,]
y=1:1370
fit <- glm(Spam~.,family=binomial(link='logit'),data=train)
summary(fit)

results = predict(fit,test)
results = ifelse(results > 0.5,1,0)
misClasificError = mean(results != test$Spam)

print(paste('Accuracy',1-misClasificError))

#logistic regression with glm() and predict()
#logistic regression vs linear regression
#https://stackoverflow.com/questions/12146914/what-is-the-difference-between-linear-regression-and-logistic-regression
#logistic regression tutorial in r
#https://datascienceplus.com/perform-logistic-regression-in-r/
#for plotting missing values in the dataset
#library(Amelia)
#missmap(training.data.raw, main = "Missing values vs observed")
