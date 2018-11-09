#datavariable=read.table(file= file.choose(), header = T, sep=" ", na.strings=" ", stringsAsFactors=F)
library(xlsx)
data = readxl::read_excel(path = "spambase.xlsx", 1)
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]
Y=1:1370
fit <- glm(Y~1/(1+exp(-n)),data=data,family=binomial())
summary(fit)
#logistic regression with glm() and predict()
#logistic regression vs linear regression
#https://stackoverflow.com/questions/12146914/what-is-the-difference-between-linear-regression-and-logistic-regression
#logistic regression tutorial in r
#https://datascienceplus.com/perform-logistic-regression-in-r/