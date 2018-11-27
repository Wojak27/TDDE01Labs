
data = read.csv(file="/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab\ 2/australian-crabs.csv",head=TRUE,sep=",")
set.seed(12345)
# Assignment 1.1
plot(data$RW,data$CL, col= data$sex, xlab = "RW", ylab = "CL")

# Assignment 1.2
#LDA - Linear Discriminant Analysis
data = read.csv(file="/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab\ 2/australian-crabs.csv",head=TRUE,sep=",")
set.seed(12345)
library(MASS)
model.lda = lda(sex~RW+CL, data= data)
predictor.dataframe = data.frame(RW = data$RW, CL = data$CL)
colnames(predictor.dataframe) = c("RW", "CL")
predict.lda = predict(model.lda, predictor.dataframe)
plot(data$RW,data$CL, col= predict.lda$class, xlab = "RW", ylab = "CL")
misClasificError = mean(predict.lda$class != data$sex)
print(misClasificError)
#Really good fit, misclassification error: 0.035. You can barelly see differences.

# Assignment 1.3
#LDA - Linear Discriminant Analysis
data = read.csv(file="/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab\ 2/australian-crabs.csv",head=TRUE,sep=",")
set.seed(12345)
library(MASS)
model.lda = lda(sex~RW+CL, data= data, prior = c(0.1,0.9))
predictor.dataframe = data.frame(RW = data$RW, CL = data$CL)
colnames(predictor.dataframe) = c("RW", "CL")
predict.lda = predict(model.lda, predictor.dataframe)
plot(data$RW,data$CL, col= predict.lda$class, xlab = "RW", ylab = "CL")
misClasificError = mean(predict.lda$class != data$sex)
print(misClasificError)
#Really good fit, misclassification error: 0.08. You can barelly see differences.

# Assignment 1.4a
#LDA - Linear Discriminant Analysis
data = read.csv(file="/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab\ 2/australian-crabs.csv",head=TRUE,sep=",")
set.seed(12345)

model.log.reg = glm(sex~RW+CL,family=binomial(link='logit'), data= data)
predictor.dataframe = data.frame(RW = data$RW, CL = data$CL)
colnames(predictor.dataframe) = c("RW", "CL")
predict.log.reg = predict(model.log.reg, predictor.dataframe, type = "response" )

predict.log.reg = ifelse(predict.log.reg > 0.5,"Male","Female") #Assignment 1.2
dataframe.sex = data.frame(sex = predict.log.reg)
plot(data$RW,data$CL, col= dataframe.sex$sex, xlab = "RW", ylab = "CL")

misClasificError = mean(dataframe.sex$sex != data$sex)
print(misClasificError)
#Really good fit, misclassification error: 0.035. You can barelly see differences. 
#Same result as in 1.2

# Assignment 1.4b
#LDA - Linear Discriminant Analysis
data = read.csv(file="/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab\ 2/australian-crabs.csv",head=TRUE,sep=",")
set.seed(12345)

model.log.reg = glm(sex~RW+CL,family=binomial(link='logit'), data= data)
predictor.dataframe = data.frame(RW = data$RW, CL = data$CL)
colnames(predictor.dataframe) = c("RW", "CL")
predict.log.reg = predict(model.log.reg, predictor.dataframe, type = "response" )

predict.log.reg = ifelse(predict.log.reg > 0.9,"Male","Female") #Assignment 1.2
dataframe.sex = data.frame(sex = predict.log.reg)
plot(data$RW,data$CL, col= dataframe.sex$sex, xlab = "RW", ylab = "CL")

misClasificError = mean(dataframe.sex$sex != data$sex)
print(misClasificError)
#Really good fit, misclassification error: 0.035. You can barelly see differences. 
#Same result as in 1.2

