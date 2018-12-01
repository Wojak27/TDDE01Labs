data = readxl::read_excel(path = "/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab\ 2/creditscoring.xls", 1)

#Assignment 2.1
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,]

#Assignment 2.2
train = as.data.frame(train)
library(tree)
label = ifelse(train$good_bad == "good", 2,1)
#train$good_bad = label
#test$good_bad = ifelse(test$good_bad == "good", 2,1)
#valid$good_bad = ifelse(valid$good_bad == "good", 2,1)
n = dim(train)[1]
fit.deviance = tree(as.factor(good_bad)~. , train, split = c("deviance")) #as.factor used from this post:
fit.gini = tree(as.factor(good_bad)~. , train, split = c("gini")) #as.factor used from this post:
#https://stackoverflow.com/questions/31843212/r-predict-func-type-class-error

#plot
#plot(fit.deviance)
#text(fit.deviance, pretty=0)
plot(fit.gini)
text(fit.gini, pretty=0)
fit1
#summary(fit1)

#predic
predict.fit.deviance.train=predict(fit.deviance, newdata=train, type="class")
predict.fit.deviance.test=predict(fit.deviance, newdata=test, type="class")
predict.fit.gini.train=predict(fit.gini, newdata=train, type="class")
predict.fit.gini.test=predict(fit.gini, newdata=test, type="class")

#misclass error
misClasificError.deviance.train = mean(predict.fit.deviance.train != train$good_bad)
misClasificError.deviance.test = mean(predict.fit.deviance.test != test$good_bad)
misClasificError.gini.train = mean(predict.fit.gini.train != train$good_bad)
misClasificError.gini.test = mean(predict.fit.gini.test != test$good_bad)

#print
print(misClasificError.deviance.train)
print(misClasificError.deviance.test) #<- better results
print(misClasificError.gini.train)
print(misClasificError.gini.test)

#Assignment 2.3
trainScore=rep(0,19)
testScore=rep(0,19)
for(i in 2:19) { 
  prunedTree=prune.tree(fit.deviance,best=i) 
  pred=predict(prunedTree, newdata=valid, type="tree") 
  trainScore[i]=deviance(prunedTree)
  testScore[i]=deviance(pred)
}
plot(2:19, trainScore[2:19], type="b", col="red", ylim=c(0,600), ylab = "Deviance", xlab = "Tree length")
axis(side=1, at=c(2:19))
legend(2, 95, legend=c("Train", "Valid"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
points(2:19, testScore[2:19], type="b", col="blue")

best.n = match(c(min(testScore[2:19])), testScore)
print(paste("Best length:", best.n))
#number of leaves = 4
#the tree length with the lowest deviance is n=4
#selecting the tree n = 4
selected.tree=prune.tree(fit.deviance,best=best.n)
pred.selected.tree=predict(selected.tree, test, type="class")
plot(selected.tree)
text(selected.tree, pretty=0)

misClasificError.selected.tree = mean(pred.selected.tree != test$good_bad)

#print
print(misClasificError.selected.tree) # this is 0.256

#Assignment 2.4
data = readxl::read_excel(path = "/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab\ 2/creditscoring.xls", 1)
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,]

library(MASS)
library(e1071)
naive.bayes.model=naiveBayes(as.factor(good_bad)~., data=train)
naive.bayes.model
naive.bayes.predict.train = predict(naive.bayes.model, newdata=train[,1:19]) 
naive.bayes.predict.test = predict(naive.bayes.model, newdata=test[,1:19]) 
#conf matrix

table(factor(train$good_bad, labels=c("Actual Bad", "Actual Good")), factor(naive.bayes.predict.train, labels=c("Bad", "Good")))
table(factor(test$good_bad, labels=c("Actual Bad", "Actual Good")), factor(naive.bayes.predict.test, labels=c("Bad", "Good")) )


misClasificError.naive.bayes.predict.train = mean(naive.bayes.predict.train != train$good_bad)
misClasificError.naive.bayes.predict.test = mean(naive.bayes.predict.test != test$good_bad)
print(misClasificError.naive.bayes.predict.train)
print(misClasificError.naive.bayes.predict.test)

#Assignment 2.5
#type = "raw" vs "class":
#https://stackoverflow.com/questions/23085096/type-parameter-of-the-predict-function
naive.bayes.predict = predict(naive.bayes.model, newdata=test[,1:19], type = "raw") 
naive.bayes.predict
pred.selected.tree.test=predict(selected.tree, test, type="vector")
pred.selected.tree.test
pred.selected.tree.roc = ifelse(pred.selected.tree.test[,2] > 0.5,1,0)
pred.selected.tree.roc


pi = seq(0.05, 0.95, by=0.05)
library(pROC)
naive.bayes.roc.vector.tpr = 1:length(pi)
naive.bayes.roc.vector.fpr = 1:length(pi)

dec.tree.roc.vector.tpr = 1:length(pi)
dec.tree.roc.vector.fpr = 1:length(pi)


for(i in 1:length(pi)){
  naive.bayes.roc = ifelse(naive.bayes.predict[,2] > pi[i],1,0)
  dec.matrix.bayes = as.matrix(table(as.factor(naive.bayes.roc), test$good_bad))
  dec.matrix.bayes
  #dec.matrix.bayes[2,2] = tp# dec.matrix.bayes[2,1] = fn
  naive.bayes.roc.vector.tpr[i] = dec.matrix.bayes[2,2]/(dec.matrix.bayes[1,2]+dec.matrix.bayes[2,2])
  naive.bayes.roc.vector.fpr[i] = dec.matrix.bayes[2,1]/(dec.matrix.bayes[2,1]+dec.matrix.bayes[1,1])
  
  pred.selected.tree.roc = ifelse(pred.selected.tree.test[,2] > pi[i],1,0)
  
  #if we get a unique vector
  if(length(unique(pred.selected.tree.roc)) > 1){
    dec.matrix.tree = as.matrix(table(as.factor(pred.selected.tree.roc), test$good_bad))
    dec.tree.roc.vector.tpr[i] = dec.matrix.tree[2,2]/(dec.matrix.tree[1,2]+dec.matrix.tree[2,2])
    dec.tree.roc.vector.fpr[i] = dec.matrix.tree[2,1]/(dec.matrix.tree[2,1]+dec.matrix.tree[1,1])
      
  }
  
  
}
plot(c(1,naive.bayes.roc.vector.fpr,0), c(1,naive.bayes.roc.vector.tpr,0), xlim=c(0, 1), ylim=c(0, 1), col = "blue", type="b", xlab = "FPR", ylab = "TPR" )
#excluding points that did not write
points(c(dec.tree.roc.vector.fpr[dec.tree.roc.vector.fpr<=1],0),c(dec.tree.roc.vector.tpr[dec.tree.roc.vector.tpr<=1],0), col = "red", type="b")
abline(a=0, b=1)
legend(2, 95, legend=c("DT", "NB"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
#naive.bayes.predict = ifelse(naive.bayes.predict > pi,1,0)


#Assignment 2.6

data = readxl::read_excel(path = "/Users/karolwojtulewicz/Google\ Drive/skola/TDDE01/Labs/Lab\ 2/creditscoring.xls", 1)
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,]

library(MASS)
library(e1071)
#loss matrix seems not to do anything
naive.bayes.model=naiveBayes(as.factor(good_bad)~., data=train, params = list(loss= matrix(c(0,10,1,0), ncol = 2)))
naive.bayes.model
naive.bayes.predict.train = predict(naive.bayes.model, newdata=train[,1:19], params = list(loss= matrix(c(0,10,1,0), ncol = 2)) ) 
naive.bayes.predict.test = predict(naive.bayes.model, newdata=test[,1:19], params = list(loss= matrix(c(0,10,1,0), ncol = 2))) 
#conf matrix

table(factor(train$good_bad, labels=c("Actual Bad", "Actual Good")), factor(naive.bayes.predict.train, labels=c("Bad", "Good")))
table(factor(test$good_bad, labels=c("Actual Bad", "Actual Good")), factor(naive.bayes.predict.test, labels=c("Bad", "Good")) )


misClasificError.naive.bayes.predict.train = mean(naive.bayes.predict.train != train$good_bad)
misClasificError.naive.bayes.predict.test = mean(naive.bayes.predict.test != test$good_bad)
print(misClasificError.naive.bayes.predict.train)
print(misClasificError.naive.bayes.predict.test)

