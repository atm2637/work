setwd("D:/±MÃD")
getwd()
data = read.table("final_forth.csv",header=T,sep=",")
n=0.3*nrow(data)
test.index=sample(1:nrow(data),n)
train=data[-test.index,]
test=data[test.index,]
par(mfrow=c(1,2))
hist(train$boxoffice)
hist(test$boxoffice)
install.packages("rpart")
library(rpart)
data.tree=rpart(boxoffice~.,data=train)
plot(data.tree)
text(data.tree,cex=8)
data.tree$variable.importance
y=data$boxoffice[-test.index]
y_hat=predict(data.tree,newdata=train,type="vector")
train.MAPE=mean(abs(y-y_hat)/y)
cat("MAPE(train)=",train.MAPE*100,"%\n")

y=data$boxoffice[test.index]
y_hat=predict(data.tree,newdata=test,type="vector")
test.MAPE=mean(abs(y-y_hat)/y)
cat("MAPE(test)=",test.MAPE*100,"%\n")
