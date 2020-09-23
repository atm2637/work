setwd("D:/±MÃD")
getwd()
data = read.table("final_third.csv",header=T,sep=",")
data=na.exclude(data)

n=0.3*nrow(data)
test.index=sample(1:nrow(data),n)
train=data[-test.index,]
test=data[test.index,]
lmtrain <- lm(formula = boxoffice~., data=train) 
summary(lmtrain)
y = data$boxoffice[test.index]
y_hat=predict(lmtrain,newdata=test,type="response")
test.MAPE=mean(abs(y-y_hat)/y)
cat("MAPE(test)=",test.MAPE*100,"%\n")

lmtrain_1 <- lm(formula = boxoffice~budget+Music+Horror+Fantasy+Adventure+Action,data=train)
summary(lmtrain_1)

y = data$boxoffice[test.index]
y_hat=predict(lmtrain_1,newdata=test,type="response")
test.MAPE=mean(abs(y-y_hat)/y)
cat("MAPE(test)=",test.MAPE*100,"%\n")
