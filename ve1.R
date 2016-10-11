load("~/Desktop/Freddie Mac data/USMortgages2008_2009.rdata")

names(D1)

#require(semTools)
#sample = splitSample(D1, div=10)

#data explore
sample_index = sample(1:nrow(D1), floor(nrow(D1)/10), replace=FALSE)
D2 <- D1[sample_index,]
D3 <- D1[-sample_index,]

head(D2)
head(D1)

summary(D1$def_flag)
summary(D2$def_flag)

D2_num = subset(D2, select=c("score","CLTV","DTI",
                             "UPB","LTV","OIR","def_flag"))

D3_num = subset(D3, select=c("score","CLTV","DTI",
                             "UPB","LTV","OIR","def_flag"))
library(car)
scatterplot.matrix(~score+CLTV+DTI+UPB+LTV+OIR, data=D2_num[1:1000,],
                   main="Scatterplot Matrix",pch=".")

D2_def = D2[D2$def_flag == TRUE,]
scatterplot.matrix(~score+CLTV+DTI+UPB+LTV+OIR, data=D2_def,
                   main="Scatterplot Matrix_def",pch=".")
#LTV CLTV show collinearity
#different densoty plot between two classes can be seen on score DTI OIR

summary(D2_num)

#logistic regression
##remove NA
temp = D2_num[complete.cases(D2_num),]
summary(temp)

#build a test set
test = D3_num[complete.cases(D3_num),]

#data prepared

#using function glm
library(Deducer)
glm.fit = glm(def_flag ~ DTI + OIR, data=temp, family=binomial)
summary(glm.fit)
coef(glm.fit)
glm.probs=predict(glm.fit,type="response")
sum(glm.probs>0.5);sum(glm.probs>0.1)

library(ROCR)
pr <- prediction(glm.probs, temp$def_flag)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]];auc
plot(prf);lines(x = c(0,1), y = c(0,1))


#use all numeric variable
glm.fit2 = glm(def_flag ~ score+CLTV+DTI+UPB+LTV+OIR, data=temp, family=binomial)
summary(glm.fit2)
coef(glm.fit2)
glm.probs=predict(glm.fit2,type="response")
sum(glm.probs>0.5);sum(glm.probs>0.1)

pr <- prediction(glm.probs, temp$def_flag)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]];auc
plot(prf);lines(x = c(0,1), y = c(0,1))

#on test dataset
fitted.results <- predict(glm.fit2,newdata=test,
                          type='response')

threshold = function(x){
  fitted.outputs <- ifelse(fitted.results > x, 1, 0)
  misClasificError <- mean(fitted.outputs != test$def_flag,na.omit="TRUE")
  return (misClasificError)
 }

rate = lapply(seq(0.1,0.9,0.1),threshold)
unlist(rate)

as.numeric(summary(D1$def_flag)[3])/nrow(D1)

pr <- prediction(fitted.results, test$def_flag)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf);lines(x = c(0,1), y = c(0,1))

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc



#library(glmnet)
#summary(D2_num)
#y = as.factor(D2_num[,8])
#glmmod<-glmnet(x,y ,alpha=1,family='binomial')
x = as.matrix(temp[,-7])
y = as.factor(temp[,7])
fit<-glmnet(x, y ,family='binomial')
summary(fit)
print(fit)
plot(fit)
coef(fit)
p = predict(fit, newx = x, type = "response")

x = as.matrix(temp[,c(3,6)])
y = as.factor(temp[,7])
fit<-glmnet(x, y ,family='binomial')
p = predict(fit, newx = x, type = "class", s = 0.5)
summary(fit)
print(fit)
plot(fit)
coef(fit)

summary(D1$def_flag)
