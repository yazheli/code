auc_calucator = function(fitted.results, test_data_y) {
  pr <- prediction(fitted.results, test_data_y)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  return(c(auc,prf))
}

########################
load("~/Desktop/Freddie Mac data/prepared_data.RData")
train$def_flag = as.factor(as.logical(train$def_flag))
test$def_flag = as.factor(as.logical(test$def_flag))


library("tree")
tree.model = tree(def_flag~.,train,split = c("deviance"))
plot(tree.model)
text(tree.model ,pretty =0)

test_result = predict(tree.model,test)
result = test_result[,2]
class_result = ifelse(result>0.1, "TRUE", "FALSE")
table(predict=class_result,real=test$def_flag)

library(ROCR)
pr <- prediction(test_result[,2], test$def_flag)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]];auc
plot(prf);lines(x = c(0,1), y = c(0,1))


library("rpart")
library("rpart.plot")
p = sum(train$def_flag==TRUE)/nrow(train)
tree_model = rpart(def_flag~., data = train, method = "class", 
                   parms = list(split = "gini",prior = c(0.93,0.07)),#??
                   control = rpart.control(minsplit = 2))

summary(tree_model)
plot(tree_model)
text(tree_model ,pretty =0)
rpart.plot(tree_model)

test_result = predict(tree_model,test,type = "prob")
result = test_result[,2]
class_result = ifelse(result>0.1, "TRUE", "FALSE")
table(predict=class_result,real=test$def_flag)

temp = auc_calucator(test_result[,2],test$def_flag)
auc = temp[[1]];auc
plot(temp[[2]]);lines(x = c(0,1), y = c(0,1))


#bagging
library(randomForest)
train = as.data.frame(train)

bag_model =randomForest(def_flag~., data=train, mtry=(ncol(train)-1), 
                         ntree = 200,do.trace=TRUE,importancer=TRUE)

save(bag_model,file="bagmodel.RData")
importance (bag_model)
varImpPlot (bag_model)
oob = predict(bag_model,type="prob")

pred1 = ifelse(oob[,2]>0.2, "TRUE", "FALSE")
cm1 <- confusionMatrix(pred1, train$def_flag);cm1

plot(bag_model)

temp = auc_calucator(oob[,2],train$def_flag)
auc = temp[[1]];auc
plot(temp[[2]]);lines(x = c(0,1), y = c(0,1))

test.pred = predict(bag_model, test,type="prob")
head(test.pred)
result = test.pred[,2]
result = ifelse(result>0.2, "TRUE", "FALSE")
table(result,test=test$def_flag)

temp = auc_calucator(test.pred[,2],test$def_flag)
auc = temp[[1]];auc
plot(temp[[2]]);lines(x = c(0,1), y = c(0,1))

#randomForest
rf_model =randomForest(def_flag~., data=train, 
                       ntree =200, do.trace=TRUE,importancer=TRUE)

save(rf_model,file="rfmodel.RData")

importance (rf_model)
varImpPlot (rf_model)

oob = predict(rf_model,type="prob")
temp = auc_calucator(oob[,2],train$def_flag)
auc = temp[[1]];auc
plot(temp[[2]]);lines(x = c(0,1), y = c(0,1))

test.pred = predict(rf_model, test,type="prob")
head(test.pred)
result = test.pred[,2]
result = ifelse(result>0.2, "TRUE", "FALSE")
table(result,test=test$def_flag)

temp = auc_calucator(test.pred[,2],test$def_flag)
auc = temp[[1]];auc
plot(temp[[2]]);lines(x = c(0,1), y = c(0,1))

#boosting
library(gbm)
load("~/Desktop/Freddie Mac data/prepared_data.RData")
boost_model =gbm(def_flag~., data=train, distribution=
                    "bernoulli",n.trees =200 , interaction.depth =2)
summary (boost_model)
y.predict=predict (boost_model ,newdata =test, n.trees = 200,type='response')

temp = auc_calucator(y.predict,test$def_flag)
auc = temp[[1]];auc
plot(temp[[2]]);lines(x = c(0,1), y = c(0,1))
#boosting not better than random forest?


