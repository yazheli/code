load("~/Desktop/Freddie_Mac_data/save_model/prepared_data.RData")
library(caret)
library(rpart.plot)
library(rpart)
library(gbm)
library(ROCR)
library(mlr)
library(unbalanced)
library(randomForest)
library(doParallel) 
library(ggplot2)
library(Rmisc) 
library(parallel)

auc_calucator = function(fitted.results, test_data_y) {
  pr <- prediction(fitted.results, test_data_y)
  prf <- ROCR::performance(pr, measure = "tpr", x.measure = "fpr")
  auc <- ROCR::performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  return(c(auc,prf))
}

new_test = test[sample(1:nrow(test), size = nrow(test)/10),]
new_train = train[sample(1:nrow(train), size = nrow(train)/10),]

new_train$def_flag = as.factor(as.logical(new_train$def_flag))
new_test$def_flag = as.factor(as.logical(new_test$def_flag))

Y = new_train[,8]
Y = as.numeric(Y)
X = new_train[,-8]
Y[which(Y==1)] = 0
Y[which(Y==2)] = 1
Y = as.factor(Y)
a = ubTomek(X, Y, verbose = TRUE)
un_train_set = cbind(a[[1]],def_flag = a[[2]])
new_train = un_train_set

un_frac_sample = function (perc.under){
  Y = new_train$def_flag
  Y = as.numeric(Y)
  X = new_train[,-30]
  Y[which(Y==1)] = 0
  Y[which(Y==2)] = 1
  Y = as.factor(Y)
  under_sample = ubSMOTE(X, Y, perc.over =100, k = 5, perc.under = perc.under, verbose = TRUE)
  un_train_set = cbind(under_sample[[1]],def_flag = under_sample[[2]])
  un_train_set$def_flag = as.character(un_train_set$def_flag)
  un_train_set$def_flag[which(un_train_set$def_flag == 1)] = 'yes'
  un_train_set$def_flag[which(un_train_set$def_flag == 0)] = 'no'
  return(un_train_set)
}

un_auc_frac = function(perc.under){
  un_train_set = un_frac_sample(perc.under)
  gbmGrid <- expand.grid(interaction.depth=(3:6), n.trees=(1:40)*100, shrinkage=.01)
  gbmGrid$n.minobsinnode = rep(10,nrow(gbmGrid))
  bootControl = trainControl(method='cv',classProbs = TRUE,number=3,summaryFunction =       twoClassSummary)
  gmbFit<- caret::train(def_flag~., data = un_train_set, 
                        method = "gbm", 
                        verbose = F, 
                        trControl = bootControl, 
                        bag.fraction=0.5,
                        metric="ROC",
                        tuneGrid=gbmGrid)
  predict=predict (gmbFit ,newdata = new_test, type='prob')
  temp = auc_calucator(predict[,2],new_test$def_flag)
  auc = temp[[1]]
  return(auc)
}


no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type="FORK") 
clusterEvalQ(cl, library(caret))
clusterEvalQ(cl, library(rpart))
clusterEvalQ(cl, library(unbalanced))
clusterEvalQ(cl, library(randomForest))
clusterEvalQ(cl, library(ROCR))
clusterEvalQ(cl, library(gbm))
clusterEvalQ(cl, library(Rmisc))
clusterEvalQ(cl, library(rpart))

Y = new_train$def_flag
Y = as.numeric(Y)
X = new_train[,-30]
Y[which(Y==1)] = 0
Y[which(Y==2)] = 1
Y = as.factor(Y)
table(Y)

under_sample = ubSMOTE(X, Y, perc.over =200, k = 5, perc.under = 1000, verbose = TRUE)
table(Y)
table(under_sample[[2]])
bestSMOTE1000_BRT = parLapply(cl, rep(1000,10), un_auc_frac)

under_sample = ubSMOTE(X, Y, perc.over =200, k = 5, perc.under = 1500, verbose = TRUE)
table(Y)
table(under_sample[[2]])
bestSMOTE1500_BRT = parLapply(cl, rep(1500,10), un_auc_frac)

under_sample = ubSMOTE(X, Y, perc.over =200, k = 5, perc.under = 2000, verbose = TRUE)
table(Y)
table(under_sample[[2]])
bestSMOTE2000_BRT = parLapply(cl, rep(2000,10), un_auc_frac)

stopCluster(cl)

