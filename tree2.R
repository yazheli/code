load("~/Desktop/Freddie Mac data/prepared_data.RData")
train$def_flag = as.factor(as.logical(train$def_flag))
test$def_flag = as.factor(as.logical(test$def_flag))

library(caret)
library(rpart.plot)
library(rpart)

# random forest model
system.time(Mod1 <- train(def_flag ~ ., method = "parRF", 
                data = train, importance = T, ntree = 200))

save(Mod1,file="Mod1.RData")

load("Mod1.RData")
Mod1$finalModel
vi <- varImp(Mod1)
vi$importance[1:10,]

# out-of-sample errors of random forest model using validation dataset 
pred1 <- predict(Mod1, val)
cm1 <- confusionMatrix(pred1, val$classe)

#boosting

gbmGrid <- expand.grid(interaction.depth=(1:3)*2, n.trees=(1:10)*20, shrinkage=.1)
gbmGrid <- expand.grid(interaction.depth=(1:6), n.trees=(1:10)*20,
                       n.minobsinnode = (5:10),shrinkage=0.1)

bootControl <- trainControl(method = "cv", number = 3)
set.seed(2)
gmbFit<- train(def_flag ~ ., 
               method = "gbm", 
               data = train, 
               verbose = F, 
               trControl = bootControl, 
               bag.fraction=0.5,
               tuneGrid=gbmGrid)

save(gmbFit,file="gmbFit.RData")

load("gmbFit.RData")
plot(gmbFit)
plot(gmbFit,plotType = "level")
resampleHist((gmbFit))
gmbFit$bestTune

y.predict=predict (gmbFit ,newdata = test, type='prob')
head(y.predict)
temp = auc_calucator(y.predict[,2],test$def_flag)
auc = temp[[1]];auc
plot(temp[[2]]);lines(x = c(0,1), y = c(0,1))
