#should I center my predictor
library(glmnet)
names(train)
x = train[,-9]
x = as.matrix(x)
y = train[,9]

cvfit = cv.glmnet(x, y, family='binomial',type.measure = "auc")
plot(cvfit)
names(cvfit)

cvfit$lambda.min
coef(cvfit, s = "lambda.min")

pre = predict(cvfit, newx = as.matrix(test[,-9]), s = "lambda.min", type = "class")

fitted.results = predict(cvfit, newx = as.matrix(test[,-9]), s = "lambda.min", 
                         type = "response")
pr <- prediction(fitted.results, test$def_flag)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]];auc
plot(prf);lines(x = c(0,1), y = c(0,1))

#should I center my predictor
new_numerical <- data.frame(lapply(numerical, function(x) scale(x)))

center_data = cbind(new_numerical,dummy_factor_data)
head(center_data)
sample_index = sample(1:nrow(center_data), floor(nrow(center_data)/10), replace=FALSE)
c_train <- center_data[sample_index,]
c_test <- center_data[-sample_index,]
x = c_train[,-9]
x = as.matrix(x)
y = c_train[,9]
cvfit = cv.glmnet(x, y, family='binomial',type.measure = "auc")
pre = predict(cvfit, newx = as.matrix(c_test[,-9]), s = "lambda.min", type = "class")

fitted.results = predict(cvfit, newx = as.matrix(c_test[,-9]), s = "lambda.min", 
                         type = "response")
pr <- prediction(fitted.results, test$def_flag)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]];auc
