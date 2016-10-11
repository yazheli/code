load("~/Desktop/Freddie Mac data/USMortgages2008_2009.rdata")
#build the model based on CA data first

D_state = D1[,-c(1,3,6,5,17,20,26)]
D_state = D_state[complete.cases(D_state),]
state = D_state$property.state

ca = data[(state=="CA"),]
tx = data[(state=="TX"),]
except_ca = data[(state!="CA"),]
 

x = ca[,-9]
x = as.matrix(x)
y = ca[,9]
cvfit = cv.glmnet(x, y, family='binomial',type.measure = "auc")
coef(cvfit, s = "lambda.min")

auc_calucator = function(model, test_data_x, test_data_y) {
  fitted.results = predict(model, newx = test_data_x, s = "lambda.min", 
                           type = "response")
  pr <- prediction(fitted.results, test_data_y)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  return(auc)
}

auc_calucator(cvfit, x, y)
auc_calucator(cvfit, as.matrix(tx[,-9]), tx[,9])
auc_calucator(cvfit, as.matrix(except_ca[,-9]), except_ca[,9])
 
