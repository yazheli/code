load("~/Desktop/Freddie_Mac_data/save_model/prepared_data.RData")
library(rpart)
library(randomForest)
library(parallel)

train$def_flag = as.factor(as.logical(train$def_flag))
test$def_flag = as.factor(as.logical(test$def_flag))
new_test = test[sample(1:nrow(test), size = nrow(test)/10),]
new_train = train[sample(1:nrow(train), size = nrow(train)/10),]
Y = as.data.frame(new_train[,8])
X = as.data.frame(new_train[,-8])
names(Y) = "def_flag"


boost_sample = function(data){
  boost_sample_index = sample(1:nrow(data), size = nrow(data), replace = TRUE)
  boost_sample = data[boost_sample_index,]
  return(boost_sample)
}

minorityclass = new_train[new_train$def_flag == TRUE,]
majorityclass = new_train[new_train$def_flag == FALSE,]
boost_minority = boost_sample(minorityclass)

dataset = rbind(boost_minority,majorityclass)

single_tree = randomForest(def_flag~., data=dataset, 
                           sampsize=c(nrow(minorityclass), nrow(minorityclass)), 
                           strata=dataset$def_flag ,replace = FALSE, 
                           ntree = 1, mtry = 5)

cl <- makeCluster(detectCores()-1)  

clusterEvalQ(cl,library(rpart))
clusterEvalQ(cl,library(randomForest))
clusterExport(cl, "single_tree")
clusterExport(cl, "boost_sample")
clusterExport(cl,c("X","Y","dataset","minorityclass"))

l= parSapply(cl, 1:10, 
             function(i,...) {
               single_tree = randomForest(def_flag~., data=dataset, 
                             sampsize=c(nrow(minorityclass), nrow(minorityclass)), 
                             strata=dataset$def_flag ,replace = FALSE, 
                             ntree = 1, mtry = 5)
               return (single_tree)
             } 
             ,simplify = FALSE)

stopCluster(cl)




predict_train = as.data.frame(predict(l[[1]],newdata = new_train, type="response"))

for (i in 2:10){
  predict_train=
    cbind(predict_train,as.data.frame(predict(l[[i]],newdata = new_train, type="response")))
}

for(i in 1:10){
  predict_train[,i] = as.numeric(predict_train[,i])-1
}



pro_train = rowSums(predict_train)/10

predict_test = predict(single_tree, newdata = new_test, type="response")


