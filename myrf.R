load("~/Desktop/Freddie_Mac_data/save_model/prepared_data.RData")
library(rpart)
library(parallel)

new_test = test[sample(1:nrow(test), size = nrow(test)/10),]
new_train = train[sample(1:nrow(train), size = nrow(train)/10),]
Y = as.data.frame(new_train[,8])
X = as.data.frame(new_train[,-8])
names(Y) = "def_flag"


boost_sample = function(predict.var, response.var){
  data = cbind(predict.var, response.var)
  boost_sample_index = sample(1:nrow(data), size = nrow(data), replace = TRUE)
  boost_sample = data[boost_sample_index,]
  return(boost_sample)
}

sample = boost_sample(Y,X)

temp = boost_sample(predict.var=X, response.var=Y)
mtry = sample(1:ncol(X), size = 10, replace = FALSE)
formula = as.formula(paste(paste(names(Y),"~"),
                           paste(names(X)[mtry],collapse="+")))
tree = rpart(formula = formula, data = temp, method = "class");tree
unique(predict(tree,type = "prob")[,2])

single_tree = function(predict.var, response.var, mtry){
  temp = boost_sample(predict.var, response.var)
  mtry = sample(1:ncol(predict.var), size = mtry, replace = FALSE)
  formula = as.formula(paste(paste(names(response.var),"~"),
                             paste(names(predict.var)[mtry],collapse="+")))
  tree = rpart(formula = formula, data = temp, method = "class")
  return(tree)
}

myRF = function(predict.var, response.var, mtry, ntree){
  tree_list = replicate(ntree, single_tree(predict.var, response.var, mtry),
                        simplify=FALSE )
  return(tree_list)
}
  
rf_tree = myRF(predict.var=X, response.var=Y, mtry=10, ntree=2)  



randomForest(Species~.,data=iris,sampsize=c(setosa=10,versicolor=20,virginica=30), strata=iris$Species)


cl <- makeCluster(detectCores()-1)  

clusterEvalQ(cl,library(rpart))
clusterExport(cl, "single_tree")
clusterExport(cl, "boost_sample")
clusterExport(cl,c("X","Y"))

l= parSapply(cl, 1:1000, function(i,...) {single_tree(predict.var=X, response.var=Y, mtry=5)},simplify = FALSE)

stopCluster(cl)

result = as.data.frame(matrix(0, ncol=1000, nrow = nrow(X)))

for(i in 1:1000){
  result[,i] = predict(l[[i]],type="prob")[,2]
}
unique(result)

re = rowMeans(result)


rf = randomForest::randomForest(def_flag ~ ., data=new_train, mtry = 5, ntree=1000)
max(predict(rf))
