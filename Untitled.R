library(ROCR)
library(Deducer)
library(grid)
library(devtools)
install_github("easyGgplot2", "kassambara")



#############
load("~/Desktop/Freddie Mac data/USMortgages2008_2009.rdata")
names(D1)
head(D1)

##simply remove NA first
#delete column seqno, first.pay.data, MSA, maturity.data,
#product.type, property.state, postal.code, loan_age
newD = D1[,-c(1,3,6,5,17,18,20,26)]

#most deleted case are caused by missing value from DTI
na_count <- sapply(newD, function(x) sum(is.na(x))); na_count

D1_removeNA = newD[complete.cases(newD),]
names(D1_removeNA)

#factor variable
#first.time.homebuyer,insurance,number.units,occupance.status,channle,PPM
#product.type,property.state,property.type,loan.purpose,orig.loan.term
#seller,servicer,loan_age 

D1_removeNA$insurance[which(D1_removeNA$insurance == 0)] = '0'
D1_removeNA$insurance[which(D1_removeNA$insurance != 0)] = '1'

#numeric variable
#score, CLTV, DTI, UPB, LTV, OIR, PPM, orig.loan.term, number.borrowers

factor_data = subset(D1_removeNA, select=c("first.time.homebuyer","insurance",
                                           "number.units","occupancy.status",
                                           "channel","PPM","property.type",
                                           "loan.purpose", "seller",
                                           "servicer","def_flag"))

element = sapply(factor_data,unique);element
count_factor = sapply(factor_data, 
                      function(x) {table(x,exclude = NULL)});count_factor


#weight of evidence
woe.tab <- function(x,y) {
  n1 <- sum(y) 
  n0 <- sum(1-y) 
  nx0n1 <- tapply(1-y,x,sum)*n1 
  nx1n0 <- tapply(y,x,sum) *n0
  nx0n1[which(nx0n1==0)]<-n1 
  nx1n0[which(nx1n0==0)]<-n0
  log(nx0n1)-log(nx1n0) 
}

woe.assign <- function(woetab, x) {
  w<-rep(0,length(x))
  ni<-names(woetab)
  for (i in 1:length(ni)) {
    w[which(x==ni[i])]<-woetab[i]
  }
  w
}

woe_seller = woe.assign(woe.tab(D1_removeNA$seller,D1_removeNA$def_flag),
                        D1_removeNA$seller)

woe_servicer = woe.assign(woe.tab(D1_removeNA$servicer,D1_removeNA$def_flag),
                        D1_removeNA$servicer)

numerical = subset(D1_removeNA, select=c("score", "CLTV", "DTI", "UPB", "LTV",
                                        "OIR", "orig.loan.term", 
                                        "number.borrowers","def_flag"))
numerical$seller = woe_seller
numerical$servicer = woe_servicer

#some plot for numerical data
library(car)
library(ggplot2)
scatterplot.matrix(~score+CLTV+DTI+UPB+LTV+OIR, data=numerical[1:2000,],
                   main="Scatterplot Matrix",pch=".")
p = list()
p[[1]] = ggplot(aes(y = score, x = def_flag), data = numerical) + geom_boxplot()
p[[2]] = ggplot(aes(y = CLTV, x = def_flag), data = numerical) + geom_boxplot()
p[[3]] = ggplot(aes(y = DTI, x = def_flag), data = numerical) + geom_boxplot()
p[[4]] = ggplot(aes(y = UPB, x = def_flag), data = numerical) + geom_boxplot()
p[[5]] = ggplot(aes(y = OIR, x = def_flag), data = numerical) + geom_boxplot()

library(easyGgplot2)
ggplot2.multiplot(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]], cols=5)

p = ggplot(numerical[1:100000,])

p + geom_point(aes(x = score, y = OIR, color = factor(def_flag)),size=0.5)
p + geom_point(aes(x = seller, y = servicer, color = factor(def_flag)),size=0.5)

library(dplyr)

a = numerical[which(numerical$def_flag == 1),] %>% group_by(seller,servicer) %>%
  summarize(Count = n())

b = numerical[which(numerical$def_flag == 0),] %>% group_by(seller,servicer) %>%
  summarize(Count = n)

p = list()
a = as.data.frame(a)
names(a)=c('seller','servicer','count')
a$count = (a$count)/sum(a$count)
p[[1]] <- ggplot(a, aes(seller, servicer)) + geom_point(aes(size = count))


b = as.data.frame(b)
names(b)=c('seller','servicer','count')
b$count = (b$count)/sum(b$count)
p[[2]] <- ggplot(b, aes(seller, servicer)) + geom_point(aes(size = count))


library(easyGgplot2)
ggplot2.multiplot(p[[1]],p[[2]], cols=2)

####
#dummy code categorical variables

relevel_order = function(x){
  tb <- table(x)
  relevel_x <- factor(x,levels = names(tb[order(tb, decreasing = TRUE)]))
  return (relevel_x)
}

temp = factor_data[,1:8]
name = names(temp)

for (i in 1:8){
  assign(name[i],factor(temp[,i]))
}

first.time.homebuyer = relevel_order(first.time.homebuyer)
dummies1 = model.matrix(~first.time.homebuyer)

insurance = relevel_order(insurance)
dummies2 = model.matrix(~insurance)

number.units = relevel_order(number.units)
dummies3 = model.matrix(~number.units)

occupancy.status = relevel_order(occupancy.status)
dummies4 = model.matrix(~occupancy.status)

channel = relevel_order(channel)
dummies5 = model.matrix(~channel)

PPM = relevel_order(PPM)
dummies6 = model.matrix(~PPM)

property.type = relevel_order(property.type)
dummies7 = model.matrix(~property.type)

loan.purpose = relevel_order(loan.purpose)
dummies8 = model.matrix(~loan.purpose)
####

dummy_factor_data = cbind(dummies1[,-1],dummies2[,-1],dummies3[,-1],dummies4[,-1],
                          dummies5[,-1],dummies6[,-1],dummies7[,-1],dummies8[,-1])
head(dummy_factor_data)

data = cbind(numerical,dummy_factor_data)
head(data)

sample_index = sample(1:nrow(data), floor(nrow(data)/10), replace=FALSE)
train <- data[sample_index,]
test <- data[-sample_index,]

#glm
glm.fit = glm(def_flag ~ . , data=train, family=binomial)
summary(glm.fit)
coef(glm.fit)
glm.probs=predict(glm.fit,type="response")
sum(glm.probs>0.5);sum(glm.probs>0.1)

pr <- prediction(glm.probs, train$def_flag)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]];auc
plot(prf);lines(x = c(0,1), y = c(0,1))

fitted.results <- predict(glm.fit,newdata=test,
                          type='response')
fitted.outputs <- ifelse(fitted.results > 0.5, 1, 0)
misClasificError <- mean(fitted.outputs != test$def_flag,na.omit="TRUE")
misClasificError

threshold = function(x){
  fitted.outputs <- ifelse(fitted.results > x, 1, 0)
  misClasificError <- mean(fitted.outputs != test$def_flag,na.omit="TRUE")
  return (misClasificError)
}

rate = lapply(seq(0.1,0.9,0.1),threshold)
unlist(rate)

fitted.outputs <- ifelse(fitted.results > 0.5, 1, 0)
table(fitted.outputs,test$def_flag)

fitted.outputs <- ifelse(fitted.results > 0.1, 1, 0)
table(fitted.outputs,test$def_flag)

as.numeric(summary(D1$def_flag)[3])/nrow(D1)

pr <- prediction(fitted.results, test$def_flag)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf);lines(x = c(0,1), y = c(0,1))

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc