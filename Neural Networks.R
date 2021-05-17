##### Gas turbines #####

gas_turbine <- read.csv(file.choose())
str(gas_turbine)
summary(gas_turbine)
attach(gas_turbine)


# Normalize

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
gt_norm <- as.data.frame(lapply(gas_turbine,FUN=normalize))

#gt_norm <- cbind(gt_norm,gas_turbine$TEY)
#colnames(gt_norm)[11] <- "TEY"

# Splitting

gt_train <- gt_norm[1:12000,]
gt_test  <- gt_norm[12001:15039,]

# Model Building

library(neuralnet)

gt_model <- neuralnet(TEY~AT+AP+AH+AFDP+GTEP+TIT+TAT+CDP+CO+NOX,data = gt_train,stepmax = 1e6)
str(gt_model)
plot(gt_model)

model_results <- compute(gt_model,gt_test[,-8])
predicted_TEY <- model_results$net.result
cor(predicted_TEY,gt_test$TEY)

gt_model_1 <- neuralnet(TEY~AT+AP+AH+AFDP+GTEP+TIT+TAT+CDP+CO+NOX,data = gt_train,hidden = c(5,3),stepmax = 1e6)
plot(gt_model_1)

model1_results <- compute(gt_model_1,gt_test[,-8])
predicted_TEY1 <- model1_results$net.result
cor(predicted_TEY1,gt_test$TEY)



##### Fire Forests #####

fire_forest <- read.csv(file.choose())
ff <- fire_forest[,-c(1:2)]
summary(ff)
str(ff)

# Normalize

ff_norm <- as.data.frame(lapply(ff[,c(1:9)],FUN=normalize))
ff_norm <- cbind(ff_norm,ff[,c(10:29)])

# Splitting

library(caTools)

split <- sample.split(ff_norm, SplitRatio = 0.7)
ff_train <- subset(ff_norm, split ==TRUE)
ff_test  <- subset(ff_norm, split == FALSE)
table(ff_train$size_category)
table(ff_test$size_category)

attach(ff_train)
attach(ff_test)

# Model building

ff_model <- nnet(ff_train$size_category~.,data = ff_train,size=5,decay = 0.0001,maxit=1000)
summary(ff_model)

predict_ff <- predict(ff_model,ff_test[,-29],type = "class")
mean(predict_ff==ff_test$size_category)
table1 <- table(predict_ff,ff_test$size_category)

#install.packages("devtools")
#install.packages("NeuralNetTools")
library(NeuralNetTools)
#library(devtools)
#library(neuralnet)

plotnet(ff_model)
