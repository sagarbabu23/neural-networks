install.packages("data.table")
install.packages("xlsx")
install.packages("car")
library(data.table)
library(xlsx)
library(car)
startup_50 <- read.csv(choose.files())
View(startup_50)
summary(startup_50)
var(startup_50$`R.D.Spend`)
var(startup_50$Administration)
var(startup_50$`Marketing.Spend`)
var(startup_50$Profit)
sd(startup_50$`R.D.Spend`)
sd(startup_50$Administration)
sd(startup_50$`Marketing Spend`)
sd(startup_50$Profit)
Newyork <- ifelse(startup_50$State=="New York",1,0)
California <- ifelse(startup_50$State=="California",1,0)
Florida <- ifelse(startup_50$State=="Florida",1,0)
startup_50 <- cbind(startup_50,Newyork,California,Florida)

colnames(startup_50)
View(startup_50)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
startup_norm <- as.data.frame(lapply(startup_50[,-(4)],FUN=normalize))
summary(startup_50$Profit)
View(startup_norm)


##startup_norm <- cbind(startup_norm,startup_50$Profit)
##colnames(startup_norm)[5] <- "Profit"
startup_train<-startup_norm[1:40,]
startup_test<-startup_norm[41:50,]

# Using multilayered feed forward nueral network
# package nueralnet
install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)  # regression
library(nnet) # classification 

# Building model
##formula_nn <- paste("startup_50$Profit",paste(colnames(startup_50[-5]),collapse ="+"))
startup_model <- neuralnet(Profit ~ R.D.Spend+Administration+Marketing.Spend+Florida+Newyork+California,data = startup_train, hidden = 10)
##startup_model <- neuralnet(formula = formula_nn,data = startup_train)

str(startup_model)
plot(startup_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
set.seed(1232)
model_results <- compute(startup_model,startup_test[,-4])
str(model_results)
View(model_results)
predicted_profit <- model_results$net.result
# predicted_profit
# model_results$neurons
cor(predicted_profit,startup_test$Profit)
plot(predicted_profit,startup_test$Profit)
model_5<-neuralnet(Profit ~ R.D.Spend+Administration+Marketing.Spend+Florida+Newyork+California,data= startup_norm,hidden = 5)
plot(model_5)
model_5_res<-compute(model_5,startup_test[,-4])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,startup_test$Profit)
plot(pred_strn_5,startup_test$Profit)


#######################computer data set#########


Computer_Data <- read.csv(choose.files())
colnames(Computer_Data)
str(Computer_Data)
View(Computer_Data)
Computer_Data$cd_dummy1 <- ifelse(Computer_Data$cd=="yes",1,0)
Computer_Data$multi_dummy1 <- ifelse(Computer_Data$multi=='yes',1,0)
Computer_Data$premium_dummy1 <- ifelse(Computer_Data$premium=='yes',1,0)

comp_data <- Computer_Data[,-(7:9)]
comp_data <- comp_data[,-1]
View(comp_data)

str(comp_data)

attach(comp_data)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
comp_data_norm <- as.data.frame(lapply(comp_data[,],FUN=normalize))
summary(comp_data$price)
View(comp_data_norm)

comp_train <- comp_data_norm[1:5000,]
comp_test <- comp_data_norm[5001:6259,]


library(neuralnet)  # regression
library(nnet) # classification 


comp_model <- neuralnet(price ~ speed+hd+ram+screen+ads+trend+cd_dummy1+multi_dummy1+premium_dummy1,data = comp_train, hidden = 10)

str(comp_model)
plot(comp_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
set.seed(12322)
comp_results <- compute(comp_model,comp_test[,])
str(comp_results)
View(comp_results)
predicted_profit <- comp_results$net.result
# predicted_profit
# model_results$neurons
cor(predicted_profit,comp_test$Profit)
plot(predicted_profit,comp_test$Profit)
model_5<-neuralnet(Profit ~ R.D.Spend+Administration+Marketing.Spend+Florida+Newyork+California,data= comp_norm,hidden = 5)
plot(model_5)
model_5_res<-compute(model_5,comp_test[,-4])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,comp_test$Profit)
plot(pred_strn_5,comp_test$Profit)


############### totyota corolla data set#####

ToyotaCorolla <- read.csv(choose.files())
Corolla<- ToyotaCorolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(Corolla)
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
corolla_norm <- as.data.frame(lapply(Corolla[,],FUN=normalize))
summary(Corolla$Price)
View(corolla_norm)


corolla_train<-corolla_norm[1:1140,]
corolla_test<-corolla_norm[1141:1436,]

# Using multilayered feed forward nueral network
# package nueralnet
install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)  # regression
library(nnet) # classification 

# Building model

corolla_model <- neuralnet(Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = corolla_train)
View(corolla_train)

str(corolla_model)
plot(corolla_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
set.seed(12321)
model_results <- compute(corolla_model,corolla_test[,-1])
str(model_results)
View(model_results)
predicted_profit <- model_results$net.result
# predicted_profit
# model_results$neurons
cor(predicted_profit,corolla_test$Profit)
plot(predicted_profit,corolla_test$Profit)
model_5<-neuralnet(Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data= corolla_norm,hidden = 5)
plot(model_5)
model_5_res<-compute(model_5,corolla_test[,-1])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,corolla_test$Profit)

