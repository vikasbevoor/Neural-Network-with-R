concrete <- read.csv("D:/Data science videos/R Codes/Assignments docs/Neural Networks/concrete (4).csv")
View(concrete)
attach(concrete)

install.packages("moments")
library(moments)
library(caTools)
library(caret)

# Data exploration
summary(concrete)
str(concrete)

# Graphical exploration
hist(strength)
summary(strength)
skewness(strength)
kurtosis(strength)
boxplot(strength)

hist(cement)
summary(cement)
skewness(cement)
kurtosis(cement)
boxplot(cement)

hist(water)
summary(water)
skewness(water)
kurtosis(water)
boxplot(water)


# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}


# apply normalization to entire data frame
conc_norm <- as.data.frame(lapply(concrete[-9], normalize))
View(conc_norm)

conc_norm <- cbind.data.frame(conc_norm, strength)
conc_norm <- conc_norm[-10]

# create training and test data
conc_train <- conc_norm[1:750, ]
conc_test <- conc_norm[751:1030, ]

## Training a model on the data 
install.packages("neuralnet")
library(neuralnet)
colnames(conc_norm)

# simple ANN with only a single hidden neuron
conc_model <- neuralnet(formula = strength ~ ., data = conc_train, hidden = 1)

# visualize the network topology
plot(conc_model)

## Evaluating model performance 
results_model <- compute(conc_model, conc_test[1:8])
str(results_model)

predicted_profit <- results_model$net.result
head(predicted_profit)

# examine the correlation between predicted and actual values
cor(predicted_profit, conc_test$Profit)


# Improving model performance ----
conc_model2 <- neuralnet(formula = strength ~ ., data = conc_train, hidden = 10, stepmax = 1e+06, threshold = 0.1 )

# plot the network
plot(conc_model2)

# evaluate the results as weconc did before
model_results2 <- compute(conc_model2, conc_test[1:8])
predicted_strength2 <- model_results2$net.result

cor(predicted_strength2, conc_test$Profit)


# More complex neural network topology with 10 hidden neurons
start_model3 <- neuralnet(formula = Profit ~ R.D.Spend+Administration+Marketing.Spend, data = start_train, hidden = 10)

# plot the network
plot(start_model3)

# evaluate the results as we did before
model_results3 <- compute(start_model3, start_test[1:3])
predicted_strength3 <- model_results3$net.result
cor(predicted_strength3, start_test$Profit)


# More complex neural network topology with 5 hidden neurons
start_model4 <- neuralnet(formula = strength ~ ., data = conc_train, hidden = 5, stepmax = 1e+06, threshold = 0.2)

# plot the network
plot(start_model4)

# evaluate the results as we did before
model_results4 <- compute(start_model4, conc_test[1:8])
predicted_strength4 <- model_results4$net.result
cor(predicted_strength4, conc_test$strength)
