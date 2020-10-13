Startups <- read.csv("D:/Data science videos/R Codes/Assignments docs/Neural Networks/50_Startups (3).csv")
View(Startups)
attach(Startups)

install.packages("moments")
library(moments)
library(caTools)
library(caret)

# Data exploration
summary(Startups)
str(Startups)

# Graphical exploration
hist(Profit)
summary(Profit)
skewness(Profit)
kurtosis(Profit)
boxplot(Profit)

hist(R.D.Spend)
summary(R.D.Spend)
skewness(R.D.Spend)
kurtosis(R.D.Spend)
boxplot(R.D.Spend)

hist(Administration)
summary(Administration)
skewness(Administration)
kurtosis(Administration)
boxplot(Administration)

hist(Marketing.Spend)
summary(Marketing.Spend)
skewness(Marketing.Spend)
kurtosis(Marketing.Spend)
boxplot(Marketing.Spend)


# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}


# apply normalization to entire data frame
start_norm <- as.data.frame(lapply(Startups[-4], normalize))
View(start_norm)


# create training and test data
sample = sample.split(start_norm,SplitRatio = 0.75)
start_train <- subset(start_norm,sample ==TRUE)
start_test <- subset(start_norm, sample==FALSE)


## Training a model on the data 
install.packages("neuralnet")
library(neuralnet)
colnames(start_norm)

# simple ANN with only a single hidden neuron
start_model <- neuralnet(formula = Profit ~ R.D.Spend+Administration+Marketing.Spend, data = start_train, hidden = 1)

# visualize the network topology
plot(start_model)

## Evaluating model performance 
start_test[1:3]
results_model <- compute(start_model, start_test[1:3])
str(results_model)


predicted_profit <- results_model$net.result
predicted_profit

# examine the correlation between predicted and actual values
cor(predicted_profit, start_test$Profit)


# Improving model performance ----
# More complex neural network topology with 3 hidden neurons
start_model2 <- neuralnet(formula = Profit ~ R.D.Spend+Administration+Marketing.Spend, data = start_train, hidden = 3)


# plot the network
plot(start_model2)

# evaluate the results as we did before
model_results2 <- compute(start_model2, start_test[1:3])
predicted_strength2 <- model_results2$net.result

cor(predicted_strength2, start_test$Profit)


# More complex neural network topology with 10 hidden neurons
start_model3 <- neuralnet(formula = Profit ~ R.D.Spend+Administration+Marketing.Spend, data = start_train, hidden = 10)

# plot the network
plot(start_model3)

# evaluate the results as we did before
model_results3 <- compute(start_model3, start_test[1:3])
predicted_strength3 <- model_results3$net.result
cor(predicted_strength3, start_test$Profit)


# More complex neural network topology with 5 hidden neurons
start_model4 <- neuralnet(formula = Profit ~ R.D.Spend+Administration+Marketing.Spend, data = start_train, hidden = 5)

# plot the network
plot(start_model4)

# evaluate the results as we did before
model_results4 <- compute(start_model4, start_test[1:3])
predicted_strength4 <- model_results4$net.result
cor(predicted_strength4, start_test$Profit)
