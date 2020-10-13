forest <- read.csv("D:/Data science videos/R Codes/Assignments docs/Neural Networks/forestfires (2).csv")

# Data exploration
summary(forest)
str(forest)

library(caret)
library(caTools)

View(forest)
forest <- forest[-c(1:2)]
attach(forest)
str(forest$size_category)

size <- ifelse(forest$size_category == "small", 1 ,2 )
View(size)
forest <- cbind(forest[-29], size)



# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
forest_norm <- as.data.frame(lapply(forest[-29], normalize))
View(forest_norm)

forest_norm <- cbind.data.frame(forest_norm, size)

# create training and test data
forest_train <- forest_norm[1:400, ]
forest_test <- forest_norm[401:517, ]

## Training a model on the data 
install.packages("neuralnet")
library(neuralnet)

# simple ANN with only a single hidden neuron
forest_model <- neuralnet(formula = size ~ ., data = forest_train, hidden = 1)

# visualize the network topology
plot(forest_model)

## Evaluating model performance 
results_model <- compute(forest_model, forest_test[1:28])

# obtain predicted strength values
str(results_model)
predicted_size <- results_model$net.result
predicted_size

# examine the correlation between predicted and actual values
cor(predicted_size, forest_test$size)

# Improving model performance ----
forest_model2 <- neuralnet(formula = size ~ ., data = forest_train, hidden = 3)

# plot the network
plot(forest_model2)

# evaluate the results as we did before
model_results2 <- compute(forest_model2, forest_test[1:28])
predicted_size2 <- model_results2$net.result
cor(predicted_size2, forest_test$size)


# Improving model performance 
forest_model3 <- neuralnet(formula = size ~ ., data = forest_train, hidden = 5)

# plot the network
plot(forest_model3)

# evaluate the results as we did before
model_results3 <- compute(forest_model3, forest_test[1:28])
predicted_size3 <- model_results3$net.result
cor(predicted_size3, forest_test$size)


# Improving model performance 
forest_model4 <- neuralnet(formula = size ~ ., data = forest_train, hidden = 10)

# plot the network
plot(forest_model4)

# evaluate the results as we did before
model_results4 <- compute(forest_model4, forest_test[1:28])
predicted_size4 <- model_results4$net.result
cor(predicted_size4, forest_test$size)
