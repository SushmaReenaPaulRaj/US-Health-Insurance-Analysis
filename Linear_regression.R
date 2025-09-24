## data pre-processing
# importing data
setwd("C:/Users/Welcome/Downloads")
insurance<-read.csv('insurance.csv')
View(insurance)

# to know the structure and summary
str(insurance)
summary(insurance)

## linear regressing model
mod_1<-lm(charges ~ age + sex + bmi + children + smoker + region, data=insurance)
summary(mod_1)
mod_2<-lm(charges ~ age + bmi + children + smoker + region, data=insurance)
summary(mod_2)
mod_3<-lm(charges ~ age + bmi + children + smoker ,data=insurance)
summary(mod_3)
mod_4<-lm(charges ~ bmi + children + smoker ,data=insurance)
summary(mod_4)
mod_5<-lm(charges ~ bmi + age + smoker  ,data=insurance)
summary(mod_5)

# training and testing
n_train <- round(0.8 * nrow(insurance))
n_train
train_indices <- sample(1:nrow(insurance), n_train)
train_indices
Data_train <-insurance[train_indices, ]
Data_train
Data_test <- insurance[-train_indices, ]
Data_test

# predicted values
r_sq <- summary(mod_2)$r.squared
r_sq
prediction<- predict(mod_2, newdata = Data_test)
prediction
residuals <- Data_test$charges - prediction
residuals
rmse <- sqrt(mean(residuals^2))
rmse

Data_test$prediction <- predict(mod_2, newdata = Data_test)
Data_test$prediction


## model performance
library(ggplot2)
ggplot(Data_test, aes(x = prediction, y = charges)) + 
  geom_point(color = "violet", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Prediction vs. Real values")

# checking whether residual 
Data_test$residuals <- Data_test$charges - Data_test$prediction
Data_test$residuals

ggplot(data = Data_test, aes(x = prediction, y = residuals)) +
  geom_pointrange(aes(ymin = 0, ymax = residuals), color = "black", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = 3, color = "red") +
  ggtitle("Residuals vs. Linear model prediction")

ggplot(Data_test, aes(x = residuals)) + 
  geom_histogram(bins = 15, fill = "red") +
  geom_vline(xintercept = 0, linetype = 5, color = "black")+
  ggtitle("Histogram of residuals")


