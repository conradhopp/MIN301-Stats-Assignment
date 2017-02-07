dataset <- read.csv("gold.csv", header = T)
dataset[1] <- NULL #Clear column with hole id values

#encoding rocktype
dataset$ROCKTYPE <- as.numeric(factor(dataset$ROCKTYPE,
                           levels = c(2170,2270,2370),
                           labels = c(1, 2, 3)))

#Splitting data into training and test set
#--------------------------------------------------------
library(caTools)
set.seed(122)
split <- sample.split(dataset$AU,SplitRatio = 0.80)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)
#--------------------------------------------------------

#feature scaling
training_set <- data.frame(scale(training_set))
test_set <- data.frame(scale(test_set))

#Fitting Multiple linear regression
#--------------------------------------------------------
#building on training set
regressor <- lm(formula = AU ~ .,
                data = training_set)

#predicting on test set
y_pred <- predict(regressor,
                  newdata = test_set)

#Backwards regression algorithm
#-------------------------------------------------------

regressor <- lm(formula = AU~.,
                data = dataset) #use dataset for complete analysis of variables

regressor <- lm(formula = AU ~ LOCATIONX + LOCATIONY + ROCKTYPE,
                data = dataset)

#testing results of regression model
randsamp <- dataset[sample(nrow(dataset),10),] #select 10 random samples
randsamp2 <- randsamp
randsamp2[4]<- NULL #clear AU columns
namevector <- "AU" #create new column for AU
randsamp2[,namevector]<- NA #make AU empty

#predict results
au_pred <- predict(regressor,
                   newdata = randsamp2)

#calculating RMSE
rmse <- function(error)
{
  sqrt(mean(error^2))
}
error <- randsamp$AU - au_pred
rmse(error)
