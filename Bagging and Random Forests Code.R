
#####################################################
###################### Bagging ######################
#####################################################

####################################
############# R code 1 #############
####################################

library(dplyr)         # to access the select() function
library(caret)         # to access the train() function
library(randomForest)  # to access the importance() and varImpPlot() functions

homes <- read.csv("C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/Home_Sales.csv", 
  header = TRUE
)

# select only variables of interest -- all except ZIP code
homes2 <- select(homes, -ZIP)


# transform variables that aren't factors into factors -- not always necessary
#  but safest in case have qualitative variables w/ more than 2 levels
str(homes2)
homes2$AIR <- as.factor(homes2$AIR)
homes2$FIREPLACE <- as.factor(homes2$FIREPLACE)


# remove observations (rows) w/ missing values
homes_noNA <- na.omit(homes2)


# split entire dataset into training and test sets (75/25 split here)
set.seed(1)
indices <- sample(1:nrow(homes_noNA), 0.25*nrow(homes_noNA), replace = FALSE)
test <- homes_noNA[indices, ]
training <- homes_noNA[-indices, ]


# use bagging (= random forest w/ all predictors considered)
set.seed(1)
bag_fit <- train(PRICE ~ ., data = training,    # training data!!
  method = "rf",   # bagging = random forest (rf) w/ mtry = total # predictors
  trControl = trainControl(method = "none"),
  importance = TRUE,  # set to TRUE if you want to examine predictor importance
  ntree = 1000,    # number of trees
  tuneGrid = data.frame(mtry = ncol(training)-1)  # mtry = total # predictors
)

bag_fit$finalModel


# make predictions using B regression trees and the test data
bag_pred <- predict(object = bag_fit, newdata = test)


# calculate RMSE
sqrt(mean((test$PRICE - bag_pred)^2))


# use importance() and/or varImpPlot() to examine the relative importance of 
#  each predictor
importance(bag_fit$finalModel)
varImpPlot(bag_fit$finalModel)



##### use bagging w/ entire dataset to make predictions for future data

# run bagging procedure
bag_fit_final <- train(PRICE ~ ., data = homes_noNA,    # full dataset
  method = "rf",
  trControl = trainControl(method = "none"),
  importance = TRUE,
  ntree = 1000,
  tuneGrid = data.frame(mtry = ncol(training)-1)
)

# then use train() output in the predict() function to make predictions



####################################
############# R code 2 #############
####################################

library(dplyr)         # to access the select() function
library(caret)         # to access the train() function
library(randomForest)  # to access the importance() and varImpPlot() functions

books <- read.csv("C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/AmazonBooks.csv", 
  header = TRUE
)


# select variables of interest only
View(books)
books2 <- select(books, -c(X, Title, Author, Publisher, ISBN.10))


# remember to transform qualitative variables that aren't factors into factors
str(books2)  # no need to transform anything here


# remove observations (rows) w/ missing values
books_noNA <- na.omit(books2)


# split entire dataset into training and test sets (75/25 split here)
set.seed(1)
indices <- sample(1:nrow(books_noNA), 0.25*nrow(books_noNA), replace = FALSE)
test <- books_noNA[indices, ]
training <- books_noNA[-indices, ]


# use bagging (= random forest w/ all predictors considered)
set.seed(1)
bag_fit_books <- train(AmazonPrice.10 ~ ., data = training,    # training data!!
  method = "rf",   # bagging = random forest (rf) w/ mtry = total # predictors
  trControl = trainControl(method = "none"),
  importance = TRUE,  # set to TRUE if you want to examine predictor importance
  ntree = 1000,    # number of trees
  tuneGrid = data.frame(mtry = ncol(training)-1)  # mtry = total # predictors
)

bag_fit_books$finalModel


# calculate accuracy
predicted <- predict(object = bag_fit_books, newdata = test)  # TEST data!
sum(test$AmazonPrice.10 == predicted)/nrow(test)


# use importance() and/or varImpPlot() to examine the relative importance of 
#  each predictor
importance(bag_fit_books$finalModel)
varImpPlot(bag_fit_books$finalModel)



##### use bagging w/ entire dataset to make predictions for future data

# run bagging procedure
bag_fit_books_final <- train(AmazonPrice.10 ~ ., data = books_noNA, # full dataset
  method = "rf",
  trControl = trainControl(method = "none"),
  importance = TRUE,
  ntree = 1000,
  tuneGrid = data.frame(mtry = ncol(training)-1)
)

# then use train() output in the predict() function to make predictions




#####################################################
################### Random Forests ##################
#####################################################

####################################
############# R code 3 #############
####################################

library(dplyr)         # to access the select() function
library(caret)         # to access the train() function
library(randomForest)  # to access the importance() and varImpPlot() functions

homes <- read.csv("C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/Home_Sales.csv", 
  header = TRUE
)

# select only variables of interest -- all except ZIP code
homes2 <- select(homes, -ZIP)


# transform variables that aren't factors into factors -- not always necessary
#  but safest in case have qualitative variables w/ more than 2 levels
str(homes2)
homes2$AIR <- as.factor(homes2$AIR)
homes2$FIREPLACE <- as.factor(homes2$FIREPLACE)


# remove observations (rows) w/ missing values
homes_noNA <- na.omit(homes2)


# split entire dataset into training and test sets (75/25 split here)
set.seed(1)
indices <- sample(1:nrow(homes_noNA), 0.25*nrow(homes_noNA), replace = FALSE)
test <- homes_noNA[indices, ]
training <- homes_noNA[-indices, ]


# use train() w/ method = "rf" for random forests
set.seed(1)
rf_fit <- train(PRICE ~ ., data = training,    # training data
  method = "rf",   # rf = random forest
  trControl = trainControl(method = "none"),
  importance = TRUE,  # set to TRUE if you want to examine predictor importance
  ntree = 1000    # number of trees
  
  # we'll use the default for mtry (# predictors for each tree) when using RFs
)

rf_fit$finalModel


# make predictions using B regression trees and the test data
rf_pred <- predict(object = rf_fit, newdata = test)


# calculate RMSE
sqrt(mean((test$PRICE - rf_pred)^2))


# use importance() and/or varImpPlot() to examine the relative importance of 
#  each predictor
importance(rf_fit$finalModel)
varImpPlot(rf_fit$finalModel)



##### use random forests w/ entire dataset to make predictions for future data

# run random forests procedure
rf_fit_final <- train(PRICE ~ ., data = homes_noNA,    # full dataset
  method = "rf",
  trControl = trainControl(method = "none"),
  importance = TRUE,
  ntree = 1000
)

# then use train() output in the predict() function to make predictions



####################################
############# R code 4 #############
####################################

library(dplyr)         # to access the select() function
library(caret)         # to access the train() function
library(randomForest)  # to access the importance() and varImpPlot() functions

books <- read.csv("C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/AmazonBooks.csv", 
  header = TRUE
)


# select variables of interest only
View(books)
books2 <- select(books, -c(X, Title, Author, Publisher, ISBN.10))


# remember to transform qualitative variables that aren't factors into factors
str(books2)  # no need to transform anything here


# remove observations (rows) w/ missing values
books_noNA <- na.omit(books2)


# split entire dataset into training and test sets (75/25 split here)
set.seed(1)
indices <- sample(1:nrow(books_noNA), 0.25*nrow(books_noNA), replace = FALSE)
test <- books_noNA[indices, ]
training <- books_noNA[-indices, ]


# use train() w/ method = "rf" for random forests
set.seed(1)
rf_fit_books <- train(AmazonPrice.10 ~ ., data = training,    # training data
  method = "rf",   # rf = random forests
  trControl = trainControl(method = "none"),
  importance = TRUE,  # set to TRUE if you want to examine predictor importance
  ntree = 1000     # number of trees
)

rf_fit_books$finalModel


# calculate accuracy
predicted <- predict(object = rf_fit_books, newdata = test)  # TEST data!
sum(test$AmazonPrice.10 == predicted)/nrow(test)


# use importance() and/or varImpPlot() to examine the relative importance of 
#  each predictor
importance(rf_fit_books$finalModel)
varImpPlot(rf_fit_books$finalModel)



##### use random forests w/ entire dataset to make predictions for future data

# run random forests procedure
rf_fit_books_final <- train(AmazonPrice.10 ~ ., data = books_noNA, # full dataset
  method = "rf",
  trControl = trainControl(method = "none"),
  importance = TRUE,
  ntree = 1000
)

# then use train() output in the predict() function to make predictions



