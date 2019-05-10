
########################################################
################### Ridge Regression ###################
########################################################

##################################
############ R code 1 ############
##################################

library(ISLR)    # to access the Hitters dataset
library(dplyr)   # to access the select() function
library(glmnet)  # to access the cv.glmnet() function


# let's examine the Hitters dataset
str(Hitters)
View(Hitters)


# remove rows w/ missing values -- none here but you WILL need to do this if
# there's missing data
Hitters2 <- na.omit(Hitters)


# randomly sample 1/4 of the observations for the test set
set.seed(5)
indices <- sample(1:nrow(Hitters2), 0.5*nrow(Hitters2), replace = FALSE)


# the test set gets the observations w/ the indices above
test <- Hitters2[indices, ]


# the training set gets the remaining half of the observations
training <- Hitters2[-indices, ]


# the cv.glmnet() requires the response and predictors to (1) be in separate
# objects and (2) be in matrices, not data frames
# use data.matrix() to change a data frame to a matrix
training_y <- data.matrix(select(training, Salary))   # response = Salary
training_x <- data.matrix(select(training, -Salary))  # predictors = all except salary
test_y <- data.matrix(select(test, Salary))
test_x <- data.matrix(select(test, -Salary))


# use cv.glmnet() to find the optimal lambda for ridge reg. and lasso
# alpha = 0 --> ridge regression; alpha = 1 --> lasso
# nfolds = number of CV folds = 10 by default
set.seed(5)
cv_ridge <- cv.glmnet(x = training_x, y = training_y, alpha = 0)


# plot of training MSE based on log(lambda)
plot(cv_ridge)


# subset the optimal lambda
best_lambda <- cv_ridge$lambda.min
best_lambda


# use coef() to find the coefficients of the ridge regression model
# s = lambda
coef(cv_ridge, s = best_lambda)    # optimal lambda found using CV


# now make predictions and calculate the overall RMSE to estimate the test error
pred_ridge <- predict(cv_ridge, s = best_lambda, newx = test_x)


# calculate RMSE for test data using ridge regression
rmse_ridge <- sqrt(mean((pred_ridge - test_y)^2))  # salaries are in $1,000s
rmse_ridge



##### compare results to those found using least squares regression
#####################################################################

### compare the RMSEs
############################

# recall: lambda = 0 --> least squares regression
pred_lm <- predict(cv_ridge, s = 0, newx = test_x)


# calculate RMSE for test data using least squares regression
rmse_lm <- sqrt(mean((pred_lm - test_y)^2))
rmse_lm

rmse_ridge  # ridge performed better


### compare the coefficients
############################

round(
  data.frame(
    LeastSquares = matrix(coef(cv_ridge, s = 0)), 
    Ridge = matrix(coef(cv_ridge, s = best_lambda))
  ), 4
)




#########################################################
####################### The LASSO #######################
#########################################################

##################################
############ R code 2 ############
##################################

library(ISLR)    # to access the Hitters dataset
library(dplyr)   # to access the select() function
library(glmnet)  # to access the cv.glmnet() function


# let's examine the Hitters dataset
str(Hitters)
View(Hitters)


# remove rows w/ missing values
Hitters2 <- na.omit(Hitters)


# set a seed for reproducible results
set.seed(5)

# randomly sample 1/4 of the observations for the test set
indices <- sample(1:nrow(Hitters2), 0.5*nrow(Hitters2), replace = FALSE)


# the test set gets the observations w/ the indices above
test <- Hitters2[indices, ]


# the training set gets the remaining half of the observations
training <- Hitters2[-indices, ]


# the cv.glmnet() requires the response and predictors to (1) be in separate
# objects and (2) be in matrices, not data frames
# use data.matrix() to change a data frame to a matrix
training_y <- data.matrix(select(training, Salary))   # response = Salary
training_x <- data.matrix(select(training, -Salary))  # predictors = all except salary
test_y <- data.matrix(select(test, Salary))
test_x <- data.matrix(select(test, -Salary))


# create a vector of lambdas to try
lambdas <- 10^seq(-4, 4, length.out = 100)


# use cv.glmnet() to find the optimal lambda for ridge reg. and lasso
# alpha = 0 --> ridge regression; alpha = 1 --> lasso
# nfolds = number of CV folds
set.seed(5)
cv_lasso <- cv.glmnet(x = training_x, y = training_y, alpha = 1, 
  lambda = lambdas, nfolds = 10
)


best_lambda_lasso <- cv_lasso$lambda.min  # subset the optimal lambda
best_lambda_lasso


# use coef() to find the coefficients of the ridge regression model 
coef(cv_lasso, s = best_lambda_lasso)    # optimal lambda found using CV


# now make predictions and calculate the overall RMSE to estimate the test error
pred_lasso <- predict(cv_lasso, s = best_lambda_lasso, newx = test_x)


# calculate RMSE for test data using ridge regression
rmse_lasso <- sqrt(mean((pred_lasso - test_y)^2))  # Salary is measured in $1,000s
rmse_lasso



##### compare results to those found using least squares regression
#####################################################################

### FIRST run R Code 1 to obtain the results for least squares regression and
### ridge regression


### compare the RMSEs
############################
rmse_lm
rmse_ridge    # winner
rmse_lasso


### compare the coefficients
############################

round(
  data.frame(
    LeastSquares = matrix(coef(cv_ridge, s = 0)), 
    Ridge = matrix(coef(cv_ridge, s = best_lambda)), 
    LASSO = matrix(coef(cv_lasso))
  ), 4
)



