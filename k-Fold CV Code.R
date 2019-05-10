
######################################################################
##### How to manually run k-fold cross-validation for regression #####
#####                       using a for loop                     #####
######################################################################

# load the CollegesNew dataset
colleges <- read.csv("C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/CollegesNew.csv",
  header = TRUE
)


##### initial stuff
#################################################

# set a seed for reproducibility since k-fold CV involves random numbers
set.seed(100)

k <- 5    # number of folds
rmse <- vector()

# store the regression coefficients for each run; each row = new run
# ncol = number of parameters in the regression model (including the intercept)
coeffs_mat <- matrix(nrow = k, ncol = 4)



##### randomly assign observations to k folds
#################################################

# randomly permute the observations/rows
perm_rows <- sample(1:nrow(colleges), nrow(colleges), replace = FALSE)

# divide data into k equally-sized groups (the folds)
folds <- split(perm_rows, as.factor(1:k))  # data structure = a list here



##### for loop -- each iteration uses different 
##### folds for training and test data
#################################################

for(i in 1:k){
  
  # split full dataset into training and test sets
  test <- colleges[folds[[i]], ]
  training <- colleges[-folds[[i]], ]
  
  # fit regression model to training set
  lm_info <- lm(gradrate ~ math + facratio + public, data = training)
  
  # store coefficients
  coeffs_mat[i, ] <- lm_info$coefficients
  
  # residual = actual grad rate (from test set!) - predicted grad rate
  resids <- test$gradrate - predict(object = lm_info, newdata = test)

  # calculate RMSE for each run
  rmse[i] <- sqrt(mean(resids^2))
}



##### results
#################################################

# print regression coefficients for each run
coeffs_mat


# print RMSE for each run
rmse


# find overall RMSE
sqrt(mean(rmse^2))


# fit the final model to full dataset if you're happy w/ the CV results (and
# thus that model)
lm_info <- lm(gradrate ~ math + facratio + public, data = colleges)
lm_info$coefficients  # or summary(lm_info)




###########################################################################
### How to manually run k-fold cross-validation for k-NN classification ###
###                         using a for loop                            ###
###########################################################################

# load the rsample package to access the attrition dataset and the dplyr package
#  to access the select() function
library(rsample)
library(dplyr)


##### initial stuff
#################################################

# use select() to select all variables (response AND predictors) you'll be using
# the first argument is the name of the data frame
attrition2 <- select(attrition, Attrition, Age, DistanceFromHome, MonthlyIncome, 
  JobLevel
)


# make sure the qualitative variables are factors
str(attrition2)
attrition2$JobLevel <- as.factor(attrition2$JobLevel)


# if (1) there are missing values and (2) you choose to leave missing data as 
#   missing (instead of imputing values), then you first need to remove
#   observations that have missing values for any of the predictor or response
#   variables you'll be working with

# use na.omit() to remove any rows w/ at least one NA
# there aren't any here, but this is something you'll need to know
attrition_noNA <- na.omit(attrition2)


# set a seed for reproducibility since k-fold CV involves random numbers
set.seed(100)

k <- 5    # number of folds
accuracy <- vector()  # vector that will be filled w/ accuracies
confusionMats <- list()  # list that will be filled w/ confusion matrices



##### randomly assign observations to k folds
#################################################

# randomly permute the observations/rows
perm_rows <- sample(1:nrow(attrition_noNA), nrow(attrition_noNA), replace = FALSE)

# divide data into k equally-sized groups (the folds)
folds <- split(perm_rows, as.factor(1:k))  # data structure = a list here



##### for loop -- each iteration uses different 
##### folds for training and test data
#################################################

for(i in 1:k){
  
  # split full dataset into training and test sets
  test <- attrition_noNA[folds[[i]], ]
  training <- attrition_noNA[-folds[[i]], ]
  
  # fit regression model to training set
  cv_info_KNN <- train(Attrition ~ Age + DistanceFromHome + MonthlyIncome + JobLevel, 
    data = training,  # use newest dataset!
    method = "knn",
    tuneGrid = expand.grid(k = 7),  # you need to set the # of nearest neighbors
    trControl = trainControl(method = "cv", number = 5)
  )
  
  # store confusion matrices
  confusionMats[[i]] <- confusionMatrix(cv_info_KNN)$table
  
  # store accuracy for each run
  accuracy[i] <- cv_info_KNN$results$Accuracy
}


##### results
#################################################

# print confusion matrix for each run -- recall these are %'s, not frequencies
confusionMats


# print accuracy for each run
accuracy


# find overall accuracy
mean(accuracy)



