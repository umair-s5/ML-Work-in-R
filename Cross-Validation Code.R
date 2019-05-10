
########################################################
#################### Holdout Method ####################
########################################################

##################################
############ R code 1 ############
##################################

# import the CollegesNew dataset
colleges <- read.csv("C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/CollegesNew.csv",
  header = TRUE
)


##### setting a seed

# set a seed for reproducible results whenever generating random numbers
set.seed(1)  # nothing special about the number 1


# sample() draws random samples -- example unrelated to colleges
sample(1:10, 5, replace = FALSE)  # randomly draw 5 #'s between 1 and 10 w/o rep

sample(1:10, 5, replace = FALSE)  # new set of numbers!


# reset seed at 1
set.seed(1)
sample(1:10, 5, replace = FALSE)  # same as when ran lines 19 then 23 above


# new seed --> different random numbers
set.seed(1000) 
sample(1:10, 5, replace = FALSE)



# I'm resetting the seed -- now you can practice w/ the code above w/o worrying 
# about potentially getting results different from mine
set.seed(1)


# use sample() to randomly sample 1/4 of the observation/row numbers
indices <- sample(nrow(colleges), 1/4*nrow(colleges), replace = FALSE)
indices


# the test set gets the observations w/ these row numbers
test <- colleges[indices, ]


# the training set gets the remaining observations
training <- colleges[-indices, ]


# fit model to the training data only
lm_info <- lm(gradrate ~ math + facratio + public, data = training)

# print the coefficients
lm_info$coefficients  # or use summary(lm_info) -- your choice


# make predictions using fitted model and test data
pred <- predict(object = lm_info, newdata = test)
pred

# calculate RMSE
rmse <- sqrt(mean((test$gradrate - pred)^2))
rmse




#######################################################################
########## k-Fold Cross-Validation for Regression -- 1 Model ##########
#######################################################################

##################################
############ R code 2 ############
##################################

# load the caret package to access the train() function
library(caret)


# import the CollegesNew dataset
colleges <- read.csv("C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/CollegesNew.csv",
  header = TRUE
)


# set a seed for reproducibility of k-fold CV results since the method involves
# random numbers
set.seed(100)


# use train() w/ specific inputs below to run CV
cv_info <- train(gradrate ~ math + facratio + public, data = colleges, 
  method = "lm",    # use "lm" if you want to run linear regression
  trControl = trainControl(method = "cv", number = 5)    # number = # of folds
)


# now the train() output contains some really useful info (e.g., RMSE)
cv_info


# fit the final model to ALL the data if you're happy w/ the CV results (and
#   thus that model)
lm_info <- lm(gradrate ~ math + facratio + public, data = colleges)


# use summary(lm_info) or lm_info$coefficients to obtain the coefficients
lm_info$coefficients




###################################################################
############ Leave-One-Out Cross-Validation -- 1 Model ############
###################################################################

##################################
############ R code 3 ############
##################################

# load the caret package to access the train() function
library(caret)


# no need to set a seed w/ LOOCV since all combos of training and test data will
#  be used

# use train() w/ method = "LOOCV" to run LOOCV
loocv_info <- train(gradrate ~ math + facratio + public, data = colleges, 
  method = "lm",    # use "lm" if you want to run linear regression
  trControl = trainControl(method = "LOOCV")
)


# look at train() output for RMSE
loocv_info


# fit the final model to ALL the data if you're happy w/ the CV results (and
#   thus that model) -- same code as for k-fold CV
lm_info <- lm(gradrate ~ math + facratio + public, data = colleges)


# use summary(lm_info) or lm_info$coefficients to obtain the coefficients
lm_info$coefficients




#######################################################################
######### k-Fold Cross-Validation for Regression -- 2+ Models #########
#######################################################################

##################################
############ R code 4 ############
##################################

# import the HollywoodMovies dataset
movies <- read.csv("C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/HollywoodMovies.csv",
  header = TRUE
)


# if (1) there are missing values and (2) you choose to leave missing data as 
#   missing (instead of imputing values), then you first need to remove
#   observations that have missing values for any of the predictor or response
#   variables you'll be working with

# load the dplyr package to access the select() function
library(dplyr)


# select all variables (response and predictors) you'll be using
movies2 <- select(movies, WorldGross, Budget, RottenTomatoes, AudienceScore)


# use na.omit() to remove any rows w/ at least one NA
movies_noNA <- na.omit(movies2)


# make sure you set the SAME seed before you use train() for EACH model or 
#   method!!  This guarantees you use the same training and test sets for each 
#   model (or classification method)

# model 1: Budget (B)
set.seed(100)
cv_info_B <- train(WorldGross ~ Budget, data = movies_noNA,  # use new dataset!
  method = "lm",
  trControl = trainControl(method = "cv", number = 5)
)


# model 2: Budget (B) and RottenTomatoes (R)
set.seed(100)
cv_info_BR <- train(WorldGross ~ Budget + RottenTomatoes, data = movies_noNA, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 5)
)


# model 3: Budget (B) and AudienceScore (A)
set.seed(100)
cv_info_BA <- train(WorldGross ~ Budget + AudienceScore, data = movies_noNA, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 5)
)



# compare RMSEs -- variable name for train() output followed by $results$RMSE
cv_info_B$results$RMSE    # model w/ budget
cv_info_BR$results$RMSE   # model w/ budget and rotten tomatoes score
cv_info_BA$results$RMSE   # model w/ budget and audience score


# fit the best of the fitted models to ALL of the data
# you don't have to worry about missing data here, as lm() already excludes any
#   rows w/ missing values for the variables included in the model
lm_info <- lm(WorldGross ~ Budget + AudienceScore, data = movies)

# use summary(lm_info) or lm_info$coefficients to obtain the coefficients
summary(lm_info)




######################################################################
####### k-Fold Cross-Validation for Classification -- 1 Method #######
######################################################################

##################################
############ R code 5 ############
##################################

# load the caret package to access the train() function
library(caret)

# install the rsample and dplyr packages: install.packages(c("rsample", "dplyr"))

# load the rsample package to access the attrition dataset and the dplyr package
#  to access the select() function
library(rsample)
library(dplyr)


# use View() to view a dataset (useful especially if you didn't import it)
View(attrition)


# use select() to select all variables (response AND predictors) you'll be using
# the first argument is the name of the data frame
attrition2 <- select(attrition, Attrition, Age, DistanceFromHome, MonthlyIncome, 
  JobLevel
)


# make sure the qualitative variables are factors
str(attrition2)  # based on the output, we need to change JobLevel to a factor
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


# use train() w/ specific inputs below to run CV
cv_info_KNN <- train(Attrition ~ Age + DistanceFromHome + MonthlyIncome + JobLevel, 
  data = attrition_noNA,  # use newest dataset!
  method = "knn",    # specify the classification method you want to run
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = data.frame(k = 7)  # specify number of nearest neighbors for k-NN
)


# look at the train() output to obtain the accuracy
cv_info_KNN  # accuracy = 0.8259 for k-NN w/ k = 7 nearest neighbors


# use confusionMatrix() to find the confusion matrix if you want to calculate
#  other predictive metrics
confusionMatrix(cv_info_KNN)


# examples of other predictive metrics -- see help documentation for more
# %'s are output instead of frequencies -- not a problem, though

# sensitivity = true positive rate
1.1/(1.1 + 15.0)    # YIKES -- this is awful and might be VERY problematic!!

# specificity = true negative rate
81.5/(81.5 + 2.4)   # solid!



# there's no final model to fit since it's k-NN; if you want to make predictions,
#  do what we did in the past (use train()) and use the k (number of nearest 
#  neighbors) that leads to the highest accuracy -- note that train() tells you 
#  the optimal k, but various k's may yield similar results


# if you want to play around w/ different values of the nearest neighbors (k), 
#  you can do so by changing the tuneGrid input for k
set.seed(100)
cv_info <- train(Attrition ~ Age + DistanceFromHome + MonthlyIncome + JobLevel, 
  data = attrition_noNA,
  method = "knn",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(k = seq(1, sqrt(nrow(attrition_noNA)) + 5, by = 2))
)


# print train() results using various numbers of nearest neighbors
cv_info  # optimal k = 31




########################################################################
####### k-Fold Cross-Validation for Classification -- 2+ Methods #######
########################################################################

##################################
############ R code 6 ############
##################################

### compare classification methods when NO parameter tuning is being done
###########################################################################

# load the rsample package to access the attrition dataset, the dplyr package
#  to access the select() function, and the caret package to access the train() 
#  function
library(rsample)
library(dplyr)
library(caret)


# use select() to select all variables (response AND predictors) you'll be using
attrition2 <- select(attrition, Attrition, Age, DistanceFromHome, MonthlyIncome, 
  JobLevel
)


# make sure the qualitative variables are factors
str(attrition2)
attrition2$JobLevel <- as.factor(attrition2$JobLevel)


# use na.omit() to remove any rows w/ at least one NA
# there aren't any here, but this is something you'll need to know just in case
attrition_noNA <- na.omit(attrition2)


# make sure you set the SAME seed before using train() for EACH classification
#  method!! --> guarantees the same training and test sets are used each time


# method 1: k-NN w/ k = 7
set.seed(100)
cv_info_KNN <- train(Attrition ~ Age + DistanceFromHome + MonthlyIncome + JobLevel, 
  data = attrition_noNA,  # use newest dataset!
  method = "knn",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = data.frame(k = 7)
)


# method 2: binary logistic regression
set.seed(100)
cv_info_LR <- train(Attrition ~ Age + DistanceFromHome + MonthlyIncome + JobLevel, 
  data = attrition_noNA,
  method = "glm",
  family = "binomial",  # include this for binary logistic regression
  trControl = trainControl(method = "cv", number = 5)
)


# method 3: LDA
set.seed(100)
cv_info_LDA <- train(Attrition ~ Age + DistanceFromHome + MonthlyIncome + JobLevel, 
  data = attrition_noNA,
  method = "lda",
  trControl = trainControl(method = "cv", number = 5),
  preProcess = c("BoxCox", "center", "scale")
)


# method 4: QDA
set.seed(100)
cv_info_QDA <- train(Attrition ~ Age + DistanceFromHome + MonthlyIncome + JobLevel, 
  data = attrition_noNA,
  method = "qda",
  trControl = trainControl(method = "cv", number = 5),
  preProcess = c("BoxCox", "center", "scale")
)


# method 5: Naive Bayes -- remember there can be issues when using train() for
#  Naive Bayes -- depends on the data
# the tuneGrid arguments below --> assume each quantitative predictor is normal
set.seed(100)
cv_info_NB <- train(Attrition ~ Age + DistanceFromHome + MonthlyIncome + JobLevel, 
  data = attrition_noNA,
  method = "nb",
  trControl = trainControl(method = "cv", number = 5),
  preProcess = c("BoxCox", "center", "scale"),
  tuneGrid = data.frame(fL = 0, usekernel = FALSE, adjust = 1)
)  # warnings (not errors!) common when using train() for Naive Bayes


# compare accuracies -- variable name for train() output followed by 
#  $results$Accuracy
cv_info_KNN$results$Accuracy  # k-NN w/ k = 7
cv_info_LR$results$Accuracy   # logistic regression
cv_info_LDA$results$Accuracy  # LDA
cv_info_QDA$results$Accuracy  # QDA
cv_info_NB$results$Accuracy   # Naive Bayes


# if you want to then make a prediction for a new observation (a made up employee 
#  here), use predict()
predict(cv_info_KNN, newdata = data.frame(Age = 30, DistanceFromHome = 20, 
  MonthlyIncome = 4000, JobLevel = "2")
)




##################################
############ R code 7 ############
##################################

##### optimize k for k-NN and then compare classification methods
####################################################################

# load the rsample package to access the attrition dataset, the dplyr package
#  to access the select() function, and the caret package to access the train() 
#  function
library(rsample)
library(dplyr)
library(caret)


# use select() to select all variables (response AND predictors) you'll be using
attrition2 <- select(attrition, Attrition, Age, DistanceFromHome, MonthlyIncome, 
  JobLevel
)


# make sure the qualitative variables are factors
str(attrition2)
attrition2$JobLevel <- as.factor(attrition2$JobLevel)


# use na.omit() to remove any rows w/ at least one NA
# there aren't any here, but this is something you'll need to know just in case
attrition_noNA <- na.omit(attrition2)


# use sample() to randomly sample 1/4 of the observation/row numbers
set.seed(100)
indices <- sample(nrow(attrition_noNA), 1/4*nrow(attrition_noNA), replace = FALSE)


# the test set gets the observations w/ these row numbers
test <- attrition_noNA[indices, ]


# the training set gets the remaining observations
training <- attrition_noNA[-indices, ]


# method 1: k-NN w/ various values of k
set.seed(100)  # seed necessary here since running k-fold CV
cv_info_KNN <- train(Attrition ~ Age + DistanceFromHome + MonthlyIncome + JobLevel, 
  data = training,  # use training data here!!!!
  method = "knn",
  trControl = trainControl(method = "cv", number = 5),
  
  # I chose something I felt was reasonable for the sequence of k's below
  tuneGrid = data.frame(k = seq(1, sqrt(nrow(training)) + 20, by = 2))
)

# optimal k is 25, BUT don't use the accuracy associated w/ it (0.8504) to assess 
#  the predictive ability of the k-NN method -- use the accuracy found below
cv_info_KNN


# estimate the test accuracy of k-NN w/ k = 25 using the test set
predicted_KNN <- predict(object = cv_info_KNN, newdata = test)  # TEST data!
accuracy_KNN <- sum(test$Attrition == predicted_KNN)/nrow(test)


# method 2: binary logistic regression
info_LR <- train(Attrition ~ Age + DistanceFromHome + MonthlyIncome + JobLevel, 
  data = training,  # training data!
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "none")  # holdout method here, not CV
)


# estimate the test accuracy
predicted_LR <- predict(object = info_LR, newdata = test)  # TEST data!
accuracy_LR <- sum(test$Attrition == predicted_LR)/nrow(test)


# method 3: LDA
info_LDA <- train(Attrition ~ Age + DistanceFromHome + MonthlyIncome + JobLevel, 
  data = training,  # training data!
  method = "lda",
  trControl = trainControl(method = "none"),  # holdout method here, not CV
  preProcess = c("BoxCox", "center", "scale")
)


# estimate the test accuracy
predicted_LDA <- predict(object = info_LDA, newdata = test)  # TEST data!
accuracy_LDA <- sum(test$Attrition == predicted_LDA)/nrow(test)


# method 4: QDA
info_QDA <- train(Attrition ~ Age + DistanceFromHome + MonthlyIncome + JobLevel, 
  data = training,  # training data!
  method = "qda",
  trControl = trainControl(method = "none"),  # holdout method here, not CV
  preProcess = c("BoxCox", "center", "scale")
)


# estimate the test accuracy
predicted_QDA <- predict(object = info_QDA, newdata = test)  # TEST data!
accuracy_QDA <- sum(test$Attrition == predicted_QDA)/nrow(test)


# method 5: Naive Bayes
# the tuneGrid arguments below --> assumes each quantitative predictor is normal
info_NB <- train(Attrition ~ Age + DistanceFromHome + MonthlyIncome + JobLevel, 
  data = training,  # training data!
  method = "nb",
  trControl = trainControl(method = "none"),  # holdout method here, not CV
  preProcess = c("BoxCox", "center", "scale"),
  tuneGrid = data.frame(fL = 0, usekernel = FALSE, adjust = 1)
)  # warnings (not errors!) common when using train() for Naive Bayes


# estimate the test accuracy
predicted_NB <- predict(object = info_NB, newdata = test)  # TEST data!
accuracy_NB <- sum(test$Attrition == predicted_NB)/nrow(test)


# compare accuracies
accuracy_KNN  # k-NN w/ k = 25
accuracy_LR   # logistic regression
accuracy_LDA  # LDA
accuracy_QDA  # QDA
accuracy_NB   # Naive Bayes


# fit final k-NN "model" using entire dataset and optimal k (25) if you want to
#  use k-NN to make predictions for any new observations
final_info_KNN <- train(Attrition ~ Age + DistanceFromHome + MonthlyIncome + JobLevel, 
  data = attrition_noNA,  # use ENTIRE dataset here
  method = "knn",
  tuneGrid = data.frame(k = 25),  # optimal k is 25, so let's use that
  
  # force train() to use the entire dataset specified in the data argument
  trControl = trainControl(method = "none")
)

# this will be the input to the object argument in the predict() function if you 
#  want to make a prediction --> 
#  predict(object = final_info_KNN, newdata = [data frame w/ new x values])
final_info_KNN



