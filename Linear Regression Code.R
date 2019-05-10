
########################################
############### R code 1 ###############
########################################

# data
advertising <- c(5, 5.5, 5.8, 5.9, 6.3, 6.8, 7, 7.1, 7.6, 8.2, 8.2, 8.6, 8.9, 9, 
                 9.3, 9.7, 10)
sales <- c(105, 108, 122, 115, 136, 112, 120, 145, 155, 137, 176, 180, 161, 189, 
           186, 205, 200)


# use plot() to create a scatterplot
plot(x = advertising, y = sales)


# use pch = 19 --> filled circles --> preferable
# use cex to change the point size (default = 1)
plot(x = advertising, y = sales, pch = 19, cex = 1.5)



########################################
############### R code 2 ###############
########################################

# import data
icecream <- read.csv(
  file = "C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/Icecream.csv",
  header = TRUE
)


# use lm() to fit a linear model
# for the formula argument, it's always y ~ x
lm_info <- lm(BARSOLD ~ TEMP, data = icecream)
lm_info

# use summary() with the lm() output to find additional regression info
summary(lm_info)


# subset only the coefficients (from the lm() output)
coeffs <- lm_info$coefficients
coeffs

# make scatterplot
plot(icecream$TEMP, icecream$BARSOLD, pch = 19, cex.lab = 1.5, cex.axis = 1.5, 
     cex = 2.1, xlab = "Daily High Temperature (degrees Fahrenheit)", 
     ylab = "Number of Ice Cream Bars Sold"
)


# use abline() to add a regression line to a scatterplot
# a = y-intercept, b = slope, lwd changes the line width
abline(a = coeffs[1], b = coeffs[2], lwd = 3)



########################################
############### R code 3 ###############
########################################

# first find the lm() output, then run summary() on the lm() output
lm_info <- lm(BARSOLD ~ TEMP, data = icecream)
lm_info

summary(lm_info)



########################################
############### R code 4 ###############
########################################

### how to make predictions using R
########################################

# Method 1: can manually type the formula: y-hat = y-int + slope*x
75.7789 + 1.1804*70


# Method 2: can subset the lm() output to obtain the coefficients
coeffs <- lm_info$coefficients
coeffs

coeffs[1] + coeffs[2]*70


# Method 3: can use the predict() function
# object takes the lm() output, NOT the summary() output
# newdata must be in the form of a data frame
predict(object = lm_info, newdata = data.frame(TEMP = 70))  # TEMP is the x variable


# can use predict() to make predictions for different inputs all at once 
# (just an FYI)
predict(object = lm_info, newdata = data.frame(TEMP = c(60, 70, 80)))



########################################
############### R code 5 ###############
########################################

# use predict() with interval = "prediction" to find a prediction interval
# note: default confidence level = 95%
predict(object = lm_info, newdata = data.frame(TEMP = 70), interval = "prediction")



########################################
############### R code 6 ###############
########################################

# import data
movies <- read.csv(
  file = "C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/HollywoodMovies.csv",
  header = TRUE, na.strings = ""
)


# use + to separate predictor variables
lm_info <- lm(DomesticGross ~ Budget + RottenTomatoes, data = movies)
summary(lm_info)



########################################
############### R code 7 ###############
########################################

# the values for multiple predictors need to be input to newdata
predict(object = lm_info, newdata = data.frame(Budget = 20, RottenTomatoes = 75))



########################################
############### R code 8 ###############
########################################

# import data
wages <- read.csv(
  file = "C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/Wages.csv",
  header = TRUE
)


# binary predictors don't need to be factors in R, but I suggest making
# them factors to avoid potential errors later on
is.factor(wages$SEX)  # not a problem but safest to make it a factor


# convert SEX into a factor
wages$SEX <- as.factor(wages$SEX)

# can see SEX is now a factor
str(wages)


# use levels() to see the levels of a factor -- note the quotes!
levels(wages$SEX)


# unique() tells you the unique elements of an object
unique(wages$SEX)  # 0, 1 -- we know what these are already (M = 0, F = 1)


# find model coefficients in the usual way
lm_info <- lm(WAGE ~ SEX, data = wages)
summary(lm_info)



########################################
############### R code 9 ###############
########################################

# be very careful when using predict() w/ qualitative variables

# doesn't work bc don't have quotes around 0 (see the levels output in R code 8)
predict(object = lm_info, newdata = data.frame(SEX = 0))

# works now that we've included the quotes
# prediction for males
predict(object = lm_info, newdata = data.frame(SEX = "0"))

# prediction for females
predict(object = lm_info, newdata = data.frame(SEX = "1"))



########################################
############### R code 10 ##############
########################################

# flip coding: 0/1 to 1/0
wages$SEX <- (-1)*wages$SEX + 1


# find model coefficients
lm_info <- lm(WAGE ~ SEX, data = wages)
summary(lm_info)



########################################
############### R code 11 ##############
########################################

# use install.packages() to install the ISLR package if you haven't already

# load the ISLR package into the current R session
# we'll work w/ the Carseats dataset from ISLR
library(ISLR)


# examine the structure of the dataset -- note the variable types
str(Carseats)  # 1 and 2 are NOT the dummy variable values for Urban or US!!


# in the output, notice R adds Yes to the Urban name
lm_info <- lm(Price ~ Urban, data = Carseats)
summary(lm_info)



########################################
############### R code 12 ##############
########################################

# examine levels of the Urban factor variable
levels(Carseats$Urban)


# use contrasts() to find the coding that R uses for dummy variables
contrasts(Carseats$Urban)  # store in urban area? No = 0, Yes = 1


# error bc in R, the levels of the factor are Yes/No, NOT 0/1
predict(object = lm_info, newdata = data.frame(Urban = "0"))


# make sure you're using the levels of the factor
predict(object = lm_info, newdata = data.frame(Urban = "Yes"))  # good



########################################
############### R code 13 ##############
########################################

# make sure a qualitative predictor w/ > 2 levels is a factor!!
# don't forget about the as.factor() function if you need to use it
str(Carseats)  # already a factor -- good


# in the output, notice how the dummy variables are created automatically
# this would NOT have happened if the variable were not a factor
# also notice R adds Good and Medium to the ShelveLoc name
lm_info <- lm(Sales ~ ShelveLoc, data = Carseats)
summary(lm_info)



########################################
############### R code 14 ##############
########################################

# use contrasts() to find the coding that R uses for this dummy variable
contrasts(Carseats$ShelveLoc)



########################################
############### R code 15 ##############
########################################

# need to know the levels for the predict() input below
levels(Carseats$ShelveLoc)


# remember to be careful w/ newdata input to predict()
predict(object = lm_info, newdata = data.frame(ShelveLoc = "Bad"))
predict(object = lm_info, newdata = data.frame(ShelveLoc = "Medium"))
predict(object = lm_info, newdata = data.frame(ShelveLoc = "Good"))



########################################
############### R code 16 ##############
########################################

# import data
bloodpress <- read.table("C:/Users/casem/Google Drive/Drew/Regression and Time Series/Data/bloodpress.txt",
  header = TRUE
)


# run lm() like usual to fit the linear model
lm_info <- lm(BP ~ Age, data = bloodpress)


# make residual plot using plot()
# plot y-hat values on x-axis and residuals on y-axis
# pull fitted values and residuals from lm() output we stored as lm_info
# I suggest using UNfilled circles, especially if there are a lot of points
#   --> don't change pch to pch = 19
plot(x = lm_info$fitted.values, y = lm_info$residuals, 
  xlab = "Fitted Value", ylab = "Residual"
)

# add horizontal line w/ intercept of 0 and slope of 0 to residual plot
abline(a = 0, b = 0, lwd = 3)



########################################
############### R code 17 ##############
########################################

# import data
movies <- read.csv(
  file = "C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/HollywoodMovies.csv",
  header = TRUE, na.strings = ""
)


# run lm() like usual to fit the linear model
lm_info <- lm(DomesticGross ~ Budget + RottenTomatoes, data = movies)


# make residual plot -- note I left the points unfilled
plot(x = lm_info$fitted.values, y = lm_info$residuals, 
  xlab = "Fitted Value", ylab = "Residual"
)

abline(a = 0, b = 0, lwd = 3)



########################################
############### R code 18 ##############
########################################

# run lm() using log(y) and then sqrt(y) transformations instead of just y
lm_info1 <- lm(log(DomesticGross) ~ Budget + RottenTomatoes, data = movies)
lm_info2 <- lm(sqrt(DomesticGross) ~ Budget + RottenTomatoes, data = movies)


# residual plot using the log y transformation
plot(x = lm_info1$fitted.values, y = lm_info1$residuals, 
  xlab = "Fitted Value", ylab = "Residual"
)

abline(a = 0, b = 0, lwd = 3)


# residual plot using the sqrt y transformation
plot(x = lm_info2$fitted.values, y = lm_info2$residuals, 
  xlab = "Fitted Value", ylab = "Residual"
)

abline(a = 0, b = 0, lwd = 3)



