
#############################
##### R as a calculator #####
#############################

### arithmetic and other basic operations
###########################################

1 + 2




1+2  # spaces not necessary
7 * 4    # use * for multiplication
7 / 4    # use / for division


# PEMDAS applies
15/3 + 2
15 / (3 + 2)


# use ^ for exponents
10^2
10^2 + 3


# use sqrt() to find the square root
sqrt(9)
16^(1/2)  # also works
16^0.5    # also works



### logarithms and exponentials
#################################

# log defaults to ln
log(10)    # ln(10)


# can change base of log
log(64, base = 2)


# exponential function: e^x
exp(3)    # same as e^3



### scientific notation
###############################

# not all digits shown, but more stored and used
12345678901234


# use "e" for 10^
1e2
2e3
2.5e4



#######################################
#### Storing variables: the basics ####
#######################################

# 10 now assigned to "a"
a <- 10


# print "a"
a

b <- 8
a + b

c <- a + b
c


# you can store and print at the same time
(c <- a + b)


# can overwrite variables -- careful!
c <- c + 2
c



### naming variables
###########################

# longer variable names are ok
myvariable <- 4


# R is **case-sensitive**
myVariable <- 5


# periods and underscores are ok, but not as the first character
my.variable <- 6
my_variable <- 7


# can't start a name w/ a number or symbol
_my_variable <- 9   # error!



##################################
##### Common data structures #####
##################################

# vectors, factors, matrices, and data frames

##### Vectors: the basics
###################################

# one way to create a vector: use the combine function, c()
vec1 <- c(1, 4, 8, 3)
vec1


# can combine vectors and "scalars"
# (a scalar is actually a vector of length 1)
w <- 6
vec2 <- c(vec1, w)
vec2


# order matters
vec3 <- c(w, vec1)
vec3


# can combine vectors
vec4 <- c(9, 10, 11)
vec5 <- c(vec3, vec4)
vec5


# use a colon to create a sequence of numbers w/ increments of 1
1:10


# descending order works
10:1


# doesn't have to be integers; still increments of 1
1.5:6.5
1.5:6.8  # careful


# can store like any vector
x <- 1:20
x


# length() specifies the length of a vector
vec5
length(vec5)
length(6:15)



##### Factors: the basics
###################################

# used w/ categorical data
# generally created from a numeric or a character vector

# using numbers
gender <- c(0, 0, 0, 1, 1, 0)
gender


# use as.factor() to change something (e.g., a vector) into a factor
gender_fac <- as.factor(gender)
gender_fac


# using strings
sex <- c("female", "female", "female", "male", "male", "female")
sex

sex_fac <- as.factor(sex)
sex_fac



##### Matrices: the basics
###################################

# use matrix() to create a matrix
# default is to fill down then over
matrix(data = c(1, 4, 3, 8), nrow = 2, ncol = 2, byrow = FALSE)
matrix(data = c(1, 4, 3, 8), nrow = 2, ncol = 2)  # same thing


# default is fill down 1st
matrix(data = c(1, 4, 3, 8), byrow = TRUE)


# fill over then down
matrix(data = c(1, 4, 3, 8), nrow = 2, ncol = 2, byrow = TRUE)


# dim() gives the dimensions of a matrix
mymat <- matrix(data = c(1, 4, 3, 8, 7, 2), nrow = 3, ncol = 2)
mymat

dim(mymat)


# nrow() gives the number of rows of a matrix
nrow(mymat)


# ncol() gives the number of columns of a matrix
ncol(mymat)



##### Data frames: the basics
###################################

# all variables must be vectors of the same length

# use data.frame() to create a data frame
my_df <- data.frame(
  person = c("Bill", "Joe", "Emma", "Andy"),
  age = c(35, 64, 25, 20),
  sex = c("M", "M", "F", "M")
)

my_df


# use str() to examine the structure of the df
str(my_df)

# note: can click on variable name in environment pane to view df




###############################################
##### Subsetting/indexing data structures #####
###############################################

### Subsetting vectors
##############################

# use [] to subset vectors based on index
x <- c(6, 4, 2, 10, 9)
x

x[2]  # 2nd element of x
x[5]
x[c(1, 4)]  # 1st and 4th elements of x
x[1:4]


# delete elements using [] with negative indices
x[-3]
x    # 3rd element wasn't deleted b/c didn't overwrite x

x2 <- x[-3]
x2

x[-c(1, 4)]    # can remove multiple elements



### Subsetting matrices
#############################

mymat

# still use [] to subset, just w/ 2 dimensions now (rows then columns)

# subset individual elements
mymat[1, 1]
mymat[3, 2]
mymat[3, 4]  # error!


# subset entire column
mymat
mymat[, 1]  # 1st column
mymat[, 2]  # 2nd column


# subset entire row
mymat[1, ]  # 1st row
mymat[2, ]  # 2nd row


# subset multiple columns
mymat
mymat[, 1:2]  # 1st and 2nd columns


# subset multiple rows
mymat
mymat[1:2, ]  # 1st and 2nd rows
mymat[c(1, 3), ]  # 1st and 3rd rows


# use [] w/ negative indices to delete elements
mymat
mymat[, -1]  # delete 1st column
mymat[-1, ]  # delete 1st row

mymat
mymat[-1, -2]  # delete 1st row and 2nd column

mymat
mymat[-c(1, 3), -1]  # delete 1st and 3rd rows, 1st column



### Subsetting data frames
#################################

# use $ w/ variable names to subset variables/columns
my_df
my_df$person    # a factor -- more to come on this soon
my_df$age       # now a numeric vector

my_df
my_df[, 1]    # same as before -- 1st columndi

my_df
my_df[1, ]    # same as before -- 1st row

my_df
my_df[1:3, 2:3]    # still a df


# some matrix functions also work w/ df's
my_df
nrow(my_df)
ncol(my_df)
dim(my_df)


# names() prints the names of the variables
names(my_df)



### Filtering data frames
##############################
my_df

my_df$sex == "M"    # determine if person is male or not
my_df[my_df$sex == "M", ]    # filter all males

my_df
my_df[my_df$age >= 30, ]     # filter all people age 30+


# filter all people between 30 and 50 years old, inclusive
# make sure use single & symbol
my_df
my_df[my_df$age >= 30 & my_df$age <= 50, ]   # & is good here
my_df[my_df$age >= 30 && my_df$age <= 50, ]  # && is bad here

my_df[my_df$age > 70, ]    # not an error; nobody is over 70


# selecting variable(s) using indices
my_df
my_df[, 1:2]   # select 1st 2 variables


# selecting variable(s) using variable name(s)
my_df
my_df[, c("age", "sex")]

my_df
my_df[my_df$sex == "M", c("person", "age")]




###############################
##### Numerical summaries #####
###############################

# measures of central tendency: mean and median
# use mean(), median()

# mtcars is a data set about cars that comes with R
mtcars  
str(mtcars)

mean(mtcars$hp)
median(mtcars$hp)


# measures of spread: standard deviation and variance
# use sd() and var()

sd(mtcars$hp)
var(mtcars$hp)  # recall variance = (sd)^2


# min, max, and range (another measure of spread)
# use min(), max(), and range()
min(mtcars$hp)
max(mtcars$hp)

range(mtcars$hp)  # returns the min and max, not the actual range
max(mtcars$hp) - min(mtcars$hp)  # returns the actual range


# summary() gives the min, first quartile, median, mean, third 
# quartile, and max
summary(mtcars$hp)


### correlation -- used when examining the relationship between 2 
### quantitative variables
# use cor()
cor(mtcars$hp, mtcars$wt)



################################
######## Basic plotting ########
################################

# plot() makes a scatterplot
xvar <- 1:5
yvar <- c(2, 3, 3, 6, 4)

plot(x = xvar, y = yvar)


# data doesn't need to be in separate vectors
plot(x = mtcars$hp, y = mtcars$wt)

# if use plot() and one variable is categorical, then side-by-side 
# boxplots are created -- one for each level of the categorical variable
plot(x = as.factor(mtcars$cyl), y = mtcars$mpg)


# pairs() makes a scatterplot matrix
pairs(mtcars)


# hist() makes a histogram
scores <- c(89, 90, 82, 79, 87, 88, 92, 93)
hist(scores)

mtcars$mpg
hist(mtcars$mpg)


# boxplot() makes a boxplot
boxplot(scores)


# side-by-side boxplots of two different variables
boxplot(mtcars$mpg, mtcars$hp)


# barplot() makes a bar graph (for categorical data!)
year <- c("Fr", "So", "Fr", "Jr", "Fr", "So", "Sr")


# table() counts the frequency of each value
counts <- table(year)
counts

barplot(counts)  # easiest to supply a table to barplot()




##############################
########## Packages ##########
##############################

# 3 "types" of packages: base, recommended, and contributed

# the base R packages: provide essential commands and built-in data sets
# these are available every time you start R
# examples: base, parallel, datasets, stats, graphics, methods, etc.


# recommended packages: come w/ R but need to be loaded when you want to 
#   access them
# examples: MASS, spatial, survival, etc.

# contributed packages do not come w/ R and need to be installed 
# (and loaded when you want to access them)
# use install.packages("[package name]") to install a package
# use library([package name]) to load a package
# typically you write the load packages code at the beginning of a script


# there are various ways to update packages:
# (1) use update.packages()
# (2) click the 'Tools' tab, then 'Check for Package Updates'
# (3) click the 'Packages' tab in the Files/Plots/Help pane and click 'Update'
update.packages()


# CRAN = the Comprehensive R Archive Network -- a network of servers
# worldwide that store up-to-date code and help documentation

# for help documentation, it's easiest just to google the package name 
# and click on the PDF from CRAN


# install the rmarkdown and ISLR packages -- only need to install once!
install.packages("rmarkdown")  # quotes necessary when installing
install.packages("ISLR")


# load the rmarkdown and ISLR packages into the current R session
library(rmarkdown)  # quotes not necessary when loading
library(ISLR)




##########################################
##### Importing/loading/reading data #####
##########################################

# use read.table() with .txt files and read.csv() with .csv files

# header, sep, na.strings, and stringsAsFactors are all important arguments 
#   to the read functions -- read.table(), read.csv(), etc.
# note that your file path names will be different than mine!!


### use read.table() to read .txt files

# note: don't separate file name onto multiple lines
auto <- read.table(
  file = "C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/Auto.data.txt"
)
View(auto)  # the variable names are treated as data since header = FALSE (default)


# need header = TRUE since want the variable names to be treated as such
auto2 <- read.table(
  file = "C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/Auto.data.txt", 
  header = TRUE
)
View(auto2)  # better but still have a missing value denoted using ? here


# use na.strings = [something] if there's missing data
# want missing data to be represented by an NA in R
# use na.strings = "?" here since the missing values are denoted using a ?
auto3 <- read.table(
  file = "C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/Auto.data.txt", 
  header = TRUE, na.strings = "?"
)
View(auto3)  # better -- want missing values to be represented using NAs


# use sep = "\t" if missing values are simply left as blank spaces (generally
# works)
health <- read.table(
  file = "C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/MindsetMatters.txt",
  header = TRUE
)  # error bc of missing values -- look at .txt file


health <- read.table(
  file = "C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/MindsetMatters.txt",
  header = TRUE, sep = "\t"
)  # good now

# note: ALWAYS make sure NAs are in the correct spots in the data set after
# you've imported data




### spreadsheets: use read.csv() to read .csv files
# when working w/ spreadsheets, use .csv rather than .xls or .xlsx files
autocsv <- read.csv(
  file = "C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/Auto.csv", 
  header = TRUE
)
View(autocsv)  # good except for missing values


# use na.strings = "?" here since the missing values are denoted using a ?
autocsv2 <- read.csv(
  file = "C:/Users/casem/Google Drive/Drew/Statistical Machine Learning/Data/Auto.csv", 
  header = TRUE, na.strings = "?"
)
View(autocsv2)  # good -- missing data now represented using NAs

dim(autocsv2)



