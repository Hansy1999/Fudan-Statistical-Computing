rm(list = ls())

### Getting Started with R ###

# Calculator
pi
exp(1)
log(2)
sqrt(pi)

#density of standard normal at value 2
1/sqrt(2*pi) * exp(-2)
dnorm(2)


# Basic graphics
plot(cars, xlab="Speed", ylab="Distance to Stop", 
     main="Stopping Distance for Cars in 1920")


# Powerful seq function
seq(0, 3, 0.5)
x <- seq(0, 3, 0.5)
y = seq(0, 3, 0.5)


# Syntax in R
?Syntax
?Arithmetic
?Logic
?Comparison #relational operators
?Extract #operators on vectors and arrays
?Control #control flow


# Case-sensitive; Predefined symbols
T
F
t
class(t)
g


### Using the R Online Help System ###

# Help search permutation-related topics
help.search("permutation")

x = 1:6
sample(x) #permutation of all elements of x
k=3
sample(x, size=k) #permutation of k elements of x

set.seed(123)
sample(x)
sample(x, size=k)


# Datasets showcase
help(density)
# The Old Faithful geyser data
help(faithful)
d <- density(faithful$eruptions, bw = "sj")
d
plot(d, main="Eruption time in minutes")


# Data list for all available packages
data()


### Functions ###

# The syntax for a function definition is
# function( arglist ) {
#   expr
#   return(value)
# }

# rolls n fair dice and returns the sum
sumdice <- function(n) {
  k <- sample(1:6, size=n, replace=TRUE)
  return(sum(k))
}

sumdice(2)
a <- sumdice(10000)
a / 10000

# An alternative
sumdice <- function(n)
  sum(sample(1:6, size=n, replace=TRUE))

# Add an extra parameter of dice side number
sumdice <- function(n, sides = 6) {
  if (sides < 1) return (0)
  k <- sample(1:sides, size=n, replace=TRUE)
  return(sum(k))
}

sumdice(5) # default 6 sides
sumdice(n=5, sides=4) # 4 sides


### Arrays, Data Frames, and Lists ###
# A matrix is a two dimensional array. 
# A data frame is not a matrix, although it can be represented in a rectangular layout like a matrix. 
# Unlike a matrix, the columns of a data frame may be different types of variables. 
# Arrays contain a single type.

help("iris")
class(iris)
dim(iris)
head(iris)
names(iris)
table(iris$Species)
summary(iris$Species)
w <- iris[[2]] #Column Sepal.Width
mean(w)
# equivalent
w = iris$Sepal.Width
w = iris[,2] # more general, can select multiple elements

library(ggplot2)
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()

ggplot(iris, aes(Sepal.Length, Sepal.Width,
                 color = Species, shape = Species)) + geom_point(size = 2)

### Example 1.13 (ggplot: parallel boxplots and violin plots)

ggplot(iris, aes(Species, Sepal.Length)) + geom_boxplot()
ggplot(iris, aes(Species, Sepal.Length)) + geom_violin()

ggplot(iris, aes(Species, Sepal.Length)) +
  geom_boxplot() + coord_flip()
ggplot(iris, aes(Species, Sepal.Length)) +
  geom_violin() + coord_flip()

attach(iris)
summary(Petal.Length[51:100]) #versicolor petal length
detach(iris) # Good practice!

#########################
# Arrays
# An array is a multiply subscripted collection of a single type of data.
x <- 1:24 # vector
dim(x) <- length(x) # 1 dimensional array
matrix(1:24, nrow=4, ncol=6) # 4 by 6 matrix
x <- array(1:24, c(3, 4, 2)) # 3 by 4 by 2 array
x
x[, ,2]
x[,3,2]

# Matrices
matrix(0, nrow=2, ncol=2)
matrix(c(0, 0, 0, 0), nrow=2, ncol=2)
matrix(0, 2, 2)

matrix(1:8, nrow=2, ncol=4, byrow = FALSE)
matrix(1:8, nrow=2, ncol=4, byrow = T)

# convert the first four columns of the iris data to a matrix
x <- as.matrix(iris[,1:4]) #all rows of columns 1 to 4
mean(x[,2]) #mean of sepal width, all species
mean(x[51:100,3]) #mean of petal length, versicolor

iris3 #50 × 4 × 3 array of iris data

# Lists
# A list is an ordered collection of objects
# The components of a list can be different types
# Lists are more general than data frames


# Wilcoxon test
?rnorm
w <- wilcox.test(rnorm(10), rnorm(10, 2))
w
typeof(w)
w$statistic
w$p.value

# A list of names
a <- matrix(runif(8), 4, 2) #a 4x2 matrix
# column names
dimnames(a) <- list(NULL, c("x", "y"))
a
# both column and row names
dimnames(a) <- list(letters[1:4], c("x", "y"))
a
# another way to assign row names
row.names(a) <- list("NE", "NW", "SW", "SE")
a


### Workspace and Files ###
rm(list = ls())
getwd()

# scan: read data into a vector or list from the console or file
# read.table: a data frame or a matrix, or is csv (comma separated values) format
# read.csv


### Using Packages ###
install.packages("MASS")
library(MASS)
help(package='MASS')




