# Week 2 Sample Code


# Sequence from 1 to 200
seq(1,100,2)


# sum
1+2+3

# sum using 'sum' function
sum(1,2,3)

# mean (without c will just output the first element)
mean(1,2,3)

# create a vector using c function
c(1,2,3)

# mean
mean(c(1,2,3))

day <- c('Mon', 'Tue')

# store the value of mean in an object
avg.data <- mean(c(1,2))

# print the value stored in avg.mean
print(avg.data)

# variables can start with letters (lowercase or uppercase)
avg.data

# R is case sensitive
Avg.data
# Error: object 'Avg.data' not found

# Variable name can't start with a number
2avg.data <- 2
# Error: unexpected symbol in "2avg.data"

#As long as variable does not start with number, It will work.
avg2.data <-  mean(c(1,2))
my.data <- mean(c(1,2))

# Special characters not allowed
*my.data <- mean(c(1,2))

# Generate mean of 2 and 3
avg.data <- mean(c(2,3))


# Logical Operations
income <- c(100, 90, 80, 105)
age <-    c(60,  50, 45, 49)
income > 95
age > 50

# T, F, F, F
income > 95 & age > 50


# GPA or experience

gpa <-        c(3.2, 3.3, 2.9 ,4)
experience <- c(5.2, 3,    2,  1)
gpa > 3.3 | experience > 3

##### RStudio ##################################
# Overview of RStudio
# Show windows
# .R file


setwd("/Users/XXXXXXX/")

# Windows
setwd("C:/users/")

# get working directory
getwd()

# Read input data
input.data <- read.csv(file = "SampleCatapult Data.csv", header = TRUE, sep = ",")

# Access number of records
dim(input.data)

# Number of rows
nrow(input.data)

# Number of columns
ncol(input.data)

# Access Column Names
input.data.columns <- colnames(input.data)

# Top few records
head(input.data, 2)

# Bottom few records
tail(input.data)

# Display number of rows
ncol(input.data)

setwd("C:/Users/Roshan Rai/OneDrive - University of Oklahoma/Rclass")
getwd()
#read data
input.data<-read.csv(file="SampleCatapult Data.csv", header=TRUE, sep=",")

#input.data[rows:columns]
input.data[2:5, ]

#extract number in first row, first column
input.data[1,2]
#all values in first column
input.data[,1]
#all values in the second column
input.data[,2]
input.data[15:19,1:3]
#extract rows from index 15-19 and columns (1st and 3rd) because 1 and 3 are selected its not together
input.data[15:19,c(1,3)]
#extract rows for 15,19,29 only on rows and columns for 1 and 15
input.data[c(15,19,29), c(1,15)]
colnames(input.data)
sample.data <- input.data[,-c(2)]
#Extract rows from 15,19 and 20 and columns (1 and 3)
input.data[c(15,19,20), c[1,3]]
input.data$ShooterName
#everything except these two columns
input.data[, c("ShooterName","Shot")] = NULL
#average distance reported by inspector 1
input.data$ShotDistanceInspector1
mean(input.data$ShotDistanceInspector1)
mean(input.data$ShotDistanceInspector1, na.rm=TRUE)
#Overall Summary
summary(input.data)

#last two columns
input.data[,c(ncol(input.data)-1,ncol(input.data))]


setwd("C:/Users/Roshan Rai/OneDrive - University of Oklahoma/Rclass")
getwd()
#read data
beer.data <- read.csv(file="BeerDataExample.csv", header=TRUE, sep=",", header=TRUE)
nrow(beer.data)
barplot(beer.data)
barplot(beer.data$Brand)
labels.data <- c("Bud Light", "Busch", "Michelob", "Coors", "Miller","Natural", "Other")

#summarize data table is getting the count
summary.data <- table(beer.data$Brand)
barplot(summary.data, main="Beer Preference", xlab = "Types of Beer")
#to increase the y limit, to have multiple colors
barplot(summary.data, main="Beer Preference", xlab = "Types of Beer", ylab = "Number of Student", ylim=c(0,100), col=rainbow(7), names.arg = labels.data)
#Pie chart
pie(summary.data)
pie(summary.data, main="Beer Preference", xlab = "Types of Beer", col=rainbow(7), labels = labels.data)

