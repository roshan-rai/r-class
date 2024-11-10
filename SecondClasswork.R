#Logical Operations
income <- c(100,90,80,105)
years <- c(6,5,4.5,4.9)
income>95
years>5
income > 95 & years>5
#income greater than 95 or income less than 85
income > 95 | income <85
mean(c(90,80,85,70,90,80,70,92,100,100))
#set working directory
setwd("C:/Users/Roshan Rai/OneDrive - University of Oklahoma/Rclass")
#get working directory to get working directory
getwd()
#read data
read.csv(file="SampleCatapult Data.csv", header=TRUE, sep=",")
#store in a variable the 
input.data<-read.csv(file="SampleCatapult Data.csv", header=TRUE, sep=",")
#number of rows
nrow(input.data)
#number of columns
NCOL(input.data)
#top records
head(input.data)
#top 7 records
head(input.data,7)
#bottom records
tail(input.data,10)
?sd
??sd
???sd
?mean
??mean
???mean
help(sd)
sqrt(29)
sqrt(-16)
mean(c(1:15))
sd(c(1:15))
seq(1, 1000, by = 2)
a <- seq(1, 1000, by = 2)
median(a)
median(seq(1, 1000, by = 2))
sum(seq(1, 1000, by = 2))
median(c(seq(1, 1000, by = 2)))
sum(c(seq(1, 1000, by = 2)))
c(seq(1, 1000, by = 2))
my.text <- "MIT5032 learners are awesome. Dr. Kumar is really enjoying interacting with them."

print(my.text)
hello <- "HELLO WORLD!"
print(hello)
print(mean(c(100,200)))
na<- 10
print(na)
print(sd(c(10,12,15)))
issue.sum <- sum(c(45, 24))
print(issue.sum)

twoFind.error <- 30
print(twoFind.error)
print(paste('The result of 3 + 4 is' , 3 + 4 ))   
seq()
addition <- c(5,6,8)
floor(sd(addition)*1000)/1000
q()
quit()
Q()
mean(c(4,6,8))
input.data <- seq(1:5)
input.data > 3
x <- min(c(2,3,0,6,3))
print(x)
seq(5, 7, by = 0.4)
dummy.data <- c(2,3,4 ) 
print(dummy.data)
(dummy.data + 1)[2]
sd(c(3,3,3))
addition <- c(5,6,8)
floor(sd(addition)*1000)/1000
NA <- 10
print(NA)
