getwd()    # Gets the current directory

# Set the working directory. 
# Note: You may want to change the path/directory  based on where you have posted the files on your computer 
setwd("/Users/naveen/OneDrive - University of Oklahoma/5032/Fall 2021//")


# -----------------------------------------------------------
#
# Script Name:    Assignment 3 - Pointers
# 
# -----------------------------------------------------------

# -----------------------------------------------------------
#
# Question 2   
#
# -----------------------------------------------------------


#------------------------------------------------------------

# Reads the input billing and vital data
billing.data <- read.csv(file = "BillingInfoAssignment.csv", header = T, sep = ",")
vital.data <- read.csv(file = "VitalInfoAssignment.csv", header = T, sep = ",")

# -------------------------------------------------------------------------------------------------
# Question #a
# merge the billing data and vital data by firstname and lastname
# Get only the unique name records from both the datasets
# -------------------------------------------------------------------------------------------------
a.names <- merge(billing.data,vital.data,by.x = c("FirstName","LastName"),by.y = c("First","Last"))
a.names


# --------------------------------------------------------------------------------------------------------------
# Question #b
# merge the billing data and vital data by firstname and lastname
# Get all the records from both the datasets
# --------------------------------------------------------------------------------------------------------------
b.names.all <- merge(billing.data,vital.data,by.x = c("FirstName","LastName"),by.y = c("First","Last"), all = T)
b.names.all

# -------------------------------------------------------------------------------------------------
# Question #c
# extract the records from the dataset a.names where the Insurance starts with the charaters "Se"
# -------------------------------------------------------------------------------------------------

# Solution 1
c.insurance.se <- a.names[substr(a.names$Insurance,1,2) == "Se",]
c.insurance.se


# -------------------------------------------------------------------------------------------------
# Question #d
# mean of diastolic and systolic blood pressure
# -------------------------------------------------------------------------------------------------

d.combined.mean <- mean(cbind(a.names$DiastolicBP,a.names$SystolicBP))
d.combined.mean

# -------------------------------------------------------------------------------------------------
# Question #e
# removing the last row of the dataset
# -------------------------------------------------------------------------------------------------

e.del.last.row <- a.names[-c(nrow(a.names)),]
e.del.last.row


# -------------------------------------------------------------------------------------------------
# Question #f
# -------------------------------------------------------------------------------------------------
# Initialize new column YearVisited with zero
e.del.last.row$YearVisited = 0
# Extract year from the date  
e.del.last.row$YearVisited <- substr(as.Date(e.del.last.row$Date,format = "%m/%d/%y"),1,4)  

e.del.last.row

# -------------------------------------------------------------------------------------------------
# Question #g
# -------------------------------------------------------------------------------------------------
# sorting the dataset by charges  in dollars column in descending order
g.sort <- e.del.last.row[order(e.del.last.row$ChargesInDollars, decreasing = T),]
g.sort

#---------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------
# Question #h
# -------------------------------------------------------------------------------------------------
# Find unique values of the insurance type using the data generated in part g.
h.unique <- unique(g.sort$Insurance)
h.unique
#---------------------------------------------------------------------------------------------------




# -------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------
# Question #j
# -------------------------------------------------------------------------------------------------
# Write R code to create a new field called “ChargesPerVisit” which calculates the
# charges per visit for each patient by dividing the total charges with the number of visits
# for each patient using the dataset generated in part g.
g.sort$ChargesPerVisit <- g.sort$ChargesInDollars/g.sort$PriorVisits
g.sort
# -------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------
# Question #k
# -------------------------------------------------------------------------------------------------
# Write R code to delete the columns – PriorVisits and Opinion in the dataset generated
# in part j. (Don’t use any hard-coded index in the R code)
k.output <- subset(g.sort, select = -c(Opinion, PriorVisits))
k.output

